#!/usr/bin/env bash
#
# Publish flame to GitHub Releases: the three library jars (flame-core, flame-web, flame-client),
# then the self-fetching `flame` executables built against them. Maven Central is not involved
# (Soundness #1929 switched the whole ecosystem to GitHub Releases).
#
# The assets have a strict order between them — the launcher's repackaged form externalizes each
# library by matching its SHA-256 digest against the release's PUBLISHED assets — so the release
# is made in two steps, exactly as it must be consumed:
#
#   1. The release is created (tagging HEAD) with the three library jars alone: the exact bytes
#      the local publish put on the launcher's compile classpath, so the digests GitHub records
#      are the hashes Burdock computed at compile time.
#   2. Once GitHub reports the assets' digests, the launcher is assembled and repackaged with
#      `--github propensive/flame` among its hints, the script VERIFIES that all three libraries
#      really externalized to this release's URLs (aborting before upload if not), and the
#      resulting executables are added to the same release.
#
# A PUBLISHED release, not a draft: a draft's asset URLs live under an `untagged-…` path that
# changes when the draft is published, which would bake dead URLs into the launcher.
#
# Usage: ./etc/ci/release.sh X.Y.Z   (or `make release VERSION=X.Y.Z`)
# Requires: `gh` authenticated with push access to propensive/flame.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

REPO="propensive/flame"
LIBRARIES="flame-core flame-web flame-client"

VERSION="${1:-}"
if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 X.Y.Z" >&2; exit 1
fi
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "fatal: $VERSION is not of the form X.Y.Z" >&2; exit 1
fi

# The version is the coordinate the launcher resolves (`flameVersion` in build.mill), so a
# release of anything else would disagree with itself. The FLAME_VERSION environment override
# cannot help here: the Mill daemon freezes the build script's `sys.env` (see the note in
# build.mill), so the pin must be edited and committed.
PINNED=$(sed -n 's/.*val flameVersion = sys.env.getOrElse("FLAME_VERSION", "\(.*\)").*/\1/p' build.mill)
if [[ "$PINNED" != "$VERSION" ]]; then
  echo "fatal: build.mill pins flameVersion=$PINNED, not $VERSION; bump and commit first" >&2
  exit 1
fi
if [[ -n "$(git status --porcelain)" ]]; then
  echo "fatal: the working tree is not clean" >&2; exit 1
fi

# Build the libraries from scratch and publish them locally: the launcher's compile classpath
# will hold exactly these bytes, so these are the bytes that must be released.
./mill clean flame >/dev/null
./mill flame.core.publishLocal + flame.web.publishLocal + flame.client.publishLocal

STAGING=$(mktemp -d)
declare -A LOCAL_DIGEST
for lib in $LIBRARIES; do
  jar="$HOME/.ivy2/local/dev.propensive/$lib/$VERSION/jars/$lib.jar"
  if [[ ! -f "$jar" ]]; then
    echo "fatal: $jar was not produced" >&2; exit 1
  fi
  cp "$jar" "$STAGING/$lib-$VERSION.jar"
  LOCAL_DIGEST[$lib]=$(shasum -a 256 "$jar" | cut -d' ' -f1)
done

# Step 1: the release, with the libraries alone. `--target` ties the tag to the commit being
# released even if main moves while the launcher builds.
gh release create "$VERSION" --repo "$REPO" --target "$(git rev-parse HEAD)" \
  --title "flame $VERSION" --notes "Uploading…" "$STAGING"/flame-*.jar

# GitHub computes each asset's SHA-256 shortly after upload; Burdock indexes by that digest, so
# wait for them all and confirm each matches the local bytes.
for lib in $LIBRARIES; do
  asset="$lib-$VERSION.jar"
  DIGEST=""
  for i in $(seq 1 60); do
    DIGEST=$(gh api "repos/$REPO/releases/tags/$VERSION" \
      --jq ".assets[] | select(.name == \"$asset\") | .digest // \"\"" 2>/dev/null || true)
    [[ -n "$DIGEST" ]] && break
    sleep 5
  done
  if [[ "$DIGEST" != "sha256:${LOCAL_DIGEST[$lib]}" ]]; then
    echo "fatal: released digest '$DIGEST' for $asset does not match local sha256:${LOCAL_DIGEST[$lib]}" >&2
    exit 1
  fi
  echo "released $asset ($DIGEST)"
done

# Step 2: the launcher, repackaged against the now-published libraries. `clean` first: the
# launcher's dependency is a fixed coordinate, and Mill's cached resolution would not notice a
# fresh publishLocal under the same version.
./mill clean flame.launcher >/dev/null
./mill flame.launcher.assembly
cp out/flame/launcher/assembly.dest/out.jar flame.jar
java -cp flame.jar soundness.repackage \
  --github propensive/flame,propensive/pyrocosm,propensive/soundness,propensive/proscala | tee /tmp/flame-release-repackage.log

# The whole point of the two-step dance: refuse to ship an executable that quietly inlined a
# library instead of referring to the release.
for lib in $LIBRARIES; do
  if ! grep -q "propensive/flame/releases/download/$VERSION/$lib-$VERSION.jar" /tmp/flame-release-repackage.log
  then
    echo "fatal: $lib did not externalize against this release; not uploading" >&2
    exit 1
  fi
done

# One executable per supported platform, all assembled here: producing an Ethereal executable
# is byte-patching a bare runner stub and appending the (platform-independent) repackaged JAR,
# not compilation, so `-Dbuild.target` cross-"builds" every platform from this machine. The
# stubs are fetched from the Soundness `runners` release and digest-verified by the assembler.
PLATFORMS="linux-x64 linux-arm64 macos-x64 macos-arm64 windows-x64"
DIST=$(mktemp -d)
for platform in $PLATFORMS; do
  ext=""; [[ "$platform" == windows-* ]] && ext=".exe"
  java "-Dbuild.executable=$DIST/flame-$platform$ext" "-Dbuild.target=$platform" -jar flame.jar
done
gh release upload "$VERSION" --repo "$REPO" "$DIST"/flame-*

# The `flame` polyglot bootstrap: a small any-shell script (rename to flame.bat/flame.ps1 on
# Windows) embedding each executable's URL and checksum, which downloads the right one,
# verifies it, replaces itself and re-invokes. Built by ziggurat's `Xeq.dispatcher`, run from
# the fat launcher assembly (ziggurat-core is among flame's dependencies for exactly this).
# The manifest needs the executables' digests, which GitHub computes asynchronously — poll as
# for the libraries. If the pinned Soundness predates `dispatcher`, warn and release without.
MANIFEST=$(mktemp)
for i in $(seq 1 60); do
  gh api "repos/$REPO/releases/tags/$VERSION" --jq \
    '.assets[] | select((.name | startswith("flame-")) and (.name | endswith(".jar") | not))
     | .name + "\t" + .browser_download_url + "\t" + (.digest // "" | sub("sha256:"; ""))' \
    > "$MANIFEST"
  grep -qv $'\t$' "$MANIFEST" && ! grep -q $'\t$' "$MANIFEST" && break
  sleep 5
done
sed -i.bak 's/^flame-//; s/\.exe\t/\t/' "$MANIFEST"

SNIPPET=""
if java -cp out/flame/launcher/assembly.dest/out.jar ziggurat.Xeq dispatcher "$DIST/flame" "$MANIFEST"
then
  gh release upload "$VERSION" --repo "$REPO" "$DIST/flame"

  # The install one-liner for the notes: ziggurat's minimal bootstrap (lib/ziggurat/etc/launch
  # — 106 bytes of POSIX shell, base64-armored), pointed at the polyglot script above, so the
  # three layers compose: one-liner -> dispatcher script -> native executable, each download
  # SHA-256-verified. The armored payload is constant; only the URL and hash vary per release.
  SCRIPT_DIGEST=""
  for i in $(seq 1 60); do
    SCRIPT_DIGEST=$(gh api "repos/$REPO/releases/tags/$VERSION" \
      --jq '.assets[] | select(.name == "flame") | .digest // ""' | sed 's/^sha256://')
    [[ -n "$SCRIPT_DIGEST" ]] && break
    sleep 5
  done
  if [[ -n "$SCRIPT_DIGEST" ]]; then
    SNIPPET=$(printf 'Install (any POSIX shell):\n\n```sh\nopenssl base64 -d <<EOF | sh -s -- https://github.com/%s/releases/download/%s/flame %s\nZj1gbWt0ZW1wYDtjdXJsIC1zTG8gJGYgJDF8fHdnZXQgLXFPICRmICQxO2Nhc2UgYG9wZW5zc2wg\nZGdzdCAtc2hhMjU2ICRmYCBpbiAqJDIpY2htb2QgK3ggJGY7ZXhlYyAkZjtlc2Fj\nEOF\n```\n\n' "$REPO" "$VERSION" "$SCRIPT_DIGEST")
  fi
else echo "warning: ziggurat.Xeq has no dispatcher at the pinned Soundness; released without the bootstrap script" >&2
fi

# The installer served from https://flame.propensive.dev/ (`curl -fsSL … | sh`): plain POSIX
# shell, embedding this release's per-platform digests, generated once they are all known and
# attached to the release as `install.sh` — the domain redirects to that asset.
etc/ci/generate-install.sh "$VERSION" > "$DIST/install.sh"
gh release upload "$VERSION" --repo "$REPO" "$DIST/install.sh"

gh release edit "$VERSION" --repo "$REPO" --notes \
  "${SNIPPET}The \`flame\` polyglot bootstrap (a small any-shell script — rename to \`flame.bat\` or \`flame.ps1\` on Windows — which downloads the right executable below, verifies its checksum, replaces itself and re-invokes), one \`flame\` executable per platform, and the \`flame-core\`, \`flame-web\` and \`flame-client\` libraries each executable externalizes, resolving further dependencies from the Soundness and proscala releases and Maven Central on first run."
echo "release $VERSION complete: $LIBRARIES + $(cd "$DIST" && echo flame*)"
