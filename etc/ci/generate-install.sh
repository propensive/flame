#!/bin/sh
# Generates the flame install script for a given release, embedding per-platform digests.
VERSION=$1
BASE="https://github.com/propensive/flame/releases/download/$VERSION"
H() { gh api "repos/propensive/flame/releases/tags/$VERSION" --jq ".assets[] | select(.name==\"$1\") | .digest" | sed 's/sha256://'; }
cat <<EOF
#!/bin/sh

# The flame installer, served from https://flame.propensive.dev/ for:
#
#     curl -fsSL https://flame.propensive.dev/ | sh
#
# Detects the operating system and CPU architecture, downloads the matching \`flame\`
# executable from the GitHub release, verifies its SHA-256 against the digest embedded
# below, and installs it as \`flame\` in ~/.local/bin (or \$FLAME_INSTALL_DIR). POSIX shell
# only; no stdin is read, so piping from curl is safe.
#
# Generated for flame $VERSION by etc/ci/release.sh; the digests are per-release.

set -e

version="$VERSION"
base="$BASE"

case "\$(uname -s)" in
  Darwin) os=macos ;;
  Linux)  os=linux ;;
  *)      echo "flame: unsupported operating system: \$(uname -s)" >&2
          echo "flame: (on Windows, download \$base/flame-windows-x64.exe)" >&2
          exit 1 ;;
esac

case "\$(uname -m)" in
  x86_64|amd64)  arch=x64 ;;
  aarch64|arm64) arch=arm64 ;;
  *)             echo "flame: unsupported architecture: \$(uname -m)" >&2; exit 1 ;;
esac

label="\$os-\$arch"

case "\$label" in
  linux-x64)   expected=$(H flame-linux-x64) ;;
  linux-arm64) expected=$(H flame-linux-arm64) ;;
  macos-x64)   expected=$(H flame-macos-x64) ;;
  macos-arm64) expected=$(H flame-macos-arm64) ;;
  *)           echo "flame: no executable is published for \$label" >&2; exit 1 ;;
esac

url="\$base/flame-\$label"
dir="\${FLAME_INSTALL_DIR:-\$HOME/.local/bin}"
mkdir -p "\$dir"
tmp="\$dir/.flame.download.\$\$"
trap 'rm -f "\$tmp"' EXIT

echo "Downloading flame \$version for \$label..."
if command -v curl >/dev/null 2>&1
then curl -fsSL "\$url" -o "\$tmp"
elif command -v wget >/dev/null 2>&1
then wget -qO "\$tmp" "\$url"
else echo "flame: neither curl nor wget is available" >&2; exit 1
fi

if command -v sha256sum >/dev/null 2>&1
then actual=\$(sha256sum "\$tmp" | cut -d' ' -f1)
elif command -v shasum >/dev/null 2>&1
then actual=\$(shasum -a 256 "\$tmp" | cut -d' ' -f1)
else actual=\$(openssl dgst -sha256 "\$tmp" | sed 's/.* //')
fi

if [ "\$actual" != "\$expected" ]
then
  echo "flame: checksum mismatch for \$url" >&2
  echo "flame:   expected \$expected" >&2
  echo "flame:   received \$actual" >&2
  exit 1
fi

chmod +x "\$tmp"
mv "\$tmp" "\$dir/flame"
trap - EXIT

echo "Installed flame \$version to \$dir/flame"

case ":\$PATH:" in
  *:"\$dir":*) ;;
  *) echo "Note: \$dir is not on your PATH; add it with:"
     echo "    export PATH=\"\$dir:\\\$PATH\"" ;;
esac

echo "The first run fetches flame's dependencies; subsequent runs start instantly."

# ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
# ┃                                                                                    ┃
# ┃  If this script has been PRINTED to your terminal, it has not been run: you are    ┃
# ┃  looking at the installer itself. To download and run it in one step, invoke:      ┃
# ┃                                                                                    ┃
# ┃      curl -fsSL https://flame.propensive.dev/ | sh                                  ┃
# ┃                                                                                    ┃
# ┃  or, if you have already saved it to a file:                                       ┃
# ┃                                                                                    ┃
# ┃      sh install.sh                                                                 ┃
# ┃                                                                                    ┃
# ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
EOF
