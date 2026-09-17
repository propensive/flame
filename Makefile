# Build the invocation-point `launcher` module as a plain (clean, no shell-preamble) assembly JAR.
# `launcher` depends on flame-client (and, through it, flame-core and flame-web) as PUBLISHED
# coordinates resolved from ~/.ivy2/local, so the libraries are published there FIRST — otherwise
# the launcher silently builds against whatever was last published (a release's jars, say, whose
# bytes then externalize to that release's download, and local changes never reach the executable).
# `clean flame.launcher` for the same reason as in `run`: the coordinate is fixed, so Mill's cached
# resolution would not notice the fresh publish.
assembly: publishLocal
	./mill clean flame.launcher
	./mill flame.launcher.assembly

# Publish flame to GitHub Releases: the three library jars first, then — once their digests are
# indexed — the repackaged `flame` executables, added to the same release. See release-launcher.sh
# in propensive/.github (run through etc/shared) for the two-step ordering and its verification.
release:
	./etc/shared release-launcher.sh flame "flame-core flame-web flame-client" $(VERSION)

# Publish the libraries to the local ~/.ivy2 (the launcher resolves them from there; burdock will
# NOT externalize a locally-published copy unless its bytes match a release asset).
publishLocal:
	./mill flame.core.publishLocal + flame.web.publishLocal + flame.client.publishLocal

# Repackage the launcher assembly into a self-fetching launcher with Burdock. The
# `burdock.externalize` macro wrapping `flame.repl` (in src/launcher/flame_launcher.scala) has
# already embedded `META-INF/burdock.deps` at compile time; running the repackager rewrites the JAR
# in place so published dependencies become on-demand `Burdock-Require` URLs and unpublished ones
# are inlined from `~/.cache/burdock`.
#
# Three publication homes are consulted: Maven Central (hashes resolved via deps.dev) for the
# third-party dependencies, and — via the `--github` hints — the release assets of the flame,
# Soundness and proscala repositories, whose per-jar SHA-256 digests the repackager matches against
# the classpath. The Soundness jars synced into `~/.ivy2/local` are the release assets byte-for-byte,
# and the proscala release publishes the same jars its tarball carries, so both the components and
# the fork toolchain externalize; the flame libraries externalize only once released (`make
# release`), and are inlined otherwise. Set GITHUB_TOKEN to lift the API rate limit.
flame.jar: assembly
	cp out/flame/launcher/assembly.dest/out.jar flame.jar
	java -cp flame.jar soundness.repackage --github propensive/flame,propensive/pyrocosm,propensive/soundness,propensive/proscala

# Package the repackaged JAR as a native executable for this machine with the pinned `xeq` builder
# script (fetched into dist/xeq and verified against etc/xeq.tsv).
flame: flame.jar xeq-fetch
	dist/xeq build --jar flame.jar --out flame

# Fetch the pinned `xeq` builder script into dist/xeq.
xeq-fetch:
	./etc/shared xeq-fetch.sh

install: flame
	cp flame ${HOME}/.local/bin/

# Run the REPL locally WITHOUT a release: publish the libraries, assemble the launcher, and run it
# directly (no burdock repackage, so the local library jars are simply bundled).
run: assembly
	java -jar out/flame/launcher/assembly.dest/out.jar

# Build and run the web front-end (serves the REPL on http://localhost:8080/).
web:
	./mill flame.web.assembly
	java -jar out/flame/web/assembly.dest/out.jar

# Compile and run the test suite with fume, which discovers the suite from the assembly named in
# .fume/config.tel (relative to this directory). Extra selection terms go in TESTS, e.g.
# `make test TESTS='tag:repl'`. CI runs the same command (the shared workflow installs the fume
# pinned in etc/tools). `make test-plain` is a fume-less fallback: `flame.runTests` (src/test/flame_test_main.scala) drives `Tests.invoke` in-process, since
# a probably `Suite` has had no `main` of its own since Soundness 0.65.0.
test:
	./mill flame.test.assembly
	fume run -c out/flame/test/assembly.dest/out.jar $(TESTS)

test-plain:
	./mill flame.test.assembly
	java -cp out/flame/test/assembly.dest/out.jar flame.runTests

# Install every library pinned in etc/refs — releases and snapshots alike, transitively —
# into the local ivy repository, as CI does, so the build resolves exactly the pinned jars rather
# than whatever a sibling checkout's `publishLocal` last installed under the same version. A
# snapshot not yet on GitHub is built from the sibling checkout named by the pin's commit.
sync-deps:
	./etc/shared sync-deps.sh

# Install the commands pinned in etc/tools (fume) through their releases' installers.
tools:
	./etc/shared tools.sh

# Publish HEAD's libraries as a snapshot — a `snapshot-<hex>` pre-release named by the filtered
# tree of the commit, at version `<flameVersion>-<hex>` — for a dependent repository to pin in
# its etc/refs before the next release. `LOCAL=1` stages and installs without publishing.
# The last line printed is the pin. See snapshot.sh in propensive/.github.
snapshot:
	./etc/shared snapshot.sh flame "$$(sed -n 's/.*val flameVersion = "\(.*\)".*/\1/p' build.mill)"

# Delete snapshot pre-releases older than DAYS (default 60) days.
snapshot-prune:
	./etc/shared snapshot-prune.sh flame $(DAYS)

dev:
	./mill -w flame.client.compile

.PHONY: xeq-fetch sync-deps tools snapshot snapshot-prune assembly release publishLocal run web test test-plain dev install
