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
# indexed — the repackaged `flame` executables, added to the same release. See etc/ci/release.sh
# for the two-step ordering and its verification.
release:
	./etc/ci/release.sh $(VERSION)

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
	java -cp flame.jar soundness.repackage --github propensive/flame,propensive/soundness,propensive/proscala

flame: flame.jar
	java -Dbuild.executable=flame -jar flame.jar

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

# Compile and run the test suite. Since Soundness 0.65.0 a probably `Suite` has no `main` (a host
# drives it through `invoke`), so `flame.runTests` (src/test/flame_test_main.scala) is the plain-`java`
# entry point, printing one line per test; `fume run -c <jar>` over the same assembly is the full
# experience.
test:
	./mill flame.test.assembly
	java -cp out/flame/test/assembly.dest/out.jar flame.runTests

dev:
	./mill -w flame.client.compile

.PHONY: assembly release publishLocal run web test dev install
