# Build the invocation-point `launcher` module as a plain (clean, no shell-preamble) assembly JAR.
# NOTE: `launcher` depends on flame-core/-web/-client as PUBLISHED Maven Central coordinates, so this
# only resolves once `make release VERSION=X.Y.Z` (below) has put those jars on Central. Until then,
# build/run the library directly with `make run`.
assembly:
	mill flame.launcher.assembly

# Publish flame's library modules (flame-core, flame-web, flame-client) to Maven Central. Signed,
# via the same Sonatype Central flow as Soundness — see etc/ci/release.sh. Run this BEFORE `make
# flame`, and wait for Central + deps.dev to index the jars, so the repackager can externalize them.
release:
	./etc/ci/release.sh $(VERSION)

# Publish the libraries to the local ~/.ivy2 (config sanity check only — local bytes differ from
# Central, so burdock will NOT externalize a locally-published copy).
publishLocal:
	mill flame.core.publishLocal + flame.web.publishLocal + flame.client.publishLocal

# Repackage the launcher assembly into a self-fetching launcher with Burdock. The
# `burdock.externalize` macro wrapping `flame.repl` (in src/launcher/flame_launcher.scala) has
# already embedded `META-INF/burdock.deps` at compile time; running the repackager rewrites the JAR
# in place so published dependencies (now including flame-core/-web/-client) become on-demand
# `Burdock-Require` URLs (resolved via deps.dev) and unpublished ones are inlined from `~/.cache/burdock`.
flame.jar: assembly
	cp out/flame/launcher/assembly.dest/out.jar flame.jar
	java -cp flame.jar soundness.repackage

flame: flame.jar
	java -Dbuild.executable=flame -jar flame.jar

install: flame
	cp flame ${HOME}/.local/bin/

# Run the REPL locally WITHOUT a Central release: publish the libraries to ~/.ivy2 so the launcher's
# published-coordinate deps resolve locally, assemble the launcher, and run it directly (no burdock
# repackage, so the local library jars are simply bundled — externalization needs the real Central
# jars and happens only in `make flame`).
run: publishLocal
	mill flame.launcher.assembly
	java -jar out/flame/launcher/assembly.dest/out.jar

# Build and run the web front-end (serves the REPL on http://localhost:8080/).
web:
	mill flame.web.assembly
	java -jar out/flame/web/assembly.dest/out.jar

# Compile and run the test suite.
test:
	mill flame.test.assembly
	java -cp out/flame/test/assembly.dest/out.jar flame.Tests

dev:
	mill -w flame.client.compile

.PHONY: assembly release publishLocal run web test dev install
