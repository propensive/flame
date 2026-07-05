# Build the REPL client as a plain (clean, no shell-preamble) assembly JAR.
assembly:
	mill flame.client.assembly

# Repackage the assembly into a self-fetching launcher with Burdock. The
# `burdock.externalize` macro wrapping `flame.repl` (in src/client/flame_client.scala) has
# already embedded `META-INF/burdock.deps` at compile time; running the repackager
# rewrites the JAR in place so published dependencies become on-demand `Burdock-Require`
# URLs (resolved via deps.dev) and unpublished ones are inlined from `~/.cache/burdock`.
flame.jar: assembly
	cp out/flame/client/assembly.dest/out.jar flame.jar
	java -cp flame.jar soundness.repackage

flame: flame.jar
	java -Dbuild.executable=flame -jar flame.jar

install: flame
	cp flame ${HOME}/.local/bin/

# Run the REPL directly from the assembly.
run: assembly
	java -jar out/flame/client/assembly.dest/out.jar

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

.PHONY: assembly run web test dev install
