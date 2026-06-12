# Build the REPL client as a plain (clean, no shell-preamble) assembly JAR.
assembly:
	mill flame.client.assembly

# Repackage the assembly into a self-fetching launcher with Burdock. The
# `burdock.Embed.dependencyHashes` macro (in src/client/flame.Package.scala) has
# already embedded `META-INF/burdock.deps` at compile time; running the bootstrapper
# rewrites the JAR in place so published dependencies become on-demand `Burdock-Require`
# URLs (resolved via deps.dev) and unpublished ones are inlined from `~/.cache/burdock`.
repackage: assembly
	cp out/flame/client/assembly.dest/out.jar flame.jar
	java -cp flame.jar burdock.Bootstrapper

# Run the REPL directly from the assembly.
run: assembly
	java -jar out/flame/client/assembly.dest/out.jar

# Compile and run the test suite.
test:
	mill flame.test.assembly
	java -cp out/flame/test/assembly.dest/out.jar flame.Tests

dev:
	mill -w flame.client.compile

.PHONY: assembly repackage run test dev
