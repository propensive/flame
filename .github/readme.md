# Flame

**Flame** is the Soundness REPL: an interactive, live-highlighted Scala 3 read-eval-print
loop. It was extracted from the [Soundness](https://soundness.dev/) monorepo into its own
project, and now depends on the published Soundness release.

## Modules

- **`core`** — the REPL engine. It drives the Scala 3 compiler to evaluate input, so it
  depends on `org.scala-lang:scala3-compiler_3` in addition to its Soundness components.
- **`client`** — the interactive front-end, `flame.repl`. `flame serve <port>` runs a REPL
  server; `flame <port>` (or bare `flame`, over a per-process UNIX socket) connects to one.
- **`test`** — a [Probably](https://github.com/propensive/probably) test suite.

## Building

```sh
mill flame.client.assembly   # plain assembly JAR
make run                     # build and start the REPL
make test                    # compile and run the test suite
```

## Dependencies

Flame depends on the exact set of published Soundness components it needs (e.g.
`dev.soundness:coaxial-core`, `harlequin-core`, `ethereal-core`, …) at version `0.61.0`,
rather than the `soundness-all` umbrella — the umbrella pulls in every module, which
introduces top-level name clashes (e.g. a linear-algebra `Vector` shadowing `scala.Vector`)
under flame's `import soundness.*`.

## Native launcher (Burdock)

The build produces a plain assembly; the native, self-fetching launcher is produced
afterwards by [Burdock](https://soundness.dev/). The compile-time macro
`burdock.Embed.dependencyHashes` (invoked in `src/client/flame.Package.scala`) records the
build's dependency hashes as `META-INF/burdock.deps` and caches the dependency JARs under
`~/.cache/burdock`. Running

```sh
make repackage
```

invokes `burdock.Bootstrapper`, which rewrites the JAR so published dependencies are fetched
on demand and unpublished ones are inlined.

> **Note:** the Burdock repackager currently raises a `ZipError` on a full Mill assembly JAR
> (its zip layer has only been exercised against small jars). The build-side wiring is in
> place; repackaging large assemblies needs a fix in Burdock itself.

## License

Flame is made available under the [Apache 2.0 License](/.github/license.md).
