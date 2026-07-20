# Flame

**Flame** is the Soundness REPL: an interactive, live-highlighted Scala 3 read-eval-print
loop. It was extracted from the [Soundness](https://soundness.dev/) monorepo into its own
project, and now depends on the published Soundness release.

## Modules

`core`, `web` and `client` are published to Maven Central under `dev.soundness` (as
`flame-core`, `flame-web`, `flame-client`), using the same publishing settings as Soundness.

- **`core`** — the REPL engine. It drives the Scala 3 compiler to evaluate input, so it
  depends on `org.scala-lang:scala3-compiler_3` in addition to its Soundness components.
- **`web`** — an alternative web front-end (`flame.web`), driving the same `core` engine.
- **`client`** — the interactive front-end library: `flame.runClient` and its command dispatch.
  `flame serve <port>` runs a REPL server; bare `flame` (over a per-process UNIX socket) connects.
- **`launcher`** — the invocation point, alone in its own module: just
  `@main def repl = externalize(runClient())`. It depends on `client`/`core`/`web` as **published
  Maven Central coordinates**, so Burdock's `externalize` records their Central jar hashes and the
  repackager turns them into on-demand downloads rather than inlining them (see below).
- **`test`** — a [Probably](https://github.com/propensive/probably) test suite.

## Building

```sh
make run                     # publish libs locally, build & start the REPL (no Central needed)
make test                    # compile and run the test suite
make release VERSION=X.Y.Z   # publish flame-core/-web/-client to Maven Central (signed)
make flame                   # after a release: build the tiny self-fetching launcher
```

## Dependencies

Flame depends on the exact set of published Soundness components it needs (e.g.
`dev.soundness:coaxial-core`, `harlequin-core`, `ethereal-core`, …) at version `0.61.0`,
rather than the `soundness-all` umbrella — the umbrella pulls in every module, which
introduces top-level name clashes (e.g. a linear-algebra `Vector` shadowing `scala.Vector`)
under flame's `import soundness.*`.

## Native launcher (Burdock)

The `launcher` module's single source wraps the entry point in [Burdock](https://soundness.dev/)'s
`externalize`, which at compile time records the SHA-256 of every jar on the launcher's classpath
into `META-INF/burdock.deps` and caches the jars under `~/.cache/burdock`. Running

```sh
make flame
```

assembles the launcher and runs `soundness.repackage`, which rewrites the JAR so that every
dependency whose exact bytes are resolvable on Maven Central (via deps.dev) — now including
`flame-core`, `flame-web` and `flame-client` — becomes an on-demand `Burdock-Require` download,
while unpublished ones (e.g. the forked `scala3-compiler`) are inlined from the cache. This is why
the libraries must be **released to Central first**: burdock keys on the published jar bytes, so a
locally-built or `publishLocal` copy would be inlined rather than externalized.

## License

Flame is made available under the [Apache 2.0 License](/.github/license.md).
