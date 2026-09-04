# Flame

**Flame** is the Soundness REPL: an interactive, live-highlighted Scala 3 read-eval-print
loop. It was extracted from the [Soundness](https://soundness.dev/) monorepo into its own
project, and now depends on the published Soundness release.

## Installing

```sh
curl -fsSL https://flame.propensive.dev/ | sh
```

installs the `flame` executable for your platform (macOS or Linux, x64 or arm64) into
`~/.local/bin` (or `$FLAME_INSTALL_DIR`), verifying it against the digest published with the
release. Each [GitHub release](https://github.com/propensive/flame/releases) also carries the
per-platform executables (including `flame-windows-x64.exe`), the `flame` polyglot bootstrap
script, and the library jars. The first run fetches flame's externalized dependencies; later runs
start instantly.

## Modules

`core`, `web` and `client` are released to GitHub Releases under `dev.propensive` (as
`flame-core`, `flame-web`, `flame-client`), using the same publishing settings as Soundness.

- **`core`** — the REPL engine. It drives the Scala 3 compiler to evaluate input, so it
  depends on `org.scala-lang:scala3-compiler_3` in addition to its Soundness components.
- **`web`** — an alternative web front-end (`flame.web`), driving the same `core` engine.
- **`client`** — the interactive front-end library: `flame.runClient` and its command dispatch.
  `flame serve <port>` runs a REPL server; bare `flame` (over a per-process UNIX socket) connects.
- **`launcher`** — the invocation point, alone in its own module: just
  `@main def repl = externalize(runClient())`. It depends on `client`/`core`/`web` as **published
  coordinates** (resolved from `~/.ivy2/local`), so Burdock's `externalize` records their jar
  hashes and the repackager turns them into on-demand downloads rather than inlining them (see
  below).
- **`test`** — a [Probably](https://github.com/propensive/probably) test suite.

## Building

```sh
make run                     # publish libs locally, build & start the REPL (no release needed)
make test                    # compile and run the test suite
make flame                   # assemble, repackage with Burdock, emit the `flame` executable
make install                 # copy it to ~/.local/bin
make release VERSION=X.Y.Z   # publish a release to GitHub Releases (see below)
```

The build compiles with the [proscala](https://github.com/propensive/proscala) fork of the Scala
compiler (the toolchain Soundness itself is built with), downloaded on demand from its GitHub
release and cached under `~/.cache/soundness/proscala`.

## Dependencies

Soundness is released as per-component jars on GitHub Releases, each embedding its own POM. Run
`make sync-releases VERSION=0.64.0` in a Soundness checkout to install a release into
`~/.ivy2/local`, from which the build resolves the `dev.propensive:<library>-<component>`
coordinates named in `build.mill`. Flame's `core` module (and so the REPL session classpath) takes
a curated set of components: everything flame's own sources use, plus the everyday Soundness
libraries a REPL user expects to reach through `import soundness.*`.

## Releasing

`make release VERSION=X.Y.Z` (with `flameVersion` in `build.mill` bumped to match) publishes one
GitHub release in two ordered steps: first the three library jars, exactly as published locally,
so that GitHub records the digests Burdock hashed at compile time; then — once those digests are
indexed — the launcher is assembled and repackaged against them, and the script verifies that every
library externalized to this release's URLs before uploading one executable per platform
(`flame-{linux,macos}-{x64,arm64}`, `flame-windows-x64.exe`), the `flame` polyglot bootstrap
script (ziggurat's `Xeq.dispatcher`), and the generated `install.sh` that
`https://flame.propensive.dev/` redirects to.

## Native launcher (Burdock)

The `launcher` module's single source wraps the entry point in [Burdock](https://soundness.dev/)'s
`externalize`, which at compile time records the SHA-256 of every jar on the launcher's classpath
into `META-INF/burdock.deps` and caches the jars under `~/.cache/burdock`. Running

```sh
make flame
```

assembles the launcher and runs `soundness.repackage`, which rewrites the JAR so that every
dependency whose exact bytes are resolvable — on Maven Central (via deps.dev) for third-party
libraries, or as a release asset of the flame, Soundness or proscala repositories — becomes an
on-demand `Burdock-Require` download, while anything unpublished is inlined from the cache. Until
the flame libraries are released, a local `make flame` inlines them; `make release` is what
externalizes them.

## License

Flame is made available under the [Apache 2.0 License](/.github/license.md).
