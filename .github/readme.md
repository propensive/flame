# Flame

**Flame** is the Soundness REPL: an interactive, live-highlighted Scala 3 read-eval-print
loop. It was extracted from the [Soundness](https://soundness.dev/) monorepo into its own
project, and now depends on the published Soundness release.

## Installing

```sh
curl -fsSL https://propensive.dev/flame | sh
```

installs the `flame` executable for your platform (macOS or Linux, x64 or arm64) into
`~/.local/bin` (or `$FLAME_INSTALL_DIR`), verifying it against the digest published with the
release. Each [GitHub release](https://github.com/propensive/flame/releases) also carries the
per-platform executables (including `flame-windows-x64.exe`), the `flame` polyglot bootstrap
script, and the library jars. The first run fetches flame's externalized dependencies; later runs
start instantly.

## The standard command line

Flame is a [Pyrocosm](https://github.com/propensive/pyrocosm) tool, so it shares the command line
every Pyrocosm tool has, alongside its own `serve` and `listen`:

```sh
flame about       # flame's version, its executable, the daemon's pid and uptime, and its config files
flame install     # install shell tab-completions and the manpage (--force overwrites the manpage)
flame quit        # stop the background daemon, and the web REPL if it is serving one
flame --version   # (or -v) just the version
```

Flame reads two [TEL](https://soundness.dev/) files: the project's `.pyrocosm/flame/config.tel`,
found by walking up from the working directory as `.git` is found, and the user's
`~/.config/flame/config.tel` (under `$XDG_CONFIG_HOME` if set), the project's taking priority. Both
accept the keywords documented in `src/client/flame.Workspace.scala` (`set`, `language`,
`classpath`, `port`, `host`, `join`, `create`, `history`), and two more that Pyrocosm reads: a bare
`serve` keeps the web REPL running in the daemon, on `port` (8080 by default), for as long as the
daemon lives — so it is simply there, without a `flame serve` ever being run — and `flame quit`
brings it down. A user file that does just that:

```
tel 1.0
port 8192
serve
```

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
# A release is cut by tagging, not by make: `git tag -s X.Y.Z && git push --tags` (below).
```

The build compiles with the [proscala](https://github.com/propensive/proscala) fork of the Scala
compiler (the toolchain Soundness itself is built with), downloaded on demand from its GitHub
release and cached under `~/.cache/soundness/proscala`.

## Dependencies

Soundness and Pyrocosm are released as per-component jars on GitHub Releases, each embedding its
own POM. The versions are pinned in `etc/refs`; `make sync-deps` installs them into
`~/.ivy2/local`, from which the build resolves the `dev.propensive:<library>-<component>`
coordinates named in `build.mill`. Flame's `core` module (and so the REPL session classpath) takes
a curated set of components: everything flame's own sources use, plus the everyday Soundness
libraries a REPL user expects to reach through `import soundness.*`.

## Releasing

A release is cut by tagging. Bump `flameVersion` in `build.mill` to match, merge it, wait for
CI to go green on that commit, and then:

```sh
git tag -s X.Y.Z && git push --tags
```

The tag fires `.github/workflows/release.yml`, which runs the shared `release.sh` in
[propensive/.github](https://github.com/propensive/.github) — the same script the whole
ecosystem releases with; what flame needs beyond the common path is the four lines in
`etc/release`. Nothing is published until the gates pass: a signed, GitHub-verified tag, a CI
run already green on that exact commit, `flameVersion` equal to the tag, and every dependency
pin a published release. If a later step fails, the release and the tag are both deleted from
origin.

It then publishes in two ordered steps: first the three library jars, exactly as published
locally, so that GitHub records the digests Burdock hashed at compile time; then — once those
digests are indexed — the launcher is assembled and repackaged against them, and the script
verifies that every library externalized to this release's URLs before uploading one executable
per platform (`flame-{linux,macos}-{x64,arm64}`, `flame-windows-x64.exe`), the `flame` polyglot
bootstrap script, and the generated `install.sh` that `https://propensive.dev/flame` redirects
to.

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
the flame libraries are released, a local `make flame` inlines them; a release is what
externalizes them.

## License

Flame is made available under the [Apache 2.0 License](/.github/license.md).
