package flame

import soundness.*

// The invocation point, alone in its own build module. `externalize` (from burdock, re-exported
// through `soundness.*`) records the SHA-256 of every jar on THIS module's compile classpath into
// `META-INF/burdock.deps` at compile time. Because the `launcher` module depends on `flame-client`,
// `flame-core` and `flame-web` as PUBLISHED Maven Central artifacts (see `build.mill`), the exact
// jar bytes on the classpath match the published ones, so `soundness.repackage` resolves each via
// deps.dev and rewrites it into an on-demand `Burdock-Require` download rather than inlining its
// classes. The whole command dispatch lives in `flame.runClient` in the (published) `client` module.
@main
def repl(): Unit = externalize(runClient())
