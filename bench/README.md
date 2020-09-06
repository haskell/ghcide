
# Benchmarks

This folder contains two Haskell programs that work together to simplify the
performance analysis of ghcide:

- `exe/Main.hs` - a standalone benchmark runner. Run with `stack exec ghcide-bench`
- `hist/Main.hs` - a Shake script for running the benchmark suite over a set of commits.
  - Run with `stack bench`,
  - Requires a `ghcide-bench` binary in the PATH,
  - Calls `stack` internally to build the project,
  - Driven by the `hist.yaml` configuration file.
    By default it compares HEAD with "master"

Further details available in the module header comments.
