Comparison with other Haskell dependency graphers
=================================================

The `cabal-db` package is more versatile than `cabal-graphdeps`, but
only works with hackage packages, whereas `cabal-graphdeps` also works
with local packages. The `stack dot` command does finer grained
depencency resolution than `cabal-graphdeps` -- namely, `stack dot`
shows the exact dependency structure whereas `cabal-graphdeps` shows
the transitive closure of the exact dependency structure -- but the
`cabal-graphdeps` package lets you override the Haskell tool chain
more easily. You'll probably want to pipe the output of
`cabal-graphdeps` through the Dot program `tred` to eliminate edges
implied by transitivity.

Examples
========

In the simplest case, just tell `cabal-graphdeps` what package to
compute deps for. For example,

    cabal-graphdeps cabal-graphdeps

will produce a graph of all non-`base` dependencies of itself.

On the other hand, a more complicated package called `prattle` might
have local non-Hackage dependencies, need extra Cabal configuration
(e.g. flags), and depend on a special Haskell tool chain (here
HaLVM). For example,

    cabal-graphdeps --cabal halvm-cabal --ghc-pkg halvm-ghc-pkg --add-sources .,hsntp,ssh-hans,ssh-hans-chatter --cabal-config halvm-cabal-sandbox/cabal.config --use-all-global-packages prattle

Run `cabal-graphdeps --help` for a full list of available options.
