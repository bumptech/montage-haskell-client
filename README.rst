========================
Haskell Client for Montage
========================

Install
=======

Built and tested with ghc 7.4.1

Install the non-hackage dependencies::

    git clone git@github.com:wmoss/StatsWeb.git
    cd StatsWeb && cabal install

    git clone git@github.com:bumptech/riak-haskell-client.git
    cd riak-haskell-client && cabal install

From montage-haskell-client/ execute::

    cabal install

We recommend using a sandbox, hsenv is particularly good.

To setup montage itself, see http://github.com/bumptech/montage

===========
Examples
===========
To setup the examples, first download hprotoc::

    cabal install hprotoc

Then execute::

    cd examples && hprotoc user.proto

You must have montage installed to run the basic proxy which the examples talk with.  In your external montage/ directory::

    cd examples && runhaskell basic_proxy.hs

See github.com/bumptech/montage for more on the montage haskell setup.

To run the examples, in examples/ execute::

    runhaskell Resolution.hs -- a basic last write wins resolution
    runhaskell Delete.hs -- tests a delete after a put
    runhaskell Many.hs -- asserts the identity of put many -> get many
    runhaskell Reference.hs -- a basic, multi-target reference get

More
===========

See the haddock documentation for type-signatures, descriptions, and source of client functions.
