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

Configuration
=======

The montage proxy is port configurable (here given the Config handle set to 7078), with riak running on 8087::

    import Network.Riak (defaultClient, connect, disconnect, Client(port), Connection)
    import Data.Conduit.Pool (Pool, createPool)

    import Network.Riak.Montage

    main :: IO ()
    main = do
        mainPool <- createPool
	    (connect $ defaultClient {port = "8087"})
	    disconnect
	    1 -- stripes
	    10 -- timeout
	    300 -- max connections

	let cfg' = cfg { proxyPort = 7078 }

	runDaemon (cfg' :: Config ResObject) mainPool

See montage for how to define resolutions for a resolution object (ResObject), which is also required to start the proxy.

Your client's request pool must connect (not bind) to that port::

    import Data.Conduit.Pool (Pool, createPool)
    import System.ZMQ as ZMQ

    import Network.Riak.MontageClient

    montageZpool <- createPool (do
        s <- ZMQ.socket ctx Req
	ZMQ.connect s "tcp://localhost:7078"
	return s
	) ZMQ.close 1 5 1

    let (bucket, key) = ("u-info", "1")
    resp <- montageGet montageZpool bucket key

See Examples and More for full documentation of client requests.

Examples
=======
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
