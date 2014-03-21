fp-in-the-cloud
===============

Functional Programming in the Cloud workshop for Codemania 2014.

Participant Instructions
===============

These instructions may change in the lead up to the workshop, so please check for the latest copy.

If you are using Windows, consider installing a Linux VM on your machine prior to the workshop and working within this. It will make life easier. 

Please ensure you have the prerequisites installed ahead of the workshop session.

# Prerequisites 

* Install the [Haskell Platform](http://www.haskell.org/platform). Make sure you have GHC version 7.6 or above and Cabal version 1.18 or above (you can use the command `cabal update` to get the latest version).

* Install Git, Ruby, and the [RHC Client tools](https://www.openshift.com/developers/rhc-client-tools-install).

* Install PostgreSQL 9.2 or above.

# Getting Started

* Run the command `rhc setup` to configure the RHC client tools.

* To create an OpenShift application using the Haskell community cartridge, use the following command:

        rhc app create pirategold http://www.accursoft.com/cartridges/scotty.yml postgresql-9.2 --from-code=http://github.com/codemiller/fp-in-the-cloud.git

* Change into the newly cloned directory: `cd pirategold`. Clone the application using the instructions in the terminal if the clone did not complete successfully.

* Run the following commands to set up a Cabal sandbox and install the dependencies.

        cabal sandbox init
        cabal install --only-dependencies

* To run the app locally start PostgreSQL, change the default database environment variable values in _src/Main.hs_ to match your local PostgreSQL instance, and issue the following command from the root of the repository:

        cabal run 127.0.0.1 4000

