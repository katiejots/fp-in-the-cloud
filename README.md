fp-in-the-cloud
===============

Functional Programming in the Cloud workshop for Codemania 2014.

Participant Instructions
===============

These instructions may change in the lead up to the workshop, so please check for the latest copy.

If you are using Windows, consider installing a Linux VM on your machine prior to the workshop and working within this. It will make life easier. 

Please ensure you have the prerequisites installed ahead of the workshop session. You do not need to clone this Git repository. You do not need to do anything other than the prerequisites before the workshop.

# Prerequisites 

* Install GHC and Cabal. Make sure you end up with GHC version 7.6 or above and Cabal version 1.18 or above (you can use the commands `cabal update` and `cabal install cabal-install --global` to get the latest version). If you are using Windows, install the [Haskell Platform](http://www.haskell.org/platform). If you are using a \*nix system, just install `ghc` (the Glasgow Haskell Compiler) and `cabal-install` with the package manager for your distribution. 

* Install Git, Ruby, and the [RHC Client tools](https://www.openshift.com/developers/rhc-client-tools-install).

* Install PostgreSQL 9.2 or above (Optional; install if you want to be able to test your app locally. Otherwise, you can push to the cloud and see your results there.)

* Create an [OpenShift Online account](https://www.openshift.com/app/account/new).

# Getting Started

* Retrieve your OpenShift Online account credentials, and/or if you wish, ask Katie for an account on the Codemania instance of OpenShift (Katie will supply the server URL). The Codemania instance will be faster and provide more resources, but will run for the time of the conference only; apps should be migrated from this instance using `rhc snapshot` before Saturday, April 5, after which time they will be deleted.

* Run the command `rhc setup` to configure the RHC client tools to talk to whichever OpenShift instance you wish to use. RHC will use OpenShift Online's servers by default; this can be changed in _$HOME/.openshift/express.conf_.

* Change into the directory in which you wish to do your workshop development work. To create an OpenShift application called _pirategold_ using the Haskell community cartridge and PostgreSQL cartridge, execute the following command:

        rhc app create pirategold http://www.accursoft.com/cartridges/scotty.yml postgresql-9.2

* Change into the newly cloned directory: `cd pirategold`. If something went wrong and the app repository was not cloned, clone it now with the command `rhc git-clone pirategold`.

* Merge in the base code for the application provided in this repository with the following command:

        git pull -s recursive -X theirs git://github.com/codemiller/fp-in-the-cloud.git

* Push the base app to the cloud with the command `git push`. This command will install all the dependencies in your OpenShift gear (container), so will take quite some time to complete.

* Run the following commands to set up a Cabal sandbox and install the dependencies locally.

        cabal sandbox init
        cabal install --only-dependencies

* To run the app locally, start PostgreSQL, change the default database environment variable values in _src/Main.hs_ (ie: 127.0.0.1, 5432, postgres, and '') to match your local PostgreSQL instance, and issue the following command from the root of the repository:

        cabal run 127.0.0.1 4000

Go to [http://localhost:4000](http://localhost:4000) in your browser to view the application.

# Making Changes

* Make your changes to the app code and test them locally. When they look right, add and commit the changes in Git with commands such as `git add -A` and `git commit -m "I changed x because y"`.

* To deploy your code changes to OpenShift, use the command `git push`.

* Use the command `rhc app show` to find the URL for your application, where you can view your changes. 

# Cloud Connections

* If something goes wrong with the application instance running in the cloud, you can use the command `rhc tail` to view the logs and `rhc ssh` to connect to the container (gear) in which your application is running. These commands should be issued from within the _pirategold_ directory, or else you should append `-a pirategold` to the commands to let RHC know to which app to apply them.

* To connect to the PostgreSQL instance running on OpenShift, you can SSH to your application with `rhc ssh` and issue the command `psql`. If you have PostgreSQL installed locally, alternatively you can use port forwarding and access the database as if it were local. To do this, you will need the database credentials; you can view them with the command `rhc app show`. Use commands such as the following to start port forwarding and connect to the DB:

        rhc port-forward
        (open another terminal or put above into the background)
        psql -h 127.0.0.1 -p 5433 -U adminabcabc1 -d pirategold

Notice this command connects on port 5433, rather than the standard 5432. OpenShift will use this port if you already have PostgreSQL running locally on 5432. Substitute whichever port appears in the `rhc port-forward` output for the PostgreSQL service, and the username/password for your instance.

# Running the Tests

The app repository contains some examples and properties, created with `doctest`. Examples begin with `>>>` while properties begin with `prop>`. These serve as documentation and you should also be able to run them (some participants may have trouble with this, depending on their operating system). To do so, issue the following commands in the _pirategold_ directory:

    cabal configure --enable-tests
    cabal build
    cabal test

Alternatively, you may run the tests in a single source file by using `doctest` explicitly. From the _pirategold_ directory:

    doctest -isrc -Wall -fno-warn-type-defaults <filename.hs>

# Workshop Outline

Here is a rough schedule for the afternoon:

* 1.30pm: Introductions
* 1.40pm: Prerequisite check
* 1.45pm: Application intro
* 1.50pm: Haskell basics in GHCI
* 3.20pm: Break
* 3.30pm: Haskell on OpenShift primer
* 4pm: Challenges
* 5.15pm: Showcase and wrap-up 
* 5.30pm: Workshop close

# Challenges

Attempt what appeals to you and ask the instructors for help if you need it.

* Change the phrase listing on the app's index page to show the phrases in upper case.
* Complete the _toPigLatin_ function in  _src/Transform.hs_ and use it to display a [Pig Latin](http://en.wikipedia.org/wiki/Pig_Latin) translation for each phrase on the app's index page.
* Add code to reject empty POST parameters.
* Add code to update phrases.
* Add code to delete phrases.
* Add code to make it possible to GET phrases by id.
* Add the ability to add a photo for each phrase, and have this displayed in the listing.
* Add more DocTest tests to the application.
* Add the Clay Haskell CSS preprocessor to the project and change it to generate CSS with this.
* Add the Aeson JSON library to the project and change the app to be able to represent phrases as JSON.
* Add Fay to the app, and use it to generate some JavaScript.
* Complete your own enhancements to the app.

# Links

General:

* [Hoogle](http://www.haskell.org/hoogle/): Search Haskell libraries by function name or type signature.
* [Hackage](http://hackage.haskell.org/): Search for Haskell libraries. You will find information here about _scotty_, _blaze_, _postgresql-simple_, _doctest_, _Clay_, _HUnit_, _QuickCheck_, _aeson_, _fay_, and much more.
* [Learn You A Haskell](http://learnyouahaskell.com/): Learn about Haskell syntax and functional programming concepts, online for free.
* [Haskell for Web Developers](http://www.stephendiehl.com/posts/haskell_web.html): Gives an overview of how web programming can be done in Haskell.
* [Haskell OpenShift Cartridge (Community Created)](https://github.com/accursoft/Haskell-Cloud)

Tutorials:

* [Brief Scotty Tutorial](http://ocharles.org.uk/blog/posts/2013-12-05-24-days-of-hackage-scotty.html)
* [More In-Depth Scotty Tutorial](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html)
* [Postgresql-Simple Tutorial](http://ocharles.org.uk/blog/posts/2012-12-03-postgresql-simple.html)
* [Brief Blaze Tutorial](http://ocharles.org.uk/blog/posts/2012-12-22-24-days-of-hackage-blaze.html)
* [Another Blaze HTML Tutorial](http://jaspervdj.be/blaze/tutorial.html)

Code examples:

* [Scotty Starter App](https://github.com/scotty-web/scotty-starter)
* [Simple Shoes Scotty App](https://github.com/dalaing/shoes-simple/)
