An introduction to 'castarter' - content analysis starter toolkit for R
================
Giorgio Comai
2017-06-27

<!-- README.md is generated from README.Rmd. Please edit that file -->
Preliminary note (2017-06-27)
-----------------------------

This is the development branch of `castarter`, where the codebase is being completely overhauled.

For a (mostly) functioning version of `castarter`, see the [master branch](https://github.com/giocomai/castarter/).

The new `castarter` aims to:

-   make it possible to run (almost) all of its functions from a web interface (through a Shiny app), allowing to create textual datasets, export them, and conduct basic word frequency analysis virtually without knowing anything about R
-   store parameters used for dataset creation in XML format, making it easy to share the procedure, and allow other users to replicate it
-   automatically generate human-readable log files that detail the procedure
-   have as few dependencies as possible on other packages
-   reducing RAM requirements in order to allow for smooth analysis of relatively big datasets (e.g. 1 million items)
-   facilitate integration with SQL databases
-   facilitate creation of a `docker` image including relevant app for easy deployments, both locally and online

By late 2017, at least some of these features are expected to be implemented, and merged back into the `master` branch.

About the author
----------------

Giorgio Comai is doctoral researcher at the School of Law and Government, Dublin City University, and a member of the Marie Curie ITN network “Post-Soviet tensions”. You can find out more about his work on his [personal website](http://giorgiocomai.eu/). He is on Twitter: <https://twitter.com/giocomai>.
