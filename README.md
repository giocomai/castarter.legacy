An introduction to ‘castarter’ - content analysis starter toolkit for R
================
Giorgio Comai
2018-08-27

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Preliminary notes

### 2018-08-27

`castarter` is being completely rewritten in order to implement a number
of enhancements. Currently, it is again possible to use all key
functions for downloading sections of websites, extracting text, and key
metadata.

An example of a script that generates such a dataset is [now
available](https://github.com/giocomai/castarter/blob/master/inst/extdata/R-Script-examples/Kremlin_en.R).

A function that allows to automatically update datasets -
`updateDataset()` - is now functioning, albeit in a relatively limited
set of scenarios.

An example Shiny app that allows to explore the dataset thus created is
now [part of the
package](https://github.com/giocomai/castarter/tree/master/inst/extdata/shiny/DatasetAnalysis)
and [can be seen in action
online](https://giocomai.shinyapps.io/Kremlin_en/).

A Shiny app that allows to read, subset and code datasets for
qualitative content analysis is now available through the
SubsetAndRead() function.

Further documentation and polishing of the code are coming soon.

### 2018-01-14

The new `castarter` aims to:

  - make it possible to run (almost) all of its functions from a web
    interface (through a Shiny app), allowing to create textual
    datasets, export them, and conduct basic word frequency analysis
    virtually without knowing anything about R
  - store parameters used for dataset creation in a format that is
    machine readbale, making it easy to share the procedure, and allow
    other users to replicate it
  - automatically generate human-readable log files that detail the
    procedure
  - have as few dependencies as possible on other packages
  - reducing RAM requirements in order to allow for smooth analysis of
    relatively big datasets (e.g. \>1 million items)
  - facilitate integration with SQL databases

## Examples of output

Some examples of analysis of media contents conducted with `castarter`
are available on the author’s blog:

  - [Word frequency of ‘Ukraine’, ‘Crimea’, and ‘Syria’ on Russia’s
    First
    Channel](http://www.giorgiocomai.eu/2015/11/03/word-frequency-of-ukraine-crimea-and-syria-on-russias-first-channel/)

Interactive examples:

  - [Official websites of South Caucasus
    presidents](https://giocomai.shinyapps.io/SouthCaucasusPresidents/)
  - [Kremlin’s official website - only time
    series](https://giocomai.shinyapps.io/Kremlin_en/)

## Installation

N.B. Please consider that this document illustrates only the most basic
parameters of `castarter` functions. More advanced options can be seen
by running `?nameOfFunction`.

`castarter` is hosted on GitHub. The easiest way to install it on your
system, is to run the following commands in R:

``` r
install.packages("devtools")
devtools::install_github("giocomai/castarter")
```

For detailed install instructions, including references to packages that
need to be preliminary installed on Linux system, as well as advanced
setups (including Docker images), see [install
instructions](./install.md).

Load `castarter` with:

``` r
library("castarter")
```

## Using castarter

`castarter` facilitates downloading and extracting textual contents from
a given section of a website. `castarter` can be used to analyse one
single website, or more websites as part of a project, facilitating
comparison among cases. In order to smoothen replication and prevent
data losses, by default `castarter` stores a copy of downloaded web
pages in html format. To do so, it includes a function that creates the
relevant folder structure, and another function that facilitates
archiving the html files in compressed format.

In `castarter` every dataset is expected to be extracted from a website,
and each website is expected to be part of a project. This is useful to
give a consistent way of storing the materials gathered and to
facilitate comparison among cases, but it does not imply additional
limitations: it is possible to have projects composed of a single
website, and it is possible to make comparisons among websites that are
part of different projects.

\[Readme to be completed\]

## Disclaimer

`castarter` is under active development:

  - some functions may not work as expected;
  - documentation is incomplete (however, most paramaters should be
    self-descriptive);
  - the code has not been optimised.

## About the author

Giorgio Comai is a researcher at OBCT/CCI
(<https://www.balcanicaucaso.org/>) and a data cruncher at EDJNET
(<https://www.europeandatajournalism.eu/>). He has completed his PhD at
the School of Law and Government, Dublin City University. You can find
out more about his work on his [personal
website](http://giorgiocomai.eu/). He is on Twitter:
<https://twitter.com/giocomai>.
