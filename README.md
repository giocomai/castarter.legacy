An introduction to ‘castarter’ - content analysis starter toolkit for R
================
Giorgio Comai
2018-10-10

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# `castarter`

`castarter` is designed to make it easy also for relatively
inexperienced users to create a textual dataset from a website, or a
section of a website, keep it up-to-date, and explore it through word
frequency graphs or a web interface that makes it possibe to tag items.

Documentation is available on [`castarter`’s
website](https://giocomai.github.io/castarter).

## Why `castarter`?

Books dedicated to content analysis typically assume that the researcher
has already created, has access or may buy access to a structured
dataset. They may include sections on sampling (Krippendorff 2004;
Riffe, Lacy, and Fico 2005), but they do not debate explicitly how new
datasets can be built. Commercial software packages generally share the
same expectation. In the R ecosystem, there are a number of packages
that can be used to access existing datasets (e.g.
`tm.plugin.lexisnexis`, `gutenbergr`, `manifestoR`) or import into R
textual content from social media through their APIs (e.g. `Rtweet`).

However, there is no package dedicated to getting into R the textual
contents of regular websites and extracting key metadata (date and
title) in order to populate a corpus that can be analysed in R or with
other software packages. ‘castarter - content analysis starter toolkit
for R’ aims to accomplish just that, offering to moderately incompetent
users the opportunity to extract textual contents from websites and
prepare them for content analysis. It allows to export the datasets in a
number of standard formats for further processing and facilitates basic
word frequency analysis in R.

`castarter` will be particularly beneficial for relatively inexperienced
users. Many of the functions it offers will look trivial to expert R
users. Yet, even experienced users will benefit of some of `castarter`’s
convenience functions, including its ability to download and parse pages
systematically (and continue download processes if it has been
interrupted), and to automatically keep a dataset up-to-date.

For further debate on the usefulness of a structured approach to
analysing web contents in the context of area studies, and for some
practical examples of how `castarter` has been used, see:

``` r
citation("castarter")
```

> Comai, Giorgio (2017). Quantitative Analysis of Web Content in Support
> of Qualitative Research. Examples from the Study of Post-Soviet De
> Facto States, *Studies of Transition States and Societies*, 9(1),
> 14-34.
> <http://publications.tlu.ee/index.php/stss/article/view/346/446>.

## What does `castarter` do?

Given a few basic criteria, it allows to:

  - download index pages of a website
  - extract direct links to individual web pages
  - systematically download all selected pages (and making it easy to
    restart the download process if it fails)
  - facilitate the extraction of key metadata (title and date), as well
    as the textual content of all pages
  - store the resulting dataset in a tabular format that can easily be
    used with other R packages (most easily with the `tidytext` package,
    but it can easily be adapted for further analysis with the `tm` and
    `quanteda` packages among others)
  - export the metadata, the full dataset, or a subset in csv, txt, or
    xlsx format, enabling further analysis with other software
  - compress and archive all downloaded html files in a standard format

As parameters for downloading web pages and extracting metadata and text
are stored by default, it is easy to keep the dataset up-to-date with
the dedicated `UpdateDataset()` function (which parses index pages for
new links, downloads them, and adds them to the latest dataset).

`castarter` facilitates basic word frequency analysis with a dedicated
series of functions.

## `castarter` in web interfaces with `shiny` apps

Key `castarter` functionalities are available also through web
interfaces, further facilitating the creation and analysis of datasets.
This is due to make `castarter` usable also to users who are not
familiar with R and are not able to code.

To see them in action, it is suggested to download an example dataset
with the following commands, which will download and store locally all
press releases issued by the Kremlin and available on their website.

``` r
library("castarter")
devtools::install_github(repo = "giocomai/castarterpresidents")
#> Skipping install of 'castarterpresidents' from a github remote, the SHA1 (3c809129) has not changed since last install.
#>   Use `force = TRUE` to force installation

CreateFolders(project = "presidents", website = "kremlin_en")
SaveWebsite(dataset = castarterpresidents::kremlin_en, project = "presidents", website = "kremlin_en")
#> Dataset saved in castarter/presidents/kremlin_en/2018-10-10-presidents-kremlin_en-dataset.rds
```

### `CreateDataset()`

`CreateDataset()` will allow to conduct the whole procedure of
extracting contents from a website directly from a web interface,
without requiring to know R or code.

It is currently not yet functional.

### `AnalyseDataset()`

`AnalyseDataset()` allows to interactively explore a textual dataset, by
creating word frequency time series of terms given by the user (or
compare among different terms when comma-separated). It makes it easy to
explore not only the word frequency, but also the actual contents by
presenting the sentences including a given keyword in an interactive
table below the graph.

Users can choose which datasets to include in the analysis among those
stored locally through an interactive interface.

It may acquire new functionalities (including convenience functions for
sharing the interface), but it is already fully functional.

An example app closely resembling the one produced by `AnalyseDataset()`
is available at the following link:
<https://giocomai.shinyapps.io/Kremlin_en/>

### `ReadAndTag()`

`ReadAndTag()` facilitates reading through the dataset, tagging articles
along different criteria, and filter the available articles either by
keyoword or tag.

This can be used to skim quickly through a dataset, or to conduct
structured qualitative analysis of the dataset. Tags are stored
automatically in the interactive session.

This app is already functional, even if not thoroughly tested (make sure
everything works as expected if you intend to use for bigger projects).

It will be possible to do some basic analysis of the tags within the
app, and to export the tagging in standard formats.

## What will `castarter` do in the foreseable future?

Most functionalities that `castarter` will likely ever do have already
been implemented, in full or in part.

Enhancements to current functions will likely focus on:

  - better `UpdateDataset()` for keeping multiple datasets up to date
  - some heuristics and suggestions for extracting links, metadata, and
    text

Forthcoming releases will likely include fully functional and enhanced
version of the following shiny apps:

    - `CreateDataset()` - to create new textual datasets
    - `AnalyseDataset()` - to conduct basic quantitative content analysis based on word frequency and time series
    - `ReadAndTag()` - to conduct basic qualitative content analysis through tagging

Other planned feature include:

  - automatically generate human-readable and machine-readable log files
    that detail the procedure used to create a dataset

## Installation

`castarter` can easily be installed from GitHub with `devtools`:

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("giocomai/castarter")
library("castarter")
```

To use interactive web interfaces, you will need to have installed on
your system also the `shiny` and `tidytext` packages.

``` r
if(!require("shiny")) install.packages("shiny")
#> Loading required package: shiny
if(!require("tidytext")) install.packages("tidytext")
#> Loading required package: tidytext
```

## Examples of output

Some examples of analysis of media contents conducted with `castarter`
are available on the author’s blog:

  - [Word frequency of ‘Ukraine’, ‘Crimea’, and ‘Syria’ on Russia’s
    First
    Channel](http://www.giorgiocomai.eu/2015/11/03/word-frequency-of-ukraine-crimea-and-syria-on-russias-first-channel/)

## Disclaimer

`castarter` is under active development:

  - some functions may not work as expected;
  - documentation is incomplete (however, most paramaters should be
    self-descriptive);
  - the code has not been optimised for performance.

## About the author

Giorgio Comai is a researcher at OBCT/CCI
(<https://www.balcanicaucaso.org/>) and a data cruncher at EDJNET
(<https://www.europeandatajournalism.eu/>). He has completed his PhD at
the School of Law and Government, Dublin City University. You can find
out more about his work on his [personal
website](http://giorgiocomai.eu/). He is on Twitter:
<https://twitter.com/giocomai>.
