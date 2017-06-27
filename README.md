An introduction to 'castarter' - content analysis starter toolkit for R
================
Giorgio Comai
2017-06-27

<!-- README.md is generated from README.Rmd. Please edit that file -->
Preliminary note (2017-06-27)
-----------------------------

`castarter` is currently under development. Due to updates to some of the packages that `castarter` dependend on (`quanteda` and `tm`), some functions may not work as expected.

All functions needed to create a textual dataset are however fully functional. The easiest way to create a new dataset is to [start from this template](https://gist.github.com/giocomai/2d8e998cd8f85e8bdd857c8d4d99a1c9).

Active development is now proceeding on a [separate development branch](https://github.com/giocomai/castarter/tree/development), where the codebase is being completely overhauled.

The new `castarter` aims to:

-   make it possible to run (almost) all of its functions from a web interface (through a Shiny app), allowing to create textual datasets, export them, and conduct basic word frequency analysis virtually without knowing anything about R
-   store parameters used for dataset creation in XML format, making it easy to share the procedure, and allow other users to replicate it
-   automatically generate human-readable log files that detail the procedure
-   have as few dependencies as possible on other packages
-   reducing RAM requirements in order to allow for smooth analysis of relatively big datasets (e.g. 1 million items)
-   facilitate integration with SQL databases
-   facilitate creation of a `docker` image including relevant app for easy deployments, both locally and online

By late 2017, at least some of these features are expected to be implemented, and merged back into the `master` branch.

Introduction
------------

Books dedicated to content analysis typically assume that the researcher has already created, has access or may buy access to a structured dataset. They may include sections on sampling (Krippendorff 2004; Riffe, Lacy, and Fico 2005), but they do not debate explicitly how new datasets can be built. Commercial software packages generally share the same expectation. In the R ecosystem, there are a number of packages that can be used to access existing datasets (e.g. [tm.plugin.lexisnexis](https://cran.r-project.org/web/packages/tm.plugin.lexisnexis/), [manifestoR](https://cran.r-project.org/web/packages/manifestoR/)) or import into R textual content from social media through their APIs (e.g. [twitteR](https://cran.r-project.org/web/packages/twitteR/)). However, there is no package dedicated to getting into R the textual contents of regular websites and extracting key metadata (date and title) in order to populate a corpus that can be analysed in R or with other software packages. 'castarter - content analysis starter toolkit for R' aims to accomplish just that, offering to moderately incompetent users the opportunity to extract textual contents from websites and prepare them for content analysis. It allows to export the datasets in a number of standard formats for further processing and facilitates basic word frequency analysis in R.

Installation
------------

N.B. Please consider that this document illustrates only the most basic parameters of `castarter` functions. More advanced options can be seen by running `?nameOfFunction`.

`castarter` is hosted on GitHub. The easiest way to install it on your system, is to run the following commands in R:

``` r
install.packages("devtools")
devtools::install_github("giocomai/castarter")
```

For detailed install instructions, including references to packages that need to be preliminary installed on Linux system, as well as advanced setups (including Docker images), see [install instructions](./install.md).

Load `castarter` with:

``` r
library("castarter")
```

Using castarter
---------------

`castarter` facilitates downloading and extracting textual contents from a given section of a website. `castarter` can be used to analyse one single website, or more websites as part of a project, facilitating comparison among cases. In order to smoothen replication and prevent data losses, by default `castarter` stores a copy of downloaded web pages in html format. To do so, it includes a function that creates the relevant folder structure, and another function that facilitates archiving the html files in compressed format.

In `castarter` every dataset is expected to be extracted from a website, and each website is expected to be part of a project. This is useful to give a consistent way of storing the materials gathered and to facilitate comparison among cases, but it does not imply additional limitations: it is possible to have projects composed of a single website, and it is possible to make comparisons among websites that are part of different projects.

In order to explain the functioning of `castarter`, this document will now proceed to show how to extract the textual part of all press releases published on the current version of the European Parliament's website (©European Union, 2010-2016 – Source: European Parliament').[1] As will be shown, `castarter` takes advantage of the fact that most modern content management systems consistently generate URLs and archive pages to present the contents of a website. This document shows only the most basic use of the package; additional parameters are explained elsewhere in the documentation or can be seen by running `?nameOfFunction`.

Prepare the environment
-----------------------

First, define the name of project and the name of website. In this case, the project will be called "EuropeanUnion", assuming other websites to be analysed later may be of other EU institutions.

``` r
SetCastarter(project = "EuropeanUnion", website = "EuropeanParliament")
```

This stores the names of project and website as options that can be retrieved by all `castarter` functions automatically, without the need to inputting them each time a function requires to save or load files.

At this stage, one may consider setting the root working folder, if one does not wish to use R's default, with:

``` r
setwd("/path/to/folder/")
```

This step is of course optional.

Then, it is time to create the folder structure where all outputs will be saved:

``` r
CreateFolders()
```

This creates the following folders in the current working directory:

-   EuropeanUnion
    -   EuropeanParliament
        -   Archives
        -   Dataset
        -   Html
        -   IndexHtml
        -   Logs
        -   Outputs
        -   Txt

Notice that there is no need to provide names of project and website to the function `CreateFolders()`, since they are retrieved from the previously set options.

Download index pages
--------------------

From the home page of the European Parliament, it is easy to reach the main page dedicated to press releases clicking on 'News', and then on 'Press releases' on the top of the page: <http://www.europarl.europa.eu/news/en/news-room/press-release> At the bottom of this page, as it is customary, the user is given the possibility to see older press released by clicking on the 'Next page' link. There is also the possibility to go back in time one page at a time, or jumping directly the oldest press release. Let's have a look at the URL's of these pages. Clicking subsequent times on the 'next page' link, these are the URLs that we obtain:

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=10>

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=20>

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=30>

Clicking on the link to the last available page, at the time of this writing, shows the following URLs, including links to press releases published in 2010:

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=5130>

There is clearly a pattern: each page shows 10 press releases, and as we go back in time, the URLs "tells" the content management system to skip the first x number of press releases before starting. This makes it easy to create the links to these index pages. In `castarter`, this can be done by using the following function:

``` r
indexLinks <- CreateLinks(
    linkFirstChunk = "http://www.europarl.europa.eu/news/en/news-room/press-release?start=",
    startPage = 0,
    endPage = 5560,
    increaseBy = 10)
```

N.B. Another frequently found way for websites to provide access to their archives is by creating pages with dates directly in their URL. If, for example, a website offers its contents in the form of <http://www.example.com/archive/2015-10-22>, then it would be possible to generate the links for all the dates, say, between January 2012 and December 2015, with the following parameters: `CreateLinks(linkFirstChunk = "http://www.example.com/", startDate = "2012-01-01", endDate = "2015-12-31", dateSeparator = "-")`

This command creates a vector (`indexLinks`) including direct links to the index pages. With the following command, it is possible to see the first links thus created:

``` r
head(indexLinks)
#> [1] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=0" 
#> [2] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=10"
#> [3] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=20"
#> [4] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=30"
#> [5] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=40"
#> [6] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=50"
```

It is now necessary to download these index pages. The following command downloads the relevant index pages and saves them in html format in the `/EuropeanUnion/EuropeanParliament/IndexHtml/` folder.

``` r
DownloadContents(links = indexLinks, type = "index")
```

If the process is interrupted for any reason, it is possible simply to run again the same function, which will check which files have already been downloaded, and will download the missing ones. Sometimes, due to a number of reasons, one or a few pages may not download correctly. It is usually appropriate to run the following function, which will re-download files that are too small to contain meaningful contents (if all pages have been downloaded correctly, the following will not do anything).

``` r
DownloadContents(links = indexLinks, type = "index", missingArticles = FALSE)
```

Then use the `ImportHtml` function to import these downloaded files in an R vector for further processing:

``` r
indexHtml <- ImportHtml(from = "index")
```

Download all articles
---------------------

Now that we have the index pages, we need to extract direct links to the individual press releases. There are various ways to do this, but the easiest approach is looking at the URLs of individual articles, and see if there is a pattern (this is very often the case in modern websites). Hovering over the titles of an index page, it is easy to see the links to indivudal news items. In this case, we see that the URL to each news item includes in the URL the following string: "/news/en/news-room/content/". We use this to extract relevant links, and discard all links to other pages or sections of the website scattered around the page that we are not interested in. The following command extracts links to individual articles, and tries to guess the title of the article.

``` r
articlesLinks <- ExtractLinks(domain = "http://www.europarl.europa.eu/",
                              partOfLink = "/news/en/news-room/",
                              indexHtml = indexHtml)
head(articlesLinks)
length(articlesLinks)
```

As expected, this function extracts more than 5000 links to individual pages. In this example, the links are correctly extracted and the titles match the links.

Now that we have direct links, it is easy to download all articles. The following command downloads all articles, stores them in a vector, saves them in .html format in the `/EuropeanUnion/EuropeanParliament/Html/` folder, and notifies of advancement.

``` r
DownloadContents(links = articlesLinks, type = "articles")
```

As for the index files, it is advised to check if all files have been downloaded correctly, and then import them into R for further analysis.

``` r
DownloadContents(links = articlesLinks, type = "articles", missingArticles = FALSE)
articlesHtml <- ImportHtml(from = "articles")
```

N.B. If due to RAM size it proves unfeasible to import all articles in R, it is possible to create a dataset directly from the downloaded html files with `CreateDatasetFromHtml()`. More details with `?CreateDatasetFromHtml`.

Extracting metadata
-------------------

It is now time to extract (or set) basic metadata from the articles: title, date, an ID, and language. We have multiple options to extract the titles (use `?ExtractTitles` to find out more). For examples, titles can be extracted from individual pages, using the metadata embedded in the html page or by extracting the text that has style "heading 1".

``` r
titles <- ExtractTitles(articlesHtml = articlesHtml,
                        articlesLinks = articlesLinks,
                        method = "htmlTitle",
                        removeString =  " | News | European Parliament")
head(titles)

# titles <- ExtractTitles(articlesHtml = articlesHtml,
#                         articlesLinks = articlesLinks,
#                         method = "htmlH1")
```

In this case, both methods seem to function reasonably well.

Extracting the date may be more complex, and `castarter` offers a number of different methods to extract dates. However, in many cases it is enough to provide the format of the date (more details are provided in the documentation of the function, accessible through the command `?ExtractDates`). In the case of the European Parliament, the date is presented in the format day-month-year, and it is extracted correctly using the default settings.

``` r
dates <- ExtractDates(articlesHtml = articlesHtml,
                      dateFormat = "dmY")
```

There may be various criteria to set a unique ID for each article. The default option uses the same number given to the .html file stored in the `/EuropeanUnion/EuropeanParliament/Html` folder.

``` r
articlesId <- ExtractId()
```

The language can be set easily in subsequent steps, but storing it in a dedicated vector makes it easier to use template files.

``` r
language <- "english"
```

We can now store the metadata in a dedicated data.frame, and export them in a spreadsheet for future reference outside of R and `castarter`.

``` r
metadata <- ExportMetadata(dates = dates,
                           articlesId = articlesId,
                           titles = titles,
                           language = language,
                           articlesLinks = articlesLinks, 
                           exportXlsx = TRUE)
```

Metadata are also used to create relevant filenames for storing individual articles in textual format. The following command calls the `boilerpipeR` library and allows to extract only the main contents of each page, removing menus and other html clutter.

``` r
contents <- ExtractTxt(articlesHtml, metadata)
```

Text extraction works well in most cases (options for more advanced users based on xpath are also available, see `?ExtractTxt`). However, at this stage it is generally a good idea to have a quick look at the extracted textual contents and remove easy-to-spot glitches. The following command prints on the terminal five random articles among all those extracted.

``` r
contents[sample(1:length(contents), 5)]
```

The following command saves the current workspace in the working directory, stores the dataset in a dedicated folder for easy retrieval and allows to export both metadata and textual contents in a single spreadsheet (e.g. by enabling the `exportXlsx = TRUE` parameter).

``` r
SaveWebsite(saveEnvironment = TRUE, dataset = TRUE)
```

It is also possible to export all articles as a folder of txt files (with `ExtractTxt(articlesHtml, metadata, export = TRUE)`. Either using those, or the spreadsheet generated by `SaveWebsite`, it will be easy to import the newly created dataset into other software packages for quantitative or qualitative content analysis.

Another option is to export a subset of documents that, for example, include a given word in a single txt file or in a spreadsheet for quick consultation, e.g. export all press releases that mention the word "internet".

``` r
ExportArticlesWith(dataset, term = "internet", txt = TRUE, csv = TRUE, xlsx = TRUE)
```

At this stage, if we are satisified with the data we have, we may want to store all downloaded files in a compressed file and remove the original folders containing the original html files to free space. This way, we still can recover all of the original files, yet we don't have the thousands (or hundreds of thousands) of html files that often are created in a `castarter` project, and may unnecessary slow down the backup process. This can easily be done with the following command.

``` r
ArchiveFolders(removeArchivedFolders = TRUE)
```

All of the above steps are made available [here](https://gist.github.com/giocomai/2d8e998cd8f85e8bdd857c8d4d99a1c9), in a format that can easily be used as a template for creating datasets from other websites.

`castarter` includes functions that make it easier to share datasets (and faciliate replication of analysis) for those that have access to an ftp server, `UploadDatasets()` and `DownloadDatasets()`.

Creating and polishing a corpus
-------------------------------

For further analysis in `castarter`, it is useful to convert the data to a corpus of the `tm` package. Some of the functions offered by `castarter` for processing the corpus are simply wrappers of `tm` functions. At this stage, it is possible to start a completely new R session, and simply load the dataset into the current workspace. All relevant information is effectively stored in the dataset.

``` r
dataset <- LoadDatasets(projectsAndWebsites = "EuropeanUnion/EuropeanParliament")
```

First, it is however advisable to have a quick look at the distribution of the dataset to see if there are oddities that require further attention.

``` r
ShowDistribution(dataset)
```

![](data/readme/README-Show%20distribution-1.png)

N.B. All `castarter` time series functions default to a rolling average of 30 days. This can be changed, for example `ShowDistribution(dataset, rollingAverage = 90)` or `ShowDistribution(dataset, rollingAverage = 1)`

In this case we see that, as is common for many websites, the first few months include only occasional publications To have more consistent results, it might be advisable to exclude that period, and to set a clear cutoff date on 1 June 2016.

``` r
dataset <- SubsetDataset(dataset, startDate = "2011-01-01", endDate = "2017-01-01")
```

N.B. `SubsetDataset` allows also to subset a dataset by keeping only documents that include a given term, e.g. `SubsetDataset(dataset,  terms = "environment")`

Looking at the distribution after subsetting, we still find some irregularities, but they can mostly be related to seasonal variations in the activity of the European Parliament that are not surprising (e.g. less activity in August and around New Year).

``` r
ShowDistribution(dataset)
```

![](data/readme/README-Show%20distribution%20after%20subset-1.png)

We can thus proceed and convert the dataset to a corpus object of the `tm` package for further analysis.

``` r
corpus <- CreateCorpus(dataset, quanteda = FALSE)
```

The next few functions are dedicated to polishing up the corpus for quantitative analysis. Each step should be evaluated by the researcher, as it will have substantive impact on the final results. The following command allows to conduct a number of common operation on the corpus, such as transforming all text to lowercase, removing punctuation and removing numbers. These operations are enabled by default, but depending on the users' needs can easily be disabled (e.g. by adding the paramater `removePunctuation = FALSE`)

``` r
corpus <- CleanCorpus(corpus)
```

When two or more separate words express a specific concept that is relevant to our research, we might want to combine those words and analyse them as one in following phases. For example, we might want to consider "European Union", and "EU" as one and the same word, "EuropeanUnion", or "human rights" as "HumanRights", or join a few terms that refer to migration and migrants.

``` r
wordCombinations <- AddWordCombinations(wordCombinations = NULL,
                                        originalCombination = "european union",
                                        toBeUsed = "EuropeanUnion")
wordCombinations <- AddWordCombinations(wordCombinations = wordCombinations,
                                        originalCombination = "eu",
                                        toBeUsed = "EuropeanUnion")
wordCombinations <- AddWordCombinations(wordCombinations = wordCombinations,
                                        originalCombination = "european parliament",
                                        toBeUsed = "EuropeanParliament")
wordCombinations <- AddWordCombinations(wordCombinations = wordCombinations,
                                        originalCombination = "migration",
                                        toBeUsed = "migrant")
wordCombinations <- AddWordCombinations(wordCombinations = wordCombinations,
                                        originalCombination = "migratory",
                                        toBeUsed = "migrant")
wordCombinations <- AddWordCombinations(wordCombinations = wordCombinations,
                                        originalCombination = "migrate",
                                        toBeUsed = "migrant")
wordCombinations <- AddWordCombinations(wordCombinations = wordCombinations,
                                        originalCombination = "human rights",
                                        toBeUsed = "HumanRights")
wordCombinations
#>   originalCombination           toBeUsed
#> 1      european union      EuropeanUnion
#> 2                  eu      EuropeanUnion
#> 3 european parliament EuropeanParliament
#> 4           migration            migrant
#> 5           migratory            migrant
#> 6             migrate            migrant
#> 7        human rights        HumanRights
```

We can then apply these word combinations to the corpus.

``` r
corpus <- CombineWords(corpus, wordCombinations)
```

It is common to remove stopwords, i.e. commonly used words that are substantially relevant to our research. The `tm` package provides a default list of stopwords, and we can add our own. However, such steps must be carefully supervisioned. "will" may be substantially irrelvant, unless we are interested, for example, in "political will"; "can" may be substantially irrelevant, unless, for example, we are interested in recycling, etc.

``` r
stopwords <- c("will", "also", "can", "said")
stopwords <- AddStopwords(newStopwords = stopwords,
                          includeDefault = TRUE, 
                          language = "en")
corpus <- RemoveStopwords(corpus, stopwords)
```

A common next step is that of document stemming. Depending on the language and the desired result, word combinations may be better applied after stemming. To understand precisely what stemming does, it may be useul to export and have a look at the stemming dictionary.

``` r
stemmingDictionary <- ExportStemmingDictionary(corpusDtm = CreateDtm(corpus), stopwords = stopwords)
```

We can then proceed with stemming the document.

``` r
corpus <- tm::tm_map(corpus, tm::stemDocument)
```

Analysing the corpus
--------------------

We are now ready to conduct some basic quantitative analysis on the corpus.

First, we can create a document-term matrix.

``` r
corpusDtm <- CreateDtm(corpus)
```

We can now see, for example, which are the most frequently used words.

``` r
ShowFreq(corpusDtm, mode = "data.frame")
#>                        term  freq
#> europeanunion europeanunion 23082
#> mep                     mep 13388
#> parliament       parliament  9538
#> european           european  8736
#> member               member  8532
#> state                 state  7803
#> committe           committe  7448
#> commiss             commiss  6881
#> countri             countri  6454
#> vote                   vote  6123
ShowFreq(corpusDtm, mode = "barchart")
```

![](data/readme/README-Most%20frequent%20words-1.png)

``` r
ShowFreq(corpusDtm, mode = "wordcloud", number = 100)
```

![](data/readme/README-Most%20frequent%20words-2.png)

N.B. After stemming, stemmed words are provided. This may be curious, but is usually more useful to provide a list of terms we are specifically interested in.

``` r
ShowFreq(corpusDtm = corpusDtm, mode = "barchart", terms = c("environment", "humanright", "migrant"))
```

![](data/readme/README-Show%20specific%20terms%20barchart-1.png)

N.B. After stemming, stemmed version of words should be provided.

`castarter` graphs are generally created with `ggplot` (and can thus be edited inside R), and can be exported as .png or .svg file for further editing in other applications by enabling the export parameter with `export = TRUE` (graphs are automatically saved in the `outputs` subfolder). Showing the data in a time series, allows to highlight variation over time.

``` r
ShowTS(corpusDtm = corpusDtm, corpus = corpus, terms = c("environment", "humanright", "migrant"), rollingAverage = 90)
#> Warning in rollup.simple_triplet_matrix(t(x), 2L, INDEX[as.character(k)], :
#> NA(s) in 'index'
```

![](data/readme/README-Create%20time%20series-1.png)

N.B. In order to zoom dynamically into the time series, it is possible to output the timeseries to dygraphs, enabling the relevant parameter `dygraphs=TRUE`.

The above graphs can also be produced to visualize comparisons among different websites.

An alternative way of presenting the data is given by the function `ShowShare()`, which shows how many press releases mention a given term in a given time period.

``` r
ShowShare(dataset = dataset, terms = "migration", breaks = "years")
```

![](data/readme/README-Show%20share-1.png)

N.B. Notice that the \`ShowShare' function is based on the dataset, and thus does not consider stemming.

Quickly create a basic ShinyApp with your own dataset
-----------------------------------------------------

In order to explore quickly the dataset, `castarter` offers a function that quickly creates a web interface that exposes some `castarter` functions. Once the dataset has been created, it is possible to create this web interface (known as a ShinyApp) with the following commands:

``` r
SetCastarter(project = "EuropeanUnion", website = "EuropeanParliament")
dataset <- LoadDatasets()
CreateShinyApp(dataset = dataset)
```

This will produce a web interface similar to [this one](https://giocomai.shinyapps.io/SouthCaucasusPresidents/).

Disclaimer
----------

`castarter` is under active development:

-   some functions may not work as expected;
-   documentation is incomplete (however, most paramaters should be self-descriptive);
-   the code has not been optimised.

Forthcoming features and other development issues
-------------------------------------------------

Only the development version is currently available: functions may be broken, or work only with corpus/corpusDtm of either the 'tm' or 'quanteda' type. Active development will take place through 2017.

Main goal: Allow easy extraction of textual contents from the internet, provide basic functions for word frequency analysis and visualization, and facilitate furhter analysis with other, more advanced packages, including `tm`, `quanteda`, `tidytext` and `RQDA`

Forthcoming features:

-   adding full support to `quanteda` for enhanced speed, while mantaining compatibility with the `tm` packgage;
-   integrate with other packages, facilitating creation of datasets from social media;
-   improve export of datasets (or subsets) via `knitr`;
-   improve support for very large datasets;
-   including additional example datasets;
-   enhancing documentation and tutorials;
-   enhancing date extraction;
-   enhancing support for updating datasets (see `?UpdateDataset`);
-   enhancing support for larger datasets, including by improving direct import from html files without importing them into R first through `CreateDatasetFromHtml`;
-   reducing dependencies, standardize code based on the `tidyverse`;
-   polishing and speeding up code.

Examples of output
------------------

Some examples of analysis of media contents conducted with `castarter` are available on the author's blog:

-   [Word frequency of ‘Ukraine’, ‘Crimea’, and ‘Syria’ on Russia’s First Channel](http://www.giorgiocomai.eu/2015/11/03/word-frequency-of-ukraine-crimea-and-syria-on-russias-first-channel/)
-   [Presidents and prime ministers in Romanian media](http://www.giorgiocomai.eu/2016/06/29/presidents-and-prime-ministers-in-romanian-media/)

Interactive example:

-   [Armenia's president website](https://giocomai.shinyapps.io/ArmeniaPresident/)
-   [Official websites of South Caucasus presidents](https://giocomai.shinyapps.io/SouthCaucasusPresidents/)

About the author
----------------

Giorgio Comai is doctoral researcher at the School of Law and Government, Dublin City University, and a member of the Marie Curie ITN network “Post-Soviet tensions”. You can find out more about his work on his [personal website](http://giorgiocomai.eu/). He is on Twitter: <https://twitter.com/giocomai>.

References
----------

Krippendorff, Klaus. 2004. *Content Analysis: An Introduction to Its Methodology*. Thousand Oaks, Calif.: Sage.

Riffe, Daniel, Stephen Lacy, and Frederick Fico. 2005. *Analyzing Media Messages Using Quantitative Content Analysis in Research*. Mahwah, N.J.: Lawrence Erlbaum. <http://public.eblib.com/choice/publicfullrecord.aspx?p=261430>.

[1] Reproduction and use of contents published on the European Parliament's website is explicitly allowed by its [terms of use](http://www.europarl.europa.eu/portal/en/legal-notice).
