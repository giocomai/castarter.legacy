An introduction to 'castarter' - content analysis starter toolkit for R
================
Giorgio Comai
2016-06-01

<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

Books dedicated to content analysis typically assume that the researcher has already created, has access or may buy access to a structured dataset.They may include sections on sampling (Krippendorff 2004; Riffe, Lacy, and Fico 2005), but they do not debate explicitly how new datasets can be built. Commercial software packages generally share the same expectation. In the R ecosystem, there are a number of packages that can be used to access existing datasets (e.g. [tm.plugin.lexisnexis](https://cran.r-project.org/web/packages/tm.plugin.lexisnexis/), [manifestoR](https://cran.r-project.org/web/packages/manifestoR/)) or import into R textual content from social media through their APIs (e.g. [twitteR](https://cran.r-project.org/web/packages/twitteR/)). However, there is no package dedicated to getting into R the textual contents of regular websites and extracting key metadata (date and title) in order to populate a corpus that can be analysed in R or with other software packages. 'castarter - content analysis starter toolkit for R' aims to accomplish just that, offering to moderately incompetent users the opportunity to extract textual contents from websites and prepare them for content analysis.

Installation
------------

`castarter` is hosted on GitHub. The easiest way to install it on your system, is to run the following commands in R:

    install.packages("devtools")
    library("devtools")
    install_github("giocomai/castarter")

Load `castarter` with:

``` r
library("castarter")
```

Using castarter
---------------

`castarter` facilitates downloading and extracting textual contents from a given section of a website. `castarter` can be used to analyse one single website, or more websites as part of a project, facilitating comparison among cases. In order to smoothen replication and prevent data losses, by default `castarter` stores a copy of downloaded web pages in .html format, and of the extracted textual contents in .txt format (other export possibilities are available and presented below). To do so, it includes a function that creates the relevant folder structure, and another function that facilitates archiving the .html files in compressed format.

First, you may want to set the root working folder, if you do not wish to use R's default, with:

In `castarter` every dataset is expected to be extracted from a website, and each website is expected to be part of a project. This is useful to give a consistent way of storing the materials gathered and to facilitate comparison among cases, but it does not imply additional limitations: it is possible to have projects of only one website, and it is possible to make comparisons among websites that are part of different projects.

First, define the name of project and name of website:

    SetCastarter(nameOfProject = "exampleProject", nameOfWebsite = "nameOfWebsiteToBeAnalysedHere")

This stores nameOfProject and nameOfWebsite as options that can be retrieved by all `castarter` functions automatically, without the need to inputting them each time.

Then, it is time to create the folder structure where all outputs will be saved:

    CreateFolderStructure()

This creates the following folders in the current working directory:

-   exampleProject
    -   exampleWebsite
        -   Archives
        -   Dataset
        -   Html
        -   IndexHtml
        -   Logs
        -   Outputs
        -   Txt

Notice that there is no need to provide nameOfProject and nameOfWebsite, since they are retrieved from the previously set options.

In order to clarify the functioning of `castarter`, I will now show how to extract and prepare for content analysis all press releases currently available on the European Parliament's website. As will be shown, `castarter` takes advantage of the fact that most modern content management systems consistently generate URLs to present the contents of a website. Before we start, we need to create the folders where files downloaded will be stored for reference

``` r
SetCastarter(nameOfProject = "exampleProject", nameOfWebsite = "nameOfWebsiteToBeAnalysedHere")
```

From the home page of the European Parliament, we can easily reach the main page dedicated to press releases clicking on 'News', and then on 'Press releases' on the top of the page: <http://www.europarl.europa.eu/news/en/news-room/press-release> At the bottom of this page, as it is customary, the user is given the possibility to see older press released by clicking on the 'Next page' link. There is also the possibility to go back in time quicker, or seeing directly the oldest press release. Let's have a look at the URL's of these pages: Clicking subsequent times on the 'next page' link, these are the URLs that we obtain:

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=10>

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=20>

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=30>

Clicking on the link to the last available page, at the time of this writing, shows the following URLs, including links to press releases published in 2010:

<http://www.europarl.europa.eu/news/en/news-room/press-release?start=4600>

There is clearly a pattern: each page shows 10 press releases, and as we go back in time, the URLs "tells" the content management system to skip the first x number of press releases before starting. This makes it easy to create the links to these index pages. In `castarter`, this can be done by using the following function:

    indexLinks <- CreateLinks(
    linkFirstChunk = "http://www.europarl.europa.eu/news/en/news-room/press-release?start=",
    startPage = 0,
    endPage = 50,
    increaseBy = 10)

This command creates a vector (`indexLinks`) including direct links to the index pages:

``` r
indexLinks <- CreateLinks(linkFirstChunk = "http://www.europarl.europa.eu/news/en/news-room/press-release?start=",
            startPage = 0,
            endPage = 40,
            increaseBy = 10)
print(indexLinks)
#> [1] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=0" 
#> [2] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=10"
#> [3] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=20"
#> [4] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=30"
#> [5] "http://www.europarl.europa.eu/news/en/news-room/press-release?start=40"
```

For this example, we will download only the first five index pages, including links to 50 press releases. In order to download all of the press releases available, it is only necessary to change the `endPage` parameter, by changing the relevant parameter (at the time of this writing, this would be `endPage = 4600` instead of `endPage = 40`). The following command downloads the relevant index pages, stores them in a vector, and saves them in .html format in the /EU/EUparliament/IndexHtml folder.

Now that we have the index pages, we need to extract direct links to the individual articles. There are various ways to do this, but the easiest approach is looking at the URLs of individual articles, and see if there is a pattern (this is usually the case in most modern websites). Hovering over the titles of an index page, it is easy to see the links to indivudal news items. In this case, we see that the URL to each news item includes in the URL the following string: "/news/en/news-room/content/". We use this to extract relevant links, and discard all links to other pages or sections of the website scattered around that we are not interested in. The following command extracts links to individual articles, and tries to guess the title of the article.

As expected, this function extracts 50 links to individual news items. In this example, the links are correctly extracted and the titles match the links. However, the titles include an unnecessary "Read more:" string and brackets. This can easily be removed in a following phase.

Now that we have direct links, it is easy to download all articles. The following command downloads all articles, stores them in a vector, saves them in .html format in the /EU/EUparliament/Html folder, and notifies of advancement.

Extracting metadata
-------------------

It is now time to extract (or set) basic metadata from the articles: title, date, an ID, and language. We have multiple options to extract the titles. We have already extracted the title of the articles from the index pages, and we could simply polish the results. However, it is also possible to extract the titles from the individual pages, using for example the metadata embedded in the html page or by extracting the text that has style "heading 1".

In this case, both methods seem to function reasonably well.

Extracting the date may be more complex, and `castarter` offers a number of different methods to extract dates. However, in many cases it is enough to provide the format of the date (more details are provided in the documentation of the function, accessible through the command `?ExtractDates`). In the case of the European Parliament, the date is presented in the format day-month-year, and it is extracted correctly using the default settings.

There may be various criteria to set a unique ID for each article. The default option uses the same number given to the .html file stored in the `/EU/EUparliament/Html` folder.

The language can be set easily in subsequent steps, but storing it in a dedicated vector makes it easier to use template files.

We can now store the metadata in a dedicated data.frame, and export them in a spreadsheet for future reference outside of R and `castarter`.

Metadata are also used to create relevant filenames for storing individual articles in textual format. The following command calls the `boilerpipeR` library and allows to extract only the main contents of each page, removing menus and other html clutter.

Text extraction works well in most cases. However, at this stage it is generally a good idea to have a quick look at the extracted textual contents and remove easy-to-spot glitches. The following command prints on the terminal five random articles among all those extracted.

    articlesTxt[sample(1:length(articlesTxt), 5)]

At this stage, `castarter` has generated a folder of txt files and has exported relevant metadata. These can be imported and used in other software for quantitative or qualitative content analysis. The following command saves the current workspace in the working directory, stores the dataset in a dedicated folder for easy retrieval and allows to export both metadata and textual contents in a single spreadsheet.

At this stage, if we are satisified with the data we have, we may want to store all downloaded files in a .zip file and remove the original folders containing the original .html files. This way, we still can recover all of the original files, yet we don't have the thousands (or hundreds of thousands) of .html files that often are created in a `castarter` project, and may unnecessary slow down the backup process. This can easily be done with the following command.

Creating and polishing a corpus
-------------------------------

For further analysis in `castarter`, it is useful to convert the data to a corpus of the `tm` package. Most of the functions offered by `castarter` are simply wrappers of `tm` functions. At this stage, it is possible to start a completely new R session, and simply load the dataset into the current workspace. All relevant information is effectively stored in the dataset.

The following command allows to conduct a number of common operation on the corpus, such as transforming all text to lowercase, removing punctuation and removing numbers. These operations are enabled by default, but depending on the users needs can easily be disabled (e.g. by adding the paramater `removePunctuation = FALSE`)

When two or more separate words express a specific concept that is relevant to our research, we might want to combine those words and analyse them as one in following phases. For example, we might want to consider "European Union", and "EU" asone and the same word, "EuropeanUnion".

We can then apply these word combinations to the corpus.

It is common to remove stopwords, i.e. commonly used words that are substantially relevant to our research. The `tm` package provides a default list of stopwords, and we can add our own. However, such steps must be carefully supervisioned. "will" may be substantially irrelvant, unless we are interested, for example, in "political will"; "can" may be substantially irrelevant, unless, for example, we are interested in recycling, etc.

A common next step is that of document stemming.

Depending on the language and the desired result, word combinations may be better applied after stemming.

References
----------

Krippendorff, Klaus. 2004. *Content Analysis: An Introduction to Its Methodology*. Thousand Oaks, Calif.: Sage.

Riffe, Daniel, Stephen Lacy, and Frederick Fico. 2005. *Analyzing Media Messages Using Quantitative Content Analysis in Research*. Mahwah, N.J.: Lawrence Erlbaum. <http://public.eblib.com/choice/publicfullrecord.aspx?p=261430>.
