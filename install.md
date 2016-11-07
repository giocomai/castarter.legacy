Install castarter
================

Preliminary steps
-----------------

In order to use `castarter`, you need to install R and, if on Linux, a few dependencies.

### On Fedora

``` install
sudo dnf install R R-RCurl curl-devel R-zoo R-XML openssl-devel libxml2-devel cairo-devel
```

### On Ubuntu

``` install
sudo apt-get install r-base r-cran-rcurl r-cran-xml r-cran-rjava r-cran-xml libssl-dev libcairo-dev libcurl4-openssl-dev r-cran-slam libxml2-dev
```

### On Windows

Download the latest version of R from [R's official website](https://www.r-project.org/%7Bhttps://www.r-project.org/)

### Installing a user interface for R

No matter which platform you are using, it is suggested you install a dedicated user interface for R such as [RStudio](https://www.rstudio.com/products/rstudio/download/) - open source and available for Linux, Windows and Mac.

Installing 'castarter'
----------------------

To actually install `castarter`, you can run these commands inside R: they will install the `devtoools` package, which allows to install `castarter` directly from Github.

``` r
install.packages("devtools")
devtools::install_github("giocomai/castarter")
```

Other options
-------------

### Use `castarter` via Docker

If you are familiar with Docker, and have it installed and running, you can instantly access a working version of Rstudio Server with pre-installed `castarter` by typing this command:

``` installing
docker run -p 8787:8787 giocomai/castarter-rocker-rstudio
```

To make sure you actually run the latest version of `castarter`, please run `devtools::install_github("giocomai/castarter")` from inside the newly opened session, usually available from the browser at `http://localhost:8787/` if run on a local machine).

Please keep in mind that - as customary for Docker - all files created while the instance is running are temporary and will be deleted as soon as docker is terminated or the host powered off.

For more detailed information on the install procedure, and on a working approach for storing files created with this Docker, please see the dedicated page on the [castarter rocker repository](https://github.com/giocomai/castarter-rocker/tree/master/rstudio).