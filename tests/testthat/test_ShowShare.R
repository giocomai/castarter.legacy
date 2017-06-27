library("castarter")
library("tidyverse")
dataset <- readRDS(file = "2017-01-11 - EuropeanUnion - EuropeanParliament - dataset.rds")


testthat::test_that(desc = "Dplyr implementation of ShowShare gives same result as legacy version", code = {
    ShowShare(dataset = dataset, terms = "russia")
}
)
