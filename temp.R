if (!require("pacman")) install.packages("pacman")
pacman::p_load("devtools", "roxygen2", "formatR", "shiny")

document()

install_github("giocomai/castarter", auth_token = "2808c6bf295f7455a85767496592880939a247cf")

formatR::tidy_app()
tidy_dir(path = file.path("R"))

devtools::use_package("RCurl", type = "Suggests")
devtools::use_package("XML", type = "Suggests")
