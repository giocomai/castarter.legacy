# Variable, global to package's namespace. 
# This function is not exported to user space and does not need to be documented.
CastarterOptions <- settings::options_manager(nameOfProject = NULL, nameOfWebsite = NULL)

#' Sets 'castarter' options
#'
#' It allows to preliminary store options frequently used by 'castarter', thus removing the requirement to specify them each time a function is called.
#' @param nameOfProject Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. 
#' @param nameOfWebsite Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @return Nothing. Used for its side effects (stores settings).
#' @export
#' @examples
#' SetCastarter(nameOfProject, nameOfWebsite)
SetCastarter <- function(nameOfProject = NULL, nameOfWebsite = NULL, ...){
    CastarterOptions(nameOfProject = nameOfProject, nameOfWebsite = nameOfWebsite)
}
