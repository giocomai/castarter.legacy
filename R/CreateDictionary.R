#' Create dictionaries
#' 
#' Create dictionaries to be used by castarter functions such as ShowFreq or ShowTS 
#'  
#' @param terms A character vector in the format: c("fruit=apple+orange", "animals=cat+dog+cow")
#' @return A dictionary of the 'quanteda' type. 
#' @export
#' @examples
#' CreateDictionary(terms = c("fruit=apple+orange", "animals=cat+dog+cow"))

CreateDictionary <- function(terms) {
    termsSplit <- stringi::stri_split_fixed(str = terms, pattern = "=")
    termNames <- rep(NA, length(termsSplit))
    termsL <- list()
    for (i in seq_along(termsSplit)) {
        termNames[i] <- stringi::stri_trim_both(termsSplit[[i]][1])
        termsL[[i]] <- stringi::stri_trim_both(unlist(stringi::stri_split_fixed(str = termsSplit[[i]][2], pattern = "+")))
    }
    termsL <- setNames(object = termsL, nm = termNames)
    termsDic <- quanteda::dictionary(x = termsL)
    return(termsDic)
}

