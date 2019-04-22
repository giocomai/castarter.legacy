## create internal list of months by language

months_by_language <- list()

# system("locale -a", intern = TRUE)

Sys.setlocale("LC_TIME", "croatian")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin2", to = "UTF-8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%b"), from = "latin2", to = "UTF-8")
Sys.setlocale("LC_TIME", "hr_HR.utf8")
months_by_language$croatian <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "czech")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin2", to = "UTF-8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%b"), from = "latin2", to = "UTF-8")
Sys.setlocale("LC_TIME", "cs_CZ.utf8")
months_by_language$czech <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "danish")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin1", to = "UTF-8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%b"), from = "latin1", to = "UTF-8")
Sys.setlocale("LC_TIME", "da_DK.utf8")
months_by_language$danish <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))
Sys.setlocale("LC_TIME", "dutch")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin1", to = "UTF-8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%b"), from = "latin1", to = "UTF-8")
Sys.setlocale("LC_TIME", "nl_NL.utf8")
months_by_language$dutch <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "estonian")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin2", to = "UTF-8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%b"), from = "latin2", to = "UTF-8")
Sys.setlocale("LC_TIME", "et_EE.utf8")
months_by_language$estonian <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = stringr::str_squish(format(ISOdate(2000, 1:12, 1), "%b")))

Sys.setlocale("LC_TIME", "german")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin1", to = "UTF-8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%b"), from = "latin1", to = "UTF-8")
Sys.setlocale("LC_TIME", "de_DE.utf8")
months_by_language$german <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "hungarian")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin2", to = "UTF-8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%b"), from = "latin2", to = "UTF-8")
Sys.setlocale("LC_TIME", "hu_HU.utf8")
months_by_language$hungarian <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "italian")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin1", to = "UTF-8")
Sys.setlocale("LC_TIME", "it_IT.utf8")
months_by_language$italian <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = ormat(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "polish")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin2", to = "UTF-8")
Sys.setlocale("LC_TIME", "pl_PL.utf8")
months_by_language$polish <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "romanian")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin2", to = "UTF-8")
Sys.setlocale("LC_TIME", "ro_RO.utf8")
months_by_language$romanian <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "russian")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "cyrillic", to = "UTF-8")
Sys.setlocale("LC_TIME", "ru_RU.utf8")
months_by_language$russian <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "spanish")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin1", to = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.utf8")
months_by_language$spanish <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "turkish")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "latin5", to = "UTF-8")
Sys.setlocale("LC_TIME", "tr_TR.utf8")
months_by_language$turkish <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))

Sys.setlocale("LC_TIME", "uk_UA.utf8")
# iconv(x = format(ISOdate(2000, 1:12, 1), "%B"), from = "UTF-8", to = "UTF-8")
months_by_language$ukrainian <-
    tibble::tibble(month_name = format(ISOdate(2000, 1:12, 1), "%B"),
                   month_abb  = format(ISOdate(2000, 1:12, 1), "%b"))


usethis::use_data(months_by_language, internal = TRUE, overwrite = FALSE)
