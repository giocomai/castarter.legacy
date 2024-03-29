#' Backs up files to Google Drive
#'
#' Backs up files to Google Drive
#'
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @return Nothing, used for its side effects.
#' @export
#' @examples
#' \dontrun{
#' backup_to_google_drive()
#' }

backup_to_google_drive <- function(r_files = TRUE,
                                   datasets = TRUE,
                                   archive_files = TRUE,
                                   project = NULL,
                                   website = NULL) {
    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }
    ## base castarter folder
    home_d <- googledrive::drive_ls() %>% dplyr::filter(name==baseFolder)
    if (nrow(home_d)==0) {
        castarter_folder_d <- googledrive::drive_mkdir(name = baseFolder)
    } else if (nrow(home_d)==1) {
        castarter_folder_d <- home_d
    } else {
        stop("castarter should find just one castarter folder. Please delete if you have more than one.")
    }
    castarter_projects_d <- googledrive::drive_ls(path = castarter_folder_d)

    ## project folder
    project_folder_d <- castarter_projects_d %>%
        dplyr::filter(name==project)

    if (nrow(project_folder_d)==0) {
        project_folder_d <- googledrive::drive_mkdir(name = project,
                                                     path = castarter_folder_d)
    } else if (nrow(project_folder_d)==1) {
        # do nothing
    } else {
        stop("castarter should find just one project folder with the same name. Please delete if you have more than one.")
    }

    ## website folder
    castarter_websites_d <- googledrive::drive_ls(path = project_folder_d)

    website_folder_d <- castarter_websites_d %>%
        dplyr::filter(name==website)

    if (nrow(website_folder_d)==0) {
        website_folder_d <- googledrive::drive_mkdir(name = website,
                                                     path = project_folder_d)
    } else if (nrow(website_folder_d)==1) {
        # do nothing
    } else {
        stop("castarter should find just one website folder with the same name within a given project folder. Please delete if you have more than one.")
    }

    website_folders <- googledrive::drive_ls(path = website_folder_d)

    if (website_folders %>% dplyr::filter(name=="Archives") %>% nrow() == 0) {
        archives_folder_d <- googledrive::drive_mkdir(name = "Archives",
                                                      path = website_folder_d)
    } else if (website_folders %>% dplyr::filter(name=="Archives") %>% nrow() == 1) {
        archives_folder_d <- website_folders %>% dplyr::filter(name=="Archives")
    }

    archive_dates_local <- list.dirs(path = file.path(baseFolder, project, website, "Archives"),
                                     full.names = FALSE,
                                     recursive = FALSE)
    archive_dates_remote <- googledrive::drive_ls(path = archives_folder_d)


    ## upload R files
    if (r_files==TRUE) {
        r_files <- fs::dir_ls(path = fs::path(baseFolder, project, website),
                              recurse = FALSE,
                              type = "file",
                              glob = "*.R")
        if (length(r_files)==0) {
            warning("No local .R files found.")
        } else {
            if(nrow(website_folders)==0) {
                purrr::walk(.x = r_files,
                            .f = function(x) googledrive::drive_upload(media = x, path = website_folder_d))
            } else {
                r_files_to_upload <- r_files[is.element(el = fs::path_file(r_files),
                                                        set = website_folders$name)==FALSE]
                if (length(r_files_to_upload)>0) {
                    purrr::walk(.x = r_files_to_upload,
                                .f = function(x) googledrive::drive_upload(media = x,
                                                                           path = website_folder_d))
                }
            }
        }

        #googledrive::drive_update()
    }

    ## upload datasets

    if (datasets == TRUE) {

        if (website_folders %>% dplyr::filter(name=="Dataset") %>% nrow() == 0) {
            dataset_folder_d <- googledrive::drive_mkdir(name = "Dataset",
                                                         path = website_folder_d)
        } else if (website_folders %>% dplyr::filter(name=="Dataset") %>% nrow() == 1) {
            dataset_folder_d <- website_folders %>% dplyr::filter(name=="Dataset")
        }


        rds_dataset_files_local <- fs::dir_ls(path = fs::path(baseFolder, project, website, "Dataset"),
                                              recurse = FALSE,
                                              type = "file",
                                              glob = "*.rds")
        rds_dataset_files_remote <- googledrive::drive_ls(path = dataset_folder_d)

        if (nrow(rds_dataset_files_remote)==0) {
            purrr::walk(.x = rds_dataset_files_local,
                        .f = function(x) googledrive::drive_upload(media = x, path = dataset_folder_d))
        } else {
            purrr::walk(.x = rds_dataset_files_local[is.element(el = rds_dataset_files_remote$name, set = fs::path_file(rds_dataset_files_local))==FALSE],
                        .f = function(x) googledrive::drive_upload(media = x, path = dataset_folder_d))
        }

    }



    if (archive_files==TRUE) {
        for (i in archive_dates_local) {
            if (archive_dates_remote %>% dplyr::filter(name==i) %>% nrow() == 0) {
                archive_date_remote_d <- googledrive::drive_mkdir(name = i,
                                                                  path = archives_folder_d)
            } else {
                archive_date_remote_d <- archive_dates_remote %>% dplyr::filter(name==i)
            }
            archive_date_remote_daily_d <- googledrive::drive_ls(path = archive_date_remote_d)
            local_daily_files <- list.files(path = file.path(baseFolder, project, website, "Archives", i),
                                            full.names = TRUE,
                                            recursive = FALSE)
            for (j in local_daily_files) {
                if (archive_date_remote_daily_d %>% dplyr::filter(name == fs::path_file(j)) %>% nrow() == 0) {
                    googledrive::drive_upload(media = j, path = archive_date_remote_d)
                }
            }
        }
    }

}


#' Restore from Google Drive
#'
#' Restore from Google Drive
#'
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder. Defaults to NULL, required for storing export parameters (with exportParameters = TRUE). This can be left blank if previously set with SetCastarter(project = "project", website = "website").
#' @return Nothing, used for its side effects.
#' @export
#' @examples
#' \dontrun{
#' restore_from_google_drive()
#' }

restore_from_google_drive <- function(r_files = TRUE,
                                      dataset = TRUE,
                                      archive_files = FALSE,
                                      project = NULL,
                                      website = NULL) {

    if (is.null(project) == TRUE) {
        project <- CastarterOptions("project")
    }
    if (is.null(website) == TRUE) {
        website <- CastarterOptions("website")
    }
    if (is.null(CastarterOptions("baseFolder"))) {
        baseFolder <- "castarter"
    } else {
        baseFolder <- CastarterOptions("baseFolder")
    }

    base_folder <- googledrive::drive_ls(recursive = FALSE) %>%
        dplyr::filter(name==baseFolder)
    project_folder <- googledrive::drive_ls(path = base_folder) %>%
        dplyr::filter(name == project)
    website_folder <- googledrive::drive_ls(path = project_folder) %>%
        dplyr::filter(name == website)

    website_folder_contents <- googledrive::drive_ls(path = website_folder)
    if (r_files == TRUE) {
        r_files_latest <- website_folder_contents %>%
            dplyr::filter(stringr::str_detect(string = name,
                                              pattern = stringr::fixed(".R"))) %>%
            dplyr::arrange(dplyr::desc(name)) %>%
            dplyr::slice(1)

        r_files_local_path <- fs::path(baseFolder, project, website)
        fs::dir_create(path = r_files_local_path)

        googledrive::drive_download(file = r_files_latest,
                                    path = fs::path(r_files_local_path, r_files_latest$name))
    }
    if (dataset==TRUE) {
        dataset_folder <- website_folder_contents %>%
            dplyr::filter(name == "Dataset")

        latest_dataset <- googledrive::drive_ls(path = dataset_folder) %>%
            dplyr::arrange(dplyr::desc(name)) %>%
            dplyr::filter(stringr::str_detect(string = name, pattern = ".rds")) %>%
            dplyr::slice(1)

        dataset_local_path <- fs::path(baseFolder, project, website, "Dataset")

        fs::dir_create(path = dataset_local_path)

        googledrive::drive_download(file = latest_dataset, path = fs::path(dataset_local_path, latest_dataset$name))
    }

}
