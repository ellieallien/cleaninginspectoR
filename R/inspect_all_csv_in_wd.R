
#' run inspect_all() on all csv files in a folder and subfolders
#' @param source_dir folder to search for csv files that inspect_all() should be run on
#' @param pattern a regex pattern on which files in the folders to select. The default is "csv$"; this should be kept at the end of the pattern
#' @param target_folder path to the folder where the issue tables should be saved
#' @param recursive logical: if TRUE (default), also searches all subfolders of the current working directory
#' @return a list of data frames with the outputs from each csv file in the current working directory (and subdirectories)#
#'
inspect_all_csv_in_dir <- function(source_dir = "./", pattern = "csv$", target_dir = "./", recursive = TRUE) {

  # fail if.. wrong input types
  assertthat::assert_that(is.string(source_dir))
  assertthat::assert_that(is.string(pattern))
  assertthat::assert_that(is.string(target_dir))
  assertthat::assert_that(is.flag(recursive))

  # fail if.. directories not found
  if (!dir.exists(source_dir)) {
    stop("source directory does not exist")
  }
  if (!dir.exists(target_dir)) {
    stop("target directory does not exist")
  }

  # search directories for pattern
  files <- list.files(path = source_dir, pattern = pattern, recursive = TRUE)

  # fail if.. no files found
  if (length(files) < 1) {
    stop(paste("found no files matching the regex pattern: '", pattern, "' in directory", source_dir))
  }

  # load files, run checks

  all_issues <- purrr::map(files, function(csvfile) {
    df <- data.table::fread(file = paste0(source_dir,"/",csvfile))

    # skip files that didn't read correctly
    if (!is.data.frame(df)) {
      warning(paste0("file '", csvfile, "not read correctly - returning empty table"))
      return(empty_issues_table())
    }
    if (nrow(df) < 1) {
      warning(paste0("file '", csvfile, "has no data - returning empty table"))
      return(empty_issues_table())
    }

    tryCatch({
      inspect_all(df)
    }, error = function(e) {
      warning(e)
      return(NULL)
    })
  })
  # target file names
  target_files <- gsub("/", "_____", files) %>% gsub(".csv$", "_issues.csv", .) %>% paste0(target_dir, "/", .)

  # save all files
  mapply(write.csv, x = all_issues, file = target_files)

  invisible(all_issues)
}
