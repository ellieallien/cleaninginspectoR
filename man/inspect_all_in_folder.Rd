% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspect_folder.R
\name{inspect_all_in_folder}
\alias{inspect_all_in_folder}
\title{run inspect_all() on all csv files in a folder and subfolders}
\usage{
inspect_all_in_folder(source_dir = "./", pattern = "csv$",
  recursive = TRUE, write.to.csv = FALSE, target_dir = "./")
}
\arguments{
\item{source_dir}{folder to search for csv files that inspect_all() should be run on}

\item{pattern}{a regex pattern on which files in the folders to select. The default is "csv$"; this should be kept at the end of the pattern}

\item{recursive}{logical: if TRUE (default), also searches all subfolders of the current working directory}

\item{write.to.csv}{logical: whether or not to write csv files with the issue tables to files (folder can be specified with target_dir)}

\item{target_folder}{path to the folder where the issue tables should be saved}
}
\value{
a list of data frames with the outputs from each csv file in the current working directory (and subdirectories)#
}
\description{
run inspect_all() on all csv files in a folder and subfolders
}
