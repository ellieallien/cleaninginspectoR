#' Data Cleaning Checks
#'
#' rudimentary checks to flag potentially problematic values in a dataset
#'
#' \subsection{Functionality}
#' \itemize{
#'   \item `inspect_all()`: applies all checks listed below
#'   \item `find_duplicates()` looks for duplicates in columns that should be unique
#'   \item `find_duplicates_uuid()` looks for a function containing "uuid" in the name and looks for duplicates
#'   \item `find_outliers()` looks for outliers in numerical columns
#'   \item `find_other_responses()` looks for values in potential "if other, specify.." type of columns
#' }
#'
#'\subsection{Output}
#'
#' The output has the same structure for all functions of this package:
#'
#' A data frame with the following columns:
#'	value	variable	has_issue	issue_type
#'\itemize{
#'   \item `index`: the index of the row of the original data in which the issue occured.  (NA if applies to multiple rows)
#'   \item `value` the suspicious value
#'   \item `variable` The column containing the suspicious value
#'   \item `has_issue` logical TRUE/FALSE: currently always true, can be ignored.
#'   \item `issue_type` a description/name of the potential issue
#' }
#'
#'\subsection{Limitations}
#'
#' 1. These checks are under no circumstances sufficient or complete in any way, and more context and data specific checks are always necessary.
#' 2. Any data protection related checks are _rudimentary_ at best and are under no circumstances sufficient to ensure data protection in any way
#' 3. Although tempting, this package should _never_ be used to automatically remove any flagged values without double checking them manually. This would seriously skew your dataset, and make uncertainty estimates invalid. Potential issues must be investigated on a case by case basis.
#'
#' @docType package
#' @name cleaninginspectoR
#' @md
NULL


