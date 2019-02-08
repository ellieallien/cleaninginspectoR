
standard_checks<-function(df,dfduplicate.column.name = NULL){

outliers
duplicates
# cleaning_log
other_recoding
gps_no

}

#' Check the dataset for duplicates in a variable.
#'
#' @param data a dataframe
#' @param duplicate.column.name the column in the dataframe
#' @return A dataframe of the issue log format, containing the index and value of duplicated values
#' @examples
#' find_duplicates(data, "_uuid")
#' @export
find_duplicates <- function (data, duplicate.column.name) {

  ### sanitise inputs
  if(!is.data.frame(data))stop("first input must be a data frame. Use typeof(data) to find out what you have instead") #ensure first input is a dataframe
  #ensure all the variable names are in the dataframe
  if(!(duplicate.column.name %in% names(data)))stop(paste0("The name of the column to check for duplication must be found in the data headers"))

  duplicate <- data[[duplicate.column.name]] %>% duplicated %>%
    which
  if (length(duplicate) == 0) {
    return(matrix(0, nrow = 0, ncol = 5, dimnames = list(NULL,
                                                         c("index", "value", "variable", "has_issue", "issue_type"))))
  }
  duplicate <- cbind(index = duplicate, value = data[[duplicate.column.name]][duplicate])
  colnames(duplicate) <- c("index", "value")
  duplicate <- cbind(duplicate, variable = duplicate.column.name,
                     has_issue = T, issue_type = paste("duplicate in", duplicate.column.name))
  as.data.frame(duplicate)
}


#' Check the dataset for duplicates in the uuid variable.
#'
#' @param data a dataframe
#' @return A dataframe of the issue log format, containing the index and value of duplicated uuids
#' @export
find_duplicates_uuid <- function (data) {

  ### sanitise inputs
  if(!is.data.frame(data))stop("first input must be a data frame. Use typeof(data) to find out what you have instead") #ensure first input is a dataframe
  ### find uuid column and check it is a single value
uuid.name <- grep("uuid", names(data), value = T)
if(length(uuid.name) == 0){stop("Could not find the uuid automatically in the dataset. Please use find_duplicates instead and give it the name of the uuid column")}
if(length(uuid.name) > 1){uuid.name <- uuid.name[1]}

return(find_duplicates(data, uuid.name))
  }



#' Find outliers in the data, deciding whether log or normal outlier functions should be used.
#'
#' @param data a dataframe
#' @return A list of numeric outliers that are 3 standard deviations from the mean
#' @export
find_outliers <- function (data)
{
  ## calculate both normal and log normal outliers for the whole dataframe
  outliers_normal <- data %>% data_validation_outliers_normal
  outliers_log_normal <- data %>% data_validation_outliers_log_normal
  outliers <- lapply(names(data), function(x) {
    ## return an empty issues dataframe of issues if no outliers are found
    if ((nrow(outliers_log_normal[[x]]) == 0) & (nrow(outliers_normal[[x]]) ==
                                                 0)) {
      return(empty_issues_table())
    }
    else if (nrow(outliers_log_normal[[x]]) < nrow(outliers_normal[[x]])) { ## for each variable, select the one with fewer outliers
      data.frame(outliers_log_normal[[x]], variable = rep(x,
                                                          nrow(outliers_log_normal[[x]])), issue_type = rep("log normal distribution outlier",
                                                                                                            nrow(outliers_log_normal[[x]])))
    }
    else {
      data.frame(outliers_normal[[x]], variable = rep(x,
                                                      nrow(outliers_normal[[x]])), issue_type = rep("normal distribution outlier",
                                                                                                    nrow(outliers_normal[[x]])))
    }
  }) %>% do.call(rbind, .)
  if (nrow(outliers) == 0) {
    return(empty_issues_table())
  }
  outliers <- data.frame(outliers, has_issue = T)
  outliers$variable <- as.character(outliers$variable)
  return(outliers)
}

#' Check the dataset for variables
#'
#' @param data a dataframe
#' @param duplicate.column.name the column in the dataframe
#' @return A dataframe of the issue log format, containing the index and value of other values that may need recoding
#' @export
find_other_responses <- function (data)
{
  frequency_tables <- data %>% select_other_columns %>% aggregate_count
  if (length(frequency_tables) == 0) {
    return(empty_issues_table())
  }
  others <- frequency_tables %>% melt %>% setcolnames(c("value", "count", "variable"))
  others <- others[others$value != "" & others$value != FALSE &
                     others$value != TRUE, ]
  if (nrow(others) == 0) {
    return(empty_issues_table())
  }
  others[, "value"] <- paste(others[, "value"], "\\\\", others[,
                                                               "count"], "instance(s)")
  others <- data.frame(index = NA, others[, c("value", "variable")],
                       has_issue = NA, issue_type = "'other' response. may need recoding.")

  return(others)
  }


#' Check the dataset for senstive variables
#'
#' @param data a dataframe
#' @return A dataframe of the issue log format, containing the variables that may contain senstitive data
#' @export
#'

sensitive_columns <- function (data)
{
  sensitive.cols<- grep("GPS.|gps.|phone.|Latitude.|Longitude.|Phone.", x = names(data), value = T)
  if(length(sensitive.cols) == 0){return(empty_issues_table())}
  sensitive.cols <- data.frame(index = NA, value = NA, variable = sensitive.cols,
                       has_issue = "yes !", issue_type = "This looks like sensitive information. Please ensure all PII is removed")
  return(sensitive.cols)}

