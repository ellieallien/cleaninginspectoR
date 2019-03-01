select_other_columns <- function (data)
{
  othernames <- grep("other|Other|autre|Autre", names(data),
                     value = T)
  data[othernames]
}

empty_issues_table <- function ()
{
  data.frame(uuid= numeric(), index = numeric(), value = numeric(), variable = character(),
             has_issue = logical(), issue_type = character())
}

aggregate_count <- function (df, split.by = NULL, ignore.missing.data = T, write.to.file = NULL)
{
  if (!is.null(split.by)) {
    insure.string.is.column.header(df, split.by)
  }
  if (!is.null(split.by)) {
    insure.is.single.value(split.by)
  }
  counts <- lapply(df, function(d) {
    wtd.table(x = d, y = (if (is.null(split.by)) {
      NULL
    }
    else {
      df[[split.by]]
    }), na.rm = ignore.missing.data)
  })
  if (!is.null(write.to.file)) {
    write.csv.untidy(counts, write.to.file)
  }
  return(counts)
}


data_validation_outliers_normal<-function(data,maximum_standard_deviations=3){
  outliers_normal<-data %>% lapply(outliers.numerical,maximum_standard_deviations=maximum_standard_deviations)
  return(outliers_normal)}

data_validation_outliers_log_normal<-function(data,maximum_standard_deviations=3){
  outliers_log_normal<- data %>% lapply(log.outliers.numerical,maximum_standard_deviations=maximum_standard_deviations)
  return(outliers_log_normal)}

# detecting outliers:
outliers.numerical<-function(x,maximum_standard_deviations=3){
  # IN:
  # x: numerical vector
  # maximum_standard_deviations: integer
  # out: vector of indicies, of values in x that deviate more than maximum_standard_deviations from the mean.
  x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
  x_data_only<-hasdata(x)
  x_data_only_indices<-hasdata(x,return.index = T)
  outliers_indices_in_data_only<-which(abs(x_data_only-mean(x_data_only))>maximum_standard_deviations*sd(x_data_only)& length(unique(x_data_only))>10)
  outliers_indices_in_original_vector<-x_data_only_indices[outliers_indices_in_data_only]
  return(
    cbind(
      index=outliers_indices_in_original_vector,
      value=x[outliers_indices_in_original_vector]) %>% as.data.frame)
}

log.outliers.numerical<-function(x,maximum_standard_deviations=3){
  # IN:
  # x: numerical vector
  # maximum_standard_deviations: integer
  # out: vector of indicies, of values in x that deviate more than maximum_standard_deviations from the mean.
  x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
  x_not_logged<-x
  x <- suppressWarnings(log(x))
  x_data_only<-hasdata(x)
  x_data_only_indices<-hasdata(x,return.index = T)
  outliers_indices_in_data_only<-which(abs(x_data_only-mean(x_data_only))>maximum_standard_deviations*sd(x_data_only) & length(unique(x_data_only))>10)
  outliers_indices_in_original_vector<-x_data_only_indices[outliers_indices_in_data_only]
  return(
    cbind(
      index=outliers_indices_in_original_vector,
      value=x_not_logged[outliers_indices_in_original_vector])%>% as.data.frame)
}


setcolnames<-function(x,colnames){
  colnames(x)<-colnames
  return(x)
}

#' has data
#' removes NA, empty strings and non-finite values from a vector
#' @param x vector
#' @param return.index if true, returns indices of the vector that have valid data. Defaults to FALSE.
#' @param
#' @param
#' @return returns the values of the input vector that contain valid data
#' @seealso \code{\link{}}
#' @examples
hasdata<-function(x,return.index=F){
  # in: vector of any class
  # out: the in vector without NULL,NA, or ""
  index<-which(!is.null(x) & !is.na(x) & x !="" & !is.infinite(x))
  value<-x[which(!is.null(x) & !is.na(x) & x !="" & !is.infinite(x))]
  if(return.index){
    return(index)
  }
  return(value)
}
