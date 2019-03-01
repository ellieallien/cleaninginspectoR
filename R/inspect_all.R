#' Apply general data cleaning checks
#'
#' Uses all other cleaning checks available in this package at once
#'
#' @param data a dataframe
#' @param uuid.column.name optional: The name of the column containing the uuids. If none is provided, will seach variable names for "uuid".
#' @details for details see `?cleaninginspectoR`
#' @return A dataframe with one row per potential issue, containing the row index, value and variable name in the original dataset, as well as a description of the issue type
#' @examples
#' inspect_all(my_df)
#' @export
inspect_all<-function(df,uuid.column.name){

  uuid.provided <- !is.null(uuid.column.name)

  if( uuid.provided){duplicate_uuids<-find_duplicates(df,duplicate.column.name = uuid.column.name)}
  if(!uuid.provided){duplicate_uuids<-find_duplicates_uuid(df)}

  rbind(sensitive_columns(df,T),
        duplicate_uuids,
        find_outliers(df,uuid.column.name),
        find_other_responses(df))

}


