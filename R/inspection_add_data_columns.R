#' Add  column(s) to inspection results
#'
#' @param inspection a dataframe with cleaninginspectoR results
#' @param df a dataframe containing the original data
#' @param variables The name(s) of the column(s) containing the variable(s) to add.

#' @details Add columns from df to cleaninginspectoR results.
#' @return A dataframe with the cleaninginspectoR results and an additional UUID column.
#' @export
inspection_add_data_columns <- function(inspection, df, variables ) {

  if(any(!names(inspection) %in% c("index","value","variable","has_issue","issue_type"))){
    extra_cols <- names(inspection)[!names(inspection) %in% c("index","value","variable","has_issue","issue_type")]
    warning(paste("Column not in standard cleaninginspectoR output: ",
                  extra_cols,
                  collapse  = "\n"
    )
    )
  }

  if(!is.data.frame(df)){stop("Input a dataframe as data ")}
  if(!is.data.frame(inspection)){stop("Input a dataframe as inspection ")}
  if(!is.character(variables)){stop("Input a character vector as variables ")}

  df$row_number <- row.names(df)

  df_index <- dplyr::select(df, !!variables, row_number)

  inspection$index <- as.character(inspection$index)
  inspection%>%
    left_join(df_index, by = c("index" = "row_number"))

}

#' Replace indices by UUID to ease retrieval of records in dataset in cleaninginspectoR results
#'
#' @param inspection a dataframe with cleaninginspectoR results
#' @param df a dataframe
#' @param uuid.column.name The name of the column containing the uuids. Default: "uuid".

#' @details Matches indices from cleaninginspectoR results with the UUIDs present in the data.
#' @return A dataframe with the cleaninginspectoR results and an additional UUID column.
#' @export
index_toUUID <- function(inspectorResult, data, uuid.column.name = "uuid") {
  inspection_add_data_columns(inspectorResult, data, variables = uuid.column.name)
}
