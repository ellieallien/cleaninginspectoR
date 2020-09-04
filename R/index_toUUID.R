#' Replace indices by UUID to ease retrieval of records in dataset in cleaninginspectoR results
#'
#' @param inspectorResult a dataframe with cleaninginspectoR results
#' @param data a dataframe
#' @param uuid.column.name The name of the column containing the uuids. Default: "uuid".

#' @details Matches indices from cleaninginspectoR results with the UUIDs present in the data.
#' @return A dataframe with the cleaninginspectoR results and an additional UUID column.
#' @export
index_toUUID <- function(inspectorResult, data, uuid.column.name = "uuid") {

  if(any(!names(inspectorResult) %in% c("index","value","variable","has_issue","issue_type"))){
    extra_cols <- names(inspectorResult)[!names(inspectorResult) %in% c("index","value","variable","has_issue","issue_type")]
    warning(paste("Column not in standard cleaninginspectoR output: ",
                  extra_cols,
                  collapse  = "\n"
    )
    )
  }

  if(!is.data.frame(data)){stop("Input a dataframe as data ")}
  if(!is.data.frame(inspectorResult)){stop("Input a dataframe as inspectorResult ")}
  if(!is.character(uuid.column.name)){stop("Input a character vector as uuid.column.name ")}



  data$row_number <- row.names(data)

  data_uuidIndex <- dplyr::select(data, !!uuid.column.name, row_number)

  inspectorResult$index <- as.character(inspectorResult$index)
  inspectorResult%>%
    left_join(data_uuidIndex, by = c("index" = "row_number"))

}
