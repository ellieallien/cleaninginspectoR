

inspection_add_data_columns<-function(inspection,df,variables){

  tibble(inspection,df[inspection$index,variables])

}
