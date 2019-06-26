tpm <- read_excel("~/Desktop/Nigeria/MSNA/Ready_Data/HQ_MSNA/trial_function/201904_Teacher.xlsx", 
                  sheet = "Clean_Data")



#'@param dataa Dataframe with "start" and "end" columns.
#'@param duration_threshold Minimum number of minutes to complete the form
check_time <- function(dataa, duration_threshold=15){
  df_col_separated <- dataa %>% separate(start, c("start_date", "start_time"), "T") %>% 
    separate(end, c("end_date", "end_time"), "T")
  df_col_separated$days_diff <- difftime(as.POSIXct(df_col_separated$end_date), as.POSIXct(df_col_separated$start_date), units="days")
  if(max(as.numeric(df_col_separated$days_diff))==0){
    print("same date")
    #Making time into numeric so that calculations can be done
    df_col_separated$start_time <- round(as.difftime(df_col_separated$start_time, units = "mins"),2)
    df_col_separated$end_time <- round(as.difftime(df_col_separated$end_time, units = "mins"),2)
    df_col_separated$duration_min <- df_col_separated$end_time-df_col_separated$start_time
    df_col_separated$time_short_flag <- ifelse(df_col_separated$duration_min<=duration_threshold,1,0)
  }  else if (max(df_col_separated$days_diff>0)) {
    print("FORMS COMPLETED ON DIFFERENT DAYS")
  }
  time_problems <- df_col_separated %>% dplyr::filter(time_short_flag==1) 
  colnames(time_problems)[grep("uuid", colnames(time_problems))] <- "uuid"
  time_problems$variable <- "Completion Duration (min)"
  time_problems$has_issue <- "TRUE"
  time_problems$issue_type <- "form duration too short"
  
  time_grab <- time_problems %>% dplyr::select(uuid,duration_min,variable,	has_issue,	issue_type)
  names(time_grab) <- c("index",	"value",	"variable",	"has_issue",	"issue_type")
  return(time_grab)
}

#apple <- check_time(tpm)