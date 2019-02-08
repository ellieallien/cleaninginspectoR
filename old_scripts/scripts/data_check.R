

############################
# setup
############################
  # empty wd
  rm(list=ls())
  # set wd to this script's folder
require(reachR)


  source("./scripts/dependencies.R")
#.install_reachR(T, branch = 'develop')
  source("./scripts/val.R")
  source("./scripts/htmlR.R")
  source("./scripts/sanitise_parameters.R")
  source("./scripts/small_horizontal_barchart.R")
  require("dplyr")
  require("knitr")
  require("reachR")

  validate<-function(parameters){
    parameters<-normalise_parameters(parameters)
    sanitise_parameters(parameters)


    # setup html files for output (overview)
    site(parameters$name)

    page("overview")
    .rewind()
    .scream("parameters")
    .sing()
    .scream("Always check that there is no sensitive data (phone numbers, names, private locations)remaining in the dataset !!!!!")
    .say ("Here is the collection of outputs you need for data analysis. Begin by examining the potential issues
                  with your data. Most of these will turn out not to be a big deal, but if you need to remove some observations
                  you will have to re-run the aggregations without them. Next, check your global and local counts")
    .say(cbind(names(parameters),unlist(parameters) %>% unname))

    
    message('loading files...')
    # load inputs
      if(!file.exists(parameters$data_file)){
        stop(paste("data file csv file not found in path",paramters$data_file))
      }

      data<-load_data(parameters$data_file)
      .say(paste("<br># records:",nrow(data)))
      .say(paste("<br># variables:",ncol(data)))
      .say(paste0("<br>missing data: ",
                 round((1-(length(as.vector(unlist(data)) %>% hasdata)/length(as.vector(unlist(data)))))* 100),"%"
                 ))
      

    # cleaning log
      if(parameters$cleaning_log_available){

        load_cleaninglog(cleaning_log_file = parameters$cleaning_log_file,
                         cleaning.log.uuid.column = parameters$cleaning.log.uuid.column,
                         cleaning.log.new.value.column = parameters$cleaning.log.new.value.column,
                         cleaning.log.variable.column = parameters$cleaning.log.variable.column
        )
      }
      
    #sampling frame  
      if(parameters$stratified){
        
        sf<-load_samplingframe(sampling.frame.file = parameters$sampling.frame.file,
                         sampling.frame.population.column = parameters$sampling.frame.population.column,
                         sampling.frame.stratum.column = parameters$sampling.frame.stratum.column,
                         data.stratum.column = parameters$data.stratum.column,return.stratum.populations = T
        )
      }

############################
# STANDARD CLEANING CHECKS
############################
message("data cleaning checks...")

  # run the standard tests implemented so far
  # comment out cleaninglogcomparison if no cleaning log available:
  issues<-rbind(data %>%
                  data_validation_all_others,
                  # complicated non vectorised ifelse.. : function that returns empty if !cleaning_log_available, otherwise result of data_validation_cleaning_log_comparison
                (function(){if(!parameters$cleaning_log_available){return(reachR:::empty_issues_table())};return(data %>% data_validation_cleaning_log_comparison(parameters$data.uuid.column.name))})(),

                  data %>% data_validation_find_duplicates(parameters$data.uuid.column.name),
                  data %>% data_validation_outliers
  )



    # output to a file.
     write.issues(issues,page = 'issues',title = "data cleaning issues",overview.page = 'overview')
     # most of these will turn out not to be a problem in the end, but the teams should have a look. clarify that if you send it.









############################
# Simple Aggregation
############################
    less_than_50_unique_values<-  lapply(data,function(x){(x %>% unique %>% length)<50}) %>% unlist


if(!parameters$stratified){
  if(parameters$do_global_counts){
    message()
    
    write.aggregation.results(results = data[,less_than_50_unique_values] %>% aggregate_count,
                              title= "Global counts",page = 'global_counts',overview.page = 'overview')
  }
  if(parameters$do_global_percent){
    
    write.aggregation.results(results = data[,less_than_50_unique_values] %>% aggregate_percent,
                              title= "Global percent",page ="global_percent")
  }
}else{
  if(parameters$do_global_counts){
  message()
  
  write.aggregation.results(results = data[,less_than_50_unique_values] %>% aggregate_count_weighted,
                            title= "Weighted global counts",page = 'global_counts',overview.page = 'overview')
}
  if(parameters$do_global_percent){
    
    write.aggregation.results(results = data[,less_than_50_unique_values] %>% aggregate_percent_weighted,
                              title= "Weighted global percent",page ="global_percent")
  }
  
}     
    
     
     
#      if(parameters$do_global_median){
#
#      write.aggregation.results(results = data %>% aggregate_median,
#                                title= "Global Median",
#                                ,page ="global_median")
# }

############################
# per group aggregation
############################


if(!parameters$stratified){
  if(parameters$do_disaggregated_counts){
  write.aggregation.results(results = data %>% aggregate_count(split.by =  parameters$disaggregate_by),
                            title= paste("counts per ",parameters$disaggregate_by),
                            page= 'local_count')
    }
  }else{
  if(parameters$do_disaggregated_counts){
  write.aggregation.results(results = data %>% aggregate_count_weighted(split.by = parameters$disaggregate_by),
                               title= paste("weighted counts per ",parameters$disaggregate_by),
                               page= 'local_count')
  }
}


  if(!parameters$stratified){
    if(parameters$do_disaggregated_percent){
      
      write.aggregation.results(results = data %>% aggregate_percent(split.by = parameters$disaggregate_by,ignore.missing.data = F),
                                title= paste("percent per ",parameters$disaggregate_by),
                                page= 'local_percent')
    }
  }else{
    if(parameters$do_disaggregated_percent){
      
      write.aggregation.results(results = data %>% aggregate_percent_weighted(split.by = parameters$disaggregate_by,ignore.missing.data = F),
                                title= paste("weighted percent per ",parameters$disaggregate_by),
                                page= 'local_percent')
    } 
  }   
     

     parameters$custom_analysis(data)

      message(
        paste("validation script finished. See '", paste0(.htmlpath(),"overview.html'"), "for results"))
}





