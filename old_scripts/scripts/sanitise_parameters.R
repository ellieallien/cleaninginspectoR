



# MAIN SANITATION FUNCTION
########################################
    # this checks if the parameters provided are all ok.

          sanitise_parameters<-function(parameters){
            start_sanitising()
            sanitise_parameters_files_exist(parameters)
            sanitise_parameters_parameters_exist(parameters)
            if(length(san_issue_log>0)){stop(paste("there were problems with the provided parameters:", san_issue_log,collapse="\n\n"))}
            data<-reachR:::read.csv.auto.sep(parameters$data_file)
            names(data)<-reachR:::to_alphanumeric_lowercase(names(data))
            
            if(parameters$stratified == T){
              samplingframe <- reachR:::read.csv.auto.sep(parameters$sampling.frame.file)
              names(samplingframe)<- reachR:::to_alphanumeric_lowercase(names(samplingframe))
            }else{samplingframe<-NULL}
            
            if(parameters$cleaning_log_available){
              cleaninglog <- reachR:::read.csv.auto.sep(parameters$cleaning_log_file)
              names(cleaninglog)<- reachR:::to_alphanumeric_lowercase(names(cleaninglog))
            }else{cleaninglog<-NULL}
            if(parameters$questionnaire_available){
              questionsfile<-reachR:::read.csv.auto.sep(parameters$questions.file)
              names(questionsfile)<- reachR:::to_alphanumeric_lowercase(names(questionsfile))
              choicesfile<-reachR:::read.csv.auto.sep(parameters$choices.file)
            }else{choicesfile<-NULL}
            
            parameters<-normalise_parameters(parameters)    
            

            sanitise_parameters_colnames_match(parameters,data,cleaninglog,samplingframe)
            
            sanitising_issue(condition = is.rcid.format(parameters$RCID),
                             message = 'research cycle ID specified in RCID must contain three capital letters followed by four digits')
            
            message(paste("found",length(san_issue_log),"issues with parameters and associated file formats necessary for validation:"))
            if(length(san_issue_log)!=0){stop(paste(san_issue_log,collapse="\n"))}
            message("starting validation!")
            return(san_issue_log)
          }
          

# SANITATION BY CORE CATEGORY
########################################

          sanitise_parameters_files_exist<-function(parameters){
            param.must.exist("data_file")
            "cleaning_log_available" %is_true_so_must_also_have% "cleaning_log_file"
            "stratified" %is_true_so_must_also_have% "sampling.frame.file"
            "questionnaire_available" %is_true_so_must_also_have% "questions.file"
            TRUE %is.true.so.file.must.exist% "data_file"
          }
          
          
          
          sanitise_parameters_parameters_exist<-function(parameters){
            
            param.must.exist("name")
            param.must.exist("RCID")
            
            "cleaning_log_available" %is_true_so_must_also_have% "cleaning.log.uuid.column"
            "cleaning_log_available" %is_true_so_must_also_have% "cleaning.log.new.value.column"
            "cleaning_log_available" %is_true_so_must_also_have% "cleaning.log.variable.column"
            "cleaning_log_available" %is_true_so_must_also_have% "data.uuid.column.name"
            
            
            "stratified" %is_true_so_must_also_have% "sampling.frame.population.column"
            "stratified" %is_true_so_must_also_have% "sampling.frame.stratum.column"
            "stratified" %is_true_so_must_also_have% "data.stratum.column"
            
            
            "questionnaire_available" %is_true_so_must_also_have% "choices.label.column.to.use"
            
            "do_disaggregated_counts" %is_true_so_must_also_have% "disaggregate_by"
            "do_disaggregated_percent" %is_true_so_must_also_have% "disaggregate_by"
            
          }
          
          
          sanitise_parameters_colnames_match<-function(parameters,data,cleaninglog=NULL,samplingframe=NULL){
            if(parameters$cleaning_log_available){
              must_be_colname_in(parameters$cleaning.log.uuid.column, name.par = "cleaning.log.uuid.column",cleaninglog, parameters$cleaning_log_file)
              must_be_colname_in(parameters$cleaning.log.new.value.column, name.par = "cleaning.log.new.value.column",cleaninglog, parameters$cleaning_log_file)
              must_be_colname_in(parameters$cleaning.log.variable.column, name.par = "cleaning.log.variable.column",cleaninglog, parameters$cleaning_log_file)
              # print(colnames(data))
              must_be_colname_in(parameters$data.uuid.column.name, name.par = "data.uuid.column.name",data, parameters$data_file)
              
            }
            
            if(parameters$stratified){
              must_be_colname_in(parameters$sampling.frame.population.column, name.par = "sampling.frame.population.column", samplingframe, parameters$sampling.frame.file)
              must_be_colname_in(parameters$sampling.frame.stratum.column, name.par = "sampling.frame.stratum.column",samplingframe, parameters$sampling.frame.file)
              must_be_colname_in(parameters$data.stratum.column, name.par = "data.stratum.column", data, parameters$data_file)

            }
            
            if(parameters$questionnaire_available){
              
              
              
            }
          }
          

# SANITATION UTILITIES
########################################          
          
  # meta  
          
          
          
    start_sanitising<-function(){
      san_issue_log<<-c()
    }
    

    normalise_parameters<-function(parameters){
      
      character.pars<-c("data.uuid.column.name",
                        "cleaning.log.uuid.column",
                        "cleaning.log.new.value.column",
                        "cleaning.log.variable.column",
                        "sampling.frame.population.column",
                        "sampling.frame.stratum.column",
                        "data.stratum.column",
                        "disaggregate_by")
      character.pars<-character.pars[which(character.pars %in% names(parameters))]
      
      params_clean<-parameters

      for(par in character.pars){
        # print(par)
        # print(params_clean[[par]])
          params_clean[[par]]<-reachR:::to_alphanumeric_lowercase(parameters[[par]])
          # print(params_clean[[par]])
          # print("------")
          }  
      return(params_clean)
    }


    
    sanitising_issue<-function(condition,message){
      if(!condition){
        
        san_issue_log<<-c(san_issue_log,message)
        return(message)}
      return(NULL)
    }
    
    

  # generic sanitation functions


  


'%must.be.one.of'<-function(param.name,y){
  sanitising_issue(parameters[[param.name]] %in% y, paste(parameters[[param.name]], "specified in ",param.name, "must be one of:",paste(y,collapse = ",",.)))
}

'%is.true.so.file.must.exist%'<-function(param.name,par.name.with.file.name){
  if(!param.name){return(NULL)}
  return(file.must.exist(par.name.with.file.name))
}

param.must.exist<-function(par.name){
  sanitising_issue(!is.null(parameters[[par.name]]),paste(par.name, "must be specified!"))
}

file.must.exist<-function(par.name.with.file.name){
  sanitising_issue(file.exists(parameters[[par.name.with.file.name]]),paste("the file'", parameters[[par.name.with.file.name]], "'specified in'", par.name.with.file.name, "' was not found"))
}


  
  
`%is_true_so_must_also_have%`<-function(name.par.A,name.par.B){
  sanitising_issue(when(parameters[[name.par.A]]) %then% (!is.null(parameters[[name.par.B]])),
                   paste(name.par.A, "is TRUE, so you must also provide a parameter for",name.par.B))
}  
  


'%must_be_colname_in%'<-function(name.par.A,data.object.name){
  sanitising_issue(parameters[[name.par.A]] %in% names(get(data.object.name)),
                   paste(parameters[[name.par.A]], "as specified in", name.par.A , "has to be a column name in ",data.object.name))
}


must_be_colname_in<-function(par,name.par,data,data.name){
  sanitising_issue(par %in% names(data),
                   paste(par, "as specified in", name.par , "has to be a column name in ",data.name))
}








have.no.empty.rows<-function(data){
  count_empty_rows<-apply(data,1,function(x){is.na(x) %>% all}) %>% which %>% length
  if(all_empty_rows!=0){
    sanitising_issue(paste("found",count_empty_rows,"completely empty rows"))
  }
}





  


is.rcid.format<-function(x){
  (grep("[A-z][A-z][A-z][0-9][0-9][0-9][a-z]*",x) %>% length)>0
  }


when<-function(x){as.logical(x)}

'%then%'<-function(x,y){
  y<-as.logical(y)
  x<-as.logical(x)
  if(!x){return(TRUE)}
  if(x & y){return(TRUE)}
  if(x & !y){return(FALSE)}
}



