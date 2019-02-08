

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
