
reach_style_small_horizontal_barchart<-function(group,percent){
  require('extrafont')
  percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}
  df<-data.frame(group=group,percent=percent)
  
  ggplot(df,aes(x=group,y=percent),ylim=1)+
    geom_col()+
    geom_text(aes(x=group,label=paste0("  ", round(percent*100),"%"," ",group)),size=6,family="Arial Narrow",hjust='left')+
    theme_tufte()+
    
    reachR:::reach_theme()+
    theme(text=element_text(family="Arial Narrow"))+
    theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )+
    coord_flip()+
    scale_y_continuous(labels = percent_formats,limits=c(0,1))
  
  
}

#########################################################################################
#  issues
#########################################################################################

