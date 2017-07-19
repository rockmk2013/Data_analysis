library(shiny)
library(ggplot2)
library(dplyr)

staff_return <- reactive({
  # Read data
  agent <- input$agent
  agent <- read.table(agent$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  single <- input$single
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  data<-combine_data(single,agent)
  
  data_list<-lapply(1:7,filter_data,dataframe=data)
  
  data_list<-lapply(data_list,remove_outlier)
  names(data_list)<-c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
  
  summary_list<-lapply(data_list,data_summarise)  
  names(summary_list)<-names(data_list)
  
  singleframe<-create_table(summary_list)
  
  tablelist<-lapply(1:7,find_daychange,summary_list)
  names(tablelist)<-c('Monday_table','Tuesday_table','Wednesday_table','Thursday_table',
                      'Friday_table','Saturday_table','Sunday_table')
  
  plotlist<-lapply(1:7,plotfun,summary_list,data_list)
  names(plotlist)<-c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

  c(list("singleframe"=singleframe),plotlist,tablelist)
})


combine_data<-function(single,agent){
  agent<-agent[-1]
  agent<-unname(agent)
  labor<-NULL
  for(i in 1:nrow(agent))labor<-append(labor,as.vector(as.matrix(agent[i,])))
  single$Labor<-labor
  single$STAR<-single$InstoreTraffic/single$Labor

  return(single)
}

filter_data<-function(dataframe,weeknum){
  df<-dataframe%>%filter(WeekNumber==weeknum)
  return(df)
}

remove_outlier<-function(df){
  df%>%
    group_by(Time)%>%
    filter((InstoreTraffic-mean(InstoreTraffic))/sd(InstoreTraffic) < 2 &  (InstoreTraffic-mean(InstoreTraffic))/sd(InstoreTraffic) > -2)
  return(df)
}

data_summarise<-function(df){
  summary<-df%>%
    group_by(Time)%>%
    summarise(sum_STAR=sum(STAR),sum_instore=sum(InstoreTraffic),k=n(),sum_labor=sum(Labor))
  best_STAR<-sum(summary$sum_STAR)/sum(summary$k)
  
  summary<-summary%>%mutate(hour_STAR=sum_STAR/k,
                            good_labor=sum_instore/best_STAR,
                            original_labor=sum_labor/k)
  summary<-data.frame(summary)
  summary$new_labor=summary$good_labor/summary$k
  upper = max(summary$original_labor)+1.5*sd(df$Labor)
  boundary<-upper*mean(summary$k)
  summary$recommend_sumlabor<-ifelse(summary$good_labor>boundary,boundary,summary$good_labor)
  summary$recommend_labor<-summary$recommend_sumlabor/summary$k
  summary$dif<-summary$recommend_sumlabor-summary$sum_labor
  summary$perdif<-summary$dif/summary$k
  return(summary)
}

create_table<-function(summary){
  singleframe<-sapply(1:7,function(x)round(summary[[x]][["perdif"]],1))
  Time<-as.data.frame(summary[[1]][[1]])
  singleframe<-cbind(Time,singleframe)
  vector<-c("Sum",colSums(singleframe[-1]))
  singleframe<-rbind(singleframe,vector)
  colnames(singleframe)<-c("Time",names(summary))
  return(singleframe)
}

find_daychange<-function(num,summary){
  df<-summary[[num]]
  frame<-cbind(df$Time,round(df$original_labor,1),round(df$recommend_labor,1))
  colnames(frame)<-c("Time","Original","Recommend")
  return(frame)
}

plotfun<-function(num,summary,dat){
  df<-summary[[num]]
  df2<-dat[[num]]
  findmax = data.frame (df2 %>% group_by(Time) %>% summarise(mean(Labor)))
  upper = max(findmax$mean.Labor.)+1.5*sd(df2$Labor)
  g<-ggplot(df)+
    geom_bar(aes(x=as.factor(Time),weight=sum_instore/5),fill="#00BFC4")+
    geom_line(aes(x=as.factor(Time),y=sum_labor,group=1,col="原始排班",linetype="原始排班"),show.legend=TRUE,lwd=1.5)+
    geom_line(aes(x=as.factor(Time),y=good_labor,group=1,col="建議排班",linetype="建議排班"),show.legend=TRUE,lwd=1.5)+
    geom_hline(aes(yintercept=upper*mean(k) ,col="排班上界",linetype="排班上界"),lwd=1)+
    scale_colour_manual(name="Legend", values = c("原始排班" = "red", "建議排班" = "orange", "排班上界" = "black"))+
    scale_linetype_manual(name="Legend", values = c("原始排班" = "solid", "建議排班" = "solid", "排班上界" = "dashed"))+
    xlab("時間")+
    ggtitle(paste0("員工平均對應顧客數為",round(sum(df$sum_STAR)/sum(df$k),1)))+
    theme_bw()+
    theme(
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5,size=16),
      axis.text.x = element_text(size=16)
    )
  return(g)
}  

output$singleframe = renderDataTable({
  singleframe <- staff_return()[["singleframe"]]
  #print(DT::datatable(singleframe, options = list(searching = FALSE, paging = FALSE)))
})

output$Monday_table = renderDataTable({
  Monday_table <- staff_return()[["Monday_table"]]
  #print(DT::datatable(Monday_table, options = list(searching = FALSE, paging = FALSE)))
})

output$Tuesday_table = renderDataTable({
  Tuesday_table <- staff_return()[["Tuesday_table"]]
  #print(DT::datatable(Tuesday_table, options = list(searching = FALSE, paging = FALSE)))
})

output$Wednesday_table = renderDataTable({
  Wednesday_table <- staff_return()[["Wednesday_table"]]
  #print(DT::datatable(Wednesday_table, options = list(searching = FALSE, paging = FALSE)))
})

output$Thursday_table = renderDataTable({
  Thursday_table <- staff_return()[["Thursday_table"]]
  #print(DT::datatable(Thursday_table, options = list(searching = FALSE, paging = FALSE)))
})

output$Friday_table = renderDataTable({
  Friday_table <- staff_return()[["Friday_table"]]
  #print(DT::datatable(Friday_table, options = list(searching = FALSE, paging = FALSE)))
})

output$Saturday_table = renderDataTable({
  Saturday_table <- staff_return()[["Saturday_table"]]
  #print(DT::datatable(Saturday_table, options = list(searching = FALSE, paging = FALSE)))
})

output$Sunday_table = renderDataTable({
  Sunday_table <- staff_return()[["Sunday_table"]]
  #print(DT::datatable(Sunday_table, options = list(searching = FALSE, paging = FALSE)))
})

output$Monday = renderPlot({
  Monday <- staff_return()[["Monday"]]
  print(Monday)
})

output$Tuesday = renderPlot({
  Tuesday <- staff_return()[["Tuesday"]]
  print(Tuesday)
})

output$Wednesday = renderPlot({
  Wednesday <- staff_return()[["Wednesday"]]
  print(Wednesday)
})

output$Thursday = renderPlot({
  Thursday <- staff_return()[["Thursday"]]
  print(Thursday)
})

output$Friday = renderPlot({
  Friday <- staff_return()[["Friday"]]
  print(Friday)
})

output$Saturday = renderPlot({
  Saturday <- staff_return()[["Saturday"]]
  print(Saturday)
})

output$Sunday = renderPlot({
  Sunday <- staff_return()[["Sunday"]]
  print(Sunday)
})
