library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(randomForest)

singleprocess <- function(single){
  single$Time<-as.factor(single$Time)
  single$normal_vacation<-as.factor(single$normal_vacation)
  single$special_vacation<-as.factor(single$special_vacation)
  single$consistent_vacation<-as.factor(single$consistent_vacation)
  single$Temperature<-as.numeric(as.character(single$Temperature))
  single$Temperature[is.na(single$Temperature)]<-mean(single$Temperature,na.rm = TRUE)
  timevariables<-as.POSIXlt(single$Date)
  single$MonthofYear<-timevariables$mon+1
  single$WeekofMonth<-ceiling(timevariables$mday/7)
  return(single)
}

output$revenue_rf_imp = renderPlot({
  single <- input$single
  if(is.null(single))
    return(NULL) 
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "big5")
  single <- singleprocess(single)
  revenue_train <- single[c(3:5,7:9,18,20:24)]
  revenue.rf <- randomForest(Revenue ~ ., data= revenue_train, importance=TRUE,proximity=TRUE,ntree = 200)
  revenue_importance <- data.frame(round(importance(revenue.rf),2))
  revenue_rf_imp <- ggplot(revenue_importance,aes(x=reorder(rownames(revenue_importance),X.IncMSE),y=X.IncMSE))+
    geom_bar(stat='identity',fill="#00BFC4") +
    coord_flip()+
    ylab("%MSE Value")+
    xlab(" ")+
    theme( plot.title = element_text(hjust = 0.5,size=12))
  return(revenue_rf_imp)
})

output$revenue_rf_tree = renderPlot({
  single <- input$single
  if(is.null(single))
    return(NULL) 
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "big5")
  single <- singleprocess(single)
  revenue_train <- single[c(3:5,7:9,18,20:24)]
  revenue_tree<-rpart(Revenue~. ,revenue_train)
  revenue_rf_tree <- rpart.plot::rpart.plot(revenue_tree)
  return(revenue_rf_tree)
})





