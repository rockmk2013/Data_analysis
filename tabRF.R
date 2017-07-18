library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(randomForest)
library(rpart.plot)

rf_return <- reactive({
  # Read data
  single <- input$single
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  # Process the data (factor -> numeric ... etc)
  new_single <- singleprocess(single)
  # Select variables
  rev_vars <- c("Weekday", "Time", "InstoreTraffic","WindowsConversion","Weather","Revenue","Temperature","NormalVacation","SpecialVacation","ConsistentVacation","MonthofYear","WeekofMonth")
  revenue_train <- new_single[,rev_vars]
  instore_vars <- c("Weekday", "Time", "InstoreTraffic","Weather","Temperature","NormalVacation","SpecialVacation","ConsistentVacation","MonthofYear","WeekofMonth")
  instore_train <- new_single[,instore_vars]
  # Train the model
  revenue.rf <- randomForest(Revenue ~ ., data= revenue_train, importance=TRUE,proximity=TRUE,ntree = 200)
  instore.rf <- randomForest(InstoreTraffic ~ ., data= instore_train, importance=TRUE,proximity=TRUE,ntree = 200)
  
  revenue_data <- rf_imp_partial(revenue.rf,revenue_train)
  instore_data <- rf_imp_partial(instore.rf,instore_train)
  
  list("revenue_rf_imp" = revenue_data[["rf_imp"]],
       "revenue_partial" = revenue_data[["partial"]],
       "instore_rf_imp" = instore_data[["rf_imp"]],
       "instore_partial" = instore_data[["partial"]]
  )
})

rf_imp_partial <- function(rf,train){
  imp <- importance(rf)
  impor <- data.frame(round(imp,2))
  # partial
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  par(mfrow=c(2, 2))
  do.call("partialPlot", list(x = rf, pred.data = train, x.var = impvar[1]))
  do.call("partialPlot", list(x = rf, pred.data = train, x.var = impvar[2]))
  do.call("partialPlot", list(x = rf, pred.data = train, x.var = impvar[3]))
  do.call("partialPlot", list(x = rf, pred.data = train, x.var = impvar[4]))
  #partialPlot(rf, train,impvar[1])
  # partialPlot(rf, train,impvar[2], main=paste("Partial Dependence on ", impvar[2]))
  # partialPlot(rf, train,impvar[3], main=paste("Partial Dependence on ", impvar[3]))
  # partialPlot(rf, train,impvar[4], main=paste("Partial Dependence on ", impvar[4]))
  partial <- recordPlot()
  # rf_imp
  rf_imp <- ggplot(impor,aes(x=reorder(rownames(impor),X.IncMSE),y=X.IncMSE))+
    geom_bar(stat='identity',fill="#00BFC4") +
    coord_flip()+
    ylab("%MSE Value")+
    xlab(" ")+
    theme( plot.title = element_text(hjust = 0.5,size=12))
  return(list("rf_imp" = rf_imp,"partial" = partial))
}


singleprocess <- function(single){
  single$Time<-as.factor(single$Time)
  single$WeekNumber<-as.factor(single$WeekNumber)
  single$normal_vacation<-as.factor(single$normal_vacation)
  single$special_vacation<-as.factor(single$special_vacation)
  single$consistent_vacation<-as.factor(single$consistent_vacation)
  single$Temperature<-as.numeric(as.character(single$Temperature))
  single$Temperature[is.na(single$Temperature)]<-mean(single$Temperature,na.rm = TRUE)
  timevariables<-as.POSIXlt(single$Date)
  single$MonthofYear<-as.factor(timevariables$mon+1)
  single$WeekofMonth<-as.factor(ceiling(timevariables$mday/7))
  names(single)[names(single) == 'normal_vacation'] <- 'NormalVacation'
  names(single)[names(single) == 'special_vacation'] <- 'SpecialVacation'
  names(single)[names(single) == 'consistent_vacation'] <- 'ConsistentVacation'
  return(single)
}

output$revenue_rf_imp = renderPlot({
  revenue_rf_imp <- rf_return()[["revenue_rf_imp"]]
  print(revenue_rf_imp)
})

output$revenue_partial = renderPlot({
  revenue_partial <- rf_return()[["revenue_partial"]]
  print(revenue_partial)
})

output$instore_rf_imp = renderPlot({
  instore_rf_imp <- rf_return()[["instore_rf_imp"]]
  print(instore_rf_imp)
})

output$instore_partial = renderPlot({
  instore_partial <- rf_return()[["instore_partial"]]
  print(instore_partial)
})



