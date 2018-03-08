require(ggplot2)
library(plotly)
require(gridExtra)
source("predictChurn.R")

getTestList <- function(){
  if(file.exists("./output/tested_customers.csv")){
    tested = read.csv("./output/tested_customers.csv")
    tested = tested[,!names(tested) %in% 'X']
    return (tested)
  }
  return(NULL)
}
getPredictedData <- function(){
  if(file.exists("./output/predicted_customers.csv")){
    predicted = read.csv("./output/predicted_customers.csv")
    predicted = predicted[,!names(predicted) %in% 'X']
    return (predicted)
  }
  return(NULL)
}

getCompareData <- function(comparelist,compareplot){
  if(file.exists("./output/compare_data.csv")){
    compared = read.csv("./output/compare_data.csv")
    compared = compared[,!names(compared) %in% 'X']
    return(compared)
  }
  return(NULL)
}
getAccuracyPlot <- function(compared){
  
  #plot grouped barplot 
  category <- c(rep('Happy',2), rep('Unhappy',2))
  dataSet <- rep(c('Tested','Predicted'),2)
  posTest = nrow(compared[compared$Tested_Churn == 'no',])
  negTest = nrow(compared[compared$Tested_Churn == 'yes',])
  posPred = nrow(compared[compared$Predicted_Churn == 'no',])
  negPred = nrow(compared[compared$Predicted_Churn == 'yes',])
  value <- c(posTest,posPred,negTest,negPred)
  
  data <- data.frame(category,dataSet,value)
  print(data)
  b <- ggplot(data, aes(fill=dataSet, y=value, x=category)) + 
    geom_bar(position="dodge", stat="identity")+
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25,colour="black")
  
  #calculate Algo Accuracy
  acc = nrow(compared[compared$Tested_Churn == compared$Predicted_Churn,])
  accPercent = round((acc/nrow(compared))*100,digits = 2)
  
  #plot gaguge meter of accuracy
  g <- gaugeMeter(accPercent,breaks=c(0,35,70,100))
  
  #create subplot and merge
  grid.arrange(b, g, ncol=2)
}

gaugeMeter <- function(pos,breaks=c(0,30,70,100)) {
  
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

getMatrixDataPlot <- function(mdata){
  
  pos = nrow(mdata[mdata$Churn == 'no',])
  neg = nrow(mdata[mdata$Churn == 'yes',])
  testMat = data.frame('category' = c('Happy','Unhappy'),'ratio' = c(pos,neg))
  # p = ggplot(data=testMat,aes(x="",y=ratio,fill = category)) +
  #   geom_bar(width = 1,stat = 'identity') + coord_polar("y",start = 0)
  p <- plot_ly(testMat, labels = ~category, values = ~ratio, type = 'pie',hoverinfo = 'text',
               text = ~paste(category," :",ratio)) %>%
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  print(testMat)
  
  return (p)
}
getFeatureImp <- function(){
  if(file.exists("./output/predictor model.rda")){
    print("yes")
    rfModel = readRDS("./output/predictor model.rda")
    groupPlot = calculateVarImportance(rfModel)
    return(groupPlot)
  }else{
    return(NULL)
  }
}


shinyInput <- function(FUN,id,val, ...) {
  inputs <- character(length(val))
  j = 1
  for (i in val) {
    inputs[j] <- as.character(FUN(paste0(id,i), ...))
    j=j+1
  }
  return (inputs)
}

showMessage <- function(){
  showModal(modalDialog(
    title = "Message",
    "No data to show. Please Predict first",
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  ))
}

deleteFiles <- function(){
  if(file.exists("./output/tested_customers.csv"))
    file.remove("./output/tested_customers.csv")
  if(file.exists("./output/predicted_customers.csv"))
    file.remove("./output/predicted_customers.csv")
  if(file.exists("./output/compare_data.csv"))
    file.remove("./output/compare_data.csv")
  if(file.exists("./output/predictor model.rda"))
    file.remove("./output/predictor model.rda")
  if(file.exists("./output/var_contri.csv"))
    file.remove("./output/var_contri.csv")
  return(1)
}