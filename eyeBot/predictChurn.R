loadpkg <- function(){
  #load required packages
  require(data.table)
  #require(logging)
  library(randomForest)
  #library(devtools)
  library(rfFC)
  library(ggplot2)
  library(plotly)
}


predictChurn <- function(filePath){
  if(!is.null(filePath)){
    churn_data = fread(filePath,fill = TRUE)
  }
  
  #loginfo("loaded data with %s rows",rowNum)
  dropCol <- c('customerID')
  
  #format the class data 
  churn_data <- churn_data[complete.cases(churn_data)]
  set.seed(10)
  churn_data <- churn_data[!duplicated(churn_data$customerID)]
  churn_data <- as.data.frame(churn_data)
  levels(churn_data$Churn) <- c("yes","no")
  churn_data$Churn[churn_data$Churn == 1] <- "yes"
  churn_data$Churn[churn_data$Churn == 0] <- "no"
  churn_data$Churn <- factor(churn_data$Churn)
  churn_data$customerID <- as.character(churn_data$customerID)
  
  #clean NA,NAN,inf values
  #impute.mean <- function(x) replace(x, is.na(x) | is.nan(x) | is.infinite(x), mean(x[!is.na(x) & !is.nan(x) & !is.infinite(x)]))
  #churn_data <- apply(churn_data,2,impute.mean)
  
  #if any column is of character,change it to factor to feed in to random search func
  for(i in names(churn_data)){
    if(is.character(churn_data[[i]]))
      churn_data[[i]] <- as.factor(churn_data[[i]])
  }
  
  #drop the customerID column in trainData and 90% of dataset for train_data
  rowNum = nrow(churn_data)
  trainRows = ceiling((rowNum*90)/100)
  train_set = churn_data[1:trainRows,]
  train_data = churn_data[1:trainRows,!(names(churn_data) %in% dropCol)]
  
  #10% of the data set for test_data
  test_set = churn_data[1:(rowNum-trainRows),]
  #cust data
  cust_att = c('customerID','age','gender','phone','annualincome','state','maritalstatus','occupation')
  cust_data = test_set[,(names(test_set) %in% cust_att)]
  tested_customers = test_set[,c(names(cust_data),'Churn')]
  write.csv(tested_customers, "./output/tested_customers.csv")
  
  
  test_data = churn_data[1:(rowNum-trainRows),!(names(churn_data) %in% dropCol)]
  
  #calculate mtry value on predictor variables
  #predict_vars = train_data[,!(names(train_data) %in% 'Churn')]
  #mtry_res = tuneRF(predict_vars,train_data$Churn,ntreeTry=1000,improve = 0.05,stepFactor = 2,doBest = FALSE)
  set.seed(5)
  rF_Model <- randomForest(Churn ~., train_data,ntree=501,mtry=4,importance=TRUE, keep.inbag=TRUE,replace=FALSE) 
  
 # save(rF_Model, file = "predictor model.rda")
  saveRDS(rF_Model, file = "./output/predictor model.rda")
  sink("predictor output")
  print(rF_Model)#print the model
  sink()
  
  #test the model with test data
  predicted_data<-predict(rF_Model,newdata=test_data,type = "response")
  table(predicted_data==test_data$Churn)
  
  #merge the result with customer data
  
  predicted_customers = cbind(cust_data,Churn = predicted_data)
  #names(predicted_customers)[names(predicted_customers) == 'predicted_data'] <- 'Churn'
 
  
  #save all in csv format
  write.csv(predicted_customers,"./output/predicted_customers.csv")
  
  #compare tested and predicted
  compare_data = data.frame(
    "customerId" = tested_customers$customerID,
    "Tested_Churn" = tested_customers$Churn,
    "Predicted_Churn" = predicted_customers$Churn
  )
  write.csv(compare_data,"./output/compare_data.csv")
  
  # calculate feature contribution of predicted churn customers
  local_incre = getLocalIncrements(rF_Model,train_data)
  churn_cust <- test_set
  churn_cust$Churn  <- predicted_data
  #churn_cust <- churn_cust[churn_cust$Churn == 'yes',]
  var_contri <- featureContributions(rF_Model,local_incre,churn_cust[,!(names(churn_data) %in% dropCol)])
  var_contri <- as.data.frame(var_contri)
  var_contri <- cbind(customerID=churn_cust$customerID,var_contri)
  write.csv(as.data.frame(var_contri),"./output/var_contri.csv")
  #saveRDS(var_contri, file="'eyeBot/output/var_contri.rds')
  
  return(1)
}

  plotVarContri <- function(customerID){
    print(customerID)
    var_contriFile = read.csv('./output/var_contri.csv')
    var_contriFile <- var_contriFile[,!names(var_contriFile) %in% 'X']
    plotData = var_contriFile[var_contriFile$customerID==customerID,]
    #plotData = var_contriFile[var_contriFile$customerID==558,]
    plotData$customerID <- NULL
    plotData <- plotData[order(plotData,decreasing = TRUE)]
    topValues = head(as.numeric(as.vector(plotData)),10)
    topLabels = colnames(plotData)[1:10]
    topData = data.frame(vars=topLabels,vals=topValues)
    # bp = ggplot(data=topData, aes(x=vars, y=vals, fill=vars,width=0.5)) +
    #   geom_bar(stat="identity", width=1)
    p <- plot_ly(topData, x = ~vars, y = ~vals, type = 'bar',
                 marker = list(color = 'rgb(26, 118, 255)',hoverinfo = 'text',
                               line = list(color = 'rgb(8,48,107)', width = 1))) %>%
      layout(title = paste("Feature Importance of Customer: ",customerID),
             xaxis = list(title = "",tickangle = -45),
             yaxis = list(title = "Feature Contribution"))
    return(p)
    
  }
  
  calculateVarImportance <- function(rF_Model){
      rfModel = readRDS("./output/predictor model.rda")
      demographic <- c('age','annualincome','gender','homeowner','maritalstatus','occupation','state','SeniorCitizen')
      service <- c('calldroprate','callfailurerate','callingnum','percentagecalloutsidenetwork','MultipleLines',
                   'OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies')
      behaviour <- c('customersuspended','numberofcomplaints','numdayscontractequipmentplanexpiring',
                     'penaltytoswitch','tenure','Contract')
      billing <- c('monthlybilledamount','numberofmonthunpaid','unpaidbalance',
                   'PaperlessBilling','PaymentMethod','MonthlyCharges','TotalCharges')
      Usage <- c('totalminsusedinlastmonth','usesinternetservice','usesvoiceservice','totalcallduration','avgcallduration')
      
      vars <- importance(rfModel,type = 1);
      demographic_Vars = vars[rownames(vars) %in% demographic,]
      demographic_Vars = data.frame("var"=names(demographic_Vars),"val"=demographic_Vars)
      service_Vars = vars[rownames(vars) %in% service,]
      service_Vars = data.frame("var"=names(service_Vars),"val"=service_Vars)
      behaviour_Vars = vars[rownames(vars) %in% behaviour,]
      behaviour_Vars = data.frame("var"=names(behaviour_Vars),"val"=behaviour_Vars)
      billing_Vars = vars[rownames(vars) %in% billing,]
      billing_Vars = data.frame("var"=names(billing_Vars),"val"=billing_Vars)
      Usage_Vars = vars[rownames(vars) %in% Usage,]
      Usage_Vars = data.frame("var"=names(Usage_Vars),"val"=Usage_Vars)
      p1 <- NULL
      p2 <- NULL
      p3 <- NULL
      p4 <- NULL
      p5 <- NULL
      if(!is.null(nrow(demographic_Vars))){
        p1 <- plot_ly(demographic_Vars, x = ~var, y = ~val, type = 'bar',text=~paste(var,':',val),hoverinfo="text",
                      showlegend = FALSE,marker = list(color = 'blue',
                                                       line = list(color = 'black', width = 0.8))) %>%
          layout(xaxis = list(title = "Predictor variables",showticklabels = FALSE),
                 yaxis = list(title = "Variable Importance"))
        # l[i] <- p1
        # i=i+1
      }
      
      
      if(!is.null(nrow(service_Vars))){
        p2 <- plot_ly(service_Vars, x = ~var, y = ~val, type = 'bar',text=~paste(var,':',val),hoverinfo="text",
                      showlegend = FALSE,marker = list(color = 'orange',
                                                       line = list(color = 'black', width = 0.8))) %>%
          layout(xaxis = list(title = "Predictor variables",showticklabels = FALSE),
                 yaxis = list(title = "Variable Importance"))
        # l[i] <- p2
        # i=i+1
      }
      
      if(!is.null(nrow(behaviour_Vars))){
        p3 <- plot_ly(behaviour_Vars, x = ~var, y = ~val, type = 'bar',text=~paste(var,':',val),hoverinfo="text",
                      showlegend = FALSE,marker = list(color = 'green',
                                                       line = list(color = 'black', width = 0.8))) %>%
          layout(xaxis = list(title = "Predictor variables",showticklabels = FALSE),
                 yaxis = list(title = "Variable Importance"))
      }
      
      if(!is.null(nrow(billing_Vars))){
        p4 <- plot_ly(billing_Vars, x = ~var, y = ~val, type = 'bar',text=~paste(var,':',val),hoverinfo="text",
                      showlegend = FALSE,marker = list(color = 'purple',
                                                       line = list(color = 'black', width = 0.8))) %>%
          layout(xaxis = list(title = "Predictor variables",showticklabels = FALSE),
                 yaxis = list(title = "Variable Importance"))
      }
      
      if(!is.null(nrow(Usage_Vars)) && nrow(Usage_Vars) > 0){
        p5 <- plot_ly(Usage_Vars, x = ~var, y = ~val, type = 'bar',text=~paste(var,':',val),hoverinfo="text",
                      showlegend = FALSE,marker = list(color = 'pink',
                                                       line = list(color = 'black', width = 0.8))) %>%
          layout(xaxis = list(title = "Predictor variables",showticklabels = FALSE),
                 yaxis = list(title = "Variable Importance"))
      }
      
      if(is.null(p5)){
        p <- subplot(p1,p2,p3,p4, nrows = 2,titleX = TRUE,titleY = TRUE) %>% layout(annotations = list(
          list(x = 0 , y = 1, text = "DEMOGRAPHIC",textangle=-35, showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                               family = 'verdana',weight = 'bold',
                                                                                                               size = 32)),
          list(x = 0.8 , y = 1, text = "SERVICE",textangle=-35, showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                           family = 'verdana',weight = 'bold',
                                                                                                           size = 32)),
          list(x = 0.1 , y = 0.2, text = "BEHAVIOUR",textangle=-35, showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                            family = 'verdana',weight = 'bold',
                                                                                                            size = 32)),
          list(x = 0.8 , y = 0.2, text = "BILLING",textangle=-35, showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                          family = 'verdana',weight = 'bold',
                                                                                                          size = 32))
        )) 
          
      }
      else{
        p <- subplot(p1,p2,p3,p4,p5, nrows = 3,titleY = TRUE) %>% layout(annotations = list(
          list(x = 0.2 , y = 1, text = "DEMOGRAPHIC",showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                               family = 'verdana',weight='bold',
                                                                                                               size = 20)),
          list(x = 0.8 , y = 1, text = "SERVICE", showarrow = F, xref='paper', yref='paper',font = list(color = '#264E86',
                                                                                                           family = 'verdana', weight='bold',
                                                                                                           size = 20)),
          list(x = 0.2 , y = .68, text = "BEHAVIOUR", showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                            family = 'verdana',weight='bold',
                                                                                                            size = 20)),
          list(x = 0.8 , y = .68, text = "BILLING", showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                          family = 'verdana',weight='bold',
                                                                                                          size = 20)),
          list(x = 0.2 , y = 0.35, text = "USAGE", showarrow = F, xref='paper', yref='paper',font = list(color = 'black',
                                                                                                         family = 'verdana',weight='bold',
                                                                                                         size = 20))
        ))
      }
  
      p
      
    }
  
