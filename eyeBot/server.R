library(shiny)
library(shinyBS)
library(DT)
library(plotly)

shinyServer(function(input, output, session) {
  dir.create(file.path(getwd(), "output"), showWarnings = FALSE)
  #setwd(file.path(mainDir, subDir))
  source("predictChurn.R")
  source("handler.R")
  loadpkg()
 
  loadData <- function(){
    tested = getTestList()
    if(!is.null(tested)){
      output$testedlist <- renderDataTable({
        tested
      }, options = list(orderClasses = TRUE))
    }
    
    predicted =  getPredictedData()
    if(!is.null(predicted)){
      action = shinyInput(actionButton,'button_', predicted$customerID, label = "Learn More", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
      predictedv2 = cbind(action,predicted)
      reactPredicted = reactiveValues(data = predictedv2)
      output$predictedlist <- DT::renderDataTable(
        reactPredicted$data , server = FALSE, escape = FALSE, selection = 'none'
      )
    }
    
    compared = getCompareData()
    if(!is.null(compared)){
      output$comparelist <- renderDataTable({
        compared
      })
    }
  }
  
  loadData()
  
  # tiggered when you click "Predict"
  observeEvent(input$PredictButton, {
    filename1 = input$Files[1]
    filename2 = input$browsefile[1]
    print(filename1)
    print(filename2)
    if(is.null(filename1) && is.null(filename2)){
      showModal(modalDialog(
        title = "Error Message",
        "No file chosen.Please choose a file for the model",
        easyClose = FALSE,
        footer = modalButton("Ok")
      ))
    }
    else{
      res = 0
      if(!is.null(filename1))
        filePath = paste("./data/",filename1,sep = "")
      else
        filePath = paste("./data/",filename2,sep = "")
      print(filePath)
      res = predictChurn(filePath)
      loadData()
      if(res == 1){
        showModal(modalDialog(
          title = "Churn Prediction Completed",
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      }
    }
    
    # output$compareplot <- renderPlot({
    #   getComparePlot(compared)
    # })
    
  })
  
  #to popup modal for variable contribution of each customer
  observeEvent(input$select_button, {
       custId <-(strsplit(input$select_button, "_")[[1]][2])
       print(strsplit(input$select_button, "_")[[1]][2])
       p = plotVarContri(as.character(custId))
       output$varContriPlot <- renderPlotly({
         p
       })
       toggleModal(session, "varContriModal", toggle = "toggle")
  })
  
  #to popup modal for feature importance
  observeEvent(input$Submit1,{
    p = getFeatureImp()
    if(is.null(p)){
      showMessage()
    }else{
      output$featurePlot <- renderPlotly({
        p
      })
      toggleModal(session, "featureModal", toggle = "toggle")
    }
    
  })
  # to popup modal for tested data plot
  observeEvent(input$Submit2,{
    tested = getTestList();
    if(!is.null(tested)){
      output$testedplot <- renderPlotly({
        getMatrixDataPlot(tested)
      })
      toggleModal(session, "testDataModal", toggle = "toggle")
    }else{
      showMessage()
    }
    
  })
  # to pop up modal for predicted data plot
  observeEvent(input$Submit3,{
    predicted =  getPredictedData()
    if(!is.null(predicted)){
      output$predictedplot <- renderPlotly({
        getMatrixDataPlot(predicted)
      })
      toggleModal(session, "predictedModal", toggle = "toggle")
    }
    else{
      showMessage()
    }
    
  })
  
  # to pop up modal for accuracy data plot
  observeEvent(input$Submit4,{
    compared =  getCompareData()
    if(!is.null(compared)){
      output$accuracyplot <- renderPlot({
        getAccuracyPlot(compared)
      })
      toggleModal(session, "accuracyModal", toggle = "toggle")
    }
    else{
      showMessage()
    }
    
  })
  
  
  observeEvent(input$deleteButton,{
    deleteFiles()
    showModal(modalDialog(
      title = "Message",
      "Deleted All Output Files",
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })
  
  # plotInput <- function(){varContriEv()}
  # 
  # output$downloadPlot <- downloadHandler(
  #   filename = "Shinyplot.png",
  #   content = function(file) {
  #     png(file)
  #     plotInput()
  #     dev.off()
  #   }) 
  
})