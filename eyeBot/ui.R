library(shiny)
library(shinyBS)
library(plotly)
library(DT)
library(shinyjs)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto; text-align:center; font-weight:bold;"
  )
}


fluidPage(
  useShinyjs(),
  
  tags$head(tags$style(
    type="text/css",
    "img {max-width: 90%;max-height: 90%; width: auto; height: auto; display: block; margin-left: auto; margin-right: auto}",
    ".thumb {height:180px;width:200px;padding-left:none;padding-right:none;}",
    ".modal-lg {
    width: 90%;height:85%;text-align: center;
    }"
    )),
  
  fluidRow(column(12,style='background-color: none;',fluidRow(column(12,tags$span(align="center",img(align="left",style='max-width: 25%;max-height: 25%; ',src='botimages/amdocsbrand.png'),img(align="left",src='botimages/amdocs.png')))))),
  # Application title
  # titlePanel("_____________Welcome to Amdocs Churn Prediction Tool__________________",
  #            windowTitle = 'eyeBot'), 
  
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        
        column(12,
               
               p("Welcome to AMDOCS eyeBOT! This intelligent Churn Prediction Tool assess all customers and aim to predict churn and loyalty behaviour based on the analysis of demographic data, customer purchases history, service usage and billing data.
", style="font-family: 'Georgia';font-weight: bold;color:#660033")
        )
      ),
      fluidRow(

        column(12,
               
               img(style='max-width: 70%;max-height: 70%;',src = "botimages/robot.png" )
        )
      ),
      fluidRow(
        
        column(12,selectInput("Files", "Choose One:",
                              c("None" = NULL,
                                "Telco Data" = "Telco-Customer-Churn.csv",
                                "EDW data" = "edw_cdr.csv")))
        
      ),
      tags$br(),
      fluidRow(
        
        column(12,actionButton("PredictButton", "Predict Churn!"))
             
      ),
      tags$br(),
      fluidRow(
        column(12,actionButton("deleteButton", "Delete Output"))
              
      )
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Grid", 
                 fluidRow( 
                   column(12,
                          
                          # fluidRow(style='padding-top:60px; padding-right:30px',
                          #          
                          #          column(4,style='padding-right:1px;',img(src = "botimages/p2.png",id='viewFeature')
                          #          ),
                          #          
                          #          column(4,style='padding-right:1px;',img(src = "botimages/p3.png" , id="plotTest")
                          #          ),
                          #          
                          #          column(4, style='padding-right:5px;',img(src = "botimages/p1.png" ,id="plotPredicted")
                          #          )
                          #          
                          # ),
                          fluidRow(style='padding-top:60px; padding-right:30px',
                                   
                                   column(4,
                                          actionButton("Submit1",  alignCenter(img(src = "botimages/giphy1.gif",class="thumb")))
                                          
                                   ),
                                   
                                   column(4,
                                          actionButton("Submit2",alignCenter(img(src = "botimages/giphy2.gif",class="thumb" )))
                                          
                                   ),
                                   
                                   column(4, 
                                          actionButton("Submit3", alignCenter(img(src = "botimages/giphy3.gif",class="thumb")))
                                          
                                   )
                                   
                          ),
                          # fluidRow(style='padding-top:20px; padding-right:30px',
                          #          
                          #          column(4,style='padding-right:1px;',img(src = "botimages/p1.png" ,id="plotAccuracy")
                          #          ),
                          #          
                          #          column(4,style='padding-right:1px;',img(src = "botimages/p2.png" )
                          #          ),
                          #          
                          #          column(4, style='padding-right:5px;',img(src = "botimages/p3.png" )
                          #          )
                          #          
                          # )
                          fluidRow(style='padding-top:20px; padding-right:30px',
                                   
                                   column(4,
                                          actionButton("Submit4",  alignCenter(img(src = "botimages/giphy4.gif",class="thumb")))
                                          
                                   ),
                                   
                                   column(4,
                                          actionButton("Submit5",  alignCenter(img(src = "botimages/giphy.gif",class="thumb")))
                                          
                                   ),
                                   
                                   column(4, 
                                          actionButton("Submit6", alignCenter(img(src = "botimages/giphy.gif",class="thumb")))
                                          
                                   )
                                   
                          )
                   ))
        ),          
        tabPanel('Tested Data',
                 tabPanel("list view",dataTableOutput("testedlist"))
        ),
        tabPanel('Predicted Data',
                 tabPanel("list view",DT::dataTableOutput("predictedlist"))
        ),
        tabPanel('Compare Data',
                 tabPanel("list view",dataTableOutput("comparelist"))
        ),
        tabPanel('Help',
                 tabPanel("akaf as asfhashf  alshflkasf lasl aslfh alsfh lasf las fahs la slf ashf")
        )
        
      ),
      # div(plotOutput('varContriPlot'),style="display:block;"),
      # bsModal("modalExample", "Your plot", "go", size = "large",
      #         plotOutput("varContriPlot"),downloadButton('downloadPlot', 'Download'))
      
      bsModal("featureModal", "Feature Grouping and Scaling Importance", "", size = "large",
              plotlyOutput('featurePlot')),
      bsModal("varContriModal", "Parameter Importance of Your Customer", "", size = "large",
              plotlyOutput('varContriPlot')),
      bsModal("testDataModal", "Tested customer Churn Plot", "", size = "large",
              plotlyOutput('testedplot')),
      bsModal("predictedModal", "Predicted Customer Churn Plot", "", size = "large",
              plotlyOutput('predictedplot')),
      bsModal("accuracyModal", "Accuracy of Prediction", "", size = "large",
              plotOutput('accuracyplot'))
    )
  ),
  
  # tags$script('document.getElementById("viewF").onclick = function() {
  #             var number = 
  #             Shiny.onInputChange("mydata", number);
  #             };
  #             '),
  tags$script('$(document).ready(function() {
              $("#viewFeature").on("click",function(){
              id = $("#viewFeature").attr("id")+Math.random()
              Shiny.onInputChange("viewFeature", id);
              })
              
              $("#plotTest").on("click",function(){
              id = $("#plotTest").attr("id")+Math.random()
              Shiny.onInputChange("plotTest", id);
              })
              
              $("#plotPredicted").on("click",function(){
              id = $("#plotPredicted").attr("id")+Math.random()
              Shiny.onInputChange("plotPredicted", id);
              })
              $("#plotAccuracy").on("click",function(){
              id = $("#plotAccuracy").attr("id")+Math.random()
              Shiny.onInputChange("plotAccuracy", id);
              })
              
              
              
              
              })')
  
  )