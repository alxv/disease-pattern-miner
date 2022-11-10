#==========================================================================================================
#Libraries
#==========================================================================================================
library(shiny)
library(tidyverse)
library(data.table)
library(lubridate)
library(stringi)
library(stringr)
library(DT)
library(formattable)
library(shinythemes)
library(shinybrowser)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(arulesSequences)
library(dplyr)
#===========================================================================================================
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  navbarPage('DPM',collapsible = TRUE,
                             tabPanel(title = "Home", value = "home",
                                      shinybrowser::detect(),
                                      HTML('<center><img src="DPM_Logo_back.png" ></center>')
                                      
                             ),
                             navbarMenu('Analysis',
                                        tabPanel('Apriori',br(),
                                                 h4('Apriori'),
                                                 HTML('<hr style="color: blue;">'),
                                                 span('First algorithm that was proposed for frequent itemset mining.'),
                                                 span('One of the most important strategies for identifying associations between items is market basket analysis. It operates by searching the data for groups of entries that commonly appear together.'),br(),
                                                 strong(' It is known as market basket analysis.'),br(),
                                                 span('Example data set available in the documentation section'),
                                                 actionButton("howto_1", '',icon = icon("circle-question"),class = "btn-warning"),
                                                 wellPanel(fileInput("file2", "please uplod your file",multiple = FALSE,
                                                                     accept = c(".csv")
                                                 )),
                                                 uiOutput("gitlink"),
                                                 HTML('<hr style="color: blue;">'),
                                                 fluidRow(uiOutput('Apri_dn_btn_ui')),
                                                 HTML('<hr style="color: blue;">'),
                                                 plotOutput('Apri_frquency'),
                                                 HTML('<hr style="color: blue;">'),
                                                 plotOutput('Apri_net'),
                                                 
                                                 HTML('<hr style="color: blue;">'),br(),
                                                 dataTableOutput('Apri_results'),
                                                 
                                                 
                                        ),
                                        tabPanel('Spade',br(),
                                                 h4('SPADE'),
                                                 HTML('<hr style="color: blue;">'),
                                                 span('This algorithm takes time factor into consideration for pattern mining. '),
                                                 strong('It is known as sequential pattern mining'),
                                                 br(),
                                                 span('Example data set available in the documentation section'),
                                                 actionButton("howto_2", '',icon = icon("circle-question"),class = "btn-warning"),
                                                 
                                                 h4('Step 1: Data preparation'),
                                                 
                                                 wellPanel(fileInput("file3", "upload the file",multiple = FALSE,
                                                                     accept = c(".csv")
                                                 )),
                                                 uiOutput("gitlink2"),
                                                 HTML('<hr style="color: blue;">'),
                                                 
                                                 fluidRow(uiOutput('Spade_prep_dn_btn_ui')),
                                                 HTML('<hr style="color: blue;">'),
                                                 plotOutput('spade_frquency'),
                                                 
                                                 h4('Step 2: Mine patterns'),
                                                 HTML('<hr style="color: blue;">'),
                                                 
                                                 wellPanel(fileInput("spdPrpd", "Upload prepared file from step 1",multiple = FALSE,
                                                                     accept = c(".txt")
                                                 )),br(),
                                                 
                                                 dataTableOutput('Spaderesults')
                                                 
                                        ),
                                        
                                        tabPanel(title = "Serendip", value = "analysis", 
                                                 h4('SERENDIP'),
                                                 HTML('<hr style="color: blue;">'),
                                                 span('This algorithm considers order of event for pattern mining. '),
                                                 span('Example data set available in the documentation section.'),
                                                 actionButton("howto_3", '',icon = icon("circle-question"),class = "btn-warning"),
                                                 
                                                 wellPanel(fileInput("file1", "Choose the file",multiple = FALSE,
                                                                     accept = c(".csv")
                                                 )),
                                                 uiOutput("gitlink3"),
                                                 HTML('<hr style="color: blue;">'),
                                                 plotOutput('Srn_frquency'),
                                                 h4('Download'),
                                                 HTML('<hr style="color: blue;">'),
                                                 fluidRow(uiOutput('Master_results')),
                                                 h4('Results'),
                                                 HTML('<hr style="color: blue;">'),
                                                 dataTableOutput('viewresults')
                                        )
                               
                             ),
                             tabPanel(title = 'Documentation', value = 'tutorial',
                                      h4('Example data'),
                                      downloadButton("demodata", "Download sample data"),
                                      p("please maintain column names as id, date and items. web app is case sensitive (avoid special characters)",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),br(),
                                      br(),
                                      h4('Documentation'),
                                      tags$iframe(style="height:700px; width:70%; scrolling=yes", 
                                                  src="Documentation_DPM.pdf"),
                                      br(),br(),
                                      h4('Tutorial'),
                                      tags$iframe(style="height:700px; width:70%; scrolling=yes", 
                                                  src="Tutorial.pdf"),         
                             )
                  )))
