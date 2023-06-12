library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyjs)
library(corrplot)
library(caTools)
library(randomForest)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyjs(),
  navbarPage("Data viewer app",
             tabPanel("Dataset",
                      sidebarPanel(
                        width = 2,
                        fileInput("file", "Choose CSV File", accept = ".csv"),
                        checkboxInput("header", "Header", TRUE),
                        actionButton("clear_data", "Clear"),
                        actionButton("deleteRows", "Delete Rows"),
                        actionButton("deleteCols", "Delete Cols"),
                        actionButton("hideshow_summary", "Hide/show summary"),
                        selectInput("col_select", "Select column",c("")),
                        textInput("new_col_name", "New column name", ""),
                        actionButton("rename_col_button", "Rename"),
                        downloadButton("downloadData", "Download")
                      ),
                      mainPanel(
                        verbatimTextOutput("summary"),
                        DTOutput("table")
                      )
             ),
             tabPanel(
               "Visualization",
               sidebarPanel(
                 width = 2,
                 selectInput("hist_select", "Select column to plot histogram",c("")),
                 actionButton("hideshow_correlation", "Hide/show correlation"),
                 actionButton("hideshow_histogram", "Hide/show histogram")
                 
               ),
               mainPanel(
                 plotOutput("correlation"),
                 plotOutput("histogram")
                 
               )
             ),
             tabPanel(
               "Prediction",
               sidebarPanel(width = 2,
                            sliderInput("split_ratio","Data split ratio to train:", min = 0.001, max = 0.999, value = 0.7),
                            selectInput("col_select_train", "Select column",c("")),
                            textInput("seed", "Seed:", ""),
                            textInput("tree", "Tree number:", ""),
                            actionButton("start_train", "Train model"),
                            actionButton("own_data", "Input my wine"),
                            actionButton("check_wine", "Check my wine")
               ),
               mainPanel(
                 DTOutput("confusion"),
                 tableOutput("predict"),
                 DTOutput("importance_classifier"),
                 textOutput("result"),
                 DTOutput("mydata_table"),
                 tableOutput("my_predict")
               )
             ),
             tabPanel(
               "Help",
               mainPanel(textOutput("help")))))
