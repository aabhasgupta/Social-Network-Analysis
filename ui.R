

###### Run the App in a dedicated web browser like internet explorer/chrome/mozilla for Smooth Performance #####

require(shinythemes)

setwd("C:/R_Files/ShinyFileUpload")
library(shiny)
shinyUI(fluidPage( theme = shinytheme("cerulean"),
  titlePanel(h1("AABHAS GUPTA_SOCIAL_NETWORK_ANALYSIS")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1","Upload the File containing the Email network"), # fileinput() function is used to get the file upload contorl option
      fileInput("file2","Upload the File containing department dataset"),
      h5(helpText("Select Header option below if you want to use the column names as per the file")),
      checkboxInput(inputId = 'header', label = 'Header File1', value = FALSE),
      checkboxInput(inputId = 'header2', label = 'Header File2', value = FALSE),
      br(),
      selectInput(inputId = 'sep', label = 'Separator for File 1', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ''),
      br(),
      selectInput(inputId = 'sep2', label = 'Separator for File 2', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = '')
      
    ),
    mainPanel(
      uiOutput("tb")

    )
    
  )
))