#--------------------------------------------------
# R UI Code for the Capstone Project Shiny App
#--------------------------------------------------

suppressWarnings(library(shiny))

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Predict Next Word"),
    
    fluidRow(HTML("<strong>Author: Luca Santabarbara</strong>") ),
    fluidRow(HTML("<strong>Date: 24-October-2021</strong>") ),
    
    fluidRow(
        br(),
        p("The application uses N-Gram Back Off model to predict next word. 
         The Shiny app first loads five N-grams generated within the exploratory analysis phase. 
          Then, applies a simple Katz's Back-off algorithm to predict the next word.")),
    br(),
    br(),
    
    fluidRow(HTML("<strong>Enter an incomplete sentence. Press \"Next Word\" button to predict the next word</strong>") ),
    fluidRow( p("\n") ),
    
    # Sidebar layout
    sidebarLayout(
        
        sidebarPanel(
            textInput("inputString", "Enter a partial sentence here",value = ""),
            submitButton("Next Word")
        ),
        
        mainPanel(
            h4("Predicted Next Word"),
            verbatimTextOutput("prediction"),
            textOutput('text1'),
            textOutput('text2')
        )
    )
))
