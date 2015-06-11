# This is the user-interface definition of a Shiny web application.
library(shiny)
shinyUI(fluidPage(
  titlePanel("Sentiment Analysis of Cars on Twitter Data"),
  
  fluidRow(
    column(4, wellPanel(
      selectInput("analysis", "Sentiment Analysis of:",
                   c("Score Sentiment",
                     "Average Sentiment",
                     "Very Positive Sentiment",
                     "Very Negative Sentiment",
                     "Word Cloud"))
    )),
    column(4,
           imageOutput("Image",height=700, width=700)
    )
  )
))
