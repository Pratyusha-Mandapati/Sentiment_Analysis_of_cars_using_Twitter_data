library(shiny)
shinyServer(function(input, output, session) {
  
  # This shows how Image sends pre-rendered images
  output$Image <- renderImage({
    if (is.null(input$analysis))
      return(NULL)
    
    
    if (input$analysis == "Score Sentiment") {
      return(list(
        src = "images/Rplot_scorevalues.png",
        contentType = "image/png",
        alt = "Score Sentiment"
      ))
      
      
    } else if (input$analysis == "Average Sentiment") {
      return(list(
        src = "images/Rplot_avgsentiment.png",
        filetype = "image/png",
        alt = "Average Sentiment"
      ))
      
      
    }else if (input$analysis == "Very Positive Sentiment") {
      return(list(
        src = "images/Rplot01_verypositive.png",
        filetype = "image/png",
        alt = "Very Positive Sentiment"
      ))
      
      
    }else if (input$analysis == "Very Negative Sentiment") {
      return(list(
        src = "images/Rplot_negativesentiment.png",
        filetype = "image/png",
        alt = "Very Negative Sentiment"
      ))
      
      
    }else if (input$analysis == "Word Cloud") {
      return(list(
        src = "images/wordcloud.png",
        filetype = "image/png",
        alt = "Word Cloud"
      ))
        
    }
    
  }, deleteFile = FALSE)
})

