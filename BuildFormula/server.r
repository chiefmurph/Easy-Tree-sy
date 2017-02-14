shinyServer(function(input, output, session) {
  output$value_depvar <- renderText({ paste(input$depvar, collapse = ",") })
  output$treeformula <- renderText({ 
    paste(input$depvar, paste(input$indepvar, collapse = " + "), sep = " ~ ") 
    })
  output$selectedwtvar <- renderText(input$wtvar)
  observe({
    updateCheckboxGroupInput(session, 
                             "indepvar", 
                             choices = setdiff(names(mtcars), c(input$depvar, input$wtvar)),
                             selected = setdiff(isolate(input$indepvar), c(input$depvar, input$wtvar)))
  })
  observe({
    updateSelectInput(session, 
                             "wtvar", 
                             choices = setdiff(
                               c("None selected", names(mtcars)), input$depvar),
                             selected = setdiff(isolate(input$wtvar), input$indepvar))
  })
})