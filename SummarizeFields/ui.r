shinyUI(pageWithSidebar(
  headerPanel("Easy Tree-sy: Summarize Fields"),
  sidebarPanel(
         fileInput('file1', 'Choose csv file holding policy detail'
                   , accept = c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.csv')
                   ),
         hr(),
#         selectInput('colName', label = "Identify the policy number column", 
#                     selectize = TRUE,
#                        choices = NULL),
         # close window from:
         # http://data-steve.github.io/shiny-shutdown-with-browser-close/
         actionButton("exitBtn", "Exit app",
                      onclick = "setTimeout(function(){window.close();},500);")
         , hr()
         , textOutput('ready')
  ),
  mainPanel(
    tableOutput("FieldSummary")
  )
)
)
