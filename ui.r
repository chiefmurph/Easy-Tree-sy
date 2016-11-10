shinyUI(pageWithSidebar(
  headerPanel("Easy Tree-sy"),
  sidebarPanel(
         fileInput('file1', 'Choose csv file holding claim detail'
                   , accept = c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.csv')
                   ),
         hr(),
         br(),
         hr(),
         selectInput('colName', label = "Select column to triangulate", 
                     selectize = TRUE,
                        choices = NULL)
  ),
  mainPanel(
    uiOutput("triangleUI")
  )
)
)
