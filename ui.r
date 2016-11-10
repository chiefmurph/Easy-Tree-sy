shinyUI(pageWithSidebar(
  headerPanel("Make Triangles from Detailed Claims Table"),
  sidebarPanel(
         fileInput('file1', 'Choose csv file holding claim detail'
                   , accept = c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.csv')
                   ),
         hr(),
         actionButton("adday", "Add 'ay' and 'ayage'"),
         actionButton("addcounts", "Add count columns"),
         br(),
         hr(),
         selectInput('colName', label = "Select column to triangulate", 
                     selectize = TRUE,
                        choices = NULL),
         actionButton("triangulate", "Form triangle"),
         checkboxGroupInput("trishow", "Show:",
                            c("plot"         = "plot",
                              "triangle"     = "tri",
                              "ata triangle" = "ata"),
                            selected = c("plot", "tri"))

  ),
  mainPanel(
    uiOutput("triangleUI")
  )
)
)
