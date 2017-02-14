shinyUI(pageWithSidebar(
  headerPanel("Easy Tree-sy: Build Formula"),
  sidebarPanel(
    fileInput('file1', 'Choose data file'
              , accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')
    ),
    hr(),
    selectInput("depvar", label = "Dependent Variable:", 
              choices = names(mtcars), 
              selected = NULL, multiple = FALSE),
  selectInput("wtvar", label = "Weight Variable:", 
              choices = c("None selected", names(mtcars)), 
              selected = NULL, multiple = FALSE),
  checkboxGroupInput("indepvar", "Independent Variable(s):", names(mtcars))
  ),
  mainPanel(
    tableOutput("FieldSummary"),
    h3("tree formula"),
  verbatimTextOutput("treeformula"),
  h4("weights"),
  verbatimTextOutput("selectedwtvar")
  )
)
)