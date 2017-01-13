shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("Easy-Tree-sy: Split portfolio into Test/Train/Validate"),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        fileInput('file1', 'Choose csv file holding claim detail'
                  , accept = c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')
        ),
        radioButtons("d0", "0", c("Tst", "Trn", "Val"), selected = "Tst", inline=TRUE),
        radioButtons("d1", "1", c("Tst", "Trn", "Val"), selected = "Tst", inline=TRUE),
        radioButtons("d2", "2", c("Tst", "Trn", "Val"), selected = "Tst", inline=TRUE),
        radioButtons("d3", "3", c("Tst", "Trn", "Val"), selected = "Tst", inline=TRUE),
        radioButtons("d4", "4", c("Tst", "Trn", "Val"), selected = "Trn", inline=TRUE),
        radioButtons("d5", "5", c("Tst", "Trn", "Val"), selected = "Trn", inline=TRUE),
        radioButtons("d6", "6", c("Tst", "Trn", "Val"), selected = "Trn", inline=TRUE),
        radioButtons("d7", "7", c("Tst", "Trn", "Val"), selected = "Val", inline=TRUE),
        radioButtons("d8", "8", c("Tst", "Trn", "Val"), selected = "Val", inline=TRUE),
        radioButtons("d9", "9", c("Tst", "Trn", "Val"), selected = "Val", inline=TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("summaryLR"),
        downloadButton("downloadSplit1", label = "Download Test"),
        downloadButton("downloadSplit2", label = "Download Train"),
        downloadButton("downloadSplit3", label = "Download Validate")
#        , verbatimTextOutput("binPolnoVals")
      )
    )
  )
)
