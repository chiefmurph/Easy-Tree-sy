# A helpful constant, also found in server.r
cbins <- c("Train", "Test", "Validate")
shinyUI(
  fluidPage(
    
    # Application title
    titlePanel(
      paste("Easy-Tree-sy: Split portfolio into",
            paste(cbins, collapse = "/"))
      ),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        fileInput('file1', 'Choose csv file holding policy detail'
                  , accept = c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')
        ),
        radioButtons("d0", "0", cbins, selected = cbins[1], inline=TRUE),
        radioButtons("d1", "1", cbins, selected = cbins[1], inline=TRUE),
        radioButtons("d2", "2", cbins, selected = cbins[1], inline=TRUE),
        radioButtons("d3", "3", cbins, selected = cbins[1], inline=TRUE),
        radioButtons("d4", "4", cbins, selected = cbins[2], inline=TRUE),
        radioButtons("d5", "5", cbins, selected = cbins[2], inline=TRUE),
        radioButtons("d6", "6", cbins, selected = cbins[2], inline=TRUE),
        radioButtons("d7", "7", cbins, selected = cbins[3], inline=TRUE),
        radioButtons("d8", "8", cbins, selected = cbins[3], inline=TRUE),
        radioButtons("d9", "9", cbins, selected = cbins[3], inline=TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("summaryLR"),
        downloadButton("downloadSplit1", label = paste0("Download ", cbins[1])),
        downloadButton("downloadSplit2", label = paste0("Download ", cbins[2])),
        downloadButton("downloadSplit3", label = paste0("Download ", cbins[3]))
#        , verbatimTextOutput("binPolnoVals")
      )
    )
  )
)
