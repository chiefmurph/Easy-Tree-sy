library(shiny)
options(shiny.maxRequestSize=30*1024^2)

# Helpful constants -- cbins also haas to go into ui.r
cbins <- c("Train", "Test", "Validate")
bins <- factor(cbins, levels = cbins)
names(bins) <- cbins

# Helpful functions
filename_only <- function(x) {
  require(tools, quietly = TRUE)
  ext <- file_ext(x)
  bname <- basename(x)
  if (ext == "") return(bname)
  ext_pos <- regexpr(ext, bname)
  substr(bname, 1, ext_pos - 2)
}
trim <- function(x) sub("[[:space:]]+$","",sub("^[[:space:]]+","",x))
  
shinyServer(function(input, output, session) {
  
  # bpno "reacts" to the radio button choices
  bpno <- reactive({
    structure(
      bins[c(input$d0, input$d1, input$d2, input$d3, input$d4,
             input$d5, input$d6, input$d7, input$d8, input$d9)],
      names = 0:9
    )
  })
  rval <- reactiveValues() # needed for rval$df

  observeEvent(input$file1, { # New file chosen
    polData <- read.csv(input$file1$datapath, header=TRUE, stringsAsFactors = FALSE)
    rval$df <- polData
  })
  
  observe({
    if (is.null(rval$df)) return(NULL)
    polData <- rval$df
    polData$binPolno <- bpno()[substring(polData$polno, 
                                           nc<-nchar(trim(polData$polno)), 
                                           nc)]
    splData <- split(polData, polData$binPolno)
    rval$splData <- splData
    z <- do.call(rbind, 
            lapply(splData, function(x) 
              data.frame(n = nrow(x),
                         lr = sum(x$incloss)/sum(x$prem))))
    z <- rbind(z, 
               data.frame(
                 n = nrow(polData),
                 lr = sum(polData$incloss) / sum(polData$prem)
                 , row.names = "Total"
               )
               )
  
    output$summaryLR <- renderTable({
      z
    })
  })

  # This is a "degugging" feature. See the verbatimTextOutput("binPolnoVals")
  # textbox at the bottom of the UI, that's normally commented out.
  # The text that's rendered can be anything helpful at the time.
  output$binPolnoVals <- renderText(
    dim(rval$splData[[1]])
  )
  
  output$downloadSplit1 <- downloadHandler(
    filename = function() {
      paste(
        paste(filename_only(input$file1$name), 
              cbins[1], sep = "_"),
        "csv", sep = ".")
      },
    content = function(file) {
      write.csv(rval$splData[[1]], file, row.names = FALSE)
    }
    )
  
  output$downloadSplit2 <- downloadHandler(
    filename = function() {
      paste(
        paste(filename_only(input$file1$name), 
              cbins[2], sep = "_"),
        "csv", sep = ".")
    },
    content = function(file) {
      write.csv(rval$splData[[2]], file, row.names = FALSE)
    }
  )
    
  output$downloadSplit3 <- downloadHandler(
    filename = function() {
      paste(
        paste(filename_only(input$file1$name), 
              cbins[3], sep = "_"),
        "csv", sep = ".")
    },
    content = function(file) {
      write.csv(rval$splData[[3]], file, row.names = FALSE)
    }
  )
  
} #end shinyServer
)
