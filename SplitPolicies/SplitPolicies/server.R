library(shiny)
options(shiny.maxRequestSize=30*1024^2)
filename_only <- function(x) {
  require(tools, quietly = TRUE)
  ext <- file_ext(x)
  bname <- basename(x)
  if (ext == "") return(bname)
  ext_pos <- regexpr(ext, bname)
  substr(bname, 1, ext_pos - 2)
}
## Only run examples in interactive R sessions
  bins <- factor(c("Tst", "Trn", "Val"), 
                 levels = c("Tst", "Trn", "Val"))
  names(bins) <- levels(bins)
#  binPolno <- structure(bins[c(1,1,1,1,2,2,2,3,3,3)], names = 0:9)
  trim <- function(x) sub("[[:space:]]+$","",sub("^[[:space:]]+","",x))
  
shinyServer(function(input, output, session) {
    binPolno <- reactive(structure(bins[c(1,1,1,1,2,2,2,3,3,3)], names = 0:9))
    bpno <- reactive({
      structure(
        bins[c(input$d0, input$d1, input$d2, input$d3, input$d4,
               input$d5, input$d6, input$d7, input$d8, input$d9)],
        names = 0:9
      )
    })
    rval <- reactiveValues()
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
    output$binPolnoVals <- renderText(
#      paste(as.character(binPolno()), collapse = ","))
#      paste(as.character(rvals$bp), collapse = ","))
#      paste(as.character(bpno()), collapse = ","))
#      filename_only(input$file1$name)
#      length(rval$splData)
      dim(rval$splData[[1]])
    )
    
    output$downloadSplit1 <- downloadHandler(
      filename = function() {
        paste(
          paste(filename_only(input$file1$name), 
                as.character(bins[1]), sep = "_"),
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
                as.character(bins[2]), sep = "_"),
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
                as.character(bins[3]), sep = "_"),
          "csv", sep = ".")
      },
      content = function(file) {
        write.csv(rval$splData[[3]], file, row.names = FALSE)
      }
    )
    
  } #end server
)
