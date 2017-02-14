options(shiny.maxRequestSize=30*1024^2)
library(excelRio)
suppressPackageStartupMessages(library(mondate))
library(reshape2)
library(ggplot2)
library(data.table)
library(DT)

shinyServer(function(input, output, session) {
  
  rval <- reactiveValues() # needed for rval$df
  
  observeEvent(input$file1, { # New file chosen
    polData <- read.csv(input$file1$datapath, header=TRUE, 
                        stringsAsFactors = TRUE)
    rval$df <- polData
  })

#  output$ready <- renderText(input$file1$name)

  output$FieldSummary <- renderTable({
    # If no table yet, nothing to summarize
    if (is.null(rval$df)) return(NULL)
    summaryStats(rval$df, names(rval$df))
                          #c("prem", "incloss", "lossRatio"))
  },
  
  include.rownames = TRUE)

  observe({
    if(input$exitBtn > 0){
      stopApp("Done")
    }
  })
  
  })
summaryStats <- function(data, fields){
  #  with(data, lapply(fields, summary))
  y <- do.call(rbind, lapply(fields, function(x) {
    z <- data[[x]]
    data.frame(
      type = class(z),
      nlevels = length(levels(z)),
      nrec = length(z),
      nNA  = sum(is.na(z)),
      nobs = sum(!is.na(z)),
      min = suppressWarnings(
        min(
          if (is.factor(z)) levels(z)
          else z, 
          na.rm = TRUE)
        ),
      max = suppressWarnings(
        max(
          if (is.factor(z)) levels(z)
          else z, 
          na.rm = TRUE)
        ),
      mean = suppressWarnings(
        if (is.factor(z)) "NA"
        else mean(z, na.rm = TRUE)
      )
    )
  }))
  row.names(y) <- fields
  y
}