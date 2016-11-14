options(shiny.maxRequestSize=30*1024^2)
library(excelRio)
suppressPackageStartupMessages(library(mondate))
library(reshape2)
library(ggplot2)
library(data.table)
library(DT)

logfile <- "log.txt"
unlink(logfile) # start fresh

timestamp <- function() format(Sys.time())

log <- function(..., file = logfile, append = TRUE) 
  cat(timestamp(), ": ", ..., "\n", sep = "", file = file, append = append)

shinyServer(function(input, output, session) {
  
  rval <- reactiveValues()
  
  output$ready <- renderText("ready")

  observeEvent(input$file1, { # New file chosen
    log("Read file '", input$file1$name)
    # Using data.table for its speed in adding columns
    dat <- data.table(
      readFromCsv(input$file1$datapath, header=TRUE, stringsAsFactors = FALSE)
    )
    if ("lossRatio" %in% names(dat)) {
      stopApp("app cannot replace organic field 'lossRatio'")
    }
    dat$lossRatio <- dat$incloss / dat$prem
    rval$df <- dat
    # Wipe out any fields that may have been previously selected
    rval$fields <- NULL
    # this for conditional panel to only show buttons after file uploaded
    # but never got to work correctly
    #output$fileUploaded <- reactive(TRUE)
    # put this after following tabPanel call?
    output$triangleUI <- renderMainPanel("Table")
    rval$tabs[[1]] <- tabPanel(input$file1$name, 
                               plotOutput("tableplot"),
                               checkboxInput("showsummary", "Show summary",
                                             value = isolate(input$showsummary)),
                               tableOutput("tablesummary"),
                               checkboxInput("showtable", "Show table",
                                             value = isolate(input$showtable)),
                               dataTableOutput("table")
    )
  })
  
  output$table <- renderDataTable({
    # If no table yet, nothing to show
    if (is.null(rval$df)) return(NULL)
    if (!input$showtable) return(NULL)
    DT::datatable(rval$df, options = list(pageLength = 5), rownames = TRUE)
  }, options = list(lengthMenu = c(5, 20, 50), pageLength = 5))
  
  output$tablesummary <- renderTable({
    # If no table yet, nothing to summarize
    if (is.null(rval$df)) return(NULL)
    if (!input$showsummary) return(NULL)
    #summary(rval$df)
    summaryStats(rval$df, c("prem", "incloss", "lossRatio"))
  },
  include.rownames = TRUE)

  output$table <- renderDataTable({
    # If no table yet, nothing to show
    if (is.null(rval$df)) return(NULL)
    if (!input$showtable) return(NULL)
    DT::datatable(rval$df, options = list(pageLength = 5), rownames = TRUE)
  })

  output$tableplot <- renderPlot({
    # If no table yet, nothing to plot
    if (is.null(rval$df)) return(NULL)
    # distribution of 'class' in the data
    dat <- rval$df
    ggplot(data = dat, aes(x = "all pols", y = prem)) + geom_boxplot()
  })
  
  output$tableplot1 <- renderPlot({
    # If no table yet, nothing to plot
    if (is.null(rval$df)) return(NULL)
    # distribution of 'class' in the data
    df <- rval$df
    cls <- sapply(df, class)
    tbl <- table(cls)
    lbls <- paste(names(tbl), "\n", tbl, sep = "")
    pie(tbl, label = lbls, main = 
          paste0("Field type(s) in '", input$file1$name, "'"))
  })
  
  # Update the column name selection box based on the numeric's in the table
  observe({
    # If no file yet, no column names should be made available.
    if (is.null(rval$df)) return(NULL)
    numnames <- names(rval$df)
    numnames <- c(Choose='', sort(numnames))
    updateSelectizeInput(session, 'colName', choices = numnames, 
                         options = list(render = I(
                           '{
                           options: function(item, escape) {
                           return "<div>" + escape(item.value) + "</div>";
                           }
  }')),
                     server = TRUE)
    })
  
  # Add column of 1's for reported Claim Counts,
  #   closed counts = f('openclosed' field)
  # Take care to update rval$df only once
  observeEvent(input$addcounts, {
    rptd <- clsd <- somethingdone <- FALSE
    if ('count.rptd' %in% names(rval$df)) {
      cat("'count.rptd' already in names. New column not added.\n")
    }
    else {
      rptd <- somethingdone <- TRUE
      count.rptd <- 1
    }
    
    if ('count.clsd' %in% names(rval$df)) {
      cat("'count.clsd' already in names. New column not added.\n")
    }
    else {
      clsd <- somethingdone <- TRUE
      count.clsd <- as.numeric(rval$df$openclosed == "Closed")
    }
    if (somethingdone) {
      log("Add count column(s)")
      rval$df <- if (rptd)
        if (clsd) 
          cbind(rval$df, count.rptd = count.rptd, count.clsd = count.clsd)
      else 
        cbind(rval$df, count.rptd = count.rptd)
      else cbind(rval$df, count.clsd = count.clsd) 
    }
  })
  
  # Add calculated fields accident year and accident year age.
  observeEvent(input$adday, {
    log("Calculate new ay and ayage fields")
    ay <- mondate:::year(rval$df$lossdate)
    rval$df$ay <- ay
    # Use 'c' to strip difftime attribute, which generates xtable warning.
    rval$df$ayage <- c(mondate(rval$df$evaldate) - mondate.ymd(ay - 1))
  })
  
  # Create the triangle when triangulate button is pressed.
  observeEvent(input$triangulate, {
    # handlerExpr will be executed within an isolate scope.
    field <- isolate(input$colName)
    if (is.null(field)) return()
    # '' is the "unchosen" value placed at the top of the selection box
    if (field == '') return()
    
    
    # Just gained another field, unless already there
    fldnms <- isolate(rval$fields)
    N <- which(fldnms == field)
    if (length(N)==0) {
      rval$fields <- fldnms <- c(fldnms, field)
      N <- length(fldnms)
    }
    N <- N + 1 #to account for zero-th tab which holds table summary
    #if (!(field %in% fldnms)) rval$fields <- c(fldnms, field)
    #if (field %in% fldnms) {cat(field, " previously trianglated\n");return()}
    
    #rval$fields <- c(fldnms, field)
    
    # Create the triangle --
    # but first (temporarily while ay, ayage are hardcoded) check for ay and ayage
    nms <- names(isolate(rval$df))
    if (all(c("ay", "ayage") %in% nms)) {
      tri <- acast(isolate(rval$df), 
                   paste0("ay", " ~ ", "ayage"), 
                   sum, 
                   value.var = field, 
                   fill = as.numeric(NA))
      
      # Create the triangle of age-to-age factors
      tri.ata <- summary(ata(tri))
    }
    else {
      tri <- tri.ata <- data.frame(
        error = "'ay' and 'ayage' must be fields in Table")
    }
    
    n <- length(rval$tabs)
    
    nlist <- isolate(input$trishow)
    displayname <- paste(field, nlist, sep = ".")
    
    rval$tabs[[N]] <- do.call(tabPanel, c(field,
                                          lapply(1:length(nlist), function(i)
                                            call(flist[i], displayname[i]))))
    # Save the results -- currently only used for rendering here below
    # triangulate_only
    triname <- paste(field, "tri", sep = ".")
    ataname <- paste(field, "ata", sep = ".")
    namename <- paste(field, "name", sep = ".")
    plotname <- paste(field, "plot", sep = ".")
    
    trishowselection <- input$trishow
    rval[[triname]] <- tri
    rval[[ataname]]  <- tri.ata
    rval[[namename]]  <- field
    
    # Here's where the rendering takes place
    
    if ("plot" %in% trishowselection) output[[plotname]] <- renderPlot({
      plot(as.triangle(rval[[triname]]), xlab = "ayage", ylab = "", main = field)
    })
    if ("tri" %in% trishowselection) output[[triname]] <- renderTable(
      tri,
      digits = 0,
      format.args = list(
        big.mark = ifelse(getOption("OutDec") == ".", ",", "."))
    )
    if ("ata" %in% trishowselection) output[[ataname]] <- renderTable(
      tri.ata,
      digits = 3
    )
    output[[namename]]  <- renderText({rval[[namename]]})
    
    # Send to browser
    output$triangleUI <- renderMainPanel(field)
  })
  
  # This code builds the UI dynamically, rather than static in UI.r
  renderMainPanel <- function(selected) renderUI({
    do.call(tabsetPanel, c("tabPanel", rval$tabs))
  })
  
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
      nobs = sum(!is.na(z)),
      min = min(z, na.rm = TRUE),
      mean = mean(z, na.rm = TRUE),
      max = max(z, na.rm = TRUE),
      sum = sum(z, na.rm = TRUE)
    )
  }))
  row.names(y) <- fields
  y
}