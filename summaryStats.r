# summaryStats.r
summaryStats <- function(data, fields){
#  with(data, lapply(fields, summary))
  y <- do.call(rbind, lapply(fields, function(x) {
    z <- dat[[x]]
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