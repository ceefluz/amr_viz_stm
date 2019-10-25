
library(RCurl)
library(RJSONIO)

url <- function(address, return.call = "json", sensor = "false", key = google_api) {
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "", "&key=", key)
  return(URLencode(u))
}

geoCode <- function(address, verbose = FALSE, key = key) {
  if(verbose) cat(address,"\n")
  u <- url(address, key = key)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA))
  }
}
