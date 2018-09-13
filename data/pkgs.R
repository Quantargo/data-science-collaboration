# from https://rpubs.com/filipstachura/rpackages
library(rvest)
library(magrittr)
library(dplyr)
library(zoo)
url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
page <- read_html(url)
page %>%
html_node("table") %>%
html_table() %>%
mutate(count = rev(1:nrow(.))) %>%
mutate(Date = as.Date(Date)) %>%
mutate(Month = format(Date, format="%Y-%m")) %>%f
group_by(Month) %>%
summarise(published = min(count)) %>%
mutate(Date = as.Date(as.yearmon(Month))) -> pkgs

library(XML)
getRdates <- function(){
  url <- paste0("http://cran.r-project.org/src/base/R-", 0:3)
  x <- lapply(url, function(x)readHTMLTable(x, stringsAsFactors=FALSE)[[1]])
  x <- do.call(rbind, x)
  x <- x[grep("R-(.*)(\\.tar\\.gz|\\.tgz)", x$Name), c(-1, -5)]
  x$Release <- gsub("(R-.*)\\.(tar\\.gz|tgz)", "\\1", x$Name)
  x$Date <- as.POSIXct(x[["Last modified"]], format="%d-%b-%Y %H:%M")
  x$Release <- reorder(x$Release, x$Date)
  x
}
rdates <- getRdates()
save(rdates, pkgs, file = "data/pkgs.rda")