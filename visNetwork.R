library(h5)
library(zoo)
library(magrittr)
library(reshape2)
library(visNetwork)

removeBraces <- function(x) {
  x <- gsub("\\s*\\[[^\\)]+\\]","", x)
  x <- gsub("\\s*\\([^\\)]+\\)","", x)
  x <- gsub("\\s*<[^\\)]+>","", x)
  x
}

splitAndComma <- function(x) {
  strsplit(x, "(,)|(and)", perl = TRUE)
}

removeArtifacts  <- function(x) {
  x <- gsub("(c\\()", "", x, perl = TRUE)
  x <- gsub("\\n", "", x)
  x <- gsub("person", "", x)
  x <- gsub("\"", "", x)
  x <- gsub("\\(", "", x)
  x <- gsub("\\)", "", x)
  x
}

wstrim <- function(x) {
  lapply(x, trimws)
}

#pdb <- tools:::CRAN_package_db()
#saveRDS(pdb, "pdb.rds")
pdb <- readRDS("pdb.rds")

### 

imports <- pdb$Imports %>% removeBraces %>% splitAndComma %>% wstrim
depends <- pdb$Depends %>% removeBraces %>% splitAndComma %>% wstrim

impdep <- lapply(1:length(imports), function(x) {
  out <- c(imports[[x]], depends[[x]])
  out <- out[!is.na(out)]
  out <- out[out != "R"]
  out
})
names(impdep) <- pdb$Package

impdep.df <- melt(impdep)
impdep.df$value.idx <- match(impdep.df$value, names(impdep))
impdep.df$L1.idx <- match(impdep.df$L1, names(impdep))
impdep.df.nona <- na.omit(impdep.df)

impdep.filter <- !is.na(impdep.df$L1.idx)
nodes.df <- impdep.df[impdep.filter, ]
#nodes.df$label <- names(impdep)[impdep.filter]

nodes <- data.frame(id = nodes.df$L1.idx,
                    label = nodes.df$L1)                                 # add labels on nodes
#group = c("GrA", "GrB"),                                     # add groups on nodes 
#value = 1,                                                # size adding value
#shape = c("circle"),                   # control shape of nodes
#title = paste0("<p><b>", nodes.df$L1,"</b></p>"),         # tooltip (html or character)
#color = c("darkblue"),# color
#shadow = FALSE)

edges.df <- unique(impdep.df.nona[, c(2, 4)])
edges <- data.frame(from = impdep.df.nona$L1.idx, to = impdep.df.nona$value.idx)

## select nodes
visNetwork::visNetwork(unique(nodes), edges) %>% 
  visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = TRUE) %>% 
  visLayout(improvedLayout = FALSE)


