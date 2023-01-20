#library(readr)
library(dplyr)
records <- read.csv("cora.arff.gz", skip = 18, 
                    col.names = c("authors", "volume", "title", "institution", 
                                  "venue", "address", "publisher", "year", 
                                  "pages", "editor", "note", "month", "UID"))
records <- records %>% select(-c("address", "note"))

# Clean year
records$authors <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$authors)
records$authors <- gsub("[[:punct:]]", "", records$authors)
records$authors <- gsub("\\s+and\\s+", " ", records$authors)
records$title <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$title)
records$title <- gsub("[[:punct:]]", "", records$title)
records$venue <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$venue)
records$venue <- gsub("[[:punct:]]", "", records$venue)
records$institution <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$institution)
records$institution <- gsub("[[:punct:]]", "", records$institution)
records$year <- gsub("[^[:alnum:]]", "", records$year)
records$pages <- gsub("[[:alpha:].()]", "", records$pages)
records$pages <- gsub("^\\s+|\\s+$", "", records$pages)
records$pages <- gsub(",$", "", records$pages)

true_membership <- records$UID
records$UID <- NULL
n_records <- nrow(records)
