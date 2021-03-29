library(readr)
library(dplyr)

records <- read_csv("fz-nophone.arff.gz", skip = 10, col_names = c("name", "addr", "city", "type", "UID"))

# Clean year
records$name <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$name)
records$name <- gsub("[[:punct:]]", "", records$name)
records$addr <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$addr)
records$addr <- gsub("[[:punct:]]", "", records$addr)
records$city <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$city)
records$city <- gsub("[[:punct:]]", "", records$city)
records$type <- gsub("(\\w)[\\.\\-](\\w)", "\\1 \\2", records$type)
records$type <- gsub("[[:punct:]]", "", records$type)

true_membership <- records$UID
records$UID <- NULL
n_records <- nrow(records)
