library(readr)
library(dplyr)
library(purrr)
library(stringr)

files <- list.files(path = "./synthdata/",
           pattern = "*_records.csv.gz", full.names = TRUE)
names(files) <- files

synthdata <- map_dfr(files, ~read_csv(., col_types = cols(.default = "c")), .id = "filename")

file.synthdata <- tibble(
  link_conf_mu = as.numeric(str_extract(synthdata$filename, "(?<=link-conf-mu-)\\d*\\.?\\d*(?=_)")), 
  dist_conf = as.integer(str_extract(synthdata$filename, "(?<=dist-conf-)\\d(?=_)")), 
  seed = as.integer(str_extract(synthdata$filename, "(?<=seed-)\\d(?=\\_)")),
  exp_num_recs = as.integer(str_extract(synthdata$filename, "(?<=exp-num-recs-)\\d+(?=\\_)"))
)

identity.synthdata <- as.integer(synthdata$uid)

synthdata$filename <- NULL
synthdata$uid <- NULL
