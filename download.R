list.of.packages <- c("data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/ke_census_2020/data")

metadata = fread("ke_census_metadata.csv")
metadata = subset(metadata, download_url!="")

for(i in 1:nrow(metadata)){
  row = metadata[i,]
  download_url = row$download_url
  file_name = paste0(row$name, ".csv")
  download.file(download_url, file_name)
}
