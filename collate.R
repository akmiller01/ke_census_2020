list.of.packages <- c("data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/ke_census_2020/data")

data_files = list.files(pattern="*.csv")
data_files = subset(data_files, data_files != "ke_census_metadata.csv")

variables = list()

for(data_file in data_files){
  tmp = fread(data_file)
  for(vname in names(tmp)){
    if(!(vname %in% names(variables))){
      variables[[vname]] = 1
    }else{
      variables[[vname]] = variables[[vname]] + 1
    }
  }
}

vdf = data.frame(varname=names(variables), occurances=unlist(variables))
fwrite(vdf,"ke_census_columns.csv")
