list.of.packages <- c("jsonlite","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/ke_census_2020/data")

package_url = "http://data.census.ke/api/3/action/package_list"
package_data = fromJSON(package_url)
table_names = package_data[["result"]]


metadata_list = list()
metadata_index = 1

base_dl_url = "http://data.census.ke/api/3/action/package_show?id="
for(table_name in table_names){
  dl_data = fromJSON(paste0(base_dl_url, table_name))
  res = dl_data[["result"]]
  res_lengths = sapply(res, length)
  flat_res = data.frame(res[res_lengths==1])
  resources = data.frame(res[["resources"]])
  resources_csv = subset(resources, format=="CSV")
  resources_xlsx = subset(resources, format=="XLSX")
  if(nrow(resources_csv)==1){
    flat_res = cbind(flat_res, resources_csv)
  }else if(nrow(resources_xlsx)==1){
    flat_res = cbind(flat_res, resources_xlsx)
  }
  metadata_list[[metadata_index]] = flat_res
  metadata_index = metadata_index + 1
}

metadata = rbindlist(metadata_list, fill=T)
fwrite(metadata,"ke_census_metadata.csv")
