list.of.packages <- c("data.table", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/ke_census_2020/data")

remove_thousands_sep = function(x){
  return(as.numeric(gsub(",","",x)))
}

c_list = list()
c_index = 1
s_list = list()
s_index = 1

all_na_strings = c("","-","..", "NA")

location_dat = fread(
  "table-1-2-list-of-counties-and-sub-counties.csv",
  na.strings=all_na_strings
)
names(location_dat) = make.names(names(location_dat))
location_dat$ambiguous = location_dat$COUNTY.NAME == location_dat$SUB.COUNTY.NAME
county_names = unique(location_dat$COUNTY.NAME)
subcounty_names = unique(location_dat$SUB.COUNTY.NAME)
ambiguous_locations = c(location_dat$COUNTY.NAME[which(location_dat$ambiguous)], "SAMBURU")
unambiguous_county_names = setdiff(county_names,ambiguous_locations)
unambiguous_subcounty_names = setdiff(subcounty_names,ambiguous_locations)
all_location_names = c(unambiguous_county_names, unambiguous_subcounty_names, ambiguous_locations)
subcounty_join = location_dat[,c("COUNTY.NAME","SUB.COUNTY.NAME"),with=F]
names(subcounty_join) = c("county", "subcounty")


# Population ####

# County and sub county population
# County and sub-county household size
# County and sub-county population density
# County and sub county number of households
d1 = fread(
  "table-2-3-population-by-sex-number-of-households-land-area-population-density-and-subcounty.csv",
  na.strings=all_na_strings
  )
names(d1) = make.names(names(d1))
names(d1)[3] = "Total.Sex"
keep = c("County","SubCounty","Total.Sex","Sex.Male","Sex.Female","Total","LandAreaSq.Km","DensityPersonsperSq.Km")
d1 = d1[,keep,with=F]
names(d1) = c("county","subcounty","pop","pop.male","pop.female","households","sq.km","pop.density")
d1$household.size = d1$pop / d1$households
d1$pop.density[which(is.na(d1$pop.density))] = d1$pop[which(is.na(d1$pop.density))] / d1$sq.km[which(is.na(d1$pop.density))]
d1.county = subset(d1,is.na(subcounty))
d1.county$subcounty = NULL
d1.subcounty = subset(d1,!is.na(subcounty))

d1.county = melt(d1.county, id.vars=c("county"))
d1.county = subset(d1.county,county %in% county_names)
d1.subcounty = melt(d1.subcounty, id.vars=c("county","subcounty"))
d1.subcounty = subset(d1.subcounty,subcounty %in% subcounty_names)

d1.county$topic = "Population"
d1.subcounty$topic = "Population"

c_list[[c_index]] = d1.county
c_index = c_index + 1
s_list[[s_index]] = d1.subcounty
s_index = s_index + 1

# Urban population by sex and county
d2.county = fread(
  "table-2-2b-of-urban-population-by-sex-number-of-households-land-area-population-density-and-county.csv",
  na.strings=all_na_strings
)
names(d2.county) = make.names(names(d2.county))
names(d2.county)[2] = "Total.Sex"
keep = c("County","Total.Sex","Sex.Male","Sex.Female")
d2.county = d2.county[,keep,with=F]
names(d2.county) = c("county","urban.pop","urban.pop.male","urban.pop.female")

d2.county = melt(d2.county, id.vars=c("county"))
d2.county = subset(d2.county,county %in% county_names)

d2.county$topic = "Urban Population"

c_list[[c_index]] = d2.county
c_index = c_index + 1

# Education ####

# Attendance status
d3 = fread(
  "table-2-2-3-years-and-above-by-school-attendance-status-area-of-residence-sex-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d3) = make.names(names(d3))
d3 = d3[c(1,4,5,7:nrow(d3)),]
d3$type = rep(c("total","male","female"), nrow(d3)/3)
d3$group = unlist(lapply(c(1:(nrow(d3)/3)), rep, times=3))
d3[,location:=.SD$County..Sub.County[which(.SD$type=="total")],by=.(group)]
d3[,c("County..Sub.County","group")] = NULL
d3 = subset(d3, location %in% all_location_names)
d3[,count:=ceiling(c(1:nrow(.SD)/3)),by=.(location)]
d3 = melt(d3,id.vars=c("location","type","count"))
d3$variable = as.character(d3$variable)
d3$variable = paste(d3$variable,d3$type,sep=".")
d3$type = NULL
d3 = dcast(d3,location+count~variable)
d3$count = NULL

s_d3_nondup = subset(d3, location %in% unambiguous_subcounty_names)
c_d3_nondup = subset(d3, location %in% unambiguous_county_names)

d3_dup = data.table(subset(d3,location %in% ambiguous_locations))
d3_dup = d3_dup[order(d3_dup$location,d3_dup$Total.NO.total),]
d3_dup$type = rep(c("subcounty","county"),nrow(d3_dup)/2)
s_d3_dup = subset(d3_dup,type=="subcounty")
s_d3_dup$type = NULL
c_d3_dup = subset(d3_dup,type=="county")
c_d3_dup$type = NULL

d3.county = rbind(c_d3_dup, c_d3_nondup)
d3.subcounty = rbind(s_d3_dup, s_d3_nondup)

d3.county = melt(d3.county, id.vars="location")
setnames(d3.county, "location", "county")
d3.subcounty = melt(d3.subcounty, id.vars="location")
setnames(d3.subcounty, "location", "subcounty")
d3.subcounty = merge(d3.subcounty, subcounty_join, by="subcounty", all.x=T)

d3.county$topic = "Attendance Status"
d3.subcounty$topic = "Attendance Status"

c_list[[c_index]] = d3.county
c_index = c_index + 1
s_list[[s_index]] = d3.subcounty
s_index = s_index + 1

# Highest level of education reached - missing?
# Highest level of education completed
d4 = fread(
  "table-2-3-age-3-years-and-above-attending-school-by-edu-level-residence-sex-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d4) = make.names(names(d4))
d4 = d4[c(1,4,5,7:nrow(d4)),]
d4_col_names = names(d4)[2:ncol(d4)]
d4[, (d4_col_names) := lapply(.SD, remove_thousands_sep), .SDcols = d4_col_names]
d4 = subset(d4,!is.na(County..Sub.County))
d4$type = rep(c("total","male","female"), nrow(d4)/3)
d4$group = unlist(lapply(c(1:(nrow(d4)/3)), rep, times=3))
d4[,location:=.SD$County..Sub.County[which(.SD$type=="total")],by=.(group)]
d4[,c("County..Sub.County","group")] = NULL
d4 = subset(d4, location %in% all_location_names)
d4[,count:=ceiling(c(1:nrow(.SD)/3)),by=.(location)]
d4 = melt(d4,id.vars=c("location","type","count"))
d4$variable = as.character(d4$variable)
d4$variable = paste(d4$variable,d4$type,sep=".")
d4$type = NULL
d4 = dcast(d4,location+count~variable)
d4$count = NULL

s_d4_nondup = subset(d4, location %in% unambiguous_subcounty_names)
c_d4_nondup = subset(d4, location %in% unambiguous_county_names)

d4_dup = data.table(subset(d4,location %in% ambiguous_locations))
d4_dup = d4_dup[order(d4_dup$location,d4_dup$Total..total),]
d4_dup$type = rep(c("subcounty","county"),nrow(d4_dup)/2)
s_d4_dup = subset(d4_dup,type=="subcounty")
s_d4_dup$type = NULL
c_d4_dup = subset(d4_dup,type=="county")
c_d4_dup$type = NULL

d4.county = rbind(c_d4_dup, c_d4_nondup)
d4.subcounty = rbind(s_d4_dup, s_d4_nondup)

d4.county = melt(d4.county, id.vars="location")
setnames(d4.county, "location", "county")
d4.subcounty = melt(d4.subcounty, id.vars="location")
setnames(d4.subcounty, "location", "subcounty")
d4.subcounty = merge(d4.subcounty, subcounty_join, by="subcounty", all.x=T)

d4.county$topic = "Education Achievement Level"
d4.subcounty$topic = "Education Achievement Level"

c_list[[c_index]] = d4.county
c_index = c_index + 1
s_list[[s_index]] = d4.subcounty
s_index = s_index + 1

# Labour ####

# Urban Population Aged 5 Years and above by Activity Status, Sex, County and Sub-County. - missing?
# Distribution of Rural Population Aged 5 Years and above by Activity Status, Sex, County and Sub-County. - missing?

# WASH ####

# Percentage Distribution of Conventional Households by Main Source of Drinking Water, Area of Residence, County and Sub-County.
d5 = fread(
  "table-2-15-hholds-by-main-source-of-drinking-water-residence-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d5) = make.names(names(d5))
d5 = d5[c(4:nrow(d5)),]
d5 = subset(d5, County..Sub.County %in% all_location_names)

s_d5_nondup = subset(d5, County..Sub.County %in% unambiguous_subcounty_names)
c_d5_nondup = subset(d5, County..Sub.County %in% unambiguous_county_names)

d5_dup = data.table(subset(d5,County..Sub.County %in% ambiguous_locations))
d5_dup = d5_dup[order(d5_dup$County..Sub.County,d5_dup$Conventional.Households),]
d5_dup$type = rep(c("subcounty","county"),nrow(d5_dup)/2)
s_d5_dup = subset(d5_dup,type=="subcounty")
s_d5_dup$type = NULL
c_d5_dup = subset(d5_dup,type=="county")
c_d5_dup$type = NULL

d5.county = rbind(c_d5_dup, c_d5_nondup)
d5.subcounty = rbind(s_d5_dup, s_d5_nondup)

d5.county = melt(d5.county, id.vars="County..Sub.County")
setnames(d5.county, "County..Sub.County", "county")
d5.subcounty = melt(d5.subcounty, id.vars="County..Sub.County")
setnames(d5.subcounty, "County..Sub.County", "subcounty")
d5.subcounty = merge(d5.subcounty, subcounty_join, by="subcounty", all.x=T)

d5.county$topic = "Drinking Water"
d5.subcounty$topic = "Drinking Water"

c_list[[c_index]] = d5.county
c_index = c_index + 1
s_list[[s_index]] = d5.subcounty
s_index = s_index + 1

# Percentage Distribution of Conventional Households by Main Mode of Human Waste Disposal, Area of Residence, County and Sub-County
d6 = fread(
  "table-2-16-households-by-main-mode-of-human-waste-disposal-residence-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d6) = make.names(names(d6))
d6 = d6[c(4:nrow(d6)),]
d6 = subset(d6, County..Sub.County %in% all_location_names)

s_d6_nondup = subset(d6, County..Sub.County %in% unambiguous_subcounty_names)
c_d6_nondup = subset(d6, County..Sub.County %in% unambiguous_county_names)

d6_dup = data.table(subset(d6,County..Sub.County %in% ambiguous_locations))
d6_dup = d6_dup[order(d6_dup$County..Sub.County,d6_dup$Conventional.Households),]
d6_dup$type = rep(c("subcounty","county"),nrow(d6_dup)/2)
s_d6_dup = subset(d6_dup,type=="subcounty")
s_d6_dup$type = NULL
c_d6_dup = subset(d6_dup,type=="county")
c_d6_dup$type = NULL

d6.county = rbind(c_d6_dup, c_d6_nondup)
d6.subcounty = rbind(s_d6_dup, s_d6_nondup)

d6.county = melt(d6.county, id.vars="County..Sub.County")
setnames(d6.county, "County..Sub.County", "county")
d6.subcounty = melt(d6.subcounty, id.vars="County..Sub.County")
setnames(d6.subcounty, "County..Sub.County", "subcounty")
d6.subcounty = merge(d6.subcounty, subcounty_join, by="subcounty", all.x=T)

d6.county$topic = "Human Waste Disposal"
d6.subcounty$topic = "Human Waste Disposal"

c_list[[c_index]] = d6.county
c_index = c_index + 1
s_list[[s_index]] = d6.subcounty
s_index = s_index + 1

# Percentage Distribution of Conventional Households by Main Mode of Solid Waste Disposal, Area of Residence, County and Sub-County
d7 = fread(
  "table-2-17-hholds-by-main-mode-of-solid-waste-disposal-area-of-residence-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d7) = make.names(names(d7))
d7 = d7[c(4:nrow(d7)),]
d7 = subset(d7, County..Sub.County %in% all_location_names)

s_d7_nondup = subset(d7, County..Sub.County %in% unambiguous_subcounty_names)
c_d7_nondup = subset(d7, County..Sub.County %in% unambiguous_county_names)

d7_dup = data.table(subset(d7,County..Sub.County %in% ambiguous_locations))
d7_dup = d7_dup[order(d7_dup$County..Sub.County,d7_dup$Conventional.Households),]
d7_dup$type = rep(c("subcounty","county"),nrow(d7_dup)/2)
s_d7_dup = subset(d7_dup,type=="subcounty")
s_d7_dup$type = NULL
c_d7_dup = subset(d7_dup,type=="county")
c_d7_dup$type = NULL

d7.county = rbind(c_d7_dup, c_d7_nondup)
d7.subcounty = rbind(s_d7_dup, s_d7_nondup)

d7.county = melt(d7.county, id.vars="County..Sub.County")
setnames(d7.county, "County..Sub.County", "county")
d7.subcounty = melt(d7.subcounty, id.vars="County..Sub.County")
setnames(d7.subcounty, "County..Sub.County", "subcounty")
d7.subcounty = merge(d7.subcounty, subcounty_join, by="subcounty", all.x=T)

d7.county$topic = "Solid Waste Disposal"
d7.subcounty$topic = "Solid Waste Disposal"

c_list[[c_index]] = d7.county
c_index = c_index + 1
s_list[[s_index]] = d7.subcounty
s_index = s_index + 1
# Energy ####

# Main Type of Cooking Fuel, Area of Residence, County and Sub-County
d8 = fread(
  "table-2-18-hholds-by-main-type-of-cooking-fuel-area-of-residence-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d8) = make.names(names(d8))
d8 = d8[c(4:nrow(d8)),]
d8 = subset(d8, County..Sub.County %in% all_location_names)

s_d8_nondup = subset(d8, County..Sub.County %in% unambiguous_subcounty_names)
c_d8_nondup = subset(d8, County..Sub.County %in% unambiguous_county_names)

d8_dup = data.table(subset(d8,County..Sub.County %in% ambiguous_locations))
d8_dup = d8_dup[order(d8_dup$County..Sub.County,d8_dup$Conventional.Households),]
d8_dup$type = rep(c("subcounty","county"),nrow(d8_dup)/2)
s_d8_dup = subset(d8_dup,type=="subcounty")
s_d8_dup$type = NULL
c_d8_dup = subset(d8_dup,type=="county")
c_d8_dup$type = NULL

d8.county = rbind(c_d8_dup, c_d8_nondup)
d8.subcounty = rbind(s_d8_dup, s_d8_nondup)

d8.county = melt(d8.county, id.vars="County..Sub.County")
setnames(d8.county, "County..Sub.County", "county")
d8.subcounty = melt(d8.subcounty, id.vars="County..Sub.County")
setnames(d8.subcounty, "County..Sub.County", "subcounty")
d8.subcounty = merge(d8.subcounty, subcounty_join, by="subcounty", all.x=T)

d8.county$topic = "Cooking Fuel"
d8.subcounty$topic = "Cooking Fuel"

c_list[[c_index]] = d8.county
c_index = c_index + 1
s_list[[s_index]] = d8.subcounty
s_index = s_index + 1

# Main Type of Lighting Fuel, Area of Residence, County and Sub-County.
d9 = fread(
  "table-2-19-households-by-main-type-of-lighting-fuel-residence-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d9) = make.names(names(d9))
d9 = d9[c(4:nrow(d9)),]
d9 = subset(d9, County..Sub.County %in% all_location_names)

s_d9_nondup = subset(d9, County..Sub.County %in% unambiguous_subcounty_names)
c_d9_nondup = subset(d9, County..Sub.County %in% unambiguous_county_names)

d9_dup = data.table(subset(d9,County..Sub.County %in% ambiguous_locations))
d9_dup = d9_dup[order(d9_dup$County..Sub.County,d9_dup$Conventional.Households),]
d9_dup$type = rep(c("subcounty","county"),nrow(d9_dup)/2)
s_d9_dup = subset(d9_dup,type=="subcounty")
s_d9_dup$type = NULL
c_d9_dup = subset(d9_dup,type=="county")
c_d9_dup$type = NULL

d9.county = rbind(c_d9_dup, c_d9_nondup)
d9.subcounty = rbind(s_d9_dup, s_d9_nondup)

d9.county = melt(d9.county, id.vars="County..Sub.County")
setnames(d9.county, "County..Sub.County", "county")
d9.subcounty = melt(d9.subcounty, id.vars="County..Sub.County")
setnames(d9.subcounty, "County..Sub.County", "subcounty")
d9.subcounty = merge(d9.subcounty, subcounty_join, by="subcounty", all.x=T)

d9.county$topic = "Lighting Fuel"
d9.subcounty$topic = "Lighting Fuel"

c_list[[c_index]] = d9.county
c_index = c_index + 1
s_list[[s_index]] = d9.subcounty
s_index = s_index + 1

# Disability ####

# Distribution of Population aged 5 years and above by Disability Status, Sex1, Area of Residence, County and Sub-County - missing?
# Distribution of Persons with Disability by Type of Disability, Sex1, Area of Residence, County and Sub County
d10 = fread(
  "table-2-27-people-with-disability-by-type-of-disability-sex-area-of-residence-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d10) = make.names(names(d10))
d10 = d10[c(4:nrow(d10)),]
d10 = subset(d10, County.Sub.County %in% all_location_names)

s_d10_nondup = subset(d10, County.Sub.County %in% unambiguous_subcounty_names)
c_d10_nondup = subset(d10, County.Sub.County %in% unambiguous_county_names)

d10_dup = data.table(subset(d10,County.Sub.County %in% ambiguous_locations))
d10_dup = d10_dup[order(d10_dup$County.Sub.County,d10_dup$Total),]
d10_dup$type = rep(c("subcounty","county"),nrow(d10_dup)/2)
s_d10_dup = subset(d10_dup,type=="subcounty")
s_d10_dup$type = NULL
c_d10_dup = subset(d10_dup,type=="county")
c_d10_dup$type = NULL

d10.county = rbind(c_d10_dup, c_d10_nondup)
d10.subcounty = rbind(s_d10_dup, s_d10_nondup)

d10.county = melt(d10.county, id.vars="County.Sub.County")
setnames(d10.county, "County.Sub.County", "county")
d10.subcounty = melt(d10.subcounty, id.vars="County.Sub.County")
setnames(d10.subcounty, "County.Sub.County", "subcounty")
d10.subcounty = merge(d10.subcounty, subcounty_join, by="subcounty", all.x=T)

d10.county$topic = "Disability Type"
d10.subcounty$topic = "Disability Type"

c_list[[c_index]] = d10.county
c_index = c_index + 1
s_list[[s_index]] = d10.subcounty
s_index = s_index + 1

# Distribution of Persons with Albinism by Sex1, Area of Residence, County and Sub County
d11 = fread(
  "table-2-28-persons-with-albinism-by-sex-area-of-residence-county-and-sub-county.csv",
  na.strings=all_na_strings
)
names(d11) = make.names(names(d11))
d11 = d11[c(4:nrow(d11)),]
d11 = subset(d11, County.Sub.County %in% all_location_names)

s_d11_nondup = subset(d11, County.Sub.County %in% unambiguous_subcounty_names)
c_d11_nondup = subset(d11, County.Sub.County %in% unambiguous_county_names)

d11_dup = data.table(subset(d11,County.Sub.County %in% ambiguous_locations))
d11_dup = d11_dup[order(d11_dup$County.Sub.County,d11_dup$Total.population.male),]
d11_dup$type = rep(c("subcounty","county"),nrow(d11_dup)/2)
s_d11_dup = subset(d11_dup,type=="subcounty")
s_d11_dup$type = NULL
c_d11_dup = subset(d11_dup,type=="county")
c_d11_dup$type = NULL

d11.county = rbind(c_d11_dup, c_d11_nondup)
d11.subcounty = rbind(s_d11_dup, s_d11_nondup)

d11.county = melt(d11.county, id.vars="County.Sub.County")
setnames(d11.county, "County.Sub.County", "county")
d11.subcounty = melt(d11.subcounty, id.vars="County.Sub.County")
setnames(d11.subcounty, "County.Sub.County", "subcounty")
d11.subcounty = merge(d11.subcounty, subcounty_join, by="subcounty", all.x=T)

d11.county$topic = "Albinism"
d11.subcounty$topic = "Albinism"

c_list[[c_index]] = d11.county
c_index = c_index + 1
s_list[[s_index]] = d11.subcounty
s_index = s_index + 1

dat_county = rbindlist(c_list)
dat_county = dcast(dat_county, county~variable+topic)
length(unique(dat_county$county)[order(unique(dat_county$county))]) # 47

dat_subcounty = rbindlist(s_list, use.names=T)
dat_subcounty = dcast(dat_subcounty, county+subcounty~variable+topic)
length(unique(dat_subcounty$subcounty)[order(unique(dat_subcounty$subcounty))]) # 334

fwrite(dat_county,"ke_2020_county.csv")
fwrite(dat_subcounty,"ke_2020_subcounty.csv")
