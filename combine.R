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
d3_dup = d3_dup[order(d3_dup$location,d3_dup$At.School..Learning.Institution.Number.female),]
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

c_list[[c_index]] = d4.county
c_index = c_index + 1
s_list[[s_index]] = d4.subcounty
s_index = s_index + 1

# Labour ####

# Urban Population Aged 5 Years and above by Activity Status, Sex, County and Sub-County.
# Distribution of Rural Population Aged 5 Years and above by Activity Status, Sex, County and Sub-County.

# WASH ####

# Percentage Distribution of Conventional Households by Main Source of Drinking Water, Area of Residence, County and Sub-County.
# Percentage Distribution of Conventional Households by Main Mode of Human Waste Disposal, Area of Residence, County and Sub-County
# Percentage Distribution of Conventional Households by Main Mode of Solid Waste Disposal, Area of Residence, County and Sub-County

# Energy ####

# Main Type of Cooking Fuel, Area of Residence, County and Sub-County
# Main Type of Lighting Fuel, Area of Residence, County and Sub-County.

# Disability ####

# Distribution of Population aged 5 years and above by Disability Status, Sex1, Area of Residence, County and Sub-County
# Distribution of Persons with Disability by Type of Disability, Sex1, Area of Residence, County and Sub County
# Distribution of Persons with Albinism by Sex1, Area of Residence, County and Sub County


dat_county = rbindlist(c_list)
dat_county = dcast(dat_county, county~variable)
length(unique(dat_county$county)[order(unique(dat_county$county))]) # 47

dat_subcounty = rbindlist(s_list, use.names=T)
dat_subcounty = dcast(dat_subcounty, county+subcounty~variable)
length(unique(dat_subcounty$subcounty)[order(unique(dat_subcounty$subcounty))]) # 334

fwrite(dat_county,"ke_2020_county.csv")
fwrite(dat_subcounty,"ke_2020_subcounty.csv")
