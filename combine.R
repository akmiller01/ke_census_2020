list.of.packages <- c("data.table", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/ke_census_2020/data")

c_list = list()
c_index = 1
s_list = list()
s_index = 1

# Population ####

# County and sub county population
# County and sub-county household size
# County and sub-county population density
# County and sub county number of households
d1 = fread(
  "table-2-3-population-by-sex-number-of-households-land-area-population-density-and-subcounty.csv",
  na.strings=c("","-","..")
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
d1.subcounty = melt(d1.subcounty, id.vars=c("county","subcounty"))

c_list[[c_index]] = d1.county
c_index = c_index + 1
s_list[[s_index]] = d1.subcounty
s_index = s_index + 1

# Urban population by sex and county
d2.county = fread(
  "table-2-2b-of-urban-population-by-sex-number-of-households-land-area-population-density-and-county.csv",
  na.strings=c("","-","..")
)
names(d2.county) = make.names(names(d2.county))
names(d2.county)[2] = "Total.Sex"
keep = c("County","Total.Sex","Sex.Male","Sex.Female")
d2.county = d2.county[,keep,with=F]
names(d2.county) = c("county","urban.pop","urban.pop.male","urban.pop.female")

d2.county = melt(d2.county, id.vars=c("county"))

c_list[[c_index]] = d2.county
c_index = c_index + 1

# Education ####

# Attendance status
d3 = fread(
  "table-2-2-3-years-and-above-by-school-attendance-status-area-of-residence-sex-county-and-sub-county.csv",
  na.strings=c("","-","..")
)
names(d3) = make.names(names(d3))
d3 = d3[c(1,4,5,7:nrow(d3)),]
d3$type = rep(c("total","male","female"), nrow(d3)/3)
d3$group = unlist(lapply(c(1:(nrow(d3)/3)), rep, times=3))
d3[,location:=.SD$County..Sub.County[which(.SD$type=="total")],by=.(group)]
d3[,c("County..Sub.County","group")] = NULL
d3 = melt(d3,id.vars=c("location","type"))
d3$variable = as.character(d3$variable)
d3$variable = paste(d3$variable,d3$type,sep=".")
d3$type = NULL
d3 = dcast(d3,location~variable)
d3 = melt(d3,id.vars="location")
# Highest level of education reached - missing?
# Highest level of education completed
d4 = fread(
  "table-2-3-age-3-years-and-above-attending-school-by-edu-level-residence-sex-county-and-sub-county.csv",
  na.strings=c("","-","..")
)
names(d4) = make.names(names(d4))
d4 = d4[c(1,4,5,7:nrow(d4)),]
d4 = subset(d4,!is.na(County..Sub.County))
d4$type = rep(c("total","male","female"), nrow(d4)/3)
d4$group = unlist(lapply(c(1:(nrow(d4)/3)), rep, times=3))
d4[,location:=.SD$County..Sub.County[which(.SD$type=="total")],by=.(group)]
d4[,c("County..Sub.County","group")] = NULL
d4 = melt(d4,id.vars=c("location","type"))
d4$variable = as.character(d4$variable)
d4$variable = paste(d4$variable,d4$type,sep=".")
d4$type = NULL
d4 = dcast(d4,location~variable)
d4 = melt(d4,id.vars="location")

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
length(unique(dat_county$county)[order(unique(dat_county$county))]) # 48

dat_subcounty = rbindlist(s_list)
dat_subcounty = dcast(dat_subcounty, county+subcounty~variable)
length(unique(dat_subcounty$subcounty)[order(unique(dat_subcounty$subcounty))]) # 343

fwrite(dat_county,"ke_2020_county.csv")
fwrite(dat_subcounty,"ke_2020_subcounty.csv")
