rm(list=ls())

library(data.table)
library(raster)

source('/homes/twolock/thesis/code/plot_data.R')

icd.codes <- fread("/homes/twolock/thesis/data/ICD_codes_ICD9_breakdown.csv")

icd.codes.split <- data.table(icd.codes)
setnames(icd.codes.split, c('cause', 'firearm'), c('icd_parent', 'firearm_parent'))

load("/home/j/Project/us_counties/locations/counties/merged_counties.rdata")
load("/home/j/Project/us_counties/covariates/counties/prepped/pop_by_race.rdata")
mcnty.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/mcnty_mapping_shape_file.shp")
state.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/state_mapping_shape_file.shp")

ages <- seq(15, 85, 5)
sexes <- c(1, 2)
age.sex.tmp <- CJ(age=ages, sex=sexes)
age.sex.tmp[,junk:=1]
loc[,junk:=1]

#pop[age>80, age:=80]
#pop <- pop[,.(pop=sum(pop)), by=.(year, state, mcnty, sex, race, age)]

structure.dt <- merge(loc, age.sex.tmp, by='junk', allow.cartesian=TRUE)

firearm.causes <- icd.codes[firearm==1, unique(cause)]

y <- 1980

all.data <- data.table()

missing.pcts <- data.table()

#pdf('/homes/twolock/thesis/test_maps.pdf')
for (y in 1980:2013) {
  print(y)
  death.dt <- fread(paste0("/home/j/Project/us_counties/mortality/data_prep/counties/02_prep_for_redistribution/deaths_by_cause_",y,".csv"))
  # death.dt[, junk := 1]

  tmp.missing <- death.dt[, .N, by=(is.missing=age==-9)]
  pct.missing <- tmp.missing[is.missing==TRUE, N] / (tmp.missing[is.missing==TRUE, N] +tmp.missing[is.missing==FALSE, N] )

  new.obs <- data.table(year=y, pct.missing=pct.missing)
  missing.pcts <- rbind(missing.pcts, new.obs)

  death.dt <- death.dt[age != -9 & age >= 15]
  death.dt[,cause := as.character(cause)]
  death.dt[,c('icd_parent', 'icd_child') := tstrsplit(cause,'.', fixed=TRUE)]
  suicide.deaths <- death.dt[cause %in% icd.codes[,unique(cause)] | icd_parent %in% icd.codes[,unique(cause)],]
  
  #suicide.deaths[,firearm := 0]
  
  suicide.deaths <- merge(suicide.deaths, icd.codes, by=c('icd_version', 'cause'), all.x=TRUE)
  suicide.deaths <- merge(suicide.deaths, icd.codes.split, by=c('icd_version', 'icd_parent'), all.x=TRUE)
  
  suicide.deaths[,is.firearm := 0]
  suicide.deaths[cause %in% firearm.causes,is.firearm := 1]
  
  gun.deaths <- suicide.deaths[,.(deaths=sum(deaths)), by=.(is.firearm, state, state_fips, county, location_id, year, sex, age)]
  wide.deaths <- dcast(gun.deaths, state + state_fips + county + location_id + year + age + sex ~ is.firearm, value.var = 'deaths')
  setnames(wide.deaths, c('0', '1'), c('other_deaths', 'firearm_deaths'))
  
  merged.loc.deaths <- merge(wide.deaths, structure.dt, by=c('location_id', 'age', 'sex'), all.y=TRUE)
  merged.loc.deaths[is.na(firearm_deaths), firearm_deaths := 0]
  merged.loc.deaths[is.na(other_deaths), other_deaths := 0]
  merged.loc.deaths[is.na(year), year := y]
  mcnty.deaths <- merged.loc.deaths[,.(firearm_deaths=sum(firearm_deaths), other_deaths=sum(other_deaths)), by=.(mcnty, year, state.y, age, sex)]
  setnames(mcnty.deaths, 'state.y', 'state')
  
  year.pop <- pop[year==y & age >= 15,.(pop=sum(pop)), by=.(year, mcnty, sex, age)]
  
  mcnty.deaths <- merge(year.pop, mcnty.deaths, by=c('year', 'mcnty', 'age', 'sex'), all.x=TRUE)
  
  # mcnty.deaths[,firearm_rate := firearm_deaths/pop]
  # mcnty.deaths[,other_rate := other_deaths/pop]
  
  # mcnty.shape.deaths <- merge(mcnty.shape, mcnty.deaths, by=c('mcnty'), all.x=TRUE)
  
  
  # #spplot(mcnty.shape.deaths, 'rate')
  # mcnty.shape.deaths@data <- data.table(mcnty.shape.deaths@data)
  
  #print(series_map(chloropleth_map = mcnty.shape.deaths, geog_id = 'mcnty', variable = 'firearm_rate',
  #           histogram=FALSE, color_ramp=woodson_pallettes("black_to_light_10"),
  #           outline_map = state.shape,
  #           map_title = paste('Firearm suicide mortality in US counties,',y)))
  all.data <- rbind(all.data, mcnty.deaths)
}
#dev.off()

write.csv(all.data, '~/thesis/data/compiled_mortality.csv', row.names=FALSE)
write.csv(missing.pcts, '~/thesis/data/pct_missing.csv', row.names=FALSE)
