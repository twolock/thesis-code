library(data.table)
library(raster)
library(INLA)
library(lme4)

inla.p.values <- function(results) {
  tmp.table <- results$summary.fixed

  p.values <- 1-(pnorm(abs(tmp.table[,'mean'])/tmp.table[,'sd'], 0, 1))
  tmp.table <- cbind(tmp.table, p = round(p.values*10000)/10000)
  return(tmp.table)
}

setwd("/snfs1/Project/us_counties/covariates/counties/prepped")
load('pop_by_race.rdata')

pop[,total.pop := sum(pop), by=.(year, state, mcnty, sex, age)]
pop[,share := pop/total.pop]
pop[total.pop == 0, share := 0]

pop <- pop[race==2]

setnames(pop, 'share', 'race_share')

load('beer_pc.rdata')
load('income_median_ppa.rdata')

income_median_ppa[, income.z := (income_median_ppa - mean(income_median_ppa))/sd(income_median_ppa), by=year]

setwd('/homes/twolock/')
my.dt <- fread('thesis/data/compiled_mortality.csv')

my.dt <- merge(my.dt, pop[,.(year, mcnty, sex, age, race_share)], by=c('year', 'mcnty', 'age', 'sex'), all.x=TRUE)
rm(pop)

my.dt <- merge(my.dt, data.table(beer_pc), by=c('year', 'mcnty'), all.x=TRUE)
rm(beer_pc)

my.dt <- merge(my.dt, income_median_ppa, by=c('year', 'mcnty'), all.x=TRUE)
rm(income_median_ppa)

# test.model <- inla(firearm_deaths ~ 1 + year + race_share + as.factor(age) + as.factor(sex) + f(mcnty, model='iid'),
#                    data=my.dt[state %in% c(6, 41) & pop > 0 ],
#                    verbose = TRUE, family='zeroinflatednbinomial0', E=pop,
#                    control.predictor=list(compute = T))

# test.model <- glm(firearm_deaths ~ year + race_share + as.factor(age) + as.factor(sex), offset=log(pop), data=my.dt[state %in% c(6, 41) & pop > 0,], family=quasipoisson(link=log))
my.dt[,county.factor := as.factor(mcnty)]

test.states <- my.dt[,unique(state)]

test.model <- inla(firearm_deaths ~ year + income.z + race_share + as.factor(state) + as.factor(age) + f(mcnty, model='iid'), E=pop,
  data=my.dt[pop > 0  & sex == 1 & state %in% test.states,], family='zeroinflatednbinomial0',
  control.compute=list(config=TRUE), verbose=TRUE)
test.pred <- inla.posterior.sample(1000, test.model)

test.coeff <- lapply(test.pred, function(x) {x$latent[!grepl('Predictor', rownames(x$latent)),]})
coeff.matrix <- do.call(rbind, test.coeff)

fixed.effects <- coeff.matrix[,which(colnames(coeff.matrix) == '(Intercept).1'):ncol(coeff.matrix)]
results.m <- apply(fixed.effects, 2, function(v){c(mean=mean(v), sd=sd(v), p=1-sum((sign(mean(v)) * v) > 0) / length(v))})

sum((sign(mean(v)) * v) > 0) / length(v)

pred.list <- do.call(rbind,lapply(test.pred, function(x) {x$latent[grepl('Predictor', rownames(x$latent)),]}))

# test.model <- inla(firearm_deaths ~ 1 + year + race_share + as.factor(age) + income.z + beer_pc, E=pop,
#   data=my.dt[pop > 0  & state == 6 & sex == 1,], family='nbinomial',
#   control.predictor=list(compute=TRUE), verbose=TRUE)

#results <- inla(firearm_deaths ~ 1 + race_share + year + as.factor(age) + as.factor(sex),
#                family = 'poisson', data=my.dt[state %in% c(6, 41) & pop > 0,], E=pop, control.predictor=list(compute = T), verbose=TRUE)

my.dt[pop > 0 & sex==1 & state %in% test.states ,test.pred := apply(exp(pred.list), 2, mean)]
my.dt[, test.count := test.pred*pop]

plot.dt <- my.dt[sex == 1, .(pop = sum(pop), test.count = sum(test.count), real.count = sum(firearm_deaths)), by=.(year, mcnty, state)]
plot.dt[,test.rate := test.count/pop]
plot.dt[, real.rate := real.count/pop]

plot.dt[, pct.change := (test.rate - real.rate)/real.rate]
plot.dt[pop==0, pct.change := 0]

mcnty.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/mcnty_mapping_shape_file.shp")
state.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/state_mapping_shape_file.shp")

source('/homes/twolock/thesis/code/plot_data.R')

series_map(chloropleth_map = copy(mcnty.shape[mcnty.shape$state %in% test.states,]),
           outline_map = state.shape[state.shape$state %in% test.states,],
           data = copy(plot.dt[state %in% test.states,]),
           geog_id = 'mcnty',
           variable = 'test.rate',
           series_dimension = 'year',
           series_sequence = 1980:2013,
           destination_folder = '/homes/twolock/thesis/',
           histogram=FALSE,
           color_ramp=woodson_pallettes("black_to_light_15"),
           map_title = 'Predicted firearm suicide mortality'
           )


# series_map(chloropleth_map = copy(mcnty.shape[mcnty.shape$state %in% c(6),]),
#            outline_map = state.shape[state.shape$state %in% c(6),],
#            data = copy(plot.dt[state %in% c(6)]),
#            geog_id = 'mcnty',
#            variable = 'real.rate',
#            series_dimension = 'year',
#            series_sequence = 1980:2013,
#            destination_folder = '/homes/twolock/thesis/',
#            histogram=TRUE,
#            color_ramp=woodson_pallettes("black_to_light_10"),
#            map_title = 'Actual firearm suicide mortality'
#            )

print(inla.p.values(test.model))
print(t(results.m))
