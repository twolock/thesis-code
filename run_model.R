library(foreign)
library(MASS)
library(TMB)
library(data.table)
library(raster)
library(INLA)
library(ggplot2)
library(parallel)

set.seed(50)

setwd("/homes/twolock/thesis")

model.dt <- fread('data/models.csv')

model.i <- as.integer(commandArgs(trailingOnly=TRUE)[1])

print(paste('model',model.i))

law.dt <- fread('data/statute_history.csv')

source('code/plot_data.R')

#mcnty.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/mcnty_mapping_shape_file.shp")
state.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/state_mapping_shape_file.shp")


years <- 1980:2013
state.dt <- merge(CJ(state=state.shape$state, year=years), law.dt[,.(state, bill_passed, bill_repealed)], by='state', all.y=TRUE)
state.dt[, has.law := 0]
state.dt[!is.na(bill_passed) & year > bill_passed, has.law := 1]
state.dt[!is.na(bill_repealed) & year > bill_repealed, has.law := 0]
state.dt[, years_since_policy := 0]
state.dt[!is.na(bill_passed), years_since_policy := pmin(35,pmax(0, year-bill_passed))]
state.dt[year > bill_repealed & !is.na(bill_repealed), years_since_policy := 0]

make.dummy.dt <- function(in.dt, v.name, include.base=TRUE) {
  v.values <- sort(in.dt[,unique(get(v.name))])
  dummy.names <- paste(v.name, v.values, sep='_')
  start.i <- 1
  if (!include.base)
    start.i <- 2
  out.dt <- in.dt[,lapply(v.values[start.i:length(v.values)], function(i) {abs(get(v.name)==i)})]
  setnames(out.dt, names(out.dt), dummy.names[start.i:length(dummy.names)])

  return(out.dt)
}

gen.logit <- function(p, l=0, u=1) {
  return(log((p-l)/(u-p)))
}
inv.gen.logit <- function(x, l=0, u=1) {
  return((exp(x)*u +l)/(1+exp(x)))
}

# set.seed(10)

setwd("/snfs1/Project/us_counties/covariates/counties/prepped")
load('pop_by_race.rdata')

pop[,total.pop := sum(pop), by=.(year, state, mcnty, sex, age)]
pop[,share := pop/total.pop]
pop[total.pop == 0, share := 0]

pop <- pop[race==1]

setnames(pop, 'share', 'race_share')

load('income_median.rdata')
load('edu_ba.rdata')
load('pop_density.rdata')
load('elev_median.rdata')
load('unemployed.rdata')
load('rural.rdata')

income_median[, income.z := (income_median - mean(income_median))/sd(income_median)]

setwd('/homes/twolock/')
my.dt <- fread('thesis/data/compiled_mortality.csv')

my.dt <- merge(my.dt, pop[,.(year, mcnty, sex, age, race_share)], by=c('year', 'mcnty', 'age', 'sex'), all.x=TRUE)
rm(pop)

my.dt <- merge(my.dt, income_median, by=c('year', 'mcnty'), all.x=TRUE)
rm(income_median)

my.dt <- merge(my.dt, edu_ba, by=c('year', 'mcnty'), all.x=TRUE)
rm(edu_ba)

my.dt <- merge(my.dt, pop_density, by=c('year', 'mcnty'), all.x=TRUE)
rm(pop_density)

my.dt <- merge(my.dt, elev_median, by=c('year', 'mcnty'), all.x=TRUE)
rm(elev_median)

my.dt <- merge(my.dt, unemployed, by=c('year', 'mcnty'), all.x=TRUE)
rm(unemployed)

my.dt <- merge(my.dt, rural, by=c('year', 'mcnty'), all.x=TRUE)
rm(rural)

my.dt[, unemployed := unemployed / 100]

# shifted.income <- my.dt[, .(year, mcnty, age, sex, income_median)]
# shifted.income[, year := year + 1]
# setnames(shifted.income, 'income_median', 'income_median_prev')

# my.dt <- merge(my.dt, shifted.income, by=c('year', 'mcnty', 'age', 'sex'))
# my.dt[, income_pct_change := (income_median-income_median_prev)/income_median_prev]

my.dt[, pop.dens.z := (pop_density-mean(pop_density))/sd(pop_density), by=year]
my.dt[, log.pop.dens := log(pop_density)]

my.dt[, elev.z := (elev_median-mean(elev_median))/sd(elev_median)]

my.dt <- merge(my.dt, state.dt[, .(year, state, has.law, years_since_policy)], by=c('year', 'state'), all.x=TRUE)

test.states <- my.dt[,unique(state)]
test.states <- test.states[test.states != 11]
# test.states <- c(2, 6, 4, 8, 15, 16, 20, 27, 26, 30, 31, 32,
  # 35, 38, 40, 41, 46, 48, 49, 53, 56)
# test.states <- c(9,12,13, 21,23,25,33,34,36,42,44,45, 50,24,39,10,47,37,51, 54)
# test.states <- c(9,12,13)

test.years <- my.dt[,unique(year)]
# test.years <- 2000:2010
if (model.dt[model_i==model.i, model_name] == 'Years')
  test.years <- 1995:2013

test.ages <- my.dt[, unique(age)]
# test.ages <- c(20, 25, 30)

test.sex <- model.dt[model_i==model.i, sex]

data.subset <- my.dt[age %in% test.ages & state %in% test.states & pop > 0 & year %in% test.years & sex==test.sex, ]
age.dummies <- make.dummy.dt(data.subset, 'age', include.base=F)
data.subset[, names(age.dummies) := age.dummies]
policy.dummies <- make.dummy.dt(data.subset, 'years_since_policy', include.base=F)
data.subset[, names(policy.dummies) := policy.dummies]
year.dummmies <- make.dummy.dt(data.subset, 'year', include.base=F)
data.subset[, names(year.dummmies) := year.dummmies]
# state.dummies <- make.dummy.dt(data.subset, 'state', include.base=F)
# data.subset[, names(state.dummies) := state.dummies]
data.subset[, test_year := (year-min(year))/(max(year-min(year)))]
# data.subset[, edu_ba := edu_ba * 10]

# summary(m1 <- inla(firearm_deaths ~ log.pop.dens + unemployed + edu_ba + test_year + f(state) + f(mcnty) + as.factor(age), data = data.subset, verbose=TRUE, family='nbinomial', E=pop, num.threads=24))
# summary(m2 <- inla(firearm_deaths ~ log.pop.dens + unemployed + edu_ba + test_year + f(state) + f(mcnty) + as.factor(age), data = data.subset, verbose=TRUE, family='poisson', E=pop, num.threads=24))
# summary(m3 <- inla(firearm_deaths ~ log.pop.dens + unemployed + edu_ba + test_year + f(state) + f(mcnty) + as.factor(age), data = data.subset, verbose=TRUE, family='zeroinflatedpoisson1', E=pop, num.threads=24))

# X <- as.matrix(data.subset[,c('log.pop.dens', 'unemployed', 'edu_ba', 'test_year', grep('age_', names(data.subset), value=TRUE), grep('years_since_policy_', names(data.subset), value=TRUE)), with=F])

exposure.type <- model.dt[model_i==model.i, unique(exposure)]
exposure.var <- 'has.law'
if (exposure.type == 'indicator')
  exposure.var <- grep('years_since_policy_', names(data.subset), value=TRUE)

covar.list <- strsplit(model.dt[model_i==model.i, covar],',')[[1]]
if ('age' %in% covar.list) {
  covar.list <- covar.list[covar.list!='age']
  covar.list <- c(covar.list, grep('age_', names(data.subset), value=TRUE))
}
year.vars <- c()
if ('test_year' %in% covar.list) {
  year.vars <- 'test_year'
}
if ('test_year' %in% covar.list & model.dt[model_i==model.i, year_var=='indicator']) {
  covar.list <- covar.list[covar.list!='test_year']
  covar.list <- c(covar.list, grep('year_', names(data.subset), value=TRUE))
  year.vars <- grep('year_', names(data.subset), value=TRUE)
}

all.vars <- exposure.var
if (length(covar.list) > 0)
  all.vars <- c(all.vars, covar.list)

X <- as.matrix(data.subset[,c(all.vars), with=F])
# X <- as.matrix(data.subset[,c('test_year', grep('age_', names(data.subset), value=TRUE), grep('years_since_policy_', names(data.subset), value=TRUE)), with=F])
B <- rep(0, ncol(X))
C <- -8

s_i <- data.subset[,as.integer(as.factor(state))-1]
B_s <- rep(0, length(unique(s_i)))
log_SD_s <- log(1)


c_i <- data.subset[,as.integer(as.factor(mcnty))-1]
B_c <- rep(0, length(unique(c_i)))
log_SD_c <- log(1)

s_c_i <- as.integer(as.factor(data.subset[,mean(state), by=mcnty][,V1]))-1

log_theta <- log(1)

data.subset[, dep.var := firearm_deaths]
if (model.dt[model_i==model.i, model_name] %in% c('Supplement', 'State year'))
  data.subset[,dep.var := other_deaths]
if (model.dt[model_i==model.i, model_name] == 'Combined')
  data.subset[,dep.var := firearm_deaths + other_deaths]

Y <- data.subset[,dep.var]

theta_u <- 200
theta_l <- 1e-12

logit_prob_z <- gen.logit(0.5)

t_B_s <- rep(0, length(unique(s_i)))
t_i <- as.vector(data.subset[,test_year])

setwd('/homes/twolock/thesis/code/')

model.name <- paste0(model.dt[model_i==model.i, likelihood],'_model')

compile(paste0(model.name,'.cpp'))
dyn.load(dynlib(model.name))

data.in <- list(
  X=X,
  Y=Y,
  offset = as.vector(data.subset[,pop]),
  s_i = s_i,
  c_i = c_i,
  s_c_i = s_c_i,
  theta_u=theta_u,
  theta_l=theta_l
)
params.in <- list(
  C=C,
  B=B,
  B_s=B_s,
  log_SD_s=log_SD_s,
  B_c=B_c,
  log_SD_c=log_SD_c
)

start.time <- proc.time()

if (grepl('zi', model.name))
  params.in$logit_prob_z <- gen.logit(0.5)
if (grepl('nbinom', model.name))
  params.in$logit_theta <- 2
if (grepl('state_slope', model.name)) {
  data.in$t_i <- t_i
  params.in$t_B_s <- t_B_s
  params.in$log_SD_t <- log(1)
}

random.in <- NULL
random.in <- c('B_s', 'B_c', 't_B_s')
map.in <- NULL
# map.in$B_c <- as.factor(rep(NA, length(B_c)))
# map.in$log_SD_c <- as.factor(NA)
openmp(12)
AD.fn <- MakeADFun(data=data.in, parameters = params.in, random=random.in, map=map.in)

opt <- optim(par=AD.fn$par, fn=AD.fn$fn, gr=AD.fn$gr, method='L-BFGS-B', control=list(maxit=10000))
# opt <- optim(par=opt$par, fn=AD.fn$fn, gr=AD.fn$gr, method='L-BFGS-B', control=list(maxit=10000))
if (opt$convergence != 0) {
  print ("MODEL DIDN'T CONVERGE")
  BREAK
}

data.subset[,pred.count:=AD.fn$report()$pred_count]

plot.dt <- data.subset[, .(pop = sum(pop), real.count = sum(dep.var)), by=.(year, mcnty, state)]
# plot.dt[, pred.rate := pred.count/pop]
plot.dt[, rate := real.count/pop]

# plot.dt[, pct.change := (pred.rate - real.rate)/real.rate]
# plot.dt[pop==0, pct.change := 0]

biggest.counties.2013 <- plot.dt[, sample(unique(mcnty), 25)]
# biggest.counties.2013 <- plot.dt[year==max(year)][rev(order(pop)), unique(mcnty)][1:25]

sd.out <- sdreport(AD.fn, getJointPrecision=TRUE)

out.path <- '/homes/twolock/thesis/results/raw/'
out.pred.dt <- data.subset[, .(year, mcnty, age, sex, pop, state, firearm_deaths, other_deaths, pred.count)]
var.names <- colnames(X)
save(sd.out, out.pred.dt, opt, var.names, file=paste0(out.path, 'model_',model.i,'.RData'))

result.table <- summary.sdreport(sd.out, p.value=TRUE)

modeling.time <- proc.time()-start.time

fixed.table <- result.table[row.names(result.table) %in% c('B', 'C', 'logit_prob_z', 'log_theta', 'logit_theta'),]

transform.coeff <- fixed.table[,c('Estimate', 'Pr(>|z^2|)')]
transform.coeff[row.names(transform.coeff) %in% c('B', 'C', 'log_theta'), 'Estimate'] <- round(exp(transform.coeff[row.names(transform.coeff) %in% c('B', 'C', 'log_theta'), 'Estimate']), 3)
transform.coeff[row.names(transform.coeff) %in% c('logit_prob_z'), 'Estimate'] <- round(inv.gen.logit(transform.coeff[row.names(transform.coeff) %in% c('logit_prob_z'), 'Estimate']), 3)
transform.coeff[row.names(transform.coeff) %in% c('logit_theta'), 'Estimate'] <- round(inv.gen.logit(transform.coeff[row.names(transform.coeff) %in% c('logit_theta'), 'Estimate'], theta_l, theta_u), 3)

transform.coeff[, 'Pr(>|z^2|)'] <- round(transform.coeff[, 'Pr(>|z^2|)'],4)
row.names(transform.coeff)[row.names(transform.coeff) == 'B'] <- colnames(X)

row.names(fixed.table)[row.names(fixed.table) == 'B'] <- colnames(X)
fixed.table[, 'Pr(>|z^2|)'] <- round(fixed.table[, 'Pr(>|z^2|)'],4)

print(fixed.table)


mvrnorm.means <- c()
for (n in unique(colnames(sd.out$jointPrecision))) {
  if (n %in% random.in) {
    mvrnorm.means <- c(mvrnorm.means, sd.out$par.random[names(sd.out$par.random)==n])
  } else {
   mvrnorm.means <- c(mvrnorm.means, sd.out$par.fixed[names(sd.out$par.fixed)==n])
  }
}

# mvrnorm.means <- sd.out$value[names(sd.out$value) %in% colnames(sd.out$jointPrecision)]

par.draws <- mvrnorm(200, mvrnorm.means, solve(sd.out$jointPrecision))

# random.draws <- par.draws[,colnames(par.draws) %in% random.in]

plot.par.draws <- exp(par.draws[,colnames(par.draws) == 'B'])
colnames(plot.par.draws) <- colnames(X)
plot.par.draws <- cbind(plot.par.draws, draw=1:nrow(plot.par.draws))
plot.par.dt <- melt(data.table(plot.par.draws), id.var='draw')
plot.par.dt[, var.group := "Other"]
plot.par.dt[grepl('age_', variable), var.group := "Age"]
plot.par.dt[variable %in% c(exposure.var), var.group := "Policy"]
plot.par.dt[variable %in% year.vars, var.group := "Year"]

plot.par.dt[, is.age := grepl('age_', variable)]

# plot.par.dt[, policy.year := NA]
plot.par.dt[var.group == 'Policy', policy.year := as.integer(gsub("years_since_policy_", "", variable))]
if ('has.law' %in% c(exposure.var))
  plot.par.dt[var.group == 'Policy', policy.year := 1]
# plot.par.dt[, age := NA]
plot.par.dt[var.group == 'Age', age := as.integer(gsub("age_", "", variable))]
if (!plot.par.dt[,'Age' %in% unique(var.group)])
  plot.par.dt[, age := NA]

print('plotting coefficients')

pdf(paste0('/homes/twolock/thesis/results/graphs/model_', model.i,'.pdf'), width=14, height=8)
print('coeff')
test.gg <- ggplot(plot.par.dt[!(var.group %in% c('Age', 'Policy'))], aes(x=variable, y=value, color=var.group)) +
  geom_hline(yintercept=1.0, linetype = 2, color='red') +
  geom_boxplot() +
  ggtitle('Estimated relative risk distributions for all variables except Age and Policy Year')
print(test.gg)
print('age')
test.gg <- ggplot(plot.par.dt[var.group=='Age'], aes(x=age, y=value, color=var.group, group=age)) +
  geom_hline(yintercept=1.0, linetype = 2, color='red') +
  geom_boxplot() +
  ggtitle('Estimated relative risk of Age')
print(test.gg)
print('exposure')
test.gg <- ggplot(plot.par.dt[var.group=='Policy'], aes(x=policy.year, y=value, color=var.group, group=policy.year)) +
  geom_hline(yintercept=1.0, linetype = 2, color='red') +
  geom_boxplot() +
  ggtitle('Estiamted relative risk of policy variable')
print(test.gg)

print('done plotting coefficients')

# dev.off()

report.list <- lapply(split(par.draws, 1:nrow(par.draws)), AD.fn$report)

print('getting count draws')

count.draws <- t(do.call(rbind, lapply(report.list, function(x){x$pred_count})))

print('got count draws')

colnames(count.draws) <- paste0('draw_', 1:ncol(count.draws))
draw.dt <- melt(cbind(data.subset[, .(year, mcnty, age, sex, pop)], count.draws), id.vars=c('year', 'mcnty', 'pop','sex', 'age'))

pred.dt <- draw.dt[, .(pop=sum(pop), pred_count=sum(value)), by=.(year, mcnty, variable)]
pred.dt[, pred.rate := pred_count / pop]

print('getting summary data')

pred.summaries <- pred.dt[, .(rate=mean(pred.rate),
  upper=quantile(pred.rate, 0.975), lower=quantile(pred.rate, 0.025)), by=.(year, mcnty)]
pred.summaries[, source := 'Prediction']

plot.dt[, source := 'Data']
plot.dt[, upper := NA]
plot.dt[, lower := NA]

tmp.plot.dt <- rbind(plot.dt[,.(year, mcnty, rate, upper, lower, source)], pred.summaries)

# library(ggplot2)

gg.dt <- tmp.plot.dt[mcnty %in% biggest.counties.2013]
gg.dt[, mcnty := as.integer(as.factor(mcnty))]

print('plotting fit')

# pdf('/homes/twolock/test/nbinom_test/random_results.pdf', width=11, height=8.5)
gg <- ggplot() +
  geom_ribbon(data=gg.dt[source=='Prediction'], aes(x=year, ymin=lower, ymax=upper, fill=source), alpha=0.3) +
  geom_line(data=gg.dt[source=='Prediction'], aes(x=year, y=rate, color=source, fill=source)) +
  geom_point(data=gg.dt[source=='Data'], aes(x=year, y=rate, color=source, fill=source)) +
  facet_wrap('mcnty', scales='free_y') +
  ggtitle('Model fit in 25 randomly selected counties')
print(gg)

# dev.off()


re.counties <- as.integer(as.factor(plot.dt[, sample(unique(mcnty), 25)]))

# pdf('/homes/twolock/test/nbinom_test/random_effects.pdf', width=11, height=8.5)

print('random effects')

state.RE <- sd.out$par.random[grepl('B_s',names(sd.out$par.random))]
RE.dt <- cbind(level='state', data.table(RE_val=state.RE))
county.RE <- sd.out$par.random[grepl('B_c',names(sd.out$par.random))]
RE.dt <- rbind(RE.dt, cbind(level='county', data.table(RE_val=county.RE)))
print('plotting random effects')
RE.gg <- ggplot(RE.dt, aes(RE_val)) +
  geom_histogram() +
  facet_wrap('level', scales='free_y')
print(RE.gg)
# dev.off()


# county.REs <- sd.out$par.random[grepl('B_c',names(sd.out$par.random))]
# county.dt <- cbind(data.subset[,.(state=mean(state)), by=mcnty], county.re = county.REs)

# re.gg <- ggplot(data=county.dt, aes(x=county.re)) +
#   geom_histogram(binwidth=0.1) + facet_wrap('state', scales='free_y')
# print(re.gg)

# dev.off()
# out.effects <- sd.out$par.random[grepl('B_c', names(sd.out$par.random))][re.counties]
# sd.out$par.fixed

# fixed.draws <- par.draws[,colnames(par.draws) == 'B']
# colnames(fixed.draws) <- colnames(X)
# fixed.draws <- exp(fixed.draws)

# age.draws <- data.table(fixed.draws[, grepl('age_', colnames(fixed.draws))])
# age.draws[, draw := 1:nrow(age.draws)]
# long.ages <- melt(age.draws, id.vars='draw')
# long.ages[, c('junk', 'age') := tstrsplit(variable, '_')]

# pdf('/homes/twolock/test/nbinom_test/age_RRs.pdf', width=11, height=8.5)
# age.gg <- ggplot(data=long.ages, aes(x=age, y=value, group=draw)) +
#   geom_line(alpha=0.1, color='navy')
# print(age.gg)
# dev.off()

# print(modeling.time)

mcnty.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/mcnty_mapping_shape_file.shp")
state.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/state_mapping_shape_file.shp")

source('/homes/twolock/thesis/code/plot_data.R')

plot.years <- sort(unique(c(seq(test.years[1], test.years[length(test.years)], 5), min(test.years), max(test.years))))
print('plotting maps')
series_map(chloropleth_map = copy(mcnty.shape[mcnty.shape$state %in% test.states,]),
           outline_map = state.shape[state.shape$state %in% test.states,],
           data = copy(tmp.plot.dt[source=='Prediction' & year %in% plot.years]),
           geog_id = 'mcnty',
           variable = 'rate',
           series_dimension = 'year',
           series_sequence = plot.years,
           histogram=TRUE,
           color_ramp=woodson_pallettes("easter_to_earth"),
           map_title = 'Predicted firearm suicide mortality'
           )

dev.off()

print(opt$convergence)
print(opt$message)

print(transform.coeff)
print(modeling.time)


print(inv.gen.logit(opt$par['logit_theta'], theta_l, theta_u))
