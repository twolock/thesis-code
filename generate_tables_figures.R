library(foreign)
library(data.table)
library(raster)
library(ggplot2)
library(TMB)


number.plug <- list()
figures <- list()
tables <- list()

code.p.val <- function(x) {
    sig.level <- rowSums(sapply(c(0.05, 0.01, 0.001), function(v) {as.integer(x < v)}))
    asterisks <- sapply(sig.level, function(i) {paste(rep('*', i), collapse='')})
}

set.seed(50)

setwd("/homes/twolock/thesis")

law.dt <- fread('data/statute_history.csv')

location.dt <- fread('data/locations.csv')

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

number.plug$state.years.policy <- state.dt[,sum(has.law)]

death.dt <- fread('data/compiled_mortality.csv')
death.dt <- death.dt[state != 11]
death.dt <- merge(death.dt, location.dt[ fips %in% state.dt[,unique(state)]][, .(state=fips, location_name)], by='state', all.x=TRUE)

number.plug$total.suicide.deaths <- death.dt[, sum(firearm_deaths+other_deaths)]

number.plug$both.pct.firearm <- death.dt[, sum(firearm_deaths)/sum(firearm_deaths+other_deaths)*100]
number.plug$both.pct.male <- death.dt[, sum(firearm_deaths+other_deaths),by=sex][,share.sex := V1/sum(V1)][sex==1,share.sex*100]

number.plug$firearm.pct.male <- death.dt[, sum(firearm_deaths),by=sex][,share.sex := V1/sum(V1)][sex==1,share.sex*100]
number.plug$other.pct.male <- death.dt[, sum(other_deaths),by=sex][,share.sex := V1/sum(V1)][sex==1,share.sex*100]

age.agg.dt <- death.dt[year==2013, .(deaths=sum(firearm_deaths), pop=sum(pop)), by=.(age, sex)]
age.agg.dt[, mort := deaths/pop * 100000]

age.agg.dt[, Sex := 'Men']
age.agg.dt[sex == 2, Sex := 'Women']
age.agg.dt <- merge(age.agg.dt, age.agg.dt[age==15, .(mort_15=mort), by=Sex], by='Sex')

age.agg.dt[, ratio := mort/mort_15]

figures$age.agg.dt <- ggplot(data=age.agg.dt, aes(x=age, y=ratio, color=Sex)) +
    geom_line() + xlab('Age bin') + ylab('Firearm suicide mortality ratio') +
    theme(legend.position = "bottom")

slope.dt <- death.dt[year %in% c(1980, 2013), .(deaths=sum(firearm_deaths), pop=sum(pop)), by=.(year, sex)]
slope.dt[,rate:=deaths/pop*100000]
wide.slope.dt <- dcast(slope.dt, sex ~ year, value.var='rate')
setnames(wide.slope.dt, c('1980', '2013'), c('val_1980', 'val_2013'))

wide.slope.dt[, slope:=-1*(val_2013-val_1980)/(2013-1980)]

number.plug$mort.slope.male <- wide.slope.dt[sex==1, slope]
number.plug$mort.slope.female <- wide.slope.dt[sex==2, slope]

agg.state.dt <- death.dt[year==2013, .(firearm_deaths=sum(firearm_deaths), overall_deaths=sum(firearm_deaths+other_deaths),pop=sum(pop)), by=location_name]

number.plug$highest.states.firearm <- agg.state.dt[order(firearm_deaths/pop, decreasing=TRUE), location_name][1:5]
number.plug$highest.states.overall <- agg.state.dt[order(overall_deaths/pop, decreasing=TRUE), location_name][1:5]

number.plug$num.mcnty <- death.dt[, length(unique(mcnty))]
number.plug$num.ages <- death.dt[, length(unique(age))]
number.plug$num.years <- death.dt[, length(unique(year))]
number.plug$num.obs <- nrow(death.dt)

number.plug$num.zero.firearm <- death.dt[, sum(as.integer(firearm_deaths==0))]
number.plug$num.zero.other<- death.dt[, sum(as.integer(other_deaths==0))]

death.dt[, firearm_rate := firearm_deaths / pop*100000]
figures$histogram.2013 <- ggplot(data=death.dt[year==2013], aes(firearm_rate)) +
    geom_histogram(color=NA, fill='cadetblue4', binwidth = 500) +
    xlab('Firearm suicide mortality rate (per 100,000)') +
    ylab('Number of observations')

number.plug$pct.zero.firearm <- death.dt[, mean(as.integer(firearm_deaths==0))*100]
number.plug$pct.zero.other<- death.dt[, mean(as.integer(other_deaths==0))*100]

number.plug$num.states.law.2013 <- state.dt[year==2013, sum(has.law)]
number.plug$name.states.law.2013 <- merge(state.dt[year==2013 & has.law == 1], location.dt[ fips %in% state.dt[,unique(state)]][, .(state=fips, location_name)], by='state', all.x=TRUE)[,unique(location_name)]

number.plug$num.states.law.ever <- state.dt[has.law==1, length(unique(state))]
number.plug$state.years.law <- state.dt[, sum(has.law)]

number.plug$first.law.year <- state.dt[, min(bill_passed,na.rm=TRUE)]
number.plug$first.law.state <- location.dt[fips==state.dt[bill_passed==min(bill_passed,na.rm=TRUE), unique(state)], location_name]

number.plug$last.law.year <- state.dt[, max(bill_passed,na.rm=TRUE)]
number.plug$last.law.state <- location.dt[fips==state.dt[bill_passed==max(bill_passed,na.rm=TRUE), unique(state)], location_name]

result.dir <- '/homes/twolock/thesis/results/raw/'

# library(TMB)

is.first <- TRUE
model.i <- 1
for (model.i in 1:6) {
    print(model.i)
    load(paste0(result.dir, 'model_',model.i,'.RData'))

    result.table <- summary.sdreport(sd.out, p.value=TRUE)

    fixed.table <- result.table[row.names(result.table) %in% c('B', 'C', 'logit_prob_z', 'log_theta', 'logit_theta'),]

    transform.coeff <- fixed.table[,c('Estimate', 'Std. Error', 'Pr(>|z^2|)')]

    # transform.coeff[, 'Estimate'] <- round(transform.coeff[, 'Estimate'],3)
    # transform.coeff[, 'Std. Error'] <- round(transform.coeff[, 'Std. Error'],3)
    # transform.coeff[, 'Pr(>|z^2|)'] <- round(transform.coeff[, 'Pr(>|z^2|)'],4)
    row.names(transform.coeff)[row.names(transform.coeff) == 'B'] <- var.names

    coeff.dt <- data.table(transform.coeff)
    coeff.dt[,variable := row.names(transform.coeff)]
    coeff.dt[, str.est := formatC(Estimate, digits=2, format="f", big.mark=',')]
    coeff.dt[, 'str.error' := formatC(get('Std. Error'), digits=3, format="f", big.mark=','), with=F]
    coeff.dt[, asterisks := code.p.val(get('Pr(>|z^2|)'))]

    coeff.dt[, paste0('Model ', model.i) := paste0(get('str.est'), get('asterisks'), ' (', get('str.error'), ')'), with=F]

    tmp.out <- coeff.dt[, c('variable', paste0('Model ', model.i)), with=F]

    # r2.dt <- data.table(t(c('r2',formatC(out.pred.dt[, sum((pred.count-mean(firearm_deaths))^2)/sum((firearm_deaths-mean(firearm_deaths))^2)], digits=3, format="f", big.mark=','))))
    # setnames(r2.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    ll.dt <- data.table(t(c('ll',formatC(-1*opt$value, digits=0, format="f", big.mark=','))))
    setnames(ll.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    tmp.out <- rbind(tmp.out, ll.dt)

    if (is.first) {
        all.models <- tmp.out
        is.first <- FALSE
    } else {
        all.models <- merge(all.models, tmp.out, by='variable', all=TRUE)
    }
}

variable.dt <- fread('/homes/twolock/thesis/data/report_variables.csv')
all.models <- merge(all.models, variable.dt, by='variable')

# keep.variables <- c('variable')
out.table <- all.models[in.table==1][order(sort.order)][,c('variable', grep('Model *', names(all.models), value=T)), with=F]
for (j in names(out.table))
    set(out.table,which(is.na(out.table[[j]])),j,'-')

table.format <- fread('/homes/twolock/thesis/data/table_1_format.csv')
table.matrix <- as.matrix(table.format)
table.matrix[c(-1, -2), -1] <- as.matrix(out.table[,grep('Model *', names(all.models), value=T), with=F])
colnames(table.matrix) <- table.matrix[1,]
table.matrix <-table.matrix[-1,]

# library(pander)

tables$table.1 <- table.matrix


appendix.table <- all.models[order(sort.order)][,c('variable', grep('Model *', names(all.models), value=T)), with=F]
for (j in names(appendix.table))
    set(appendix.table,which(is.na(appendix.table[[j]])),j,'-')

table.format <- fread('/homes/twolock/thesis/data/table_appendix_1_format.csv')
table.matrix <- as.matrix(table.format)
table.matrix[c(-1, -2), -1] <- as.matrix(appendix.table[,grep('Model *', names(all.models), value=T), with=F])
colnames(table.matrix) <- table.matrix[1,]
table.matrix <-table.matrix[-1,]

# library(pander)

tables$appendix.table.1 <- table.matrix


is.first <- TRUE
model.i <- 1
for (model.i in 13:16) {
    print(model.i)
    load(paste0(result.dir, 'model_',model.i,'.RData'))

    result.table <- summary.sdreport(sd.out, p.value=TRUE)

    fixed.table <- result.table[row.names(result.table) %in% c('B', 'C', 'logit_prob_z', 'log_theta', 'logit_theta'),]

    transform.coeff <- fixed.table[,c('Estimate', 'Std. Error', 'Pr(>|z^2|)')]

    # transform.coeff[, 'Estimate'] <- round(transform.coeff[, 'Estimate'],3)
    # transform.coeff[, 'Std. Error'] <- round(transform.coeff[, 'Std. Error'],3)
    # transform.coeff[, 'Pr(>|z^2|)'] <- round(transform.coeff[, 'Pr(>|z^2|)'],4)
    row.names(transform.coeff)[row.names(transform.coeff) == 'B'] <- var.names

    coeff.dt <- data.table(transform.coeff)
    coeff.dt[,variable := row.names(transform.coeff)]
    coeff.dt[, str.est := formatC(Estimate, digits=2, format="f", big.mark=',')]
    coeff.dt[, 'str.error' := formatC(get('Std. Error'), digits=3, format="f", big.mark=','), with=F]
    coeff.dt[, asterisks := code.p.val(get('Pr(>|z^2|)'))]

    coeff.dt[, paste0('Model ', model.i) := paste0(get('str.est'), get('asterisks'), ' (', get('str.error'), ')'), with=F]

    tmp.out <- coeff.dt[, c('variable', paste0('Model ', model.i)), with=F]

    # r2.dt <- data.table(t(c('r2',formatC(out.pred.dt[, sum((pred.count-mean(firearm_deaths))^2)/sum((firearm_deaths-mean(firearm_deaths))^2)], digits=3, format="f", big.mark=','))))
    # setnames(r2.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    ll.dt <- data.table(t(c('ll',formatC(-1*opt$value, digits=0, format="f", big.mark=','))))
    setnames(ll.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    tmp.out <- rbind(tmp.out, ll.dt)

    if (is.first) {
        all.models <- tmp.out
        is.first <- FALSE
    } else {
        all.models <- merge(all.models, tmp.out, by='variable', all=TRUE)
    }
}

variable.dt <- fread('/homes/twolock/thesis/data/report_variables.csv')
all.models <- merge(all.models, variable.dt, by='variable')

# keep.variables <- c('variable')
out.table <- all.models[in.table==1][order(sort.order)][,c('variable', grep('Model *', names(all.models), value=T)), with=F]
for (j in names(out.table))
    set(out.table,which(is.na(out.table[[j]])),j,'-')

table.format <- fread('/homes/twolock/thesis/data/table_2_format.csv')
table.matrix <- as.matrix(table.format)
table.matrix[c(-1, -2), -1] <- as.matrix(out.table[,grep('Model *', names(all.models), value=T), with=F])
colnames(table.matrix) <- table.matrix[1,]
table.matrix <-table.matrix[-1,]

# library(pander)

tables$table.2 <- table.matrix

appendix.table <- all.models[order(sort.order)][,c('variable', grep('Model *', names(all.models), value=T)), with=F]
for (j in names(appendix.table))
    set(appendix.table,which(is.na(appendix.table[[j]])),j,'-')

table.format <- fread('/homes/twolock/thesis/data/table_appendix_2_format.csv')
table.matrix <- as.matrix(table.format)
table.matrix[c(-1, -2), -1] <- as.matrix(appendix.table[,grep('Model *', names(all.models), value=T), with=F])
colnames(table.matrix) <- table.matrix[1,]
table.matrix <-table.matrix[-1,]

# library(pander)

tables$appendix.table.2 <- table.matrix


best.models <- c(5, 6)
model.i <- 5
all.effects <- data.table()
for (model.i in best.models) {
    print(model.i)
    load(paste0(result.dir, 'model_',model.i,'.RData'))

    result.table <- summary.sdreport(sd.out, p.value=TRUE)

    fixed.table <- result.table[row.names(result.table) %in% c('B', 'C', 'logit_prob_z', 'log_theta', 'logit_theta'),]

    transform.coeff <- fixed.table[,c('Estimate', 'Std. Error', 'Pr(>|z^2|)')]

    # transform.coeff[, 'Estimate'] <- round(transform.coeff[, 'Estimate'],3)
    # transform.coeff[, 'Std. Error'] <- round(transform.coeff[, 'Std. Error'],3)
    # transform.coeff[, 'Pr(>|z^2|)'] <- round(transform.coeff[, 'Pr(>|z^2|)'],4)
    row.names(transform.coeff)[row.names(transform.coeff) == 'B'] <- var.names

    coeff.dt <- data.table(transform.coeff)
    coeff.dt[,variable := row.names(transform.coeff)]
    coeff.dt[, Sex := "Men"]
    if (model.i %% 2 == 0) coeff.dt[, Sex := 'Women']

    effect.dt <- coeff.dt[grepl('years_since_policy_', variable)]
    effect.dt[, Upper := exp(Estimate + qnorm(0.975) * get('Std. Error'))]
    effect.dt[, Lower := exp(Estimate - qnorm(0.975) * get('Std. Error'))]

    effect.dt[, Mean := exp(Estimate)]

    effect.dt <- merge(effect.dt, variable.dt, by='variable', all.x=TRUE)

    tmp.effect <- effect.dt[, .(Mean, Upper, Lower, Sex, num.val)]
    all.effects <- rbind(all.effects, tmp.effect)
}

# all.effects[, min.val := min(Mean), by=Sex]

number.plug$men.RR.year <- 11
number.plug$women.RR.year <- 12

number.plug$men.RR.mean <- all.effects[Sex=='Men' & num.val == number.plug$men.RR.year, Mean]
number.plug$men.RR.Upper <- all.effects[Sex=='Men' & num.val == number.plug$men.RR.year, Upper]
number.plug$men.RR.Lower <- all.effects[Sex=='Men' & num.val == number.plug$men.RR.year, Lower]

number.plug$women.RR.mean <- all.effects[Sex=='Women' & num.val == number.plug$women.RR.year, Mean]
number.plug$women.RR.Upper <- all.effects[Sex=='Women' & num.val == number.plug$women.RR.year, Upper]
number.plug$women.RR.Lower <- all.effects[Sex=='Women' & num.val == number.plug$women.RR.year, Lower]


# setnames(all.effects, 'num.val', 'Years since policy was passed')

RR.plot <- ggplot(data=all.effects, aes(x=num.val, y=Mean, ymax=Upper, ymin=Lower, color=Sex)) +
    geom_hline(yintercept=1.0, linetype = 2, color='red') +
    geom_pointrange(position = position_jitter(w = 0.5)) + xlab('Years since PTP policy was passed') +
    ylab('Estimated relative risk relative to 0 years of policy') +
    theme(legend.position = "bottom")

figures$RR.plot <- RR.plot


plot.pair <- sort(c(6, 8))


for (base.model.i in c(5, 6)) {
    for (comp.model.i in base.model.i+c(2,4)) {
        plot.pair <- c(base.model.i, comp.model.i)
        tmp.i <- 1
        all.dt <- data.table()
        for (n in plot.pair) {
            load(paste0('/homes/twolock/thesis/results/raw/model_',n,'.RData'))
            # all.results[[n]] <- sd.out
            random.dt <- cbind(effect.type='random', data.table(Mean=sd.out$par.random, SE=sqrt(sd.out$diag.cov.random)))
            fixed.SEs <- sqrt(diag(sd.out$cov.fixed))
            fixed.dt <- cbind(effect.type='fixed', data.table(Mean=sd.out$par.fixed[names(sd.out$par.fixed)=='B'], SE=fixed.SEs[names(fixed.SEs) == 'B']))

            to.append <- rbind(fixed.dt, random.dt)
            to.append[, model:=paste0('Model_',tmp.i)]

            to.append[, par_i := 1:.N]

            all.dt <- rbind(all.dt, to.append)
            tmp.i <- tmp.i + 1
        }

        plot.dt <- dcast(melt(all.dt, id.vars=c('effect.type', 'model', 'par_i')), effect.type + variable + par_i ~ model)

        number.plug[[paste0('comp.Mean.',paste(plot.pair, collapse='_'))]] <- plot.dt[variable=='Mean', cor(Model_1, Model_2, method='pearson')]
        number.plug[[paste0('comp.SE.',paste(plot.pair, collapse='_'))]] <- plot.dt[variable=='SE', cor(Model_1, Model_2, method='pearson')]

        y.name <- 'Negative binomial'
        if (comp.model.i > 8)
            y.name <- 'Zero-inflated Poisson'

        figures[[paste0('comp.plot.',paste(plot.pair, collapse='_'))]] <- ggplot(plot.dt, aes(x=Model_1, y=Model_2)) +
            geom_abline(slope=1, color='black') +
            geom_point(shape=1, size=2) + coord_equal() + ggtitle('Mean') + facet_wrap(~variable, scales='free') +
            xlab('Poisson') + ylab(y.name)
    }
}


supplementary.models <- c(13, 14)
# model.i <- 5
all.effects <- data.table()
for (model.i in supplementary.models) {
    print(model.i)
    load(paste0(result.dir, 'model_',model.i,'.RData'))

    result.table <- summary.sdreport(sd.out, p.value=TRUE)

    fixed.table <- result.table[row.names(result.table) %in% c('B', 'C', 'logit_prob_z', 'log_theta', 'logit_theta'),]

    transform.coeff <- fixed.table[,c('Estimate', 'Std. Error', 'Pr(>|z^2|)')]

    # transform.coeff[, 'Estimate'] <- round(transform.coeff[, 'Estimate'],3)
    # transform.coeff[, 'Std. Error'] <- round(transform.coeff[, 'Std. Error'],3)
    # transform.coeff[, 'Pr(>|z^2|)'] <- round(transform.coeff[, 'Pr(>|z^2|)'],4)
    row.names(transform.coeff)[row.names(transform.coeff) == 'B'] <- var.names

    coeff.dt <- data.table(transform.coeff)
    coeff.dt[,variable := row.names(transform.coeff)]
    coeff.dt[, Sex := "Men"]
    if (model.i %% 2 == 0) coeff.dt[, Sex := 'Women']

    effect.dt <- coeff.dt[grepl('years_since_policy_', variable)]
    effect.dt[, Upper := exp(Estimate + qnorm(0.975) * get('Std. Error'))]
    effect.dt[, Lower := exp(Estimate - qnorm(0.975) * get('Std. Error'))]

    effect.dt[, Mean := exp(Estimate)]

    effect.dt <- merge(effect.dt, variable.dt, by='variable', all.x=TRUE)

    tmp.effect <- effect.dt[, .(Mean, Upper, Lower, Sex, num.val)]
    all.effects <- rbind(all.effects, tmp.effect)
}

# setnames(all.effects, 'num.val', 'Years since policy was passed')

supplement.RR.plot <- ggplot(data=all.effects, aes(x=num.val, y=Mean, ymax=Upper, ymin=Lower, color=Sex)) +
    geom_hline(yintercept=1.0, linetype = 2, color='red') +
    geom_pointrange(position = position_jitter(w = 0.5)) + xlab('Years since PTP policy was passed') +
    ylab('Estimated relative risk relative to 0 years of policy') +
    theme(legend.position = "bottom")

figures$supplement.RR.plot <- supplement.RR.plot

statute.table <- fread('/homes/twolock/thesis/data/statute_table.csv')
for (j in names(statute.table))
    set(statute.table,which(statute.table[[j]]==''),j,NA)

tables$statute.table <- statute.table

is.first <- TRUE
model.i <- 7
for (model.i in 7:10) {
    print(model.i)
    load(paste0(result.dir, 'model_',model.i,'.RData'))

    result.table <- summary.sdreport(sd.out, p.value=TRUE)

    fixed.table <- result.table[row.names(result.table) %in% c('B', 'C', 'logit_prob_z', 'log_theta', 'logit_theta'),]

    transform.coeff <- fixed.table[,c('Estimate', 'Std. Error', 'Pr(>|z^2|)')]

    # transform.coeff[, 'Estimate'] <- round(transform.coeff[, 'Estimate'],3)
    # transform.coeff[, 'Std. Error'] <- round(transform.coeff[, 'Std. Error'],3)
    # transform.coeff[, 'Pr(>|z^2|)'] <- round(transform.coeff[, 'Pr(>|z^2|)'],4)
    row.names(transform.coeff)[row.names(transform.coeff) == 'B'] <- var.names

    coeff.dt <- data.table(transform.coeff)
    coeff.dt[,variable := row.names(transform.coeff)]
    coeff.dt[, str.est := formatC(Estimate, digits=2, format="f", big.mark=',')]
    coeff.dt[, 'str.error' := formatC(get('Std. Error'), digits=3, format="f", big.mark=','), with=F]
    coeff.dt[, asterisks := code.p.val(get('Pr(>|z^2|)'))]

    coeff.dt[, paste0('Model ', model.i) := paste0(get('str.est'), get('asterisks'), ' (', get('str.error'), ')'), with=F]

    tmp.out <- coeff.dt[, c('variable', paste0('Model ', model.i)), with=F]

    # r2.dt <- data.table(t(c('r2',formatC(out.pred.dt[, sum((pred.count-mean(firearm_deaths))^2)/sum((firearm_deaths-mean(firearm_deaths))^2)], digits=3, format="f", big.mark=','))))
    # setnames(r2.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    ll.dt <- data.table(t(c('ll',formatC(-1*opt$value, digits=0, format="f", big.mark=','))))
    setnames(ll.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    tmp.out <- rbind(tmp.out, ll.dt)

    if (is.first) {
        all.models <- tmp.out
        is.first <- FALSE
    } else {
        all.models <- merge(all.models, tmp.out, by='variable', all=TRUE)
    }
}

variable.dt <- fread('/homes/twolock/thesis/data/report_variables.csv')
all.models <- merge(all.models, variable.dt, by='variable')

appendix.table <- all.models[order(sort.order)][,c('variable', grep('Model *', names(all.models), value=T)), with=F]
for (j in names(appendix.table))
    set(appendix.table,which(is.na(appendix.table[[j]])),j,'-')

table.format <- fread('/homes/twolock/thesis/data/table_appendix_3_format.csv')
table.matrix <- as.matrix(table.format)
table.matrix[c(-1, -2), -1] <- as.matrix(appendix.table[,grep('Model *', names(all.models), value=T), with=F])
colnames(table.matrix) <- table.matrix[1,]
table.matrix <-table.matrix[-1,]

# library(pander)

tables$appendix.table.3 <- table.matrix


is.first <- TRUE
model.i <- 11
for (model.i in 11:12) {
    print(model.i)
    load(paste0(result.dir, 'model_',model.i,'.RData'))

    result.table <- summary.sdreport(sd.out, p.value=TRUE)

    fixed.table <- result.table[row.names(result.table) %in% c('B', 'C', 'logit_prob_z', 'log_theta', 'logit_theta'),]

    transform.coeff <- fixed.table[,c('Estimate', 'Std. Error', 'Pr(>|z^2|)')]

    # transform.coeff[, 'Estimate'] <- round(transform.coeff[, 'Estimate'],3)
    # transform.coeff[, 'Std. Error'] <- round(transform.coeff[, 'Std. Error'],3)
    # transform.coeff[, 'Pr(>|z^2|)'] <- round(transform.coeff[, 'Pr(>|z^2|)'],4)
    row.names(transform.coeff)[row.names(transform.coeff) == 'B'] <- var.names

    coeff.dt <- data.table(transform.coeff)
    coeff.dt[,variable := row.names(transform.coeff)]
    coeff.dt[, str.est := formatC(Estimate, digits=2, format="f", big.mark=',')]
    coeff.dt[, 'str.error' := formatC(get('Std. Error'), digits=3, format="f", big.mark=','), with=F]
    coeff.dt[, asterisks := code.p.val(get('Pr(>|z^2|)'))]

    coeff.dt[, paste0('Model ', model.i) := paste0(get('str.est'), get('asterisks'), ' (', get('str.error'), ')'), with=F]

    tmp.out <- coeff.dt[, c('variable', paste0('Model ', model.i)), with=F]

    # r2.dt <- data.table(t(c('r2',formatC(out.pred.dt[, sum((pred.count-mean(firearm_deaths))^2)/sum((firearm_deaths-mean(firearm_deaths))^2)], digits=3, format="f", big.mark=','))))
    # setnames(r2.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    ll.dt <- data.table(t(c('ll',formatC(-1*opt$value, digits=0, format="f", big.mark=','))))
    setnames(ll.dt, c('V1', 'V2'), c('variable', paste0('Model ', model.i)))

    tmp.out <- rbind(tmp.out, ll.dt)

    if (is.first) {
        all.models <- tmp.out
        is.first <- FALSE
    } else {
        all.models <- merge(all.models, tmp.out, by='variable', all=TRUE)
    }
}

variable.dt <- fread('/homes/twolock/thesis/data/report_variables.csv')
all.models <- merge(all.models, variable.dt, by='variable')

appendix.table <- all.models[order(sort.order)][,c('variable', grep('Model *', names(all.models), value=T)), with=F]
for (j in names(appendix.table))
    set(appendix.table,which(is.na(appendix.table[[j]])),j,'-')

table.format <- fread('/homes/twolock/thesis/data/table_appendix_4_format.csv')
table.matrix <- as.matrix(table.format)
table.matrix[c(-1, -2), -1] <- as.matrix(appendix.table[,grep('Model *', names(all.models), value=T), with=F])
colnames(table.matrix) <- table.matrix[1,]
table.matrix <-table.matrix[-1,]

# library(pander)

tables$appendix.table.4 <- table.matrix

source('/homes/twolock/thesis/code/woodson_pallettes.R')

mcnty.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/mcnty_mapping_shape_file.shp")
state.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/state_mapping_shape_file.shp")

load('/homes/twolock/thesis/results/raw/model_5.RData')

collapsed.dt <- out.pred.dt[year==2013, .(mort=100000*sum(pred.count)/sum(pop)), by=mcnty]
collapsed.dt[, id := as.character(mcnty)]

fort.dt <- data.table(fortify(mcnty.shape))
fort.dt <- merge(fort.dt, collapsed.dt, by='id', all.x=TRUE)

fort.state <- fortify(state.shape)

map.gg <- ggplot(data=fort.dt, aes(x=long, y=lat, group=group)) +
  geom_polygon(color=NA, aes(fill=mort)) + coord_fixed() +
  geom_polygon(data=fort.state, fill=NA, color='white') +
  theme_minimal() +
  scale_fill_gradientn(colors=rev(woodson_pallettes('easter_to_earth')),name='Estimated firearm suicide deaths per 100,000 people') +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),panel.grid.major=element_blank(), legend.position='bottom',
        panel.grid.minor=element_blank()) +
  guides(fill=guide_colorbar(title.position = 'top'))#,barwidth=10))
  
figures$map.gg <- map.gg

out.path <- '/homes/twolock/thesis/data/tables_figures.RData'

save(tables, figures, number.plug, file=out.path)
