library(data.table)
library(ggplot2)

setwd('/homes/twolock/thesis/')
my.dt <- fread('data/compiled_mortality.csv')

sex.key <- c('male', 'female')

my.dt[, total.suicide := firearm_deaths + other_deaths]

# collapsed.dt <- my.dt[, .(total.suicide=sum(total.suicide), firearm_deaths=sum(firearm_deaths), other_deaths=sum(other_deaths)), by=.(year, state, mcnty)]

share.dt <- my.dt[, .(firearm_deaths=sum(firearm_deaths), other_deaths=sum(other_deaths), pop=sum(pop)), by=.(year, age, sex)]
share.dt.long <- melt(share.dt, id.vars=c('year', 'age', 'sex', 'pop'))

share.dt.long[, c('method', 'junk') := tstrsplit(variable, '_')]
setnames(share.dt.long, 'value', 'deaths')

share.dt.long[, share := (deaths/sum(deaths)), by=.(year, age, sex)]

all.suicide.dt <- share.dt.long[, .(deaths=sum(deaths)), by=.(year, age, sex, method, pop)]

tmp.all <- all.suicide.dt[, .(deaths=sum(deaths)), by=.(year, age, sex, pop)]
tmp.all[, method:='all']

all.suicide.dt <- rbind(all.suicide.dt, tmp.all)

all.suicide.dt[, rate := deaths/pop]

wide.dt <- dcast(all.suicide.dt, year + age + method ~ sex , value.var='rate')
setnames(wide.dt, c('1', '2'), c('rate_m', 'rate_f'))

wide.dt[, rate_ratio := rate_m/rate_f]

pdf('descriptive_plots.pdf', width=14, height=8)
for (s in share.dt.long[, unique(sex)]) {
	gg <- ggplot(share.dt.long[sex==s,], aes(x=year, y=deaths, fill=method)) + 
		geom_area(position='stack') +
		facet_wrap('age') +
		ggtitle(paste0('Suicide deaths share by means (sex = ',sex.key[s],')'))
	print(gg)
}

gg <- ggplot(wide.dt, aes(x=year, y=rate_ratio, color=method)) + 
	geom_hline(aes(yintercept=1), color='red', linetype=2) +
	geom_line() +
	facet_wrap('age', scales='free_y') +
	ggtitle('Time trend in ratio of suicide mortality rate in men to suicide mortality rate in women')
	# ylim(1, 16)
print(gg)

gg <- ggplot(wide.dt[method=='all'], aes(x=age, y=rate_ratio, color=year, group=year)) + 
	geom_hline(aes(yintercept=1), color='red', linetype=2) +
	geom_line() +
	# facet_wrap('age', scales='free_y') +
	ggtitle('Age distribution of ratio of suicide mortality rate in men to suicide mortality rate in women')
	# ylim(1, 16)
print(gg)

dev.off()
