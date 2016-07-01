library(data.table)
library(ggplot2)

all.results <- list()
all.dt <- data.table()

plot.pair <- sort(c(6, 8))

tmp.i <- 1

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

pdf(paste0('/homes/twolock/thesis/results/model_comparisons/models_',paste(plot.pair, collapse='_'),'.pdf'), width=11, height=8.5)
for (tmp.m in c('Mean', 'SE')) {
  tmp.gg <- ggplot(data=plot.dt[variable==tmp.m], aes(x=Model_1, y=Model_2)) + 
    geom_abline(slope=1, color='black') + 
    geom_point(aes(color=effect.type), shape=1, size=2) +
    theme(aspect.ratio = 1) +
    scale_shape(solid = FALSE) +
    ggtitle(tmp.m)

  print(tmp.gg)
}
dev.off()
