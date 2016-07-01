
library(data.table)

setwd("/homes/twolock/thesis")

model.dt <- fread('data/models.csv')


model.i.list <- model.dt[model_name != 'Spline', unique(model_i)]
# model.i.list <- c(17, 18)
model.i.list <- 1:16
rscript.path <- '/homes/twolock/thesis/code/Rscript_shell.sh'
code_dir <- '/homes/twolock/thesis/code/'

for (i in model.i.list) {
	launch.string <- paste0("qsub -pe multi_slot 64 -N firearm_",i,' ',rscript.path, ' ', code_dir,"run_model.R ", i)
	print(launch.string)
	system(launch.string)
}
