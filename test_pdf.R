library(ggplot2)


setwd('/homes/twolock/thesis/')

x <- runif(500, 0, 100)
y <- 1.3 * x + 10 + 0.05 * x * rnorm(500, 0, 5)

pdf('test_pdf.pdf', width=14, height=8)
my.gg <- ggplot() +
	geom_point(aes(x=x,y=y),color='maroon') +
	ggtitle('hefjdksl')
print(my.gg)
dev.off()

