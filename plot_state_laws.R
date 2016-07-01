library(data.table)
library(raster)
library(ggplot2)

setwd("/homes/twolock/thesis")

law.dt <- fread('data/statute_history.csv')

source('code/plot_data.R')

#mcnty.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/mcnty_mapping_shape_file.shp")
state.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/state_mapping_shape_file.shp")


years <- 1980:2013
state.dt <- merge(CJ(state=state.shape$state, year=years), law.dt[,.(state, bill_passed, bill_repealed)], by='state', all.y=TRUE)
state.dt[, years_since_policy := 0]
state.dt[!is.na(bill_passed), years_since_policy := pmin(15,pmax(0, year-bill_passed))]
state.dt[year > bill_repealed & !is.na(bill_repealed), years_since_policy := 0]

law.dt[, start.year := pmax(1980, bill_passed)]
law.dt[, end.year := pmin(2013, bill_repealed)]
law.dt[!is.na(bill_passed) & is.na(bill_repealed), end.year := 2013]

pdf('/homes/twolock/thesis/test_line_plot.pdf', width=5, height=11)
test.gg <- ggplot(data=law.dt, aes(x=state_name, ymin=start.year, ymax=end.year)) +
  geom_linerange() + coord_flip()
print(test.gg)
dev.off()

# series_map(chloropleth_map = copy(state.shape),
#            outline_map = state.shape,
#            data = copy(state.dt),
#            geog_id = 'state',
#            variable = 'years_since_policy',
#            series_dimension = 'year',
#            series_sequence = 1980:2013,
#            destination_folder = '/homes/twolock/thesis/',
#            histogram=TRUE,
#            color_ramp=woodson_pallettes("black_to_light_10"),
#            map_title = 'Years since handgun permit law was passed'
#            )
