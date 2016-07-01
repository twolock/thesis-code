library(data.table)
library(raster)

setwd('/homes/twolock/thesis')

source('code/plot_data.R')

mcnty.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/mcnty_mapping_shape_file.shp")
state.shape <- shapefile("/home/j/Project/us_counties/locations/counties/prepped_shp/state_mapping_shape_file.shp")

mort.data <- fread('data/compiled_mortality.csv')

series_map(chloropleth_map = copy(mcnty.shape),
           outline_map = state.shape,
           data = copy(mort.data),
           geog_id = 'mcnty',
           variable = 'other_rate',
           series_dimension = 'year',
           series_sequence = 1980:2013,
           destination_folder = '/homes/twolock/thesis/',
           histogram=FALSE,
           color_ramp=woodson_pallettes("black_to_light_10"),
           map_title = 'Firearm suicide rate in the US'
           )
