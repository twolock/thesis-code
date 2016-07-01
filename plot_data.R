##############################
# This function plots maps and distribution plots to a PDF.
# Author: Rebecca Stubbs
# Written 9/10/2015
##############################

library(data.table)
library(ggplot2)
library(grid)
library(ggthemes)
library(maptools)

source('/homes/twolock/thesis/code/woodson_pallettes.R')

# For documentation, see: https://rpubs.com/BeccaStubbs/woodson_examples

##############################################################################
# SERIES MAP FUNCTION
##############################################################################

series_map<-function(chloropleth_map,
                     outline_map=NULL,
                     data=NULL,
                     geog_id,
                     variable,
                     map_title=" ",
                     additional_variable_name_string=NULL,
                     series_dimension=NULL,
                     series_sequence=NULL,
                     destination_folder=NULL,
                     color_ramp=woodson_pallettes("easter_to_earth"),
                     histogram=TRUE,
                     override_scale=NULL,
                     outline_size=.1,
                     outline_color="white",
                     return_map_object_only=FALSE,
                     legend_position="bottom",
                     title_font_size=NULL,
                     title_font_face="plain",
                     legend_font_size=NULL,
                     legend_font_face="plain",
                     legend_bar_width=.4,
                     color_value_breaks=NULL){      
  
  ##############################################################################################################################
  # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Information about this Function~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  #
  # This is an all-purpose mapping function that plots a chloropleth map in R. This function is designed to make generating
  # simple, beautiful maps in R a quick and easy process. The aesthetic of these maps are designed to be simple, easy-to-read, 
  # and have minimal visual distractions. 
  # 
  # You can use this function to create a single map of 1 variable, or you can use this function to create a series of maps
  # that loops through dimensions of a variable (say, years, or types of vaccine coverage). 
  #
  # MANDATORY INPUTS:
  # * chloropleth_map
  #      A SpatialPolygons object with data as a data.table rather than a data.frame.
  # * geog_id
  #     A string-- the name of the column that serves as the geographic ID that specifies the unit of analyisis for your data
  # * variable
  #     A string-- the name of the column that will serve as the values you want to plot by your geog_id
  #      
  # 
  # 
  # OPTIONAL INPUTS:
  # If the variable you want to map is already within the @data object of the SpatialPolygons object, and you only want to plot
  # one dimension/version of that variable, you're all set to go. However, if the data you would like to plot is in a separate 
  # table, you can use this function to merge on your data to your map object, and then plot it (assuming your geog_id's match,
  # and are the same data type). You can also specify a variety of things to tinker with your map's aesthetics.
  # 
  # * data 
  #      A data.table that contains the data you want to map (must contain geog_id, and the variable of interest, if specified.
  #      If a series dimension and/or series sequence is defined, those must also exist in this data set)
  # * outline_map
  #      Another SpatialPolygons object that you want to use the outlines from.
  # * histogram
  #     TRUE/FALSE. If "TRUE", the plot will contain a histogram of the values at the bottom.
  # * series_dimension
  #     A string-- the name of the column that will serve as the variable you loop through to create a series map. For example, 
  #     year. 
  # * series_sequence
  #     A vector c(x,y,z...) that specifies a subset of the series dimensions you want to map. For example, if you have a
  #     data set that contains all years between 1980-2014, you can specify that you only want to plot out every other year
  #     by setting series sequence to be seq(1980,2014,2). This function will make sure all of the items you speficy actually
  #     exist within your series_dimension. 
  # * color_ramp
  #     A list of colors that will serve as the colors you "stretch" through based on your data values. This will default to a 
  #     color scheme described in wodson_pallettes called "Easter to Earth" that displays variation well when there are many
  #     geographic units. The fewer geographic units, the simpler you probably want your color ramp to be. See woodson_palletes
  #     for more options, or create your own.
  # * map_title
  #     A string that serves as the basis for your map title (if no dimensions are specified, the title will be as it is 
  #     specified. If a dimension is specified, a phrase constructed using the series dimension and what you are mapping will
  #     be added to the plot title [ex="year:1990"].
  # * additional_variable_name_string
  #     This is an additonal string that you want to add to the name of the PDF to describe any other breakdowns you might be 
  #     using. For example, if you had to map something by year, age, sex, you would first need to subset your data to be one
  #     age/sex group before plotting it out by year. If you subset your data in a loop, you could use this string to specify
  #     something along the lines of paste0("age_",a,"_sex_",s). NOTE: You need to put in a similar paste0 statement in your 
  #     map title if you also want this sub-breakdown described in the title of your map, not just the file path to the PDF.
  # * destination_folder
  #     A string file path to a folder you want a PDF created in that will hold your map(s). The map will be called the variable
  #     name, plus any additional_variable_name_string you specify.
  # * override_scale
  #     A vector with two items that will be used to stretch the color ramp instead of the min/max values present in the data set.
  #     should be structured "c(min,max)".
  # * outline_size
  #     A numeric value that specifies how large you want your white outlines to be if you have specified an outline you want 
  #     shown on your map. Default value is .1. 
  # 
  # 
  # If you have a wishlist for functionalities, and/or would like to contribute to this effort, feel free to contact the author,
  # Rebecca Stubbs, at stubbsrw@uw.edu.
  ##############################################################################################################################
  
  # Copying objects such that the original names of the variables are unaltered
  chloropleth_map<-copy(chloropleth_map)
  outline_map<-copy(outline_map)
  data<-copy(data)
  
  # Defensive Checks on Input Data:
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check that the specified geog_id exists, check for the variable's existence if no external data is provided
  try(if((geog_id %in% names(chloropleth_map@data)==F)&("geog_id" %in% names(chloropleth_map@data)==F)) stop("That geographic ID does not appear to exist within your chloropleth map object.")) # Check to see if the "geog_id" field is in your raw data
  if (length(data)==0){try(if((variable %in% names(chloropleth_map@data)==F)) stop("That variable does not appear to exist within your data set [the spatial polygons data frame you say contains the data, too]."))}# Check to see if the variable is in the external data provided:
  
  # If external data is specified, check to see if geog_id and the varaible is within the data
  if (length(data)>0){try(if((geog_id %in% names(data)==F)) stop("That geographic ID does not appear to exist within your data set."))} 
  if (length(data)>0){try(if((variable %in% names(data)==F)) stop("That variable does not appear to exist within your data set."))}# Check to see if the variable is in the external data provided:
  
  # Preparing Data For Mapping
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Setting the geographic ID variables as "geog_id" in both the data and geographic rdata objects
  setnames(chloropleth_map@data,geog_id,"geog_id")
  
  # Defining the data you want to map: If it's in addition to the chloropleth map object, change the geog_id column name to "geog_id" to facilitate the merge
  if (length(data)>0){ # If external data was provided...
    setnames(data,geog_id,"geog_id") #If an external data source is provided, set that name to geog_id as well
  }else{data<-copy(chloropleth_map@data)} # if no external data was provided, "data" is now just the data that was in the map object in the first place.
  
  # Renaming the variable to be "variable" 
  setnames(data, variable, "variable") # Renaming the variable of interest to "variable" within the dataset
  
  # If there is a series dimension specified, check to make sure it is in the data set and, set that name to be "series_dimension". 
  if (length(series_dimension)>0){ # If you plan to loop through miltiple dimensions...
    try(if((series_dimension %in% names(data)==F)) stop("That series dimension (what you want to iterate through) does not appear to exist within your data set.")) # Check to make sure it exists in your data
    setnames(data,series_dimension,"series_dimension")}else{data[,series_dimension:="*&^! no dimensions"]} # If a series dimension is specified, rename it "series_dimension". If there is no series dimension, add a column and call it "no dimensions"; we'll only loop through once to plot whatever variable you have with no series.
  
  # Sub-setting the data such that only the variables that matter are kept
  data<-data[, list(geog_id=as.character(geog_id), variable, series_dimension)] # Sub-setting your data c keep only the relevant columns
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fortifying the Map and Joining on the Data
  
  # "fortifying" the Rdata shapefiles
  chloropleth_map <-data.table(suppressWarnings(fortify(chloropleth_map, region="geog_id")))
  if (length(outline_map)>0){outline_map<-data.table(suppressWarnings(fortify(outline_map)))} # If an outline map is specified, fortify the outline map as well.
  
  # Renaming the chloropleth map "ID" field as geog_id
  setnames(chloropleth_map,"id","geog_id")
  
  # creating one long, huge object that you can subset by merging together the data and the map, if the data isn't already in the map.
  chloropleth_map<-merge(data, chloropleth_map, by="geog_id", allow.cartesian=T)
  
  # If no series sequence is defined, but a series dimension is defined, loop through every option or layer of the dimensions
  if (length(series_sequence)==0){map_dims<-unique(chloropleth_map$series_dimension)}
  # If a series sequence is defined (the function was passed a vector of particular values within the series dimension): 
  if (length(series_sequence)>0){
    print("Note: The color ramp will stil be set based on ALL dimensions of your variable, unless you override it otherwise.") # Printing a warning 
    # If you have specified a series you want to loop through (for example, only SOME years), 
    # we will loop through that instead of every unique option. First, though, we check to make sure each of those options actually exists within the column.
    for (select_dimension in series_sequence){ 
      try(if((select_dimension %in% unique(chloropleth_map$series_dimension)==F)) stop(paste0("The dimension ",select_dimension," does not appear to exist in the column you have specified to hold your dimensions.")))}
    # If the above checks all pass...:                          
    map_dims<-series_sequence} #Set the "dimensions" as the series_sequence specified; the map will iterate through that sequence.
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting the range of values the colors will scale between
  ## The default:
  #Discovering what the min and max are of the variable across the whole data series (all values of that variable)
  maximum<-max(na.omit(chloropleth_map[["variable"]])); minimum<-min(na.omit(chloropleth_map[["variable"]]))
  
  ##  Overriding the min/max scale with input values, if desired:
  # If an override was provided, setting minimum to the first in the list, and maximum to the second in the list provided.
  if (length(override_scale)>0){minimum<-override_scale[1];maximum<-override_scale[2]}
  
  
  ###########################################
  ## LOOPING ACROSS DIMENSIONS
  ########################################### 
  
  # If an output folder is specified, it means that you have decided you want your maps to be written to a PDF. 
  # this line starts a PDF, since we want the PDF to contain each of the maps we make in series , so we need to open it before we start looping through our variable values. 
  if (length(destination_folder)>0){pdf(paste0(destination_folder,variable,additional_variable_name_string,".pdf"))} #If you want it written to a pdf, (because you specified a destination folder) open it!
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Starting the Loop
  for (select_dimension in map_dims){ #for each dimension you want to plot...
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Determining map title, and subsetting the data
    
    if (select_dimension=="*&^! no dimensions"){main_map_title<-map_title}else{main_map_title<-paste0(map_title,": ",select_dimension)} # Determining the map title
    print(main_map_title) # print out on screen what variable and dimension the script is on!
    subset<-chloropleth_map[series_dimension==select_dimension]  # Sub-setting the fortified object to map out 1 layer/dimension (ex: year) of the variable of interest  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Creating the Map Plot in GGPlot2
    
    map_plot<-ggplot(subset)  + #starting the ggplot object based on the subset, fortified data  
      geom_polygon(aes(x=long, y=lat, group=group, fill=variable), color=NA, size=0.0) +  # Telling the ggplot that you want to be "filling" the polygons with the values from variables
      scale_fill_gradientn(colours=rev(color_ramp), limits=c(minimum, maximum), values=color_value_breaks) + #Defining the colors (based on the mapcolors from cubehelix above) for each value
      #more map formatting: Keeping the image clean by avoiding unecessary scales, keeping the background white, etc.
      scale_x_continuous("", breaks=NULL) + # Getting rid of the x-scale
      scale_y_continuous("", breaks=NULL) + # Getting rid of the y-scale
      coord_fixed(ratio=1) # Making sure that 1 unit on the x axis is the same as 1 unit on the y axis
      
      # Adding a legend and titles
      if (legend_position %in% c("bottom","top")){
        map_plot<-map_plot+guides(fill=guide_colourbar(title=" ", barheight=legend_bar_width, barwidth=20, label=TRUE, ticks=FALSE)) + # Legend for the colors
          labs(title = main_map_title) + # Setting the title as the map title specified in the function
          theme_minimal() + # Defining a "tufte" theme for the map (this eliminates unecessary things like background grey matrices, etc)
          theme(legend.position=legend_position) # Specifying that you want the legend at the bottom of the map. 
      }
    
    # Adding a legend and titles
    if (legend_position %in% c("right","left")){
      map_plot<-map_plot+guides(fill=guide_colourbar(title=" ", barheight=20, barwidth=legend_bar_width, label=TRUE, ticks=FALSE)) + # Legend for the colors
        labs(title = main_map_title) + # Setting the title as the map title specified in the function
        theme_minimal() + # Defining a "tufte" theme for the map (this eliminates unecessary things like background grey matrices, etc)
        theme(legend.position=legend_position) # Specifying that you want the legend at the bottom of the map. 
    }
    
    map_plot<-map_plot + theme(plot.title = element_text(size = title_font_size,  face=title_font_face))+
      theme(legend.text = element_text(size = legend_font_size, face=legend_font_face))
    
    
    if (length(outline_map)>0){    ## If there was an outline specified, add outline geometry in white
      if (length(outline_color)>0){outline_map_color<-outline_color}
      
      map_plot<-map_plot+geom_path(data = outline_map, 
                                   aes(x = long, y = lat, group = group),
                                   color = outline_map_color, size = outline_size)} 
    
    ## If you just want the map plot as an object you can pass to other things...
    if (return_map_object_only==TRUE){return(map_plot)}else{
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Making a histogram of the distribution of that year's values
      if (histogram==TRUE){ # If you have specified that you do want the histogram at the bottom:
        histo<-ggplot(subset, aes(x=variable)) + geom_histogram(aes(fill = ..count..), bins=30, na.rm = TRUE)+xlim(minimum, maximum)+
          scale_fill_gradient("Count", low =  color_ramp[3], high = color_ramp[length(color_ramp)-2])+theme_minimal()+
          guides(fill=guide_colourbar(title=" ", barheight=0, barwidth=0, label=FALSE, ticks=FALSE))+
          labs(title="Distribution",x= "",y="")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank())}
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Printing the Plot:
      if (histogram==TRUE){# Combining Histogram and Map to plot into a single image.
        grid.newpage() # Starting a new page
        pushViewport(viewport(layout = grid.layout(5, 1))) # Defining the ratio of the histogram to map to be 5 sections vertically, 1 horizontally
        vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) # Defining a function that allows setting the layout of the viewport 
        print(map_plot, vp = vplayout(1:4, 1)) # Printing the map plot to the viewport in vertical slots 1-4, covering all the x-space
        print(histo, vp = vplayout(5, 1)) # Printing the histogram to the bottom of the map: 
      }else{print(map_plot)} #If you didn't want the histogram, just print out the map!
      
    } # Closing the "if return map object=TRUE" clause
  } # Closing the loop of dimensions
  
  if (length(destination_folder)>0){dev.off();print("PDF ready to view.")} #If you were writing this to a PDF, you can close it, and check it out!
  
  
} # Closing Function!
