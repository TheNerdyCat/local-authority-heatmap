##################################################
## Project:           LA Heatmap
## Script purpose:    Produce a frequency heatmap by local authority  
## Date:              2019-05-13
## Author:            Edward Sims
##################################################


# Import packages ---------------------------------------------------------


# Data import pckgs
library(data.table)

# Data manipulation pckgs
library(dplyr)
library(plyr)

# Geospatial pckgs
library(maps) 
library(raster) 
library(mapdata)
library(maptools)
library(rgdal)
library(rgeos)
library(broom)

# Data vis pckgs
library(ggmap)
library(ggplot2)
library(ggthemes)


setwd('C:/Users/Edward Sims/Documents/merchant_reports/automated_dashboard') # Set working directory



# Frequency Heatmap -------------------------------------------------------

# IDEAS!
#  - Make a choice between regional or LA map before executing. Have a function for both. 
#  - Error messages & debugging?
#  - Loading bar?


create_la_freq_heatmap <- function() {
  
  load_geo_data <- function() {
    
    # File paths
    pcd_lookup_path <- '../../geo_lookups/nspl.csv'
    la_lookup_path <- '../../geo_lookups/la_name_lookup.csv'
    reg_lookup_path <- '../../geo_lookups/region_name_lookup.csv'
    
    message("Reading postcode lookup data...")
    
    # Postcode lookup data
    pcd_lookup <- fread(pcd_lookup_path, data.table = FALSE, showProgress = FALSE) # Import pcd lookup data
    pcd_cols_keep <- c('pcds', 'laua', 'rgn')                        # Columns to subset pcd_lookup by
    pcd_lookup_subset <- pcd_lookup[ , pcd_cols_keep]        # Create subset
    colnames(pcd_lookup_subset)[1] <- 'pcd'
    
    message("Reading LA lookup data...")
    
    # LA lookup data
    la_lookup <- fread(la_lookup_path, data.table = FALSE, showProgress = FALSE)   # Import la lookup for names
    la_lookup <- la_lookup[ , -3]                            # Remove useless cols
    colnames(la_lookup) <- c('laua', 'laua_name')            # Change colnames to match merge data
    
    message("Reading Region lookup data...")
    
    # Region lookup data
    reg_lookup <- fread(reg_lookup_path, data.table = FALSE, showProgress = FALSE) # Import la lookup for names
    reg_lookup <- reg_lookup[ , -c(3,4)]                          # Remove useless cols
    colnames(reg_lookup) <- c('rgn', 'reg_name')           # Change colnames to match merge data
    
    
    # Create geography lookup
    message("Merging with LA lookup...")
    geo_lookup <- merge(pcd_lookup_subset, la_lookup, by = 'laua', all.x=TRUE) # Merge pcd lookup with la_name lookup
    
    message("Merging with Region lookup...")
    geo_lookup <- merge(geo_lookup, reg_lookup, by = 'rgn', all.x=TRUE) # Merge pcd lookup with la_name lookup

    cat("Number of rows in geo_lookup: ", nrow(geo_lookup), "\n")
    return(geo_lookup)
    
  }
  create_map_shapefiles <- function() {
    
    # File paths
    geofile_path <- 'map_shapefiles/Local_Authority_Districts_December_2018_Full_Clipped_Boundaries_UK.shp'
    reg_geofile_path <- 'map_shapefiles/Regions_December_2017_Full_Clipped_Boundaries_in_England'
    country_geofile_path <- 'map_shapefiles/Countries_December_2017_Full_Clipped_Boundaries_in_UK.shp'
    
    # Map shapefile
    message("Loading LA map shapefile...")
    shapefile <- shapefile(geofile_path)                     # Load map shapefile
    message("Tidying LA map shapefile...")
    mapdata <- tidy(shapefile, region = "lad18nm")           # Reshape for ggplot2 using Broom package
    colnames(mapdata)[7] <- 'laua_name'
    
    # Region map shapefile
    message("Loading Region map shapefile...")
    reg_shapefile <- shapefile(reg_geofile_path) # Load region map shapefile
    message("Tidying Region map shapefile...")
    reg_mapdata <- tidy(reg_shapefile, region = "rgn17nm") # Reshape for ggplot2
    
    # Country map shapefile
    message("Loading Country map shapefile...")
    country_shapefile <- shapefile(country_geofile_path) # Load country map shapefile
    message("Tidying Country map shapefile...")
    country_mapdata <- tidy(country_shapefile, region = "ctry17nm") # Reshape for ggplot2
    
    map_data_list <- list('mapdata' = mapdata,
                          'reg_mapdata' = reg_mapdata,
                          'country_mapdata' = country_mapdata)
    return(map_data_list)
    
  } 
  load_merchant_data <- function() {
    
    
    ## EDIT FILE PATH HERE ##
    
    ev_outcome_path <- '../evaluations_data/evaluationsoutcome 19-04.csv' 
    
    
    message("Loading data...")
    ev_outcome_df <- fread(ev_outcome_path, 
                           header = TRUE, 
                           stringsAsFactors = TRUE, 
                           data.table = FALSE) # Import evaluations outcome
    
    
    ## EDIT MERCHANT NAME HERE ##
    merchant_name <- 'VIRGIN STRAUSS WATER UK LIMITED T/A VIRGIN PURE' # Choose merchant here
    ev_outcome_cols_keep <- c('ApplicationID', 'Postcode') # Keep only credit amount and pcd cols
    
    
    merchant_df <- ev_outcome_df[ev_outcome_df$MerchantName == merchant_name , ev_outcome_cols_keep] # Subset df
    colnames(merchant_df)[2] <- c('pcd')
    message("Merging data with geo_lookup...")
    merchant_pcd_merge <- merge(merchant_df, geo_lookup, all.x=TRUE) # Merge the evaluations merchant data with pcd lookup to get 
    merchant_pcd_merge <- merchant_pcd_merge[ , -c(1,3)] # Remove pcd column
    merchant_df_freq <- setDT(merchant_pcd_merge)[, .(freq = .N), by = .(laua_name)] # Create frequencies by la_name (data.table func)
    print(head(merchant_df_freq[order(merchant_df_freq$freq, decreasing=TRUE), ]))
    cat("Max frequency: ", max(merchant_df_freq$freq), "\n")
    
    
    ## EDIT BINS HERE ## 
    freq_breaks <- c(1,1.999,2,2.999,3,3.999,4,4.999,5,Inf) # Define breaks for freq bins
    merchant_df_freq$freq_bins <- cut(merchant_df_freq$freq, breaks = freq_breaks,right = FALSE)
    
    return(merchant_df_freq)
  }  ### REQUIRES MANUAL EDITING ###
  create_final_map_df <- function() {
    message("Merging map_data and merchant data...")
    map_df_complete <- merge(x=map_data_list$mapdata, y=merchant_df_freq, all.x=TRUE) # Merge map and merchant df
    map_df_complete <- map_df_complete[order(map_df_complete$order),] # Sort df by order column (important for map plots)
    
    return(map_df_complete)
  }
  plot_heatmap <- function() {
    gg <- ggplot() 
    gg <- gg + geom_polygon(data = map_df_complete, aes(x = long, y = lat, group = group, fill = freq_bins), color = "#000000", size = 0.1)
    gg <- gg + coord_fixed(1)
    gg <- gg + theme_minimal()
    
    
    ## EDIT BREAKS AND LABELS HERE ##
    gg <- gg + scale_fill_manual(breaks = c("[1,1.999)","[2,2.999)","[3,3.999)","[4,4.999)","[5,Inf)"),
                                 values = c('#FFE7D5','#FFCEA8','#FFB071','#FF913B','#FF7000'),
                                 labels = c('1','2','3','4','5+'))
    
    ## EDIT LEGEND TITLE HERE ## 
    gg <- gg + labs(fill = 'No. Applications \nto Date by Local Authority')
    
    
    gg <- gg + theme(panel.grid.major = element_blank())
    gg <- gg + theme(panel.grid.minor = element_blank())
    gg <- gg + theme(axis.title.x = element_blank())
    gg <- gg + theme(axis.text.x = element_blank())
    gg <- gg + theme(axis.ticks.x = element_blank())
    gg <- gg + theme(axis.title.y = element_blank()) 
    gg <- gg + theme(axis.text.y = element_blank())
    gg <- gg + theme(axis.ticks.y = element_blank())
    # Layer in Region and Country polygons
    gg <- gg + geom_polygon(aes(x = long, y = lat, group = group),
                            data = map_data_list$reg_mapdata, color = "black",size = 0.25, fill=NA)
    gg <- gg + geom_polygon(aes(x = long, y = lat, group = group),
                            data = map_data_list$country_mapdata, color = "black",size = 0.3, fill=NA)
    return(gg)
  }        ### REQUIRES MANUAL EDITING ###
  
  
  # Execute functions
  geo_lookup <- load_geo_data()
  map_data_list <- create_map_shapefiles()
  merchant_df_freq <- load_merchant_data()
  map_df_complete <- create_final_map_df()
  message("Plotting map...")
  
  return(plot_heatmap())
  
}

# Save map output
png('virgin_la_frequency_map.png', units = "in", width = 5, height = 6, res = 500)
create_la_freq_heatmap()
dev.off()


## NEED TO EDIT TO INCLUDE WALES, N.IRELAND AND SCOTLAND AS REGIONS
create_reg_freq_heatmap <- function() {
  
  load_geo_data <- function() {
    
    # File paths
    pcd_lookup_path <- '../../geo_lookups/nspl.csv'
    reg_lookup_path <- '../../geo_lookups/region_name_lookup.csv'
    
    message("Reading postcode lookup data...")
    
    # Postcode lookup data
    pcd_lookup <- fread(pcd_lookup_path, data.table = FALSE, showProgress = FALSE) # Import pcd lookup data
    pcd_cols_keep <- c('pcds', 'rgn')                        # Columns to subset pcd_lookup by
    pcd_lookup_subset <- pcd_lookup[ , pcd_cols_keep]        # Create subset
    colnames(pcd_lookup_subset)[1] <- 'pcd'
    
    message("Reading Region lookup data...")
    
    # Region lookup data
    reg_lookup <- fread(reg_lookup_path, data.table = FALSE, showProgress = FALSE) # Import la lookup for names
    reg_lookup <- reg_lookup[ , -c(3,4)]                          # Remove useless cols
    colnames(reg_lookup) <- c('rgn', 'reg_name')           # Change colnames to match merge data
    
    missing_regions <- data.frame(rgn = c("M99999999","L99999999","N99999999","W99999999","S99999999"),
                                  reg_name = c("Isle of Man","Channel Islands","Northern Ireland","Wales","Scotland")
                                  )
    region_lookup_all <- merge(reg_lookup, missing_regions, all = TRUE) # Add missing regions to reg lookup
    
    message("Merging with Region lookup...")
    geo_lookup <- merge(pcd_lookup_subset, reg_lookup, by = 'rgn', all.x = TRUE) # Merge pcd lookup with la_name lookup

    cat("Number of rows in geo_lookup: ", nrow(geo_lookup), "\n")
    return(geo_lookup)
    
  }
  create_map_shapefiles <- function() {
    
    # File paths
    reg_geofile_path <- 'map_shapefiles/Regions_December_2017_Full_Clipped_Boundaries_in_England'
    country_geofile_path <- 'map_shapefiles/Countries_December_2017_Full_Clipped_Boundaries_in_UK.shp'
    
    # Region map shapefile
    message("Loading Region map shapefile...")
    reg_shapefile <- shapefile(reg_geofile_path) # Load region map shapefile
    message("Tidying Region map shapefile...")
    reg_mapdata <- tidy(reg_shapefile, region = "rgn17nm") # Reshape for ggplot2
    colnames(reg_mapdata)[7] <- 'reg_name'
    
    # Country map shapefile
    message("Loading Country map shapefile...")
    country_shapefile <- shapefile(country_geofile_path) # Load country map shapefile
    message("Tidying Country map shapefile...")
    country_mapdata <- tidy(country_shapefile, region = "ctry17nm") # Reshape for ggplot2
    
    map_data_list <- list('reg_mapdata' = reg_mapdata,
                          'country_mapdata' = country_mapdata)
    return(map_data_list)
    
  } 
  load_merchant_data <- function() {
    
    
    ## EDIT FILE PATH HERE ##
    
    ev_outcome_path <- '../evaluations_data/evaluationsoutcome 19-04.csv' 
    
    
    message("Loading data...")
    ev_outcome_df <- fread(ev_outcome_path, 
                           header = TRUE, 
                           stringsAsFactors = TRUE, 
                           data.table = FALSE) # Import evaluations outcome
    
    
    ## EDIT MERCHANT NAME HERE ##
    merchant_name <- 'VIRGIN STRAUSS WATER UK LIMITED T/A VIRGIN PURE'
    ## EDIT FREQUENCY/CREDITAMOUNT HERE ## 
    ev_outcome_cols_keep <- c('ApplicationID', 'Postcode') 
    
    
    merchant_df <- ev_outcome_df[ev_outcome_df$MerchantName == merchant_name , ev_outcome_cols_keep] # Subset df
    colnames(merchant_df)[2] <- c('pcd')
    message("Merging data with geo_lookup...")
    merchant_pcd_merge <- merge(merchant_df, geo_lookup, all.x=TRUE) # Merge the evaluations merchant data with pcd lookup to get 
    merchant_pcd_merge <- merchant_pcd_merge[ , -c(1,3)] # Remove pcd column
    merchant_df_freq <- setDT(merchant_pcd_merge)[, .(freq = .N), by = .(reg_name)] # Create frequencies by la_name (data.table func)
    print(merchant_df_freq[order(merchant_df_freq$freq, decreasing=TRUE), ])
    cat("Max frequency: ", max(merchant_df_freq$freq), "\n")
    
    
    ## EDIT BINS HERE ## 
    freq_breaks <- c(1,9.999,10,19.999,20,29.999,30,39.999,40,Inf) # Define breaks for freq bins
    merchant_df_freq$freq_bins <- cut(merchant_df_freq$freq, breaks = freq_breaks,right = FALSE)
    
    return(merchant_df_freq)
  }  ### REQUIRES MANUAL EDITING ###
  create_final_map_df <- function() {
    message("Merging map_data and merchant data...")
    map_df_complete <- merge(x=map_data_list$reg_mapdata, y=merchant_df_freq, all.x=TRUE) # Merge map and merchant df
    map_df_complete <- map_df_complete[order(map_df_complete$order),] # Sort df by order column (important for map plots)
    
    return(map_df_complete)
  }
  plot_heatmap <- function() {
    gg <- ggplot() 
    gg <- gg + geom_polygon(data = map_df_complete, aes(x = long, y = lat, group = group, fill = freq_bins), color = "#000000", size = 0.1)
    gg <- gg + coord_fixed(1)
    gg <- gg + theme_minimal()
    
    
    ## EDIT BREAKS AND LABELS HERE ##
    gg <- gg + scale_fill_manual(breaks = c("[1,9.999)","[10,19.999)","[20,29.999)","[30,39.999)","[40,Inf)"),
                                 values = c('#FFE7D5','#FFCEA8','#FFB071','#FF913B','#FF7000'),
                                 labels = c('1 to 9','10 to 19','20 to 29','30 to 39','40+'))
    
    ## EDIT LEGEND TITLE HERE ## 
    gg <- gg + labs(fill = 'No. Applications \nto Date by Region')
    
    
    gg <- gg + theme(panel.grid.major = element_blank())
    gg <- gg + theme(panel.grid.minor = element_blank())
    gg <- gg + theme(axis.title.x = element_blank())
    gg <- gg + theme(axis.text.x = element_blank())
    gg <- gg + theme(axis.ticks.x = element_blank())
    gg <- gg + theme(axis.title.y = element_blank()) 
    gg <- gg + theme(axis.text.y = element_blank())
    gg <- gg + theme(axis.ticks.y = element_blank())
    # Layer in Country polygons
    gg <- gg + geom_polygon(aes(x = long, y = lat, group = group),
                            data = map_data_list$country_mapdata, color = "black",size = 0.3, fill=NA)
    return(gg)
  }        ### REQUIRES MANUAL EDITING ###
  
  
  # Execute functions
  geo_lookup <- load_geo_data()
  map_data_list <- create_map_shapefiles()
  merchant_df_freq <- load_merchant_data()
  map_df_complete <- create_final_map_df()
  message("Plotting map...")
  
  return(plot_heatmap())
  
} 

# Save map output
png('virgin_reg_frequency_map.png', units = "in", width = 5, height = 6, res = 500)
create_reg_freq_heatmap()
dev.off()



