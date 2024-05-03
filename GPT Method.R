


# Load required libraries
library(XML)
library(tidyverse)

# Parse GPX file
gpx_parsed <- htmlTreeParse(file = "gecho.gpx", useInternalNodes = TRUE)

# Extract track points
trackpoints <- xpathApply(gpx_parsed, "//trkpt", xmlAttrs)

# Create data frame
trackpoints_df <- map_df(trackpoints, ~{
  lat <- as.numeric(.x["lat"])
  lon <- as.numeric(.x["lon"])
  time <- as.POSIXct(xpathSApply(.x, "./time", xmlValue), tz = "UTC")
  depth <- as.numeric(xpathSApply(.x, "./extensions/gpxtpx:trackpointextension/gpxtpx:depth", xmlValue))
  tibble(lat = lat, lon = lon, time = time, depth = depth)
})

# View the resulting data frame
print(trackpoints_df)





# Attempt 2 ---------------------------------------------------------------



# Load required libraries
library(XML)
library(tidyverse)

# Parse GPX file
gpx_parsed <- xmlParse("gecho.gpx")

# Extract track points
trackpoints <- getNodeSet(gpx_parsed, "//trkpt")

# Extract data from track points
trackpoints_df <- map_df(trackpoints, function(tp) {
  lat <- as.numeric(xmlGetAttr(tp, "lat"))
  lon <- as.numeric(xmlGetAttr(tp, "lon"))
  time <- as.POSIXct(xmlValue(xmlChildren(tp)[[1]]), tz = "UTC")
  depth <- as.numeric(xmlValue(xpathApply(tp, ".//gpxtpx:depth", xmlValue)))
  tibble(lat = lat, lon = lon, time = time, depth = depth)
})

# View the resulting data frame
print(trackpoints_df)






# Attempt 3 -----------------------------------------------------------------------



# Load required libraries
library(XML)
library(tidyverse)

# Parse GPX file
gpx_parsed <- xmlParse("gecho.gpx")

# Extract track points
trackpoints <- xpathApply(gpx_parsed, "//trkpt", xmlAttrs)

# Create data frame
trackpoints_df <- map_df(trackpoints, ~{
  lat <- as.numeric(.x["lat"])
  lon <- as.numeric(.x["lon"])
  time <- as.POSIXct(xpathSApply(.x, "./time", xmlValue), tz = "UTC")
  depth <- as.numeric(xpathSApply(.x, "./extensions/gpxtpx:trackpointextension/gpxtpx:depth", xmlValue))
  tibble(lat = lat, lon = lon, time = time, depth = depth)
})

# View the resulting data frame
print(trackpoints_df)



# Attempt 4 handling namespaces properly  ----------------------------------


# Load required libraries
library(XML)
library(tidyverse)

# Parse GPX file
gpx_parsed <- xmlParse("Export5323.gpx")

# Define namespaces
ns <- c(gpx = "http://www.topografix.com/GPX/1/1",
        gpxtpx = "http://www.garmin.com/xmlschemas/TrackPointExtension/v1")

# Extract track points
trackpoints <- getNodeSet(gpx_parsed, "//gpx:trkpt", namespaces = ns)

#(iterative GPT revisions of "create data frame" step)
# Create data frame
trackpoints_df <- map_df(trackpoints, ~{
  lat <- as.numeric(xmlGetAttr(.x, "lat"))
  lon <- as.numeric(xmlGetAttr(.x, "lon"))
  time <- as.POSIXct(xmlValue(xmlChildren(.x)[[1]]), tz = "UTC")
  depth <- as.numeric(xpathApply(.x, ".//gpxtpx:depth", xmlValue, namespaces = ns))
  tibble(lat = lat, lon = lon, time = time, depth = depth)
})

# View the resulting data frame
view(trackpoints_df)



# Attempt 4.2 -  Including track segments (ABANDONED) ---------------------------------


# Load required libraries
library(XML)
library(tidyverse)

# Parse GPX file
gpx_parsed <- xmlParse("Export5323.gpx")

# Define namespaces
ns <- c(gpx = "http://www.topografix.com/GPX/1/1",
        gpxtpx = "http://www.garmin.com/xmlschemas/TrackPointExtension/v1")

# Extract track points
trackpoints <- getNodeSet(gpx_parsed, "//gpx:trkpt", namespaces = ns)

# Create data frame with track segment information
track_segment_counter <- 1  # Initialize track segment counter
track_segment_id <- NULL    # Initialize track segment ID vector

trackpoints_df <- map_df(trackpoints, ~{
  lat <- as.numeric(xmlGetAttr(.x, "lat"))
  lon <- as.numeric(xmlGetAttr(.x, "lon"))
  time <- as.POSIXct(xmlValue(xmlChildren(.x)[[1]]), tz = "UTC")
  depth <- as.numeric(xpathApply(.x, ".//gpxtpx:depth", xmlValue, namespaces = ns))
  
  # Extract track segment information
  track_segment <- track_segment_counter
  
  # Check if a new track segment is encountered
  if (!identical(track_segment_id, xmlValue(xmlParent(xmlParent(.x))))) {
    # If yes, increment the track segment counter and update track segment ID
    track_segment_counter <<- track_segment_counter + 1
    track_segment_id <<- xmlValue(xmlParent(xmlParent(.x)))
  }
  
  # Create data frame row
  tibble(lat = lat, lon = lon, time = time, depth = depth, track_segment = track_segment)
})

# View the resulting data frame
view(trackpoints_df)




# Read and Convert: Attempt 4.3 - show date and time columns --------------------------------


# Load required libraries
library(XML)
library(tidyverse)

# Parse GPX file
gpx_parsed <- xmlParse("Export5323.gpx")

# Define namespaces
ns <- c(gpx = "http://www.topografix.com/GPX/1/1",
        gpxtpx = "http://www.garmin.com/xmlschemas/TrackPointExtension/v1")

# Extract track points
trackpoints <- getNodeSet(gpx_parsed, "//gpx:trkpt", namespaces = ns)
# Create data frame with separate columns for date and time
trackpoints_df <- map_df(trackpoints, ~{
  lat <- as.numeric(xmlGetAttr(.x, "lat"))
  lon <- as.numeric(xmlGetAttr(.x, "lon"))
  timestamp <- xmlValue(xmlChildren(.x)[[1]])
  
  # Extract date and time from timestamp
  datetime <- as.POSIXct(timestamp, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  date <- as.Date(datetime)
  time <- format(datetime, "%H:%M:%S")
  
  depth <- as.numeric(xpathApply(.x, ".//gpxtpx:depth", xmlValue, namespaces = ns))
  
  tibble(lat = lat, lon = lon, date = date, time = time, depth = depth)
})

# View the resulting data frame
view(trackpoints_df)




# Filter to only Lake Waco Coordinates ------------------------------------

#filter coordinates
lw_gpx <- trackpoints_df |> 
  filter(lat > 31.4800) |> 
  filter(lat < 31.6500) |> 
  filter(lon < -97.1800) |> 
  filter(lon > -97.3500)



# add date id column-----------------------------------------------------------------


lw_gpx_id <- lw_gpx |>
  mutate(DID = as.numeric(factor(date, levels = unique(date))))




# Lake Waco water level history -------------------------------------------

#read in from URL
lw_history <- data.table::fread("https://waterdatafortexas.org/reservoirs/individual/waco.csv")

#select
lw_water_level <- select(lw_history, date, water_level)

# Load the 'units' package
library(units)

# Convert the 'height_feet' column to meters
lw_water_level <- lw_water_level |>
  mutate(water_level_m = set_units(water_level, "ft") |> set_units("m"))

# Merge lw_gpx_id with lw_water_level based on the "date" column
lw_gpx_id_meters <- merge(lw_gpx_id, lw_water_level, by = "date", all.x = TRUE)

# Calculate bed_elev_m by subtracting depth from water_level
lw_gpx_id_meters$bed_elev_m <- (as.numeric(lw_gpx_id_meters$water_level_m) - lw_gpx_id_meters$depth)

# View the resulting data frame
view(lw_gpx_id_meters)



# ggplot map --------------------------------------------------------------

library(ggplot2)

#depth in meters map
ggplot() +
  coord_quickmap() +
  geom_point(data = lw_gpx_id_meters, shape = 19, aes(x = lon, y = lat, color = depth)) +
  scale_colour_viridis_c(direction = -1) +
  theme_void() +
  labs(title = "Depth in Meters")


#lake bed elevation map
ggplot() +
  coord_quickmap() +
  geom_point(data = lw_gpx_id_meters, shape = 19, aes(x = lon, y = lat, color = bed_elev_m)) +
  scale_colour_viridis_c(direction = 1) +
  theme_void() +
  labs(title = "Bed Elevation Map")




# Raster Map --------------------------------------------------------------


#load packages
library(raster)
library(sp)

# Convert data frame to SpatialPointsDataFrame  
coordinates(lw_gpx_id_meters) <- c("lon", "lat")   #this step is only needed once


###plot raster grid

# Create higher resolution raster grid
r_high_res <- raster(extent(lw_gpx_id_meters), res = c(0.0015, 0.0015))  # Adjust resolution as needed

# Assign values to raster based on average bed_elev_m of nearby points
r_avg_high_res <- rasterize(lw_gpx_id_meters, r_high_res, field = "bed_elev_m", fun = mean)

# Define blue color gradient
blue_palette <- colorRampPalette(c("#220091", "#85e0ff"))

# Plot the higher resolution raster with larger size
lw_raster <- plot(r_avg_high_res, col = blue_palette(100), main = "Lake Waco Bed Elevation", axes = FALSE, box = FALSE)









# Mapping Attempts --------------------------------------------------------------

############ sf ###############

install.packages("sf")

library(sf)

# Convert data frame to sf object
lw_gpx_id_meters_sf <- st_as_sf(lw_gpx_id_meters, coords = c("lon", "lat"))

# Create ggplot2 plot
ggplot() +
  geom_sf(data = lw_gpx_id_meters_sf, aes(color = bed_elev_m)) +
  scale_color_gradient(low = "blue", high = "red")



########### mapview ###########

install.packages("mapview")

library(mapview)

# Create mapview map
mapview(lw_gpx_id_meters_sf, zcol = "bed_elev_m", col.regions = terrain.colors(10))



###### tmap ##############
install.packages("tmap")

library(tmap)

# Create static map
tm_shape(lw_gpx_id_meters_sf) +
  tm_basemap("OpenStreetMap") +
  tm_dots(col = "bed_elev_m", size = 0.5)




########## raster ##########

install.packages("raster")

library(raster)

# Convert data frame to SpatialPointsDataFrame
coordinates(lw_gpx_id_meters) <- c("lon", "lat")

# Create higher resolution raster grid
r_high_res <- raster(extent(lw_gpx_id_meters), res = c(0.0015, 0.0015))  # Adjust resolution as needed

# Assign values to raster based on average bed_elev_m of nearby points
r_avg_high_res <- rasterize(lw_gpx_id_meters, r_high_res, field = "bed_elev_m", fun = mean)

# Plot the higher resolution raster
plot(r_avg_high_res, col = terrain.colors(100), main = "Lake Waco Bed Elevation")




########## raster #2 ###########

#load packages
library(raster)

# Convert data frame to SpatialPointsDataFrame  
coordinates(lw_gpx_id_meters) <- c("lon", "lat")   #this step is only needed once


###plot raster grid

# Create higher resolution raster grid
r_high_res <- raster(extent(lw_gpx_id_meters), res = c(0.0015, 0.0015))  # Adjust resolution as needed

# Assign values to raster based on average bed_elev_m of nearby points
r_avg_high_res <- rasterize(lw_gpx_id_meters, r_high_res, field = "bed_elev_m", fun = mean)

# Define blue color gradient
blue_palette <- colorRampPalette(c("#220091", "#85e0ff"))

# Plot the higher resolution raster with larger size
lw_raster <- plot(r_avg_high_res, col = blue_palette(100), main = "Lake Waco Bed Elevation", axes = FALSE, box = FALSE)




# EXPERIMENTAL ZONE -------------------------------------------------------




# CURRENT EXPERIMENT ------------------------------------------------------




# ARCHIVED EXPERIMENTS ----------------------------------------------------



# Filter to only Lake Waco Coordinates ------------------------------------

#filter coordinates
lw_gpx <- trackpoints_df |> 
  filter(lat > 31.4800) |> 
  filter(lat < 31.6500) |> 
  filter(lon < -97.1800) |> 
  filter(lon > -97.3500)



# add date id column-----------------------------------------------------------------


lw_gpx_id <- lw_gpx |>
  mutate(DID = as.numeric(factor(date, levels = unique(date))))





# Plot with leaflet -------------------------------------------------------

#plot with leaflet
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = lw_gpx, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3)




# US plot -----------------------------------------------------------------

#usa map plot data
us <- map_data("usa")


#full usa map
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_quickmap() +
  geom_point(data = lw_gpx, shape = 19, aes(x = lon, y = lat, color = depth)) +
  scale_colour_viridis_c() +
  theme_void()

#local map
ggplot() +
  coord_quickmap() +
  geom_point(data = lw_gpx_id_meters, shape = 19, aes(x = lon, y = lat, color = bed_elev_m)) +
  scale_colour_viridis_c(direction = 1) +
  theme_void()


#local map
ggplot() +
  coord_quickmap() +
  geom_point(data = lw_gpx_id_meters, shape = 19, aes(x = lon, y = lat, color = bed_elev_m)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_void()



library(raster)

# Convert data frame to SpatialPointsDataFrame
coordinates(lw_gpx_id_meters) <- c("lon", "lat")

# Create raster grid
r <- raster(extent(lw_gpx_id_meters), res = c(0.01, 0.01))

# Assign values to raster based on average bed_elev_m of nearby points
r_avg <- rasterize(lw_gpx_id_meters, r, field = "bed_elev_m", fun = mean)

# Plot the raster
plot(r_avg, col = terrain.colors(10), main = "Spatial Gradient of bed_elev_m")



