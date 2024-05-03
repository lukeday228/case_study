


# Packages used in this script --------------------------------------------

library(XML)
library(tidyverse)
library(raster)
library(sp)
library(data.table)



# Read and Convert GPX to data frame --------------------------------

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

# Read data into a data table
lw_history <- fread("https://waterdatafortexas.org/reservoirs/individual/waco.csv")

# Convert data table to data frame
lw_history <- as.data.frame(lw_history)

# Select columns using dplyr select function
lw_water_level <- dplyr::select(lw_history, date, water_level)

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

#duplicate "lw_gpx_id_meters"
lw_gpx_spacial <- copy(lw_gpx_id_meters)

# Convert data frame to SpatialPointsDataFrame  
(coordinates(lw_gpx_spacial) <- c("lon", "lat"))   #this step is only needed once


###plot raster grid

# Create higher resolution raster grid
r_high_res <- raster(extent(lw_gpx_spacial), res = c(0.0015, 0.0015))  # Adjust resolution as needed

# Assign values to raster based on average bed_elev_m of nearby points
r_avg_high_res <- rasterize(lw_gpx_spacial, r_high_res, field = "bed_elev_m", fun = mean)

# Define blue color gradient
blue_palette <- colorRampPalette(c("#220091", "#85e0ff"))

# Plot the higher resolution raster with larger size
lw_raster <- plot(r_avg_high_res, col = blue_palette(100), main = "Lake Waco Bed Elevation", axes = FALSE, box = FALSE)






