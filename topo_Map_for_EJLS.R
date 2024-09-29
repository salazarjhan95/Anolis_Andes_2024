library(ggplot2)
library(rnaturalearth)
library(ggspatial)
library(sf)
library(raster)
library(elevatr)
library(metR)
library(cowplot)
library(rnaturalearthdata)

setwd("")

# Define box for Colombia
bbox <- st_bbox(c(xmin = -80, ymin = -5, xmax = -66, ymax = 12), crs = st_crs(4326))

# Convert the bbox to an sf object
bbox_sf <- st_as_sf(st_as_sfc(bbox))

# Fetch elevation data using elevatr
elev_data <- get_elev_raster(locations = bbox_sf, z = 6)  # Adjust z for more or less detail

# Convert raster to data frame for ggplot
elev_df <- as.data.frame(as(elev_data, "SpatialPixelsDataFrame"))
names(elev_df) <- c("elevation", "x", "y")

# Create hillshade data (assuming DEM is in meters)
slope <- terrain(elev_data, opt = 'slope')
aspect <- terrain(elev_data, opt = 'aspect')
hillshade <- hillShade(slope, aspect, angle = 40, direction = 270)

# Convert hillshade to data frame for ggplot
hillshade_df <- as.data.frame(as(hillshade, "SpatialPixelsDataFrame"))
names(hillshade_df) <- c("hillshade", "x", "y")

# Sample data for each sample site
species_data_2017 <- read.csv("Sites_2017.csv")




# Savng the plot as a .jepg; don't forget to unblock the dev.off() argument at the end
#jpeg(file="Maps_2017_for_EJLS.jpg", width=10, height=10, units="in", res=300)

# Create the map
ggplot() +
  
  # Elevation layer with custom color gradient
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation), alpha = 0.8) +
  
  scale_fill_gradientn(colors = c("#0000FF", "#0000CC", "#3399FF", "#00FFFF", 
                       "#008100", "#FFFF00", "#FFA500", "#FF0000"), 
                       name = "Elevation (m)") +
  
  scale_shape_manual(values = c(6, 0, 1, 2, 7)) +  # Different shapes for each sample site
  
  # Colombia border
  geom_sf(data = ne_countries(scale = "medium", country = "Colombia", returnclass = "sf"),
          fill = NA, color = "black", linewidth = 2) +
  
  # Sample site points
  geom_point(data = species_data_2017, aes(x = Longitude, y = Latitude, shape = Site), 
             color = "#001427", size = 10) +
  
  # Map theme and labels
  coord_sf(xlim = c(-80, -66), ylim = c(-5, 12)) +
  labs(x = "Longitude", y = "Latitude") +
  
  # Additional map elements
  theme_minimal() +
  theme(legend.position = "right", 
        legend.title = element_text(family = "serif", size = 18, face = "bold"), 
        legend.text = element_text(family = "serif", size = 15)) +
  annotation_scale(location = "bl", width_hint = 0.4,       
                   style = "bar",         
                   height = unit(0.25, "cm"), 
                   text_cex = 1.5,               
                   text_family = "serif" ) +  
  
  annotation_north_arrow(location = "tl", style = north_arrow_nautical,
                         height = unit(3, "cm"),       # Height of the arrow
                         width = unit(3, "cm")) + # Width of the arrow
  
  theme(axis.text.x = element_text(family="serif", size = 20, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 20, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1, size=25)) +
  theme(axis.title.x = element_text(family="serif",vjust=0.3, size=25))

#dev.off()
