## VISUALIZATIONS ##

#Tables: 
#  ---  ALL Event tables for main DID results
#  --- Alternative specification simple comparison
#  --- List of countries that did not converge 
#  --- List of countries that had null results

#Figures: 
#  --- Map of hydropower over conflict (DONE)
#  --- All regional DIDs
#  --- All country DIDs  (DONE)
#  --- 1.25 grid overaly 
#  --- parallel trends figure(s)

# Color Scale 
"#8a269d"
"#381e6f"
"#ef8a51"
"#eeac34"
"#cb5279"

# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)
library(countrycode)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)

# Assuming you've already read the data into 'hydropower' and 'ucdp' data frames
hydropower <- read_excel("hydropower.xlsx")
ucdp <- read_excel("ucdp.xlsx")

# Collapse (sum) the ucdp data by country_id
ucdp_country <- ucdp %>%
  group_by(country_id) %>%
  summarise(conflict_count = n(), .groups = 'drop')  # Count the number of conflict events per country_id

# Log-transform the conflict count to reduce skewness (using log1p to handle zeros)
ucdp_country <- ucdp_country %>%
  mutate(log_conflict_count = log1p(conflict_count))  # log(1 + conflict_count)

# Convert country_id to ISO3C codes using the countrycode function
ucdp_country <- ucdp_country %>%
  mutate(iso3c = countrycode(country_id, "cown", "iso3c"))

# Now read the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")  # Get world map as sf object

# Merge the conflict counts with the world map based on the ISO3C country code
world_with_conflict <- world %>%
  left_join(ucdp_country, by = c("iso_a3" = "iso3c"))

# Convert the hydropower locations to spatial points (hydropower dataset has Latitude and Longitude)
hydropower_sf <- st_as_sf(hydropower, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot() + 
  geom_sf(data = world_with_conflict, aes(fill = log_conflict_count), color = "gray", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "Log of Conflict Events") +  # Using 'viridis' color scale for conflict events
  # Plotting hydropower locations with a black circle around the points
  geom_sf(data = hydropower_sf, aes(color = "Hydropower Location"), size = .5, shape = 21, fill = "white", stroke = 0.3) +  # white fill with black border
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman"),
    axis.text.x = element_text(size = 8, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_blank(),  # Remove x axis title
    axis.title.y = element_blank(),  # Remove y axis title
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman"),  # Keep the legend titles in Times New Roman
    plot.caption = element_text(family = "Times New Roman", size = 8, hjust = 0.5),  # Set caption font to Times New Roman
    plot.margin = margin(10, 10, 30, 10)  # Adjust bottom margin for the annotation
  ) +
  labs(
    title = "Global Hydropower Locations and Conflict Events (1989-2023)",
    caption = "Source: UCDP Georeferenced Event Dataset (GED) Global version 24.1 and Global Energy Monitor Global Hydropower Tracker"
  ) +
  coord_sf(expand = FALSE) +  # Adjust the map view to focus on the relevant regions
  annotate("text", 
           x = -180, y = -65, 
           label = "Sources: UCDP Georeferenced Event Dataset (GED) Global version 24.1 and Global Energy Monitor Global Hydropower Tracker", 
           size = 3.5, hjust = 0, vjust = 0, family = "Times New Roman", color = "black", lineheight = 1.2) +
  xlim(-180, 180) +  # Set limits for longitude (no extra space west/east)
  ylim(-60, 90) +    # Set limits for latitude (focus on the relevant regions, limiting southward extension)
  scale_color_manual(values = c("Hydropower Location" = "black")) +  # Ensure hydropower points are shown with the right color legend
  guides(
    color = guide_legend(title = "Dots", override.aes = list(size = 1))  # Make the legend dots bigger
  )










# Load required libraries
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Get world data as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a hexagonal grid with a cell size of 1.25 degrees
grid <- st_make_grid(world, cellsize = c(1.25, 1.25), square = FALSE)

# Convert the grid to an sf object
grid_sf <- st_sf(geometry = grid)

# Plot the world map with hexagonal grid overlay and adjusted boundaries
ggplot() +
  geom_sf(data = world, fill = NA, color = "black") +
  geom_sf(data = grid_sf, fill = NA, color = "#cb5279", alpha = 0.5) +
  coord_sf(ylim = c(-50, 80), xlim = c(-140, 150)) +  # Set latitude and longitude limits
  ggtitle("1.25 Degree Hexagonal Unit Overlay of World Map") +  # Add title
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(family = "Times New Roman", size = 16))  # Set title font to Times New Roman
