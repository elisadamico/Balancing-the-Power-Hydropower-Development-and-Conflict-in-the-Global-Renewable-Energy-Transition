rm(list = ls()) 
#graphics.off()

#	Balancing the Power: A Comparative Analysis of Hydropower Development and 
#                     Conflict in the Global Renewable Energy Transition"

# 	The global shift to renewable energy, vital for sustainable development, 
#     requires understanding socio-economic and political impacts. This research 
#     uses staggered treatment difference-in-differences quasi-experimental analysis, 
#     focusing on regions with large-scale hydropower projects in the renewable energy 
#     transition. Framed within the need to balance renewable energy goals with caution 
#     in vulnerable areas, the study examines the causal relationship between hydropower 
#     development and water-based conflict outcomes. Given the significant alterations 
#     to water distribution caused by these plants, it explores if conflict outcomes 
#     related to resource changes manifest statistically. It also assesses how displacement 
#     and economic shifts shape conflict outcomes, concluding with policy recommendations 
#     for politically and environmentally sustainable hydropower development in renewable 
#     energy transitions.

# Data Sources: 
##    UCDP Georeferenced Event Dataset (GED) Global version 24.1
#     https://ucdp.uu.se/downloads/ged/ged241.pdf
##    Library of Congress Global hydropower tracker
#     https://www.loc.gov/item/2023592028/

library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(sf)
library(did)
library(future)
library(furrr)

# Load data
hydropower <- read_excel("hydropower.xlsx")
ucdp <- read_excel("ucdp.xlsx")
ucdpconflict <- read_csv("ucdpconflict.csv")

# Create HEX grid units
world <- ne_countries(scale = "medium", returnclass = "sf")
bbox <- st_bbox(world)

hex_grid <- st_make_grid(
  st_as_sfc(bbox), 
  cellsize = 1.25, 
  what = "polygons",
  square = FALSE
)

hex_sf <- st_sf(geometry = st_sfc(hex_grid, crs = st_crs(world)))

world <- st_make_valid(world)
hex_sf <- st_make_valid(hex_sf)
if (st_crs(hex_sf) != st_crs(world)) {
  hex_sf <- st_transform(hex_sf, crs = st_crs(world))
}

hex_sf_valid <- hex_sf[st_is_valid(hex_sf), ]
hex_with_countries <- st_join(hex_sf_valid, world)
hex_with_countries <- hex_with_countries[!is.na(hex_with_countries$featurecla), ]
hex_with_countries <- hex_with_countries %>% filter(!is.na(admin) & admin != "Antarctica")

ucdpconflict$iso_n3 <- countrycode(ucdpconflict$gwno, origin = 'gwn', destination = 'iso3n')
ucdpconflict$iso_n3 <- as.numeric(as.character(ucdpconflict$iso_n3))
hex_with_countries$iso_n3 <- as.numeric(as.character(hex_with_countries$iso_n3))
hex_with_countries <- hex_with_countries %>%
  left_join(ucdpconflict, by = "iso_n3")

# Function to generate and plot results for each region
generate_plots <- function(region_code, region_name) {
  hex_with_countries_region <- hex_with_countries %>%
    filter(region == region_code, income_grp %in% c("5. Low income", "4. Lower middle income"))
  
  hydropower_sf <- st_as_sf(hydropower, coords = c("Longitude", "Latitude"), crs = st_crs(world))
  hydropower_hex <- st_join(hex_with_countries_region, hydropower_sf, join = st_intersects)
  
  hydropower_first <- hydropower_hex %>%
    arrange(geometry, 'Start Year') %>%
    distinct(geometry, .keep_all = TRUE) %>%
    select(geometry, firsttreat = 'Start Year')
  
  ucdp_sf <- st_as_sf(ucdp, coords = c("longitude", "latitude"), crs = st_crs(world))
  ucdp_hex <- st_join(hex_with_countries_region, ucdp_sf, join = st_intersects)
  ucdp_summary <- ucdp_hex %>%
    group_by(geometry, year) %>%
    summarise(
      conflict_count = n(),
      total_deaths_civilians = sum(deaths_civilians, na.rm = TRUE),
      total_best = sum(best, na.rm = TRUE),
      first_region = first(region.y),
      avg_type_of_violence = mean(type_of_violence, na.rm = TRUE),
      .groups = 'drop'
    )
  
  years <- 1989:2024
  panel_data <- expand.grid(geometry = hex_with_countries_region$geometry, year = years)
  panel_data <- as.data.frame(panel_data)
  hydropower_first <- as.data.frame(hydropower_first)
  ucdp_summary <- as.data.frame(ucdp_summary)
  
  panel_data <- panel_data %>%
    left_join(ucdp_summary, by = c("geometry", "year")) %>%
    left_join(hydropower_first, by = "geometry") %>%
    mutate(firsttreat = replace_na(firsttreat, 0), 
           conflict_count = replace_na(conflict_count, 0))
  
  panel_data <- panel_data %>%
    mutate(hex_id = as.numeric(factor(geometry, levels = unique(geometry))))
  
  panel_data <- panel_data %>%
    mutate_all(~ ifelse(is.na(.), 0, .))
  
  result <- att_gt(yname = "conflict_count",
                   tname = "year",
                   idname = "hex_id",
                   gname = "firsttreat",
                   data = panel_data,
                   bstrap = TRUE,
                   anticipation = 1,
                   panel = FALSE,
                   control_group = "notyettreated")
  
  event <- aggte(result, type = "dynamic", na.rm = TRUE)
  summary(event)
  
  plot <- ggdid(event) + 
    labs(title = paste("Region:", region_name)) +  # Use labs() instead of ggtitle
    scale_color_manual(values = c("#eeac34", "#8a269d"), 
                       labels = c("Pre-Treatment", "Post-Treatment")) +  # Set custom labels for the color scale
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
      axis.text.y = element_text(size = 8, family = "Times New Roman"),
      axis.title.x = element_text(size = 10, family = "Times New Roman"),
      axis.title.y = element_text(size = 10, family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman")
    )
  
  print(event)
  return(plot)
}

# List of regions and their names
regions <- list(
  list(code = 4, name = "Africa"),
  list(code = 3, name = "Asia"),
  list(code = 5, name = "Latin America"),
  list(code = 2, name = "Middle East")
)

# Plan for parallel processing
plan(multisession)

# Generate and print plots for each region in parallel
plots <- future_map(regions, ~ generate_plots(.x$code, .x$name))

# Print all plots
for (plot in plots) {
  print(plot)
}



###############################
### Create grid-cell units ###
###############################

### Structuring Panel Data ###
#resolution <- 1  # 1 degree resolution for latitude and longitude
#latitude_seq <- seq(-90, 90, by = resolution)
#longitude_seq <- seq(-180, 180, by = resolution)
#grid_cells <- expand.grid(latitude = latitude_seq, longitude = longitude_seq)
#grid_sf <- st_as_sf(grid_cells, coords = c("longitude", "latitude"), crs = st_crs(world))
#grid_with_countries <- st_join(grid_sf, world)
#grid_with_countries <- grid_with_countries %>% 
#  filter(!is.na(admin) & admin != "Antarctica")

## if UCDP conflict merge ##
#grid_with_countries$gwno <- countrycode(grid_with_countries$fips_10, origin = 'fips', destination = 'gwn')
# 9 unmatched

# Create sequence of years from 1985 to 2024
#years <- 1989:2024
#panel_data <- expand.grid(gridcell = unique(grid_with_countries$geometry), year = years)

# Round latitude and longitude to integers and format into c(latitude, longitude)
#ucdp$gridcell <- paste0("c(", round(ucdp$latitude), ", ", round(ucdp$longitude), ")")
#hydropower$gridcell <- paste0("c(", round(hydropower$Latitude), ", ", round(hydropower$Longitude), ")")

#ucdp_summary <- ucdp %>%
#  group_by(gridcell, year) %>%
#  summarise(conflict_count = n())

#hydropower_first <- hydropower %>%
#  arrange(gridcell, 'Start Year') %>%
#  distinct(gridcell, .keep_all = TRUE)

#hydropower_first <- hydropower_first %>%
#  select(gridcell, firsttreat = 'Start Year')

### Merging Data ###

#panel_data <- panel_data %>%
#  mutate(gridcell = as.character(gridcell))

#panel_data <- panel_data %>%
#  left_join(ucdp_summary, by = c("gridcell", "year"))

#panel_data <- panel_data %>%
#  left_join(hydropower_first, by = "gridcell")

### Cleaning ###
#panel_data <- panel_data %>%
#  mutate(firsttreat = replace_na(firsttreat, 0), 
#        conflict_count = replace_na(conflict_count, 0)
#         )

#panel_data <- panel_data %>%
#  mutate(gridcell_id = as.numeric(factor(gridcell, levels = unique(gridcell))))

panel_data <- panel_data %>%
  mutate_all(~ ifelse(is.na(.), 0, .))

print(colnames(panel_data))

### Running Models ###
result <- att_gt(yname = "conflict_count",
                                 tname = "year",
                                 idname = "hex_id",
                                 gname = "firsttreat",
                                 data = panel_data,
                                 bstrap = TRUE,
                                 anticipation = 1,
                                 panel = FALSE,               
                                 # xformla = ~total_deaths_civilians + avg_type_of_violence, # "total_best" # "total_deaths_civilians" "avg_type_of_violence"
                                 control_group = "notyettreated"
)



event <- aggte(result, type = "dynamic", na.rm = TRUE)
summary(event)

ggdid_plot <- ggdid(event) + 
  ggtitle("Region: X") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = "Times New Roman", color = "black"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )

ggdid_plot


## Ask content experts
# --- next do by country 
# Middle East, Africa, Less Developed




##################################
## Exacerbating in existing conflict countries?

# Does not work - Income Group
## grid_with_countries <- grid_with_countries %>% filter(income_grp == "5. Low income")
## grid_with_countries <- subset(grid_with_countries, income_grp %in% c("5. Low income", "4. Lower middle income"))
## grid_with_countries <- subset(grid_with_countries, income_grp %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income"  ))
### MAYBE SOMETHING ^
## grid_with_countries <- subset(grid_with_countries, income_grp %in% c("5. Low income", "4. Lower middle income", "3. Upper middle income", "2. High income: nonOECD"  ))

#[1] "3. Upper middle income" 
#[2] "1. High income: OECD"   
#[3] "4. Lower middle income" 
#[4] "2. High income: nonOECD"
#[5] "5. Low income"  

# Does not work - Economy 
#grid_with_countries <- subset(grid_with_countries, economy %in% c("7. Least developed region"))
#grid_with_countries <- subset(grid_with_countries, economy %in% c("7. Least developed region", "6. Developing region"))

#[1] "6. Developing region"     
#[2] "5. Emerging region: G20"  
#[3] "7. Least developed region"
#[4] "3. Emerging region: BRIC" 
#[5] "4. Emerging region: MIKT" 

# if ucdp conflict from 1989+
##grid_with_countries <- subset(grid_with_countries, ifucdpconflict == 1)

# By if UCDP & Region
#grid_with_countries <- subset(grid_with_countries, region == 1)
#grid_with_countries <- subset(grid_with_countries, region == 2)
#grid_with_countries <- subset(grid_with_countries, region == 4)

#unique_sovereignt_grid <- unique(grid_with_countries$region)
#print(unique_sovereignt_grid)

# Plot the map
#ggplot(data = grid_with_countries) +
#  geom_sf() +
#  theme_minimal() +
#  labs(title = "Grid with Countries")



