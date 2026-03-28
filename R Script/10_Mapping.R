## --------------------------------------------------------------------------
## Mapping data on Ethiopia (visualizing)
## --------------------------------------------------------------------------

# Read shapefile
floristic <- st_read("Shapefiles/FEE1/fee_1.shp")

# Read your cleaned and joined data
clean_data_updated

# Re-read and process floristic regions shapefile
st_crs(floristic) <- 4326

# Filter to your regions of interest
regions_of_interest <- c("AF", "AR", "BA", "GD", "GG", "GJ", "HA", "IL", "KF", "SD", "SU", "TU", "WG", "WU")
floristic <- floristic %>%
  filter(ADMIN_NAME %in% regions_of_interest)

### Function to calculate richness per region and return geomap ###
make_richness_map <- function(clean_data, floristic, prediction_filter = NULL, title = "Species Richness: All Species") {
  
  if (!is.null(prediction_filter)) {
    filtered <- clean_data %>% filter(prediction == prediction_filter)
  } else {
    filtered <- clean_data
  }
  
  # Select only taxon and floristic region columns
  filtered <- filtered %>%
    dplyr::select(taxon, all_of(regions_of_interest)) %>%
    distinct()  # Ensure no duplicates
  
  # Pivot longer
  region_long <- filtered %>%
    pivot_longer(cols = all_of(regions_of_interest), names_to = "region", values_to = "presence") %>%
    filter(presence == "present") %>%
    distinct(taxon, region) %>%
    count(region, name = "species_count")
  
  # Join counts with floristic shapefile
  map_data <- floristic %>%
    left_join(region_long, by = c("ADMIN_NAME" = "region")) %>%
    mutate(species_count = replace_na(species_count, 0))
  
  # Create centroids for labeling
  centroids <- st_centroid(map_data)
  centroids_coords <- st_coordinates(centroids)
  centroids_df <- cbind(map_data, centroids_coords)
  
  # Plot
  p <- ggplot(map_data) +
    geom_sf(aes(fill = species_count), color = "black") +
    scale_fill_gradient(name = "Species Count", low = "#deebf7", high = "#08519c") +
    geom_text(data = centroids_df, aes(X, Y, label = ADMIN_NAME), size = 3, color = "black") +
    theme_minimal() +
    labs(title = title, x = "", y = "") +
    theme(legend.position = "right")
  
  return(p)
}

### Create maps ###
map_all <- make_richness_map(clean_data, 
                             floristic, 
                             prediction_filter = NULL, 
                             title = "Species Richness: All Species")
map_correct <- make_richness_map(clean_data, 
                                 floristic, 
                                 prediction_filter = "correct", 
                                 title = "Species Richness: Correctly Predicted")
map_over <- make_richness_map(clean_data, 
                              floristic, 
                              prediction_filter = "over", 
                              title = "Species Richness: Overpredicted")
map_under <- make_richness_map(clean_data, 
                               floristic, 
                               prediction_filter = "under", 
                               title = "Species Richness: Underpredicted")

# Print or save
print(map_all)
print(map_correct)
print(map_over)
print(map_under)

# To save maps as images:
ggsave("map_all_species.png", plot = map_all, width = 10, height = 8, bg = "white")
ggsave("map_correct_species.png", plot = map_correct, width = 10, height = 8, bg = "white")
ggsave("map_overpredicted_species.png", plot = map_over, width = 10, height = 8, bg = "white")
ggsave("map_underpredicted_species.png", plot = map_under, width = 10, height = 8, bg = "white")

# ==============================================================================
## Map species over floristic region and HFP
# ==============================================================================

floristic <- st_read("Shapefiles/FEE1/fee_1.shp")
floristic_eth <- floristic %>% 
  filter(!ADMIN_NAME %in% c("EW", "EE"))

# Specify the species you want to map
genus_name   <- "Aloe"
species_name <- "gilbertii"

species_df <- all_occurrences %>%
  filter(GENUS == genus_name,
         SP1   == species_name)

# Convert filtered occurrences to an sf object
points_sf <- st_as_sf(
  species_df,
  coords = c("LONG", "LAT"),
  crs    = st_crs(floristic_eth)
)

points_within <- st_filter(points_sf, floristic_eth)

floristic_eth <- floristic_eth %>%
  mutate(
    has_species = lengths(st_intersects(., points_within)) > 0
  )

st_crs(floristic_eth)
st_crs(points_within)

floristic_eth   <- st_set_crs(floristic_eth, 4326)
points_within   <- st_set_crs(points_within, 4326)

target_crs <- st_crs(ethiopia_no_lakes)

floristic_eth     <- st_transform(floristic_eth, target_crs)
points_within     <- st_transform(points_within, target_crs)
ethiopia_no_lakes <- st_make_valid(ethiopia_no_lakes) 

ethiopia_proj <- st_transform(ethiopia_no_lakes, 32637)

ethiopia_centroids <- st_centroid(ethiopia_proj)

ethiopia_centroids <- st_transform(ethiopia_centroids, target_crs)

coords <- st_coordinates(ethiopia_centroids)
ethiopia_coords <- cbind(ethiopia_centroids, coords)

# Plot
ggplot() +
  # Fill floristic regions
  geom_sf(data = floristic_eth,
          aes(fill = has_species),
          color = "darkgray",
          alpha = 0.6) +
  
  geom_text(data = ethiopia_coords,
            aes(x = X, y = Y, label = ADMIN_NAME),
            size = 3,
            color = "black") +
  
  # Species occurrence points
  geom_sf(data = points_within,
          color = "black",
          size = 2,
          show.legend = FALSE) +
  
  scale_fill_manual(
    values = c(`FALSE` = "lightgray", `TRUE` = "yellow"),
    guide  = "none"
  ) +
  
  coord_sf(
    xlim   = st_bbox(ethiopia_no_lakes)[c("xmin", "xmax")],
    ylim   = st_bbox(ethiopia_no_lakes)[c("ymin", "ymax")],
    expand = FALSE
  ) +
  
  theme_bw() +
  labs(
    title = paste("Total Floristic Regions of", genus_name, species_name, "in Ethiopia"),
    x     = "Longitude",
    y     = "Latitude"
  )

ggplot() +
  geom_sf(data = hfp_2009_quant10, 
          aes(fill = as.factor(layer)), 
          colour = "transparent") +
  
  geom_sf(data = ethiopia_no_lakes, 
          fill = "transparent", 
          colour = "gray20", 
          size = 1) +
  
  stat_sf_coordinates(data = ethiopia_no_lakes, 
                      geom = "label", 
                      aes(label = ADMIN_NAME), 
                      size = 2) +
  
  geom_sf(data = highlight_regions,
          fill="blue", alpha=0.0, 
          colour = "black", 
          linewidth   = 2) +  
  
  scale_fill_manual(values = hfp_pal,
                    name   = "HFP Quantile Classes") +
  coord_sf(xlim   = st_bbox(ethiopia_no_lakes)[c("xmin", "xmax")],
           ylim   = st_bbox(ethiopia_no_lakes)[c("ymin", "ymax")],
           expand = FALSE) +
  ggtitle("2009 Human Footprint in Ethiopian Floristic Regions") +
  theme_bw()
  theme(
    plot.title   = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 12)
  )