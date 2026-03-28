## --------------------------------------------------------------------------
## Protected Area data
## --------------------------------------------------------------------------

# Create binomial for presence/absence protected areas for just mis-predicted species
mis_pa <- data %>% 
  filter(presence_pa %in% c("yes", "no")) %>%
  mutate(pa_binomial = case_when(
    presence_pa == "yes" ~ 1,
    presence_pa == "no" ~ 0
  ))

# Model for mis-predicted species
pa_1 <- glm(prediction_binomial ~ 1 + pa_binomial, data = mis_pa, family = "binomial")

summary(pa_1)

plot_model(pa_1, type="pred", terms="pa_binomial")

## --------------------------------------------------------------------------
## PA overlapping
## --------------------------------------------------------------------------

# Read the three separate shapefiles
protected_0 <- st_read("Shapefiles/WDPA_WDOECM_Apr2025_Public_ETH_shp_0/WDPA_WDOECM_Apr2025_Public_ETH_shp-polygons.shp")
protected_1 <- st_read("Shapefiles/WDPA_WDOECM_Apr2025_Public_ETH_shp_1/WDPA_WDOECM_Apr2025_Public_ETH_shp-polygons.shp")
protected_2 <- st_read("Shapefiles/WDPA_WDOECM_Apr2025_Public_ETH_shp_2/WDPA_WDOECM_Apr2025_Public_ETH_shp-polygons.shp")

protected_0 <- st_transform(protected_0, crs = 4326)
protected_1 <- st_transform(protected_1, crs = 4326)
protected_2 <- st_transform(protected_2, crs = 4326)

# Combine them into one shapefile
protected_areas <- rbind(protected_0, protected_1, protected_2)

overlap_occ <- st_as_sf(clean_occurrences, 
                        coords = c("LONG", "LAT"), 
                        crs = 4326)

protected_areas <- st_transform(protected_areas, crs = st_crs(overlap_occ))

# Spatial join to protected areas
occ_in_protected <- st_join(overlap_occ, protected_areas, join = st_intersects)

# Presence in protected areas
presence_prot <- occ_in_protected %>%
  mutate(present = TRUE) %>%
  distinct(taxon, NAME.y, present, IUCN_CAT)

pa_df <- expand.grid(
  taxon = unique(overlap_occ$taxon),
  NAME = unique(protected_areas$NAME)
)

pa_df <- pa_df %>%
  dplyr::select(taxon, NAME.y = NAME)

presence_absence <- pa_df %>%
  left_join(presence_prot, by = c("taxon", "NAME.y")) %>%
  mutate(present = ifelse(is.na(present), FALSE, present))

# Count of species with protection in at least one protected area
species_with_protection <- presence_absence %>%
  filter(present == TRUE) %>%
  distinct(taxon)
n_species_with_protection <- nrow(species_with_protection)

species_presence <- presence_absence %>%
  group_by(taxon) %>%
  summarise(present = any(present), .groups = "drop") %>%
  separate(taxon, into = c("genus", "species"), sep = " ", extra = "merge", fill = "right") %>%
  mutate(presence = ifelse(present, "present", "absent")) %>%
  dplyr::select(genus, species, presence) %>%
  mutate(presence_binary = ifelse(presence == "present", 1, 0)) %>%
  mutate(taxon = paste(genus, species, sep = " "))

clean_data <- clean_data %>% 
  merge(species_presence, by = "taxon") 

clean_data <- clean_data %>% 
  select(-genus.y, -species.y) %>% 
  rename(genus = genus.x, species = species.x)

## --------------------------------------------------------------------------
## PA IUCN Categories - Not used in paper (IUCN categories not well documented)
## --------------------------------------------------------------------------

# Define valid IUCN categories
iucn_cats <- c("Ia", "Ib", "II", "III", "IV", "V", "VI")

# Filter valid IUCN categories only
# Replace 'occ_in_protected' with your actual dataframe name if different
occ_clean <- occ_in_protected %>%
  filter(IUCN_CAT %in% iucn_cats)

# Create a table of presence per species and IUCN category
iucn_presence <- occ_clean %>%
  mutate(present = "present") %>%
  distinct(taxon, IUCN_CAT, present)

# Generate all combinations of species × IUCN categories
df_iucn <- expand.grid(
  taxon = unique(occ_in_protected$taxon),
  IUCN_CAT = iucn_cats
)

# Merge and fill in "absent" where species is not present
presence_absence_iucn <- df_iucn %>%
  left_join(iucn_presence, by = c("taxon", "IUCN_CAT")) %>%
  mutate(present = ifelse(is.na(present), "absent", present))

# Pivot to wide format: one row per species, IUCN categories as columns
presence_matrix <- presence_absence_iucn %>%
  pivot_wider(
    names_from = IUCN_CAT,
    values_from = present,
    values_fill = "absent"
  )

# Build presence dataframe: taxon x IUCN_CAT
iucn_presence_prot <- occ_in_protected %>%
  mutate(present = TRUE) %>%
  distinct(taxon, IUCN_CAT, present)

# Create all combinations of taxon and IUCN categories
df_iucn <- expand.grid(
  taxon = unique(overlap_occ$taxon),
  IUCN_CAT = unique(protected_areas$IUCN_CAT)
)

# If species is present in II, IV, or VI. 
presence_matrix <- presence_matrix %>%
  dplyr::select(taxon, II, IV, VI)

# Combine with other dataset
clean_data <- clean_data %>% 
  left_join(presence_matrix, by = "taxon")

# clean_data = presence/absence in a single protected area, binary (0,1) presence/absence, and presence/absence in IUCN category (II, IV, VI) along with all other calculations (presence in floristic regions, total floristic regions, altitude, eoo, aoo)