## --------------------------------------------------------------------------
## Floristic region data
## --------------------------------------------------------------------------

# Look at just mis-predicted species
# Load in data
mis_fr <- data %>%
  select(genus, species, family, prediction, category, floristic_region_codes, data_collection_type) %>% 
  filter(prediction %in% c("under", "over"))

# Remove empty cells
mis_fr <- mis_fr %>%
  filter(
    !apply(., 1, function(row) all(str_trim(row) == ""))
  )

# Separate the codes into a list column
mis_fr <- mis_fr %>%
  mutate(code_list = str_split(floristic_region_codes, ",\\s*"))

# Get all unique codes across all rows
all_codes <- mis_fr %>%
  pull(code_list) %>%
  unlist() %>%
  unique() %>%
  sort()

# Create a column with "present" or "not present"
for (code in all_codes) {
  mis_fr[[code]] <- sapply(mis_fr$code_list, function(x) ifelse(code %in% x, "present", "not present"))
}

# Create a taxon column of genus and species
mis_fr <- mis_fr %>%
  mutate(taxon = paste(genus, species, sep = " "))

# Drop the list column if desired
mis_fr <- mis_fr %>% select(-code_list)

# Calculate how many floristic regions each species is present in
species_flor_counts <- mis_fr %>%
  group_by(taxon) %>%
  summarize(total_flor_regions = sum(across(all_of(all_codes)) == "present")) %>%
  ungroup()

# Join the count back to the original dataframe
mis_fr <- mis_fr %>%
  left_join(species_flor_counts, by = "taxon")

## --------------------------------------------------------------------------
## GLM for FR
## --------------------------------------------------------------------------

# Convert predictions to binary (0,1)
mis_fr <- mis_fr %>% 
  mutate(prediction_binomial = case_when(
    prediction=="under"~0, 
    prediction=="over"~1, 
    TRUE~NA_integer_
  ))

fr_1 <- glm(prediction_binomial ~ total_flor_regions, data = mis_fr, family = "binomial")

summary(fr_1)

plot_model(fr_1, type="pred", terms="total_flor_regions")

## --------------------------------------------------------------------------
## All species data for floristic region using coordinates
## --------------------------------------------------------------------------
# Load in spatial data
floristic <- st_read("Shapefiles/FEE1/fee_1.shp")
## BRAHMS occurrences unpublished and not available for public use currently!
all_occurrences <- read.csv("Raw Data/Ethiopian endemics full BRAHMS download(Manual_georeferencing_data)(all).csv", fileEncoding = "latin1")

# Combine genus and species to make a taxon list, change the names of a few on the original dataset to match these on BRAHMS then revert back to list from Jack.

# BRAHMS occurrences
brahms_occurrences <- all_occurrences %>%
  mutate(taxon = paste(GENUS, SP1, sep = " ")) %>%
  mutate(taxon = str_replace_all(taxon, "Brillantaisia grottanellii�", "Brillantaisia grottanellii")) %>%
  mutate(taxon = str_trim(taxon))

# Clean up list of species used 
data <- data %>%
  mutate(taxon = str_trim(taxon)) %>%
  mutate(taxon = case_when(
    taxon == "Afroligusticum piovanii" ~ "Carum piovanii", taxon == "Afroligusticum piovanii" ~ "Carum piovanii", 
    taxon == "Pseudog phalium petitianum" ~ "Pseudognaphalium petitianum",
    taxon == "Baccharoides filigera" ~ "Vernonia filigera",
    taxon == "Cenchrus nanus" ~ "Pennisetum humile",
    taxon == "Cenchrus uliginosus" ~ "Pennisetum uliginosum",
    taxon == "Silene scottii" ~ "Lychnis scottii",
    taxon == "Trifolium abyssinicum" ~ "Trifolium calocephalum",
    taxon == "Ceropegia aristolochioides" ~ "Ceropegia burgeri",
    taxon == "Afrosciadium abyssinicum" ~ "Peucedanum abyssinicum",
    taxon == "Kleinia polycotoma" ~ "Kleinia gypsophila",
    TRUE ~ taxon  # keep existing value if no match
  ))

# Combined used species with full set of BRAHMS
all_occurrences <- brahms_occurrences %>%
  semi_join(data, by = "taxon")

# Double check that all species are present
length(unique(all_occurrences$taxon))
missing_species <- data %>%
  anti_join(all_occurrences, by = "taxon")

# Remove latitude / longitude at 0,0 
clean_occurrences <- all_occurrences %>%
  filter(!(LAT == 0.000000 & LONG == 0.000000))

length(unique(clean_occurrences$taxon)) 
# Removed Hypercium gnidiifolium so 1 less species because the only coordinate is 0,0

overlap_occ <- st_as_sf(clean_occurrences, 
                        coords = c("LONG", "LAT"), 
                        crs = 4326)

# All layers using the same CRS
st_crs(floristic) <- 4326
floristic <- st_transform(floristic, crs = st_crs(overlap_occ))

# Spatial join to floristic regions
occ_in_floristic <- st_join(overlap_occ, floristic, join = st_intersects)

# Floristic region count per species (remove Eritrea floristic regions)
regions_of_interest <- c("AF", "AR", "BA", "GD", "GG", "GJ", "HA", "IL", "KF", "SD", "SU", "TU", "WG", "WU")

occ_in_floristic <- occ_in_floristic %>%
  filter(ADMIN_NAME %in% regions_of_interest)

occurrence_matrix <- occ_in_floristic %>%
  distinct(taxon, ADMIN_NAME) %>%
  mutate(present = "present") %>%
  pivot_wider(
    names_from = ADMIN_NAME,
    values_from = present,
    values_fill = "not present"
  ) %>%
  mutate(total_flor_regions = rowSums(across(all_of(regions_of_interest)) == "present"))

# Join with data
clean_data <- data %>%
  left_join(occurrence_matrix, by = "taxon") %>% 
  mutate(prediction_binomial = case_when(
    prediction=="under"~1, 
    prediction=="over"~2, 
    prediction=="correct"~0,
    TRUE~NA_integer_
  ))
## clean_data = all species altitude and floristic region totals + prediction binomial - double copy of mapped floristic regions and textual floristic regions

## ---------------------------------------------------------------------------
## Combining floristic region info from text (for mispredicted species only) and mapped (for all species) - checking if the text and mapped are the same, and if not, then adding those together. 
## ---------------------------------------------------------------------------

# List of region columns to process
regions_of_interest

# Merge the two dataframes by taxon
merged_df <- merge(occurrence_matrix, mis_fr, by = "taxon", suffixes = c("_map", "_text"), all.x = TRUE)

# Create a new dataframe to hold the result
combined_df <- merged_df["taxon"]

# Function to apply the logic for combining presence/absence
combine_values <- function(text_val, map_val) {
  if (!is.na(text_val) && text_val == "present") return("present")
  if (!is.na(map_val) && map_val == "present") return("present")
  if (!is.na(text_val)) return(text_val)
  return(map_val)
}

# Loop through each region and apply the combine_values function
for (region in regions_of_interest) {
  text_col <- paste0(region, "_text")
  map_col <- paste0(region, "_map")
  combined_df[[region]] <- mapply(combine_values, merged_df[[text_col]], merged_df[[map_col]])
}

combined_df$total_flor_regions <- rowSums(combined_df[regions_of_interest] == "present", na.rm = TRUE)

clean_data <- data %>% 
  left_join(combined_df, by = "taxon")

clean_data <- clean_data %>% 
  mutate(prediction_binomial = case_when(
    prediction=="under"~1, 
    prediction=="over"~2, 
    prediction=="correct"~0,
    TRUE~NA_integer_
  ))

clean_data <- clean_data %>% 
  select(genus, species, taxon, family, prediction, prediction_binomial, category, confidence, altitude, min_alt, max_alt, mean_alt, alt_range, mean_eoo, mean_aoo, total_flor_regions = total_flor_regions.y, AF = AF.y, AR = AR.y, BA = BA.y, GD = GD.y, GG = GG.y, GJ = GJ.y, HA = HA.y, IL = IL.y, KF = KF.y, SD = SD.y, SU = SU.y, TU = TU.y, WG = WG.y, WU = WU.y)

# clean_data = combined mapped/text floristic regions and all other collected data (altitude, eoo, aoo)