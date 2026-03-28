## --------------------------------------------------------------------------
## GADM Regions 
## --------------------------------------------------------------------------

# Load in spatial data
gadm_1 <- st_read("Shapefiles/GADM/gadm41_ETH_1.shp")
gadm_2 <- st_read("Shapefiles/GADM/gadm41_ETH_2.shp")
gadm_3 <- st_read("Shapefiles/GADM/gadm41_ETH_3.shp")
all_occurrences <- read.csv("Raw Data/Ethiopian endemics full BRAHMS download(Manual_georeferencing_data)(all).csv", fileEncoding = "latin1")

# Combine genus and species to make a taxon list, change the names of a few on the Jack to match these on BRAHMS then revert back to list from Jack.

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
st_crs(gadm_2) <- 4326
gadm_1 <- st_transform(gadm_1, crs = st_crs(overlap_occ))
gadm_2 <- st_transform(gadm_2, crs = st_crs(overlap_occ))
gadm_3 <- st_transform(gadm_3, crs = st_crs(overlap_occ))


occ_in_gadm_1 <- st_join(overlap_occ, gadm_1, join = st_intersects)
occ_in_gadm_2 <- st_join(overlap_occ, gadm_2, join = st_intersects)
occ_in_gadm_3 <- st_join(overlap_occ, gadm_3, join = st_intersects)


gadm1_regions <- gadm_1$NAME_1
gadm2_regions <- gadm_2$NAME_2
gadm3_regions <- gadm_3$NAME_3

occ_in_gadm_1 <- occ_in_gadm_1 %>% 
  filter(NAME_1 %in% gadm1_regions)
occ_in_gadm_2 <- occ_in_gadm_2 %>%
  filter(NAME_2 %in% gadm2_regions)
occ_in_gadm_3 <- occ_in_gadm_3 %>% 
  filter(NAME_3 %in% gadm3_regions)

occurrence_matrix_gadm1 <- occ_in_gadm_1 %>%
  distinct(taxon, NAME_1) %>%
  mutate(present = "present") %>%
  pivot_wider(
    names_from = NAME_1,
    values_from = present,
    values_fill = "not present"
  ) %>%
  mutate(total_gadm_1_regions = rowSums(across(all_of(gadm1_regions)) == "present"))
gadm_df1 <- occurrence_matrix_gadm1 %>% 
  select(taxon, total_gadm_1_regions)

gadmd2_regions <- unique(occ_in_gadm_2$NAME_2)
occurrence_matrix_gadm2 <- occ_in_gadm_2 %>%
  distinct(taxon, NAME_2) %>%
  mutate(present = "present") %>%
  pivot_wider(
    names_from = NAME_2,
    values_from = present,
    values_fill = "not present"
  ) %>%   
  mutate(total_gadm_2_regions = rowSums(across(all_of(gadmd2_regions)) == "present"))
gadm_df2 <- occurrence_matrix_gadm2 %>% 
  select(taxon, total_gadm_2_regions)

occurrence_matrix_gadm3 <- occ_in_gadm_3 %>%
  distinct(taxon, NAME_3) %>%
  mutate(present = "present") %>%
  pivot_wider(
    names_from = NAME_3,
    values_from = present,
    values_fill = "not present"
  ) %>%
  mutate(total_gadm_3_regions = rowSums(across(-taxon) == "present"))

gadmd3_regions <- unique(occ_in_gadm_3$NAME_3)
gadm_df3 <- occurrence_matrix_gadm3 %>% 
  select(taxon, total_gadm_3_regions)

gadm_df <- merge(gadm_df1, gadm_df2, by = "taxon")
gadm_df <- merge(gadm_df, gadm_df3, by = "taxon")
gadm_df <- merge(gadm_df, final_predictors, by = "taxon")

mispred <- data %>% 
  select(taxon, prediction)

gadm_df <- merge(gadm_df, mispred, by = "taxon")

gadmn_multinom <- nnet::multinom(prediction.x ~ total_gadm_1_regions + total_gadm_2_regions + total_gadm_3_regions + sqrt_eoo + sqrt_aoo + total_flor_regions, data = gadm_df, na.action = "na.fail")

gadm_dredge <- dredge(
  gadmn_multinom,
  subset = (total_gadm_1_regions + total_gadm_2_regions + total_gadm_3_regions + sqrt_eoo + sqrt_aoo + total_flor_regions == 1))
gadm_dredge
