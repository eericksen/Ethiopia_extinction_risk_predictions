## --------------------------------------------------------------------------
## Summing Biome and HFP recalculations
## --------------------------------------------------------------------------

# For each floristic region a species is present in, add areas together for biome / HFP. Create a column with that value. Then add a column to get the proportion of area that species covers. 

# Biome and HFP calculations were completed using the method in Bachman et al. 2024, replacing botanical country with floristic region. 

## --------------------------------------------------------------------------
## BIOMES
## --------------------------------------------------------------------------

biome_composition_nolake_df <- read_csv("Predictor Recalculation/biome_composition.csv")

# Reshape species_presence to long format
pivot_fr <- clean_data %>%
  dplyr::select(taxon, AF, AR, BA, GD, GG, GJ, HA, IL, KF, SD, SU, TU, WG, WU) %>%
  mutate(across(-taxon, ~ ifelse(.x == "present", 1, 0)))%>%
  pivot_longer(cols = -taxon, names_to = "region_code", values_to = "presence") %>%
  filter(presence == 1)

biome_areas_nolake <- dplyr::select(
  biome_composition_nolake_df,
  `Tropical and subtropical moist broadleaf forests`, 
  `Tropical and subtropical grasslands, savannas, and shrublands`, 
  `Deserts and xeric shrublands`, 
  `Flooded grasslands and savannas`, 
  `Montane grasslands and shrublands`, 
  region_code = region_id
)
# Join with biome area data
presence_with_biomes_nolake <- pivot_fr %>%
  left_join(biome_areas_nolake, by = "region_code")

# Sum biome areas by species
biome_cols <- c("Tropical and subtropical moist broadleaf forests", "Tropical and subtropical grasslands, savannas, and shrublands", "Deserts and xeric shrublands", "Flooded grasslands and savannas", "Montane grasslands and shrublands")

biome_sum_by_species_nolake <- presence_with_biomes_nolake %>%
  group_by(taxon) %>%
  summarise(across(all_of(biome_cols), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"))

# Biome proportions
biome_proportions_nolake <- biome_sum_by_species_nolake %>%
  rowwise() %>%
  mutate(total_area = sum(c_across(starts_with("sum_")))) %>%
  mutate(across(starts_with("sum_"), ~ .x / total_area, .names = "prop_{.col}")) %>%
  ungroup() %>%
  dplyr::select(taxon, starts_with("prop_")) %>%
  rename(
    trop_subtrop_moist_broad_recalc = `prop_sum_Tropical and subtropical moist broadleaf forests`,
    trop_subtrop_grass_sav_shrub_recalc = `prop_sum_Tropical and subtropical grasslands, savannas, and shrublands`,
    desert_recalc = `prop_sum_Deserts and xeric shrublands`,
    flood_recalc = `prop_sum_Flooded grasslands and savannas`,
    montane_grass_recalc = `prop_sum_Montane grasslands and shrublands`
  )

clean_data_updated <- clean_data %>%
  left_join(biome_proportions_nolake, by = "taxon")

## --------------------------------------------------------------------------
## DELTA HFP
## --------------------------------------------------------------------------

# Load Delta HFP
region_composition_df <- read_csv("Predictor Recalculation/hfpdelta_composition.csv")

hfp_delta_names <- region_composition_df %>%
  select(hfp_delta_class1, hfp_delta_class2, hfp_delta_class3, hfp_delta_class4, hfp_delta_class5, region_code = region_id)

# Join with delta HFP data
presence_with_delta_hfp <- pivot_fr %>%
  left_join(hfp_delta_names, by = "region_code")

# Sum delta HFP by species
delta_cols <- c("hfp_delta_class1", "hfp_delta_class2", "hfp_delta_class3", "hfp_delta_class4", "hfp_delta_class5")

delta_sum_by_species <- presence_with_delta_hfp %>%
  group_by(taxon) %>%
  summarise(across(all_of(delta_cols), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"))

# Delta HFP proportions
delta_proportions <- delta_sum_by_species %>%
  rowwise() %>%
  mutate(total_area = sum(c_across(starts_with("sum_")))) %>%
  mutate(across(starts_with("sum_"), ~ .x / total_area, .names = "prop_{.col}")) %>%
  ungroup() %>%
  select(taxon, starts_with("prop_"))

clean_data_updated <- clean_data_updated %>%
  left_join(delta_proportions, by = "taxon")

clean_data_updated <- clean_data_updated %>%
  rename(hfp_delta_class1 = prop_sum_hfp_delta_class1, hfp_delta_class2 = prop_sum_hfp_delta_class2, hfp_delta_class3 = prop_sum_hfp_delta_class3, hfp_delta_class4 = prop_sum_hfp_delta_class4, hfp_delta_class5 = prop_sum_hfp_delta_class5)

## --------------------------------------------------------------------------
## QUANTILE HFP
## --------------------------------------------------------------------------

# Load Quantile HFP
hfp_quantile_nolake <- read.csv("Predictor Recalculation/hfp2009_composition.csv")

hfp_quantile_updated <- hfp_quantile_nolake %>%
  rename(region_code = region_id)

presence_with_quantile_hfp <- pivot_fr %>%
  left_join(hfp_quantile_updated, by = "region_code")

# Sum quantile HFP by species
quantile_cols <- c("hfp_2009_class1", "hfp_2009_class2", "hfp_2009_class3", "hfp_2009_class4", "hfp_2009_class5", "hfp_2009_class6", "hfp_2009_class7", "hfp_2009_class8", "hfp_2009_class9", "hfp_2009_class10")

quantile_sum_by_species <- presence_with_quantile_hfp %>%
  group_by(taxon) %>%
  summarise(across(all_of(quantile_cols), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"))

row_sum <- hfp_quantile_nolake %>%
  filter(region_id == "GG") %>%
  select(where(is.numeric)) %>%
  rowSums(na.rm = TRUE) %>%
  sum()  # This sums in case there are multiple rows

print(row_sum)

# Quantile HFP proportions
quantile_proportions <- quantile_sum_by_species %>%
  rowwise() %>%
  mutate(total_area = sum(c_across(starts_with("sum_")))) %>%
  mutate(across(starts_with("sum_"), ~ .x / total_area, .names = "prop_{.col}")) %>%
  ungroup() %>%
  select(taxon, starts_with("prop_"))

clean_data_updated <- clean_data_updated %>%
  left_join(quantile_proportions, by = "taxon")

clean_data_updated <- clean_data_updated %>%
  rename(hfp_2009_class1 = prop_sum_hfp_2009_class1, hfp_2009_class2 = prop_sum_hfp_2009_class2, hfp_2009_class3 = prop_sum_hfp_2009_class3, hfp_2009_class4 = prop_sum_hfp_2009_class4, hfp_2009_class5 = prop_sum_hfp_2009_class5, hfp_2009_class6 = prop_sum_hfp_2009_class6, hfp_2009_class7 = prop_sum_hfp_2009_class7, hfp_2009_class8 = prop_sum_hfp_2009_class8, hfp_2009_class9 = prop_sum_hfp_2009_class9, hfp_2009_class10 = prop_sum_hfp_2009_class10)

# clean_data_updated = taxon, genus, species, family, predicion, prediction_binomial, category, confidence, altitude, min_alt, max_alt, mean_alt, alt_range, mean_eoo, mean_aoo, total_flor_regions, presence/absence in all floristic regions, presence/absence in protected area, presence_binary in protected area, IUCN category presence (II, IV, VI), proportion of range in biomes, proportion of range in delta hfp classes 1-5, proportion of range in quantile hfp classes 1-10