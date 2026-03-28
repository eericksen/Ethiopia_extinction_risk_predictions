## --------------------------------------------------------------------------
## Tidying dataset for analysis
## --------------------------------------------------------------------------

# Moving all to one dataset - draft_all_predictors and mispredictions to draft_mis_predictors
draft_all_predictors <- clean_data_updated %>%
  select(taxon, genus, species, family, prediction, predicted_threat, prediction_binomial, accuracy_binomial, category, observed_threat, threat_binomial, confidence, mean_alt, total_flor_regions, mean_eoo, mean_aoo, pa_binomial = presence_binary, protected = presence, II, IV, VI, trop_subtrop_moist_broad = trop_subtrop_moist_broad_recalc, trop_subtrop_grass_sav_shrub = trop_subtrop_grass_sav_shrub_recalc, desert_xeric = desert_recalc, flooded_grass = flood_recalc, montane_grass = montane_grass_recalc, hfp_delta_class1, hfp_delta_class2, hfp_delta_class3, hfp_delta_class3, hfp_delta_class4, hfp_delta_class5, hfp_2009_class1, hfp_2009_class2, hfp_2009_class3, hfp_2009_class4, hfp_2009_class5, hfp_2009_class6, hfp_2009_class7, hfp_2009_class8, hfp_2009_class9, hfp_2009_class10)

# Finalizing dataset
final_predictors <- draft_all_predictors

final_predictors$hfp_quant_1to3 <- final_predictors$hfp_2009_class1 + final_predictors$hfp_2009_class2 + final_predictors$hfp_2009_class3

final_predictors$hfp_quant_1to4 <- final_predictors$hfp_2009_class1 + final_predictors$hfp_2009_class2 + final_predictors$hfp_2009_class3 + final_predictors$hfp_2009_class4

final_predictors$sqrt_eoo <- sqrt(final_predictors$mean_eoo)
final_predictors$sqrt_aoo <- sqrt(final_predictors$mean_aoo)

final_predictors$sqrt_desert <- sqrt(final_predictors$desert_xeric)

final_predictors <- final_predictors %>% 
  mutate(new_pred = case_when(
    (predicted_threat == "T" & observed_threat == "T") ~ "threatened",
    (predicted_threat == "NT" & observed_threat == "NT") ~ "not_threatened",
    (predicted_threat == "T" & observed_threat == "NT") ~ "over",
    (predicted_threat == "NT" & observed_threat == "T") ~ "under"
  ))

# Adding confidence of Bachman predictions (threatened, .lower, .upper)
eth_df <- read.csv("Raw Data/ethiopia_predicted.csv")
eth_df <- eth_df %>% 
  select(taxon = taxon_name, threatened, .lower, .upper)
final_predictors <- final_predictors %>% 
  left_join(eth_df, by = "taxon")

final_predictors_mispred <- final_predictors %>%
  filter(prediction_binomial != 0)

write.csv(final_predictors, "Clean_data.csv", row.names = FALSE)
