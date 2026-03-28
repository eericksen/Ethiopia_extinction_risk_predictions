## Manipulating dataset
library(tidyverse)

# Mutating some data
clean_data <- clean_data %>% 
  mutate(observed_threat = case_when(
    category %in% c("CR", "EN", "VU") ~  "T", 
    category %in% c("NT", "LC") ~ "NT"
  ))

clean_data <- clean_data %>%
  mutate(threat_binomial = case_when(
    observed_threat == "T" ~ 1,
    observed_threat == "NT" ~ 0
  ))

clean_data <- clean_data %>%
  mutate(predicted_threat = case_when(
    (category %in% c("CR", "EN", "VU") & prediction == "correct") | prediction == "over" ~ "T",
    (category %in% c("NT", "LC") & prediction == "correct") | prediction == "under" ~ "NT",
    TRUE ~ "other"
  ))

clean_data <- clean_data %>%
  mutate(accuracy_binomial = case_when(
    observed_threat == predicted_threat ~ 1,
    observed_threat!= predicted_threat ~0
  ))
clean_data <- clean_data %>% 
  separate(taxon, into = c("genus", "species"), sep = " ")

clean_data <- clean_data %>% 
  mutate(taxon = paste(genus, species))
