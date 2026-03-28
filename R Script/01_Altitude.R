## --------------------------------------------------------------------------
## Altitutde data
## --------------------------------------------------------------------------

# Load in dataset and organize table
data <- read.csv("Raw Data/all_eth_data.csv", header = TRUE)

altitude <- data %>%
  select(taxon, genus, species, family, prediction, category, min_alt, max_alt, alt_range, mean_alt)

# Convert predictions to numbers (0-2)
altitude <- altitude %>% 
  mutate(prediction_binomial = case_when(
    prediction=="under"~1, 
    prediction=="over"~2, 
    prediction=="correct"~0,
    TRUE~NA_integer_
  ))

# Plot boxplots of mean altitude, max altitude, and altitude range 
ggplot(altitude, aes(x = prediction, y = mean_alt, fill = prediction)) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Altitude Mean by Misprediction Type",
       x = "Misprediction Type", y = "Altitude")

ggplot(altitude, aes(x = prediction, y = max_alt, fill = prediction)) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Altitude Max by Misprediction Type",
       x = "Misprediction Type", y = "Altitude")

ggplot(altitude, aes(x = prediction, y = alt_range, fill = prediction)) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Altitude Range by Misprediction Type",
       x = "Misprediction Type", y = "Altitude")

# Mean and max of all species
ggplot(altitude, aes(x = fct_reorder(taxon, mean_alt))) +
  geom_linerange(aes(ymin = mean_alt, ymax = max_alt), linetype = 3, color = "black") + 
  geom_point(aes(y = mean_alt), size = 2, color = "black") + 
  geom_point(aes(y = max_alt), size = 2, color = "red") + 
  theme_bw() + 
  labs(x = NULL, y = "Altitude (m)") + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

## ---------------------------------------------------------------------------
## GLM for Altitude
## ---------------------------------------------------------------------------

# Get just mispredicted species
mis_alt <- data %>% 
  filter(prediction %in% c("under", "over")) %>% 
  select(genus, species, taxon, family, prediction, min_alt, max_alt, mean_alt, alt_range, prediction_binomial)

# GLM for maximum altitude
alt_1 <- glm(prediction_binomial ~ 1 + max_alt, data = mis_alt, family = "binomial")

summary(alt_1)

plot_model(alt_1, type="pred", terms="max_alt")

# GLM for range in altitude
alt_2 <- glm(prediction_binomial ~ 1 + alt_range, data = mis_alt, family = "binomial")

summary(alt_2)

plot_model(alt_2, type="pred", terms="alt_range")

# GLM for mean altitude
alt_3 <- glm(prediction_binomial ~ 1 + mean_alt, data = mis_alt, family = "binomial")

summary(alt_3)

plot_model(alt_3, type="pred", terms="mean_alt")

