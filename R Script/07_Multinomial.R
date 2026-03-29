## --------------------------------------------------------------------------
## Multinomial Analysis
## --------------------------------------------------------------------------

## --------------------------------------------------------------------------
## Exploratory Plots
## --------------------------------------------------------------------------

final_predictors <- read_csv("Clean Data/final_predictors.csv")

# Range Size
plot1 <- ggplot(final_predictors, aes(x = new_pred, y = total_flor_regions, fill = new_pred)) +
  geom_boxplot()
plot2 <- ggplot(final_predictors, aes(x = new_pred, y = sqrt_eoo, fill = new_pred)) +
  geom_boxplot()
plot3 <- ggplot(final_predictors, aes(x = new_pred, y = sqrt_aoo, fill = new_pred)) +
  geom_boxplot()
grid.arrange(plot1, plot2, plot3, ncol = 3)

# Alt
plot4 <- ggplot(final_predictors, aes(x = new_pred, y = mean_alt, fill = new_pred)) +
  geom_boxplot()
plot4

# Protected Area
plot5 <- ggplot(final_predictors, aes(x = new_pred, fill = new_pred)) +
  geom_bar() +
  facet_wrap(~ protected) +
  labs(y = "Count", title = "Protected Status")

plot6 <- ggplot(final_predictors, aes(x = new_pred, fill = new_pred)) +
  geom_bar() +
  facet_wrap(~ II) +
  labs(y = "Count", title = "II")

plot7 <- ggplot(final_predictors, aes(x = new_pred, fill = new_pred)) +
  geom_bar() +
  facet_wrap(~ IV) +
  labs(y = "Count", title = "IV")

plot8 <- ggplot(final_predictors, aes(x = new_pred, fill = new_pred)) +
  geom_bar() +
  facet_wrap(~ VI) +
  labs(y = "Count", title = "VI")
grid.arrange(plot5, plot6, plot7, plot8, nrow = 2, ncol = 2)

# Biome
plot9 <- ggplot(final_predictors, aes(x = new_pred, y = trop_subtrop_moist_broad, fill = new_pred)) +
  geom_boxplot()
plot10 <- ggplot(final_predictors, aes(x = new_pred, y = trop_subtrop_grass_sav_shrub, fill = new_pred)) +
  geom_boxplot()
plot11 <- ggplot(final_predictors, aes(x = new_pred, y = montane_grass, fill = new_pred)) +
  geom_boxplot()
plot12 <- ggplot(final_predictors, aes(x = new_pred, y = sqrt_desert, fill = new_pred)) +
  geom_boxplot()
grid.arrange(plot9, plot10, plot11, plot12, nrow = 2, ncol = 2)

# HFP
plot13 <- ggplot(final_predictors, aes(x = new_pred, y = hfp_delta_class5, fill = new_pred)) +
  geom_boxplot()
plot14 <- ggplot(final_predictors, aes(x = new_pred, y = hfp_quant_1to4, fill = new_pred)) +
  geom_boxplot()
plot15 <- ggplot(final_predictors, aes(x = new_pred, y = hfp_quant_1to3, fill = new_pred)) +
  geom_boxplot()
grid.arrange(plot13, plot14, plot15, ncol = 3)

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, plot11, plot12, plot13, plot14, plot15, nrow=3, ncol=5)

## --------------------------------------------------------------------------
## Final Model
## --------------------------------------------------------------------------

# Use nnet for multinomial model
final_model <- nnet::multinom(new_pred ~ 
                                total_flor_regions + 
                                sqrt_eoo +
                                sqrt_aoo + 
                                protected + 
                                hfp_delta_class5 + 
                                hfp_quant_1to4 + 
                                hfp_quant_1to3,
                              data = final_predictors, 
                              na.action = "na.fail")

# Subset the dredge to only get 1 or NO range size variable or 1 or NO HFP variable 
final_dredge <- dredge(
  final_model,
  subset = 
    (total_flor_regions + sqrt_eoo + sqrt_aoo <= 1) & 
    ((hfp_quant_1to3 + hfp_quant_1to4 + hfp_delta_class5 <= 1))
)
final_dredge

# Cutoff of 6 delta AIC
best_models <- subset(final_dredge, delta < 6)

best_models

# Remove nested models
non_nested <- !nested(best_models)

print(non_nested)

non_nested_models <- get.models(best_models, subset = non_nested)

non_nested_table <- best_models[non_nested, ]

print(non_nested_table)

# Get model info
model_01 <- get.models(non_nested_table, subset = 1)[[1]]
model_02 <- get.models(non_nested_table, subset = 2)[[1]]
model_03 <- get.models(non_nested_table, subset = 3)[[1]]

# Model 1-3 tables
tidy(model_01, conf.int = TRUE, exponentiate = FALSE) %>% 
  arrange(term, y.level) %>% 
  kable() %>% 
  kable_styling("basic", full_width = FALSE)
tidy(model_02, conf.int = TRUE, exponentiate = FALSE) %>% 
  arrange(term, y.level) %>% 
  kable() %>% 
  kable_styling("basic", full_width = FALSE)
tidy(model_03, conf.int = TRUE, exponentiate = FALSE) %>% 
  arrange(term, y.level) %>% 
  kable() %>% 
  kable_styling("basic", full_width = FALSE)

# Check collinearity of model variables
check_collinearity(model_01)
check_collinearity(model_02)
check_collinearity(model_03)

## --------------------------------------------------------------------------
## Floristic region - plotting and effects/reference grid
## --------------------------------------------------------------------------

eff_long_fr <- as.data.frame(Effect("total_flor_regions", model_01, xlevels = list(total_flor_regions = seq(0,10, by = 0.5)))) %>% 
  select(total_flor_regions, starts_with("prob.")) %>% 
  pivot_longer(
    cols         = -total_flor_regions,
    names_to     = "FloristicRegion",
    names_prefix = "prob\\.",
    values_to    = "Probability"
  ) %>%
  mutate(
    FloristicRegion = gsub("\\.", "_", FloristicRegion),
    FloristicRegion = factor(
      FloristicRegion,
      levels = c("over", "not_threatened", "threatened", "under")
    )
  )

ggplot(eff_long_fr, aes(x = total_flor_regions, y = Probability, fill = FloristicRegion)) +
  geom_area(position = "stack") +
  scale_fill_manual(
    name   = "Prediction",
    breaks = c("under", "threatened", "not_threatened", "over"),
    labels = c("Under-predicted", "Correct (threatened)", "Correct (not threatened)", "Over-predicted"),
    values = c(
      under           = "#E9DE9D",
      threatened      = "#8DD3C7",
      not_threatened  = "#5DA899",
      over            = "#C26A77"
    )
  ) +
  guides(fill = guide_legend(title.position = "bottom", title.hjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2), expand = c(0, 0)) +
  labs(
    title = "Predicted Probability by Floristic Region",
    x     = "Total Floristic Regions",
    y     = "Prediction Probability"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    legend.margin   = ggplot2::margin(t = 10, unit = "pt")
  )

get_effect_df <- function(model, model_name, var = "total_flor_regions") {
  eff <- Effect(var, model, xlevels = list(total_flor_regions = seq(0,10, by = 0.5)))
  as.data.frame(eff) %>%
    mutate(model = model_name)
}

# Collect effect data for each model
effects_list <- list(
  get_effect_df(model_01, "model_01"),
  get_effect_df(model_02, "model_02"),
  get_effect_df(model_03, "model_03"))

# Combine into one data frame
all_effects_df <- bind_rows(effects_list)

# Reshape to long format for plotting
long_df <- all_effects_df %>%
  pivot_longer(
    cols        = starts_with("prob."),
    names_to    = "class",
    names_prefix= "prob.",
    values_to   = "prob"
  )

# Add CIs and sort 
long_df_ci <- long_df %>%
  mutate(
    ci_lower = case_when(
      class == "threatened" ~ L.prob.threatened,
      class == "not_threatened" ~ L.prob.not_threatened,
      class == "over"    ~ L.prob.over,
      class == "under"   ~ L.prob.under
    ),
    ci_upper = case_when(
      class == "threatened" ~ U.prob.threatened,
      class == "not_threatened" ~ U.prob.not_threatened,
      class == "over"    ~ U.prob.over,
      class == "under"   ~ U.prob.under
    )
  ) %>%
  arrange(class, model, total_flor_regions)

# Colors and labels
my_cols <- c(
  "#5DA899", "#C26A77", "#0072B2")

my_labs <- c("Model 1", "Model 2", "Model 3")

long_df_ci$class <- factor(long_df_ci$class,
                           levels = c("threatened",
                                      "over",
                                      "under",
                                      "not_threatened"))

# Plot with updated aesthetics
ggplot(long_df_ci,
       aes(x      = total_flor_regions,
           y      = prob,
           colour = model,
           fill   = model,
           group  = model)) +
  geom_ribbon(aes(ymin = ci_lower,
                  ymax = ci_upper),
              alpha  = 0.2,
              colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ class,
             nrow = 2,
             ncol = 2,
             scales = "free_x",
             strip.position = "bottom") +
  labs(title = "Predicted Probabilities by Best Models (Floristic Region)",
       x     = "Total Floristic Regions") +
scale_y_continuous(
  name     = "Predicted Probability",
  breaks   = scales::pretty_breaks(n = 5),
  position = "right"
) +
theme_bw() +
  theme(
    strip.placement    = "outside",
    legend.position    = "bottom"
  ) +
  scale_color_manual(name   = NULL,
                     values = my_cols,
                     labels = my_labs) +
  scale_fill_manual(name   = NULL,
                    values = my_cols,
                    labels = my_labs) +
  scale_x_continuous(breaks = seq(0, 10, by = 2))

eff <- Effect("total_flor_regions", model_01, xlevels = list(total_flor_regions = 1:10))
df_eff_all <- as.data.frame(eff)
write.csv(df_eff_all, "ref_grid_flor.csv", row.names = FALSE)

eff <- Effect("total_flor_regions", model_02, xlevels = list(total_flor_regions = 1:10))
df_eff_all <- as.data.frame(eff)
write.csv(df_eff_all, "ref_grid_flor2.csv", row.names = FALSE)

eff <- Effect("total_flor_regions", model_03, xlevels = list(total_flor_regions = 1:10))
df_eff_all <- as.data.frame(eff)
write.csv(df_eff_all, "ref_grid_flor3.csv", row.names = FALSE)

## --------------------------------------------------------------------------
## HFP Class 1-4 - plotting and effects/reference grid
## --------------------------------------------------------------------------

eff_long_fr <- as.data.frame(Effect("hfp_quant_1to4", model_02, xlevels = list(hfp_quant_1to4 = seq(0,1.0, by = 0.05)))) %>% 
  select(hfp_quant_1to4, starts_with("prob.")) %>% 
  pivot_longer(
    cols         = -hfp_quant_1to4,
    names_to     = "HFP14",
    names_prefix = "prob\\.",
    values_to    = "Probability"
  ) %>%
  mutate(
    HFP14 = gsub("\\.", "_", HFP14),
    HFP14 = factor(
      HFP14,
      levels = c("over", "not_threatened", "threatened", "under")
    )
  )

ggplot(eff_long_fr, aes(hfp_quant_1to4, Probability, fill = HFP14)) +
  geom_area(position = "stack") +
  scale_fill_manual(
    name   = "Prediction",
    breaks = c("under", "threatened", "not_threatened", "over"),
    labels = c("Under-predicted", "Correct (threatened)", "Correct (not threatened)", "Over-predicted"),
    values = c(
      under           = "#E9DE9D",
      threatened      = "#8DD3C7",
      not_threatened  = "#5DA899",
      over            = "#C26A77"
    )
  ) +
guides(fill = guide_legend(
  title.position = "bottom",
  title.hjust = 0.5
)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + 
  scale_x_continuous(breaks = seq(0, 1.0, by = 0.2), expand = c(0, 0)) +
  labs(
    title = "Predicted Probability by Floristic Region",
    x     = "Total Floristic Regions",
    y     = "Prediction Probability"
  ) +
  theme_bw()  +
  theme(
    legend.position     = "bottom",
    legend.box          = "vertical",   
    legend.margin   = ggplot2::margin(t = 10, unit = "pt"),
    legend.title.align  = 0.5
  )+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
  labs(
    title = "Predicted Probability by HFP Class 1-4",
    x     = "Proportional HFP Quantile Class 1-4",
    y     = "Prediction Probability"
  )

get_effect_df <- function(model, model_name, var = "hfp_quant_1to4") {
  eff <- Effect(var, model, xlevels = list(hfp_quant_1to4 = seq(0,1.0, by = 0.05)))
  as.data.frame(eff) %>%
    mutate(model = model_name)
}

# Collect effect data for each model
effects_list <- list(
  get_effect_df(model_02, "model_02"),
  get_effect_df(model_03, "model_03"))

# Combine into one data frame
all_effects_df <- bind_rows(effects_list)

# Reshape to long format for plotting
long_df <- all_effects_df %>%
  pivot_longer(
    cols        = starts_with("prob."),
    names_to    = "class",
    names_prefix= "prob.",
    values_to   = "prob"
  )

# Add CIs and sort 
long_df_ci <- long_df %>%
  mutate(
    ci_lower = case_when(
      class == "threatened" ~ L.prob.threatened,
      class == "not_threatened" ~ L.prob.not_threatened,
      class == "over"    ~ L.prob.over,
      class == "under"   ~ L.prob.under
    ),
    ci_upper = case_when(
      class == "threatened" ~ U.prob.threatened,
      class == "not_threatened" ~ U.prob.not_threatened,
      class == "over"    ~ U.prob.over,
      class == "under"   ~ U.prob.under
    )
  ) %>%
  arrange(class, model, hfp_quant_1to4)

# Colors and labels
my_cols <- c(
  "#C26A77", "#0072B2"
)
my_labs <- c("Model 2", "Model 3")

long_df_ci$class <- factor(long_df_ci$class,
                           levels = c("threatened",
                                      "over",
                                      "under",
                                      "not_threatened"))

# Plot with updated aesthetics
ggplot(long_df_ci,
       aes(x      = hfp_quant_1to4,
           y      = prob,
           colour = model,
           fill   = model,
           group  = model)) +
  geom_ribbon(aes(ymin = ci_lower,
                  ymax = ci_upper),
              alpha  = 0.2,
              colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ class,
             nrow = 2,          
             ncol = 2,          
             scales = "free_x",
             strip.position = "bottom") +
  scale_y_continuous(
    name     = "Predicted Probability",
    breaks   = scales::pretty_breaks(n = 5),
    position = "right"
  ) +
  labs(title = "Predicted Probabilities by Best Models (HFP 1 to 3)",
       x = "Total Floristic Regions",
       y = "Predicted Probability") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.placement = "outside") +
  scale_color_manual(name   = NULL,
                     values = my_cols,
                     labels = my_labs) +
  scale_fill_manual(name   = NULL,
                    values = my_cols,
                    labels = my_labs)

eff <- Effect("hfp_quant_1to4", model_02, xlevels = list(hfp_quant_1to4 = seq(0.1,1.0, by = 0.1)))
df_eff_all <- as.data.frame(eff)
write.csv(df_eff_all, "ref_grid_1to4.csv", row.names = FALSE)

eff <- Effect("hfp_quant_1to4", model_03, xlevels = list(hfp_quant_1to4 = seq(0.1,1.0, by = 0.1)))
df_eff_all <- as.data.frame(eff)
write.csv(df_eff_all, "ref_grid_1to4_2.csv", row.names = FALSE)

## --------------------------------------------------------------------------
## HFP Delta Class 5 - plotting and effects/reference grid
## --------------------------------------------------------------------------

eff_long_fr <- as.data.frame(Effect("hfp_delta_class5", model_01, xlevels = list(hfp_delta_class5 = seq(0,1.0, by = 0.05)))) %>% 
  select(hfp_delta_class5, starts_with("prob.")) %>% 
  pivot_longer(
    cols         = -hfp_delta_class5,
    names_to     = "HFP5",
    names_prefix = "prob\\.",
    values_to    = "Probability"
  ) %>%
  mutate(
    HFP5 = gsub("\\.", "_", HFP5),
    HFP5 = factor(
      HFP5,
      levels = c("over", "not_threatened", "threatened", "under")
    )
  )

ggplot(eff_long_fr, aes(hfp_delta_class5, Probability, fill = HFP5)) +
  geom_area(position = "stack") +
  scale_fill_manual(
    name   = "Prediction",
    breaks = c("under", "threatened", "not_threatened", "over"),
    labels = c("Under-predicted", "Correct (threatened)", "Correct (not threatened)", "Over-predicted"),
    values = c(
      under           = "#E9DE9D",
      threatened      = "#8DD3C7",
      not_threatened  = "#5DA899",
      over            = "#C26A77"
    )
  ) +
  guides(fill = guide_legend(
    title.position = "bottom",
    title.hjust = 0.5
  )) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + 
  scale_x_continuous(breaks = seq(0, 1.0, by = 0.2), expand = c(0, 0)) +
  labs(
    title = "Predicted Probability by Floristic Region",
    x     = "Proportional HFP Delta Class 5",
    y     = "Prediction Probability"
  ) +
  theme_bw()  +
  theme(
    legend.position     = "bottom",
    legend.box          = "vertical",   
    legend.title.align  = 0.5          
  )



get_effect_df <- function(model, model_name, var = "hfp_delta_class5") {
  eff <- Effect(var, model, xlevels = list(hfp_delta_class5 = seq(0,1.0, by = 0.05)))
  as.data.frame(eff) %>%
    mutate(model = model_name)
}

# Collect effect data for each model
effects_list <- list(
  get_effect_df(model_01, "model_01"))

# Combine into one data frame
all_effects_df <- bind_rows(effects_list)

# Reshape to long format for plotting
long_df <- all_effects_df %>%
  pivot_longer(
    cols        = starts_with("prob."),
    names_to    = "class",
    names_prefix= "prob.",
    values_to   = "prob"
  )

# Add CIs and sort 
long_df_ci <- long_df %>%
  mutate(
    ci_lower = case_when(
      class == "threatened" ~ L.prob.threatened,
      class == "not_threatened" ~ L.prob.not_threatened,
      class == "over"    ~ L.prob.over,
      class == "under"   ~ L.prob.under
    ),
    ci_upper = case_when(
      class == "threatened" ~ U.prob.threatened,
      class == "not_threatened" ~ U.prob.not_threatened,
      class == "over"    ~ U.prob.over,
      class == "under"   ~ U.prob.under
    )
  ) %>%
  arrange(class, model, hfp_delta_class5)

# Colors and labels
my_cols <- c(
  "#5DA899"
)
my_labs <- c("Model 1")

long_df_ci$class <- factor(long_df_ci$class,
                           levels = c("threatened",
                                      "over",
                                      "under",
                                      "not_threatened"))

# Plot with updated aesthetics
ggplot(long_df_ci,
       aes(x      = hfp_delta_class5,
           y      = prob,
           colour = model,
           fill   = model,
           group  = model)) +
  geom_ribbon(aes(ymin = ci_lower,
                  ymax = ci_upper),
              alpha  = 0.2,
              colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ class,
             nrow = 2,          
             ncol = 2,          
             scales = "free_x",
             strip.position = "bottom") +
  scale_y_continuous(
    name     = "Predicted Probability",
    breaks   = scales::pretty_breaks(n = 5),
    position = "right"
  ) +
  labs(title = "Predicted Probabilities by Best Models (Delta 5)",
       x = "Total Floristic Regions",
       y = "Predicted Probability") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.placement = "outside") +
  scale_color_manual(name   = NULL,
                     values = my_cols,
                     labels = my_labs) +
  scale_fill_manual(name   = NULL,
                    values = my_cols,
                    labels = my_labs)

eff <- Effect("hfp_delta_class5", model_01, xlevels = list(hfp_delta_class5 = seq(0.1,1.0, by = 0.1)))
df_eff_all <- as.data.frame(eff)
write.csv(df_eff_all, "ref_grid_delta5.csv", row.names = FALSE)

## --------------------------------------------------------------------------
## Protected Area - plotting and effects/reference grid
## --------------------------------------------------------------------------

eff_long_fr <- as.data.frame(Effect("protected", model_02)) %>% 
  select(protected, starts_with("prob.")) %>% 
  pivot_longer(
    cols         = -protected,
    names_to     = "PA",
    names_prefix = "prob\\.",
    values_to    = "Probability"
  ) %>%
  mutate(
    PA = gsub("\\.", "_", PA),
    PA = factor(
      PA,
      levels = c("over", "not_threatened", "threatened", "under")
    )
  )

eff_long_fr$PA <- factor(
  eff_long_fr$PA,
  levels = c("over", "not_threatened", "threatened", "under")
)

ggplot(eff_long_fr,
       aes(x     = protected,
           y     = Probability,
           fill  = PA,
           group = PA)) +
  
  geom_area(position = "stack") +
  

  geom_line(aes(colour = PA),
            position = position_stack(),
            size     = 1.2) +

  scale_fill_manual(
    name   = "Prediction",
    breaks = c("under", "threatened", "not_threatened", "over"),
    labels = c(
      "Under-predicted",         
      "Correct (threatened)",
      "Correct (not threatened)",
      "Over-predicted"          
    ),
    values = pal
  ) +
  

  scale_colour_manual(
    name   = "Prediction",
    breaks = c("under", "threatened", "not_threatened", "over"),
    labels = c(
      "Under-predicted",        
      "Correct (threatened)",
      "Correct (not threatened)",
      "Over-predicted"          
    ),
    values = pal
  ) +
  
  guides(
    fill   = guide_legend(title.position = "bottom",
                          title.hjust     = 0.5),
    colour = guide_legend(title.position = "bottom",
                          title.hjust     = 0.5)
  ) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
  labs(
    title = "Predicted Probability by Floristic Region",
    x     = "Presence in Protected Area",
    y     = "Prediction Probability"
  ) +
  theme_bw() +
  theme(
    legend.position    = "bottom",
    legend.box         = "horizontal",
    legend.title.align = 0.5
  )

get_effect_df <- function(model, model_name, var = "protected") {
  eff <- Effect(var, model)
  as.data.frame(eff) %>%
    mutate(model = model_name)
}

# Collect effect data for each model
effects_list <- list(
  get_effect_df(model_02, "model_02")
  )

# Combine into one data frame
all_effects_df <- bind_rows(effects_list)

# Reshape to long format for plotting
long_df <- all_effects_df %>%
  pivot_longer(
    cols        = starts_with("prob."),
    names_to    = "class",
    names_prefix= "prob.",
    values_to   = "prob"
  )

# Add CIs and sort
long_df_ci <- long_df %>%
  mutate(
    ci_lower = case_when(
      class == "threatened" ~ L.prob.threatened,
      class == "not_threatened" ~ L.prob.not_threatened,
      class == "over"    ~ L.prob.over,
      class == "under"   ~ L.prob.under
    ),
    ci_upper = case_when(
      class == "threatened" ~ U.prob.threatened,
      class == "not_threatened" ~ U.prob.not_threatened,
      class == "over"    ~ U.prob.over,
      class == "under"   ~ U.prob.under
    )
  ) %>%
  arrange(class, model, protected)

# Colors and labels
my_cols <- c(
  "#C26A77"
)
my_labs <- c("Model 2")

long_df_ci$class <- factor(long_df_ci$class,
                           levels = c("threatened",
                                      "over",
                                      "under",
                                      "not_threatened"))

# Plot with updated aesthetics
ggplot(long_df_ci,
       aes(x     = protected,
           y     = prob,
           colour = model,
           group  = model)) +
  geom_line(size     = 1,   linetype = "dashed") +
  geom_point(size    = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.05) +
  scale_colour_manual(values = my_cols, labels = my_labs, name = NULL) +
  scale_y_continuous(name     = "Predicted Probability",
                     breaks   = scales::pretty_breaks(n = 5),
                     position = "right") +
  labs(title = "Predicted Probability by Protected Area",
       x     = "Protected") +
  facet_wrap(~ class, nrow = 2, strip.position = "bottom") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_blank(),
    legend.position  = "bottom"
  )

eff <- Effect("protected", model_02)
df_eff_all <- as.data.frame(eff)
write.csv(df_eff_all, "ref_grid_pa.csv", row.names = FALSE)