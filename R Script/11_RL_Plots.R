## --------------------------------------------------------------------------
## Red List Categories & Mispredictions Plot - Supporting Information Plot
## --------------------------------------------------------------------------

### orchid assessments
library(tidyverse)
library(readxl)
library(rWCVP)
library(sf)
library(scales)
library(biscale)
library(cowplot)
library(ggblend)
library(imager)
library(data.table)
library(glue)
library(gt)
library(ggdist); theme_set(theme_ggdist())
options(ggblend.check_blend = FALSE)

setwd(this.path::here())

source("custom_bivariate_palette_functions.r")

threatened_cats <- c( "VU", "EN","CR")
nonthreatened_cats<- c("LC", "NT")

fancy_plot <- final_predictors %>% 
  mutate(true_cat = case_when(
    category %in% threatened_cats ~ "Threatened (assessed)",
    category %in% nonthreatened_cats~"Not threatened (assessed)"))
fancy_plot <- fancy_plot %>% 
  mutate(likely_threatened_conf = case_when(
    predicted_threat == "T" & confidence=="confident"~"Threatened (high confidence)",
    predicted_threat == "NT"& confidence=="confident"~"Not threatened (high confidence)",
    predicted_threat == "T" & confidence=="low_confidence"~"Threatened (low confidence)",
    predicted_threat == "NT"& confidence=="low_confidence"~"Not threatened (low confidence)",
    is.na(predicted_threat)~NA_character_
  )
  )

# SUMMARIES ####

fancy_plot %>% count(likely_threatened_conf) %>% gt()

n <- fancy_plot %>% count(likely_threatened_conf) %>% pull(n)
#1952 missing preds
#3907 low confidence predictions

pal_cat <- c("LC"="#8ccba7",
             "NT" = "#c8e7af",
             "DD" = "gray80",
             "VU" =  "#fee3ba",
             "EN" = "#f9b6a1",
             "CR" = "#eb9793",
             "Threatened (assessed)" = "#bc322b",
             "Threatened (high confidence)" = "#df7872",
             "Threatened (low confidence)" = "#e6aeac",
             #"Predicted threatened \n(uncertain)" = "#e6c0be",
             # "Predicted not threatened \n(uncertain)" = "#bed1e6" ,
             "Not threatened (low confidence)" = "#acc7e6", 
             "Not threatened (high confidence)" = "#76a7e0",
             "Not threatened (assessed)" = "#2b6dbb",
             "No estimate available" = "gray90")                     

fancy_plot %>%
  count(likely_threatened_conf) %>%
  mutate(
    prop    = n / sum(n),
    threat  = factor(
      replace_na(likely_threatened_conf, "No estimate available"),
      levels = c(
        "Not threatened (high confidence)",
        "Not threatened (low confidence)",
        "Threatened (low confidence)",
        "Threatened (high confidence)"
      )
    )
  ) %>%
  arrange(threat) %>%
  mutate(
    cumprop = cumsum(prop),
    text1   = c("Not Threatened", "Not threatened",   
                "Threatened",    "Threatened"), 
    text2   = c("high confidence", "low confidence", "low confidence","high confidence"),
    text = gsub("<br><span style='font-size: 9pt'>*()*</span>", "", fixed=TRUE,
                paste0(
                  "<span style='font-size: 11pt'>",
                  text1, 
                  "</span><br><span style='font-size: 9pt'>*(",
                  text2, 
                  ")*</span>"))
  ) %>%
  ggplot(aes(x = 1, y = prop)) +
  coord_cartesian(expand=FALSE, xlim=c(0.9, 1.6))+
  geom_col(aes(fill=threat), width = 0.2) +
  geom_segment(data=labels, aes(y=y, x=0.9,xend=1.105))+
  geom_segment(data=labels, aes(y=y, yend=y+c(0, -0.01, 0, 0.05),x=1.105, xend=1.12))+
  ggtext::geom_richtext(data=labels, aes(y=y, x=1.12, label=label),
                          lineheight = 0.6,  
                          label.padding = unit(c(0.3, 0.4, 0.2, 0.3), "lines"),
                          vjust=c(0.5, 1, 0.25, -0.8), hjust=0)+
  ggtext::geom_richtext(aes(label=text), y=c(0.97, 0.88, 0.78, 0.65),  
                          fill = NA, label.color = NA)+
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = .1),
    limits = c(0, 1.02)
  ) +
  scale_fill_manual(values = pal_cat, guide = "none") +
  coord_cartesian(expand = FALSE, xlim = c(0.9, 1.6)) +
  theme_bw() +
  theme(
    axis.ticks     = element_blank(),
    axis.text.x    = element_blank(),
    panel.grid     = element_blank(),
    panel.border   = element_blank()
  ) +
  ylab(NULL)

ggsave("threat_prop_breakdown.pdf", width=6.5, height=9)

p_preds <- 
  fancy_plot %>% 
  filter(is.na(observed_threat), !is.na(predicted_threat)) %>% 
  mutate(threat = factor(
                         levels=c(
                                  "Not threatened (high confidence)" ,
                                  "Not threatened (low confidence)",
                                  "Threatened (low confidence)",
                                  "Threatened (high confidence)")),
         y = case_when(confidence=="low_confidence"~"Not threatened (low confidence)",
                       predicted_threat=="threatened"~"Threatened (high confidence)",
                       predicted_threat=="not threatened"~"Not threatened (high confidence)")) %>% 
  ggplot(aes(y=factor(y), x=threatened))+
  stat_slab(data=. %>% filter(y=="Not threatened (high confidence)"),
            aes(fill=y, colour=y),  height=0.8)+
  stat_slab(data=. %>% filter(y=="Not threatened (low confidence)"),
            aes(fill=y, colour=y), height=0.5)+
  stat_slab(data=. %>% filter(y=="Threatened (high confidence)"),
            aes(fill=y, colour=y), height=2)+
  scale_fill_manual(values = pal_cat, guide="none")+
  scale_colour_manual(values = colorspace::darken(pal_cat, 0.2), guide="none")+
  ggnewscale::new_scale_colour()+
  ggnewscale::new_scale_fill()+
  stat_slab(data=. %>% filter(y=="Not threatened (low confidence)"),
            aes(fill = after_stat(x > 0.433), colour=after_stat(x > 0.433)),
            height=0.5)+
  scale_fill_manual(values=c("transparent", "#e6aeac"), guide="none")+ 
  scale_colour_manual(values=c("transparent", colorspace::darken("#e6aeac", 0.2)), guide="none")+ 
  stat_pointinterval()+
  geom_vline(xintercept=0.433, linetype="dashed", colour="gray40")+
  scale_x_continuous(expand=c(0,0),limits=c(-0.0001, 1.00001), breaks= c(0, 0.25, 0.5, 0.75, 1))+
  ylab(NULL)+xlab("Predicted probability threatened")+
  theme(panel.grid.major.y = element_line(colour="gray80"))

preds_labels <- data.frame(
  x=c(0.15, 0.433-0.012, 0.433+0.012, 0.75),
  y =c(1,2,2,3),
  likely_threatened_conf = c("Not threatened (high confidence)" ,
                             "Not threatened (low confidence)",
                             "Threatened (low confidence)",
                             "Threatened (high confidence)")
  
) %>% 
  left_join(fancy_plot %>% count(likely_threatened_conf))


p_preds+
  geom_text(data=preds_labels, 
            aes(x=x, y=y, 
                label=paste0(gsub(" (", "\n(", likely_threatened_conf, fixed=TRUE), "\nn = ",n)),
            vjust=1.2, hjust=c(0.5, 1, 0, 0.5))+ylab(NULL)+
  theme(axis.text.y = element_blank())

ggsave("preds_dist.pdf", height=5, width=4)


#### CATEGORY PREDICTIONS PLOTS ####
dd_sp <- read.csv("eth_assessed_01.csv")
fancy_plot <- fancy_plot %>% 
  select(taxon, genus, species, family, predicted_threat, confidence, threatened, .lower, .upper, category)
dd_sp <- dd_sp %>% 
  rename(category = Category)
dd_sp <- dd_sp %>% 
  select(taxon, genus, species, family, predicted_threat, confidence, threatened, .lower, .upper, category)
fancy_plot <- bind_rows(fancy_plot, dd_sp)
fancy_plot[63, "threatened"] <- 0.606719199
fancy_plot[63, ".lower"] <- 0.530067161
fancy_plot[63, ".upper"] <- 0.686960551
fancy_plot[65, "threatened"] <- 0.006806115
fancy_plot[65, ".lower"] <- 0.002710184
fancy_plot[65, ".upper"] <- 0.012141702
fancy_plot[241, "threatened"] <- 0.170516442
fancy_plot[241, ".lower"] <- 0.097281591
fancy_plot[241, ".upper"] <- 0.266723841
fancy_plot[294, "threatened"] <- 0.435828698
fancy_plot[294, ".lower"] <- 0.36906538
fancy_plot[294, ".upper"] <- 0.52373149
fancy_plot[377, "threatened"] <- 0.291052428
fancy_plot[377, ".lower"] <- 0.219228528
fancy_plot[377, ".upper"] <- 0.365651001
fancy_plot[395, "threatened"] <- 0.468489208
fancy_plot[395, ".lower"] <- 0.404214562
fancy_plot[395, ".upper"] <- 0.548056904
fancy_plot[288, "threatened"] <- 0.397774402
fancy_plot[288, ".lower"] <- 0.333182526
fancy_plot[288, ".upper"] <- 0.477751719
fancy_plot[289, "threatened"] <- 0.397774402
fancy_plot[289, ".lower"] <- 0.333182526
fancy_plot[289, ".upper"] <- 0.477751719

fancy_plot <- fancy_plot %>% 
  mutate(predicted_threat = case_when(
    predicted_threat == "threatened" ~ "T",
    predicted_threat == "not threatened" ~ "NT",
    predicted_threat == "T" ~ "T",
    predicted_threat == "NT" ~ "NT"
  ))
fancy_plot <- fancy_plot %>% 
  distinct(taxon, .keep_all = TRUE)

p_pred_cats <- fancy_plot %>% 
  ggplot(aes(y=factor(category, levels=names(pal_cat)), x=threatened, fill=category))+
  geom_vline(xintercept=0.433, linetype="dashed", colour="gray40")+
  stat_slab()+
  scale_fill_manual(values = pal_cat, guide="none")+
  scale_colour_manual(values = colorspace::darken(pal_cat, 0.2), guide="none")+
  ggnewscale::new_scale_fill()+
  stat_slab(data=. %>% filter(category %in% nonthreatened_cats),
            aes(fill = after_stat(x > 0.433)))+
  stat_slab(data=. %>% filter(category %in% threatened_cats),
            aes(fill = after_stat(x < 0.433)))+
  stat_slab(aes(colour=category), fill=NA, linewidth=0.6)+
  scale_fill_manual(values=c("transparent", "white"), guide="none")+ 
  stat_pointinterval()+
  scale_thickness_shared()+
  scale_x_continuous(expand=c(0,0),limits=c(-0.0001, 1.5), breaks= c(0, 0.25, 0.5, 0.75, 1))+
  ylab(NULL)+xlab("Predicted probability threatened")+
  theme(panel.grid.major.y = element_line(colour="gray80"))
p_pred_cats

labs_df <-  fancy_plot %>% 
  filter(!is.na(category), !is.na(threatened)) %>%
  count(category, predicted_threat, confidence) %>% 
  pivot_wider(names_from = confidence, values_from=n) %>% 
  mutate(x=ifelse(predicted_threat=="T",0.8, 0.18),
         n=confident+low_confidence,
         label1=paste0("(H:",confident, "; L:", low_confidence, ")")) 

ann_df <- 
  bind_rows(
    labs_df %>% group_by(predicted_threat) %>% 
      summarise(n = sum(n)) %>% 
      mutate(x=ifelse(predicted_threat=="T",0.8, 0.25),
             y="LC",
             label=paste0("n = ",n),
             type=1),
    
    labs_df %>% group_by(predicted_threat) %>% 
      summarise(confident=sum(confident),
                low_confidence=sum(low_confidence)) %>% 
      mutate(x=ifelse(predicted_threat=="T",0.8, 0.25),
             y="LC",
             type=2,
             label=paste0("(H:",confident, "; L:", low_confidence, ")"))
    
    
  ) %>% 
  select(threatened=x,category=y,label2=label, type)

ylabs = labs_df %>% 
  mutate(category = factor(category, levels=names(pal_cat))) %>% 
  group_by(category) %>% 
  summarise(n = sum(n)) %>%  
  mutate(x=-0.25,
         y=category,
         type=3,
         label=paste0(category, "\nn = ",n)) %>% 
  arrange(category) %>% pull(label)

#p_pred_cats <- 
p_pred_cats+
  geom_text(data=labs_df,
            aes(x=x, label=paste0("n = ",n)), vjust=-3, size=3)+
  geom_text(data=labs_df,
            aes(x=x, label=label1), vjust=-1.5, size=2.5)+
  geom_text(data=ann_df,
            aes(label=label2), 
            size=c(4,2.5,3)[ann_df$type],
            hjust=c(0.5,0.5,0)[ann_df$type],
            vjust=c(4,8,-1)[ann_df$type]
  )+
  scale_y_discrete(expand = expansion(add = c(0.8, 0.1))) +
  scale_x_continuous(expand=c(0,0.005),
                     limits=c(-0.0001, 1.0001), 
                     breaks= c(0, 0.25, 0.5, 0.75, 1))

ggsave("eth_end_performance.pdf", width=5, height=5.5)
ggsave("eth_end_performance.png", width=5, height = 5.5)

labs_df

means_by_cat <- fancy_plot %>% 
  group_by(category) %>% 
  summarise(
    mean_threatened = mean(threatened, na.rm = TRUE),
    n              = n()
  ) %>% 
  arrange(desc(mean_threatened))
ggplot(means_by_cat, aes(x = fct_reorder(category, mean_threatened), y = mean_threatened)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Mean P(threatened)")
## Quick run through
mispred_threat <- final_predictors %>% 
  filter(observed_threat == "T")
criteria <- criteria %>% 
  mutate(taxon = paste(genus, species, sep = " "))
criteria <- criteria %>% 
  mutate(taxon = case_when(
    taxon == "Afroligusticum piovanii" ~ "Carum piovanii",
    taxon == "Ceropegia aristolochioides" ~ "Ceropegia burgeri",
    taxon == "Kleinia polycotoma" ~ "Kleinia gypsophila",
    taxon == "Silene scottii" ~ "Lychnis scottii",
    taxon == "Cenchrus uliginosus" ~ "Pennisetum uliginosum",
    taxon == "Afrosciadium abyssinicum" ~ "Peucedanum abyssinicum",
    TRUE ~ taxon
  ))
mispred_threat <- mispred_threat %>% 
  left_join(criteria, by = "taxon")
mispred_threat <- mispred_threat %>% 
  select(taxon, genus = genus.x, species = species.x, prediction, category = category.x, criteria, confidence)
table(mispred_threat$prediction)

glm(prediction ~ category + criteria, data = mispred_threat, family = binomial)
table(final_predictors$prediction)

threat_correct <- mispred_threat %>% 
  filter(prediction == "correct")
threat_under <- mispred_threat %>% 
  filter(prediction == "under")
table(threat_under$criteria)
table(threat_correct$category)
