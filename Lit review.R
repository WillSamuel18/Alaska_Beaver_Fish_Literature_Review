################################################################################
#################### Alaska Beaver and Fish Literature Review ##################
################ Authors: Will Samuel (UAF) and Erin Larson (UAA) ##############
################################################################################


library(tidyverse)
library(ggplot2)
library(readxl)
library(cowplot)
library(patchwork)





#Quick data processing

lit <- read_excel("Annotated bibliography.xlsx")
str(lit)



lit_filtered <- lit %>%
  filter(!is.na(Region), !is.na(`Data type`),
         `Include in Map` == "Y")
  
nrow(lit_filtered) #62 articles that directly discuss beaver/fish interactions



lit_filtered$Region <- factor(
  lit_filtered$Region,
  levels = c("Arctic", "Interior/Yukon", "SWAK", "SCAK", "SEAK")
)


lit_filtered <- lit_filtered %>%
  mutate(`General effect on fish` = na_if(`General effect on fish`, "NA"))



#Plotting
cols <- c(
  "#1B3A4B",
  "#2A6F97",
  "#61A5C2",
  "#2D6A4F",
  "#74C69D",
  "#B08968",
  "#D9ED92"
)


cols <- c("#1B3A4B", "#2D6A4F", "#B08968")



Regions_plot <- ggplot(lit_filtered, aes(x = Region, fill = `Data type`)) +
  geom_bar(width = 0.7) + #color = "black",
  coord_flip() +
  scale_fill_manual(values = cols) +
  labs(x = NULL, y = "Number of studies", fill = "Data type") +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank()
  )
Regions_plot

ggsave("Figures/Regions_plot.jpeg", Regions_plot, 
       width = 7, 
       height = 5, 
       dpi = 300)



cols_effect <- c(
  "Negative" = "#B04A4A",
  "Neutral" = "#7A868C",
  "Mixed" = "#8B6F47",
  "Positive" = "#2F7D4A"
)

effect_order <- c("Negative", "Neutral", "Mixed", "Positive")

lit_filtered2 <- lit_filtered %>%
  mutate(`General effect on fish` = factor(`General effect on fish`, levels = effect_order))





effects_plot <- ggplot(lit_filtered2, aes(x = Region, fill = `General effect on fish`)) +
  geom_bar(
    data = dplyr::filter(lit_filtered2, !is.na(`General effect on fish`)),
    width = 0.7
  ) +
  coord_flip() +
  scale_fill_manual(
    values = cols_effect,
    limits = effect_order,
    breaks = effect_order
  ) +
  labs(x = NULL, y = "Number of studies", fill = "General effect on fish") +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank()
  )

effects_plot

ggsave("Figures/effects_plot.jpeg", effects_plot, 
       width = 8, 
       height = 6, 
       dpi = 300)




datatype_plot <- ggplot(lit_filtered2, aes(x = `Data type`, fill = `General effect on fish`)) +
  geom_bar(
    data = dplyr::filter(lit_filtered2, !is.na(`General effect on fish`)),
    width = 0.7
  ) +
  coord_flip() +
  scale_fill_manual(
    values = cols_effect,
    limits = effect_order,
    breaks = effect_order
  ) +
  labs(x = NULL, y = "Number of studies", fill = "General effect on fish") +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank()
  )

datatype_plot

ggsave("Figures/datatype_plot.jpeg", datatype_plot, 
       width = 8, 
       height = 6, 
       dpi = 300)




pubs_over_time <- lit_filtered %>%
  mutate(
    Year = as.numeric(Year),
    Decade = floor(Year / 10) * 10
  ) %>%
  ggplot(aes(x = Decade)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1960, 2030, 10)) +
  labs(x = "Decade", y = "Number of studies") +
  theme_cowplot()
pubs_over_time

ggsave("Figures/pubs_over_time.jpeg", pubs_over_time, 
       width = 7, 
       height = 5, 
       dpi = 300)



panel <- (effects_plot + datatype_plot)/(Regions_plot + pubs_over_time)

ggsave("Figures/panel.jpeg", panel, 
       width = 12, 
       height = 10, 
       dpi = 300)

