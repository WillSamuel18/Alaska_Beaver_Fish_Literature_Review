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

#lit <- read_excel("Annotated bibliography.xlsx")
lit <- read_excel("Annotated bibliography Final.xlsx")
str(lit)



lit_filtered <- lit %>%
  filter(!is.na(Region), !is.na(`Data type`),
         `Include in Map` == "Y")
  
nrow(lit_filtered) #70 articles that directly discuss beaver/fish interactions



lit_filtered$Region <- factor(
  lit_filtered$Region,
  levels = c("Arctic", "Interior/Yukon", "SWAK", "SCAK", "SEAK")
)


lit_filtered <- lit_filtered %>%
  mutate(`General effect on fish` = na_if(`General effect on fish`, "NA"))





# Summary Statisitics -----------------------------------------------------
#Overall
nrow(lit_filtered) #70
sum(lit_filtered$`Article Type` == "Publication", na.rm = TRUE) #20
sum(lit_filtered$`Article Type` == "Agency report", na.rm = TRUE) #33
sum(lit_filtered$`Article Type` == "Thesis/dissertation", na.rm = TRUE) #11
sum(lit_filtered$`Article Type` == "Conference proceeding", na.rm = TRUE) #1
sum(lit_filtered$`Article Type` == "Book", na.rm = TRUE) #0
sum(lit_filtered$`Article Type` %in% c("White paper", "Unpublished data"), na.rm = TRUE) #5

sum(lit_filtered$`Data type` == "Empirical", na.rm = TRUE) #51
sum(lit_filtered$`Data type` == "Management Action", na.rm = TRUE) #6
sum(lit_filtered$`Data type` == "TEK", na.rm = TRUE) #13

sum(lit_filtered$`General effect on fish` == "Positive", na.rm = TRUE) #14
sum(lit_filtered$`General effect on fish` == "Negative", na.rm = TRUE) #45
sum(lit_filtered$`General effect on fish` == "Neutral", na.rm = TRUE) #5
sum(lit_filtered$`General effect on fish` == "Mixed", na.rm = TRUE) #6



#SEAK
sum(lit_filtered$Region == "SEAK", na.rm = TRUE) #12

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "SEAK", na.rm = TRUE) #10
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "SEAK", na.rm = TRUE) #1
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "SEAK", na.rm = TRUE) #1


sum(lit_filtered$`Article Type` == "Publication" & lit_filtered$Region == "SEAK", na.rm = TRUE) #5
sum(lit_filtered$`Article Type` == "Agency report" & lit_filtered$Region == "SEAK", na.rm = TRUE) #4
sum(lit_filtered$`Article Type` == "Thesis/dissertation" & lit_filtered$Region == "SEAK", na.rm = TRUE) #1


#SCAK
sum(lit_filtered$Region == "SCAK", na.rm = TRUE) #15

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "SCAK", na.rm = TRUE) #11
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "SCAK", na.rm = TRUE) #1
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "SCAK", na.rm = TRUE) #3


#SWAK
sum(lit_filtered$Region == "SWAK", na.rm = TRUE) #13

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "SWAK", na.rm = TRUE) #8
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "SWAK", na.rm = TRUE) #1
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "SWAK", na.rm = TRUE) #4


#Interior/Yukon
sum(lit_filtered$Region == "Interior/Yukon", na.rm = TRUE) #23

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "Interior/Yukon", na.rm = TRUE) #17
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "Interior/Yukon", na.rm = TRUE) #2
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "Interior/Yukon", na.rm = TRUE) #4


#Arctic

sum(lit_filtered$Region == "Arctic", na.rm = TRUE) #7

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "Arctic", na.rm = TRUE) #5
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "Arctic", na.rm = TRUE) #1
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "Arctic", na.rm = TRUE) #1




# Plotting ----------------------------------------------------------------


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
  labs(title = "C", x = NULL, y = "Number of studies", fill = "Data type") +
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
  labs(title = "A", x = NULL, y = "Number of studies", fill = "General effect on fish") +
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
  labs(title = "B", x = NULL, y = "Number of studies", fill = "General effect on fish") +
  theme_cowplot() +
  theme(
    legend.position = "none",
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
  labs(title = "D", x = "Decade", y = "Number of studies") +
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





# Effects by life stage (standalone figure) -------------------------------
# Documents can involve more than one life stage, so a document is counted once
# for each life stage it involves (bars sum to more than 70). Codes: s = spawning
# adults, r = rearing juveniles, m = migrating (non-spawning movement),
# p = present (life stage not specified). One document has no life stage coded.

lifestage_labels <- c(
  s = "Spawning adults",
  m = "Migrating",
  r = "Rearing juveniles",
  p = "Present (unspecified)"
)

lit_lifestage <- lit_filtered2 %>%
  filter(!is.na(Lifestage), Lifestage != "NA") %>%
  mutate(Lifestage = str_to_lower(Lifestage)) %>%
  separate_rows(Lifestage, sep = ",") %>%
  mutate(Lifestage = str_trim(Lifestage)) %>%
  filter(Lifestage %in% names(lifestage_labels)) %>%
  distinct(`In text citation`, Lifestage, .keep_all = TRUE) %>%
  mutate(Lifestage = factor(lifestage_labels[Lifestage],
                            levels = rev(lifestage_labels)))

# Summary statistics by life stage
lit_lifestage %>%
  count(Lifestage, `General effect on fish`) %>%
  pivot_wider(names_from = `General effect on fish`, values_from = n, values_fill = 0)
# Spawning adults: 25 negative, 2 neutral, 4 mixed, 0 positive (31 documents)
# Rearing juveniles: 10 negative, 1 neutral, 3 mixed, 13 positive (27 documents)

lifestage_plot <- ggplot(lit_lifestage, aes(x = Lifestage, fill = `General effect on fish`)) +
  geom_bar(width = 0.7) +
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
lifestage_plot

ggsave("Figures/lifestage_plot.jpeg", lifestage_plot,
       width = 8,
       height = 5,
       dpi = 300)




# Species x region evidence map ---------------------------------------------
# Each tile is shaded by the number of documents that discussed a species in a
# region. Labels give the total and, underneath, the number of documents with
# negative, neutral, mixed, and positive effects (same colors as Figure 2).
# A document is counted once for each species it discusses.

species_lookup <- c(
  "Coho" = "Coho Salmon",
  "Chinook" = "Chinook Salmon",
  "King" = "Chinook Salmon",
  "Sockeye" = "Sockeye Salmon",
  "Chum" = "Chum Salmon",
  "Pink" = "Pink Salmon",
  "Arctic Grayling" = "Arctic Grayling",
  "Dolly Varden" = "Dolly Varden",
  "Humpback Whitefish" = "Whitefishes and ciscoes",
  "Broad Whitefish" = "Whitefishes and ciscoes",
  "Least Cisco" = "Whitefishes and ciscoes",
  "Bering Cisco" = "Whitefishes and ciscoes",
  "Sheefish" = "Whitefishes and ciscoes",
  "Northern Pike" = "Northern Pike",
  "Burbot" = "Burbot",
  "Cutthroat Trout" = "Coastal Cutthroat Trout",
  "Stickleback" = "Sticklebacks"
)

lit_species <- lit_filtered2 %>%
  separate_rows(Species, sep = ",") %>%
  mutate(Species = str_trim(Species)) %>%
  filter(Species != "", Species != "NA") %>%
  mutate(Species_group = recode(Species, !!!species_lookup)) %>%
  distinct(`In text citation`, Species_group, .keep_all = TRUE)

# Any species names not in the lookup table show up here (should be empty)
setdiff(unique(lit_species$Species), names(species_lookup))

region_order <- c("SEAK", "SCAK", "SWAK", "Interior/Yukon", "Arctic")

species_counts <- lit_species %>%
  mutate(Region = as.character(Region)) %>%
  bind_rows(mutate(., Region = "All regions")) %>%
  count(Species_group, Region, `General effect on fish`) %>%
  pivot_wider(names_from = `General effect on fish`, values_from = n, values_fill = 0) %>%
  mutate(Total = Negative + Neutral + Mixed + Positive) %>%
  complete(Species_group, Region,
           fill = list(Negative = 0, Neutral = 0, Mixed = 0, Positive = 0, Total = 0)) %>%
  mutate(Region = factor(Region, levels = c(region_order, "All regions")))

species_order <- species_counts %>%
  filter(Region == "All regions") %>%
  arrange(Total) %>%
  pull(Species_group)

species_counts <- species_counts %>%
  mutate(Species_group = factor(Species_group, levels = species_order))

species_counts  # table of totals and effect counts by species and region

# Long format for the colored effect counts under each total
species_effects <- species_counts %>%
  filter(Total > 0) %>%
  pivot_longer(c(Negative, Neutral, Mixed, Positive),
               names_to = "General effect on fish", values_to = "n") %>%
  mutate(`General effect on fish` = factor(`General effect on fish`, levels = effect_order),
         x_offset = c(-0.27, -0.09, 0.09, 0.27)[as.integer(`General effect on fish`)],
         x = as.integer(Region) + x_offset,
         y = as.integer(Species_group) - 0.2)

heat_max <- max(species_counts$Total[species_counts$Region != "All regions"])

species_map <- ggplot() +
  geom_tile(data = filter(species_counts, Region != "All regions"),
            aes(x = as.integer(Region), y = as.integer(Species_group), fill = Total),
            color = "white", linewidth = 1.2) +
  geom_text(data = filter(species_counts, Total > 0),
            aes(x = as.integer(Region), y = as.integer(Species_group) + 0.17, label = Total),
            fontface = "bold", size = 4.6) +
  geom_text(data = species_effects,
            aes(x = x, y = y, label = n, color = `General effect on fish`),
            fontface = "bold", size = 3.4, show.legend = FALSE) +
  # invisible points so the effect colors get a legend matching Figure 2
  geom_point(data = species_effects,
             aes(x = x, y = y, color = `General effect on fish`),
             shape = 15, size = 0, alpha = 0) +
  geom_vline(xintercept = 5.5, color = "grey60") +
  scale_fill_gradient(low = "grey97", high = "#7FA7BD", limits = c(0, heat_max),
                      name = "Number of studies") +
  scale_color_manual(values = cols_effect, limits = effect_order, breaks = effect_order,
                     name = "General effect on fish") +
  scale_x_continuous(breaks = 1:6, labels = c(region_order, "All regions"),
                     position = "top", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq_along(species_order), labels = species_order,
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.5, 6.5), ylim = c(0.5, length(species_order) + 0.5), clip = "off") +
  labs(x = NULL, y = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6, alpha = 1), order = 1),
         fill = guide_colorbar(order = 2, barwidth = unit(4, "cm"))) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
species_map

ggsave("Figures/species_region_map.jpeg", species_map,
       width = 10,
       height = 9,
       dpi = 300)





# Species x region evidence map, version 2 ---------------------------------
# Same tiles (shaded by number of studies, label = total), with a stacked bar
# under each species row showing the proportion of that species' documents
# (all regions combined) with negative, neutral, mixed, and positive effects.

v2_tiles <- species_counts %>%
  filter(Region != "All regions") %>%
  mutate(x = as.integer(Region),
         y = as.integer(Species_group))

v2_totals <- species_counts %>%
  filter(Region == "All regions") %>%
  mutate(y = as.integer(Species_group))

bar_left <- 0.55   # bar spans the five region columns
bar_right <- 5.45

v2_bars <- v2_totals %>%
  pivot_longer(c(Negative, Neutral, Mixed, Positive),
               names_to = "General effect on fish", values_to = "n") %>%
  mutate(`General effect on fish` = factor(`General effect on fish`, levels = effect_order)) %>%
  arrange(Species_group, `General effect on fish`) %>%
  group_by(Species_group) %>%
  mutate(prop = n / sum(n),
         xmax = bar_left + cumsum(prop) * (bar_right - bar_left),
         xmin = xmax - prop * (bar_right - bar_left)) %>%
  ungroup()

species_map_v2 <- ggplot() +
  geom_rect(data = v2_tiles,
            aes(xmin = x - 0.48, xmax = x + 0.48, ymin = y - 0.05, ymax = y + 0.45, fill = Total),
            color = NA) +
  geom_text(data = filter(v2_tiles, Total > 0),
            aes(x = x, y = y + 0.2, label = Total),
            fontface = "bold", size = 4.4) +
  geom_rect(data = filter(v2_bars, n > 0),
            aes(xmin = xmin, xmax = xmax, ymin = y - 0.38, ymax = y - 0.14,
                color = `General effect on fish`),
            fill = NA, linewidth = 0, show.legend = FALSE) +
  geom_rect(data = filter(v2_bars, n > 0),
            aes(xmin = xmin, xmax = xmax, ymin = y - 0.38, ymax = y - 0.14),
            fill = cols_effect[as.character(filter(v2_bars, n > 0)$`General effect on fish`)],
            color = "white", linewidth = 0.4) +
  geom_text(data = filter(v2_bars, prop >= 0.08),
            aes(x = (xmin + xmax) / 2, y = y - 0.26, label = n),
            color = "white", fontface = "bold", size = 3.3) +
  geom_text(data = v2_totals,
            aes(x = 6, y = y + 0.05, label = Total),
            fontface = "bold", size = 4.4) +
  # invisible points so the effect colors get a legend matching Figure 2
  geom_point(data = v2_bars,
             aes(x = 1, y = 1, color = `General effect on fish`),
             shape = 15, size = 0, alpha = 0) +
  scale_fill_gradient(low = "grey97", high = "#7FA7BD", limits = c(0, heat_max),
                      name = "Number of studies") +
  scale_color_manual(values = cols_effect, limits = effect_order, breaks = effect_order,
                     name = "General effect on fish") +
  scale_x_continuous(breaks = 1:6, labels = c(region_order, "All regions"),
                     position = "top", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq_along(species_order), labels = species_order,
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.5, 6.5), ylim = c(0.5, length(species_order) + 0.5), clip = "off") +
  labs(x = NULL, y = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6, alpha = 1), order = 1),
         fill = guide_colorbar(order = 2, barwidth = unit(4, "cm"))) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
species_map_v2

ggsave("Figures/species_region_map_v2.jpeg", species_map_v2,
       width = 10,
       height = 10,
       dpi = 300)
