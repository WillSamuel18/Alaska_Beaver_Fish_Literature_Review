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
  
nrow(lit_filtered) #73 articles that directly discuss beaver/fish interactions



lit_filtered$Region <- factor(
  lit_filtered$Region,
  levels = c("Arctic", "Interior/Yukon", "SWAK", "SCAK", "SEAK")
)


lit_filtered <- lit_filtered %>%
  mutate(`General effect on fish` = na_if(`General effect on fish`, "NA"))





# Summary Statisitics -----------------------------------------------------
#Overall
nrow(lit_filtered) #73
sum(lit_filtered$`Article Type` == "Publication", na.rm = TRUE) #19
sum(lit_filtered$`Article Type` == "Agency report", na.rm = TRUE) #37
sum(lit_filtered$`Article Type` == "Thesis/dissertation", na.rm = TRUE) #11
sum(lit_filtered$`Article Type` == "Conference proceeding", na.rm = TRUE) #1
sum(lit_filtered$`Article Type` == "Book", na.rm = TRUE) #0
sum(lit_filtered$`Article Type` %in% c("White paper", "Unpublished data"), na.rm = TRUE) #5

sum(lit_filtered$`Data type` == "Empirical", na.rm = TRUE) #53
sum(lit_filtered$`Data type` == "Management Action", na.rm = TRUE) #7
sum(lit_filtered$`Data type` == "TEK", na.rm = TRUE) #13

sum(lit_filtered$`General effect on fish` == "Positive", na.rm = TRUE) #12
sum(lit_filtered$`General effect on fish` == "Negative", na.rm = TRUE) #48
sum(lit_filtered$`General effect on fish` == "Neutral", na.rm = TRUE) #4
sum(lit_filtered$`General effect on fish` == "Mixed", na.rm = TRUE) #9



#SEAK
sum(lit_filtered$Region == "SEAK", na.rm = TRUE) #11

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "SEAK", na.rm = TRUE) #10
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "SEAK", na.rm = TRUE) #0
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "SEAK", na.rm = TRUE) #1


sum(lit_filtered$`Article Type` == "Publication" & lit_filtered$Region == "SEAK", na.rm = TRUE) #5
sum(lit_filtered$`Article Type` == "Agency report" & lit_filtered$Region == "SEAK", na.rm = TRUE) #3
sum(lit_filtered$`Article Type` == "Thesis/dissertation" & lit_filtered$Region == "SEAK", na.rm = TRUE) #1


#SCAK
sum(lit_filtered$Region == "SCAK", na.rm = TRUE) #14

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "SCAK", na.rm = TRUE) #10
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "SCAK", na.rm = TRUE) #1
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "SCAK", na.rm = TRUE) #3


#SWAK
sum(lit_filtered$Region == "SWAK", na.rm = TRUE) #13

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "SWAK", na.rm = TRUE) #8
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "SWAK", na.rm = TRUE) #1
sum(lit_filtered$`Data type` == "TEK" & lit_filtered$Region == "SWAK", na.rm = TRUE) #4


#Interior/Yukon
sum(lit_filtered$Region == "Interior/Yukon", na.rm = TRUE) #28

sum(lit_filtered$`Data type` == "Empirical" & lit_filtered$Region == "Interior/Yukon", na.rm = TRUE) #20
sum(lit_filtered$`Data type` == "Management Action" & lit_filtered$Region == "Interior/Yukon", na.rm = TRUE) #4
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
  labs(title = "C", x = NULL, y = "Number of documents", fill = "Data type") +
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
  labs(title = "A", x = NULL, y = "Number of documents", fill = "General effect on fish") +
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
  labs(title = "B", x = NULL, y = "Number of documents", fill = "General effect on fish") +
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
  labs(title = "D", x = "Decade", y = "Number of documents") +
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
# Built from the "Species effects" tab (one row per document x species), so the
# unit is the species-level reported effect. A reported effect that involves more
# than one life stage is counted once for each life stage, so bars sum to more
# than the 135 species-level reported effects. Codes: s = spawning adults
# (including adults delayed or blocked on spawning migrations), r = rearing
# juveniles, m = migrating (non-spawning movement), p = present (not specified).

lifestage_labels <- c(
  s = "Spawning adults",
  m = "Migrating",
  r = "Rearing juveniles",
  p = "Present (unspecified)"
)

lit_lifestage <- read_excel("Annotated bibliography Final.xlsx", sheet = "Species effects") %>%
  semi_join(lit_filtered, by = "In text citation") %>%
  rename(`General effect on fish` = `Effect on this species`) %>%
  mutate(`Life stage` = str_to_lower(`Life stage`)) %>%
  separate_rows(`Life stage`, sep = ",") %>%
  mutate(Lifestage = str_trim(`Life stage`)) %>%
  filter(Lifestage %in% names(lifestage_labels)) %>%
  distinct(`In text citation`, `Species group`, Lifestage, .keep_all = TRUE) %>%
  # Where a life stage had a different effect from the species-level effect
  # (e.g., "s: Neutral"), use that effect for that life stage
  mutate(stage_effect = str_match(`Life-stage effect (if different)`,
                                  paste0(Lifestage, ":\\s*(\\w+)"))[, 2],
         `General effect on fish` = coalesce(stage_effect, `General effect on fish`),
         `General effect on fish` = factor(`General effect on fish`, levels = effect_order)) %>%
  mutate(Lifestage = factor(lifestage_labels[Lifestage],
                            levels = rev(lifestage_labels)))

# Summary statistics by life stage
lit_lifestage %>%
  count(Lifestage, `General effect on fish`) %>%
  pivot_wider(names_from = `General effect on fish`, values_from = n, values_fill = 0)
nrow(lit_lifestage)

lifestage_plot <- ggplot(lit_lifestage, aes(x = Lifestage, fill = `General effect on fish`)) +
  geom_bar(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = cols_effect,
    limits = effect_order,
    breaks = effect_order
  ) +
  labs(x = NULL, y = "Number of reported effects", fill = "General effect on fish") +
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




# Effects by lateral dam position (panel B of the life-stage figure) ---------
# Document-level effects, grouped by where the beaver dams described in each
# document were located ("Dam position" column, classified 2026-11-20):
#   Main channel = dam across a channel with its own through-flow that fish
#     travel along (streams and tributaries, lake outlets and inlets, and
#     groundwater- or tributary-fed sloughs and spring brooks)
#   Off-channel  = dam impounding a side channel, oxbow, floodplain pond, or
#     backwater beside the main channel
#   Both         = document described dams in both positions
#   Not reported = position not described
# 4 of 68 classifications were inferred from site descriptions (Dolloff 1993;
# Carey and Koch 2019; S. Zavoico unpublished data; Yukon Fish and Game
# Association 2007); see "Dam position basis".

position_levels <- c("Main channel", "Off-channel", "Both", "Not reported")

lit_position <- lit_filtered2 %>%
  filter(!is.na(`General effect on fish`)) %>%
  mutate(`Dam position` = factor(`Dam position`, levels = rev(position_levels)))

# Summary statistics by dam position
lit_position %>%
  count(`Dam position`, `General effect on fish`) %>%
  pivot_wider(names_from = `General effect on fish`, values_from = n, values_fill = 0)
# Main channel: 5 positive, 7 mixed, 1 neutral, 40 negative (n = 53)
# Off-channel:  3 positive, 1 mixed, 1 neutral, 3 negative (n = 8)
# Both:         2 positive, 1 mixed, 0 neutral, 4 negative (n = 7)
# Not reported: 2 positive, 0 mixed, 2 neutral, 1 negative (n = 5)

position_plot <- ggplot(lit_position, aes(x = `Dam position`, fill = `General effect on fish`)) +
  geom_bar(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = cols_effect,
    limits = effect_order,
    breaks = effect_order
  ) +
  labs(title = "B", x = NULL, y = "Number of documents", fill = "General effect on fish") +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank()
  )
position_plot

# Combined figure: (A) life stage, (B) dam position, shared legend
lifestage_position_plot <- (lifestage_plot + labs(title = "A")) / position_plot +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
lifestage_position_plot

ggsave("Figures/lifestage_position_plot.jpeg", lifestage_position_plot,
       width = 8,
       height = 8,
       dpi = 300)




# Species effects and research effort (standalone figure) -------------------
# Reads the "Species effects" tab: one row per document x species, with the
# effect on that species (coded per species, not inherited from the document).
# Left (A): number of documents for each species, split by effect on that species
# (all regions combined), with totals at the end of each bar.
# Right (B): number of documents that addressed each species in each region
# (research effort only; no effect colors). Blank tiles = no documents found.
# A document is counted once for each species it addresses, so totals sum to
# more than the number of documents. Species rows in A and B share the same order.

species_effects <- read_excel("Annotated bibliography Final.xlsx", sheet = "Species effects") %>%
  semi_join(lit_filtered, by = "In text citation") %>%   # only documents used in the figures
  left_join(select(lit_filtered, `In text citation`, Region), by = "In text citation") %>%
  rename(Species_group = `Species group`,
         `General effect on fish` = `Effect on this species`) %>%
  mutate(`General effect on fish` = factor(`General effect on fish`, levels = effect_order))

nrow(species_effects)                              # document x species rows
n_distinct(species_effects$`In text citation`)     # documents with species rows

region_order <- c("SEAK", "SCAK", "SWAK", "Interior/Yukon", "Arctic")

species_totals <- species_effects %>%
  count(Species_group, name = "n")

species_order <- species_totals %>%
  arrange(n) %>%
  pull(Species_group)

species_effects <- species_effects %>%
  mutate(Species_group = factor(Species_group, levels = species_order))
species_totals <- species_totals %>%
  mutate(Species_group = factor(Species_group, levels = species_order))

# Summary table: effect counts by species
species_effects %>%
  count(Species_group, `General effect on fish`) %>%
  pivot_wider(names_from = `General effect on fish`, values_from = n, values_fill = 0)

species_effects_plot <- ggplot(species_effects,
                               aes(x = Species_group, fill = `General effect on fish`)) +
  geom_bar(width = 0.7) +
  geom_text(data = species_totals, aes(x = Species_group, y = n, label = n),
            inherit.aes = FALSE, hjust = -0.3, size = 4, fontface = "bold") +
  coord_flip() +
  scale_x_discrete(expand = expansion(add = 0.5)) +  # rows line up with the heat map
  scale_fill_manual(values = cols_effect, limits = effect_order, breaks = effect_order) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "A", x = NULL, y = "Number of reported effects", fill = "General effect on fish") +
  theme_cowplot() +
  theme(
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank()
  )
species_effects_plot

effort_tiles <- species_effects %>%
  mutate(Region = factor(as.character(Region), levels = region_order)) %>%
  count(Species_group, Region, name = "Total") %>%
  complete(Species_group, Region, fill = list(Total = 0))

species_effort_plot <- ggplot(effort_tiles, aes(x = Region, y = Species_group, fill = Total)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(data = filter(effort_tiles, Total > 0), aes(label = Total),
            size = 4, fontface = "bold") +
  scale_fill_gradient(low = "grey95", high = "#61A5C2", guide = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = 0.5)) +
  labs(title = "B", x = NULL, y = NULL) +
  theme_cowplot() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
species_effort_plot

species_figure <- (species_effects_plot + species_effort_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
species_figure

ggsave("Figures/species_effects_effort.jpeg", species_figure,
       width = 12,
       height = 8,
       dpi = 300)
