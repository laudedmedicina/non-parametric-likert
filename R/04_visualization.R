# ============================================================
# 03_visualization.R
# Purpose : Generate all publication-grade figures.
# Input   : data/processed/clean_data.csv
# Output  : outputs/figures/
# ============================================================

library(tidyverse)
library(ggpubr)
source("R/utils_theme.R")   # loads my_theme, PALETTE, fonts

# ------------------------------------------------------------
# 1. Load clean data
# ------------------------------------------------------------
df <- read.csv("data/processed/clean_data.csv", stringsAsFactors = FALSE) %>%
  mutate(
    gender        = factor(gender),
    university    = factor(university),
    academic_year = factor(academic_year,
                           levels = c("Second", "Third", "Fourth",
                                      "Fifth",  "Sixth")),
    residence     = factor(residence)
  )

# ------------------------------------------------------------
# 2. Reshape to long format
#    Each row = one person × one score type × one demographic
# ------------------------------------------------------------
my_theme <- theme(
  
  # --- Background ---
  plot.background   = element_rect(fill = "white", color = NA),
  panel.background  = element_rect(fill = "white", color = NA),
  plot.margin       = margin(20, 25, 15, 15),
  
  # --- Grid ---
  panel.grid.major  = element_line(color = "#EAEAEA", linewidth = 0.35),
  panel.grid.minor  = element_blank(),
  
  # --- Borders ---
  panel.border      = element_blank(),
  axis.line.x       = element_line(color = "#333333", linewidth = 0.45),
  axis.line.y       = element_line(color = "#333333", linewidth = 0.45),
  axis.ticks        = element_line(color = "#333333", linewidth = 0.35),
  axis.ticks.length = unit(3, "pt"),
  
  # --- Title & subtitle ---
  plot.title        = element_text(family = "CENSCBK", face = "bold",
                                   size = 15, color = "#111111",
                                   margin = margin(b = 5), hjust = 0),
  plot.subtitle     = element_text(family = "CENSCBK", face = "plain",
                                   size = 11, color = "#555555",
                                   margin = margin(b = 12), hjust = 0),
  plot.caption      = element_text(family = "CENSCBK", face = "plain",
                                   size = 9, color = "#777777",
                                   hjust = 1, margin = margin(t = 10)),
  
  # --- Axis labels ---
  axis.title.x      = element_text(family = "CENSCBK", face = "plain",
                                   size = 12, color = "#222222",
                                   margin = margin(t = 10)),
  axis.title.y      = element_text(family = "CENSCBK", face = "plain",
                                   size = 12, color = "#222222",
                                   margin = margin(r = 10)),
  
  # --- Tick labels ---
  axis.text.x       = element_text(family = "CENSCBK", face = "plain",
                                   size = 10, color = "#333333"),
  axis.text.y       = element_text(family = "CENSCBK", face = "plain",
                                   size = 10, color = "#333333"),
  
  # --- Facet strips ---
  strip.background  = element_rect(fill = "#F0F0F0", color = NA),
  strip.text        = element_text(family = "CENSCBK", face = "bold",
                                   size = 11, color = "#222222",
                                   margin = margin(5, 5, 5, 5)),
  
  # --- Legend ---
  legend.position    = "top",
  legend.direction   = "horizontal",
  legend.title       = element_text(family = "CENSCBK", face = "bold",
                                    size = 11, color = "#222222"),
  legend.text        = element_text(family = "CENSCBK", face = "plain",
                                    size = 10, color = "#333333"),
  legend.background  = element_rect(fill = "white", color = NA),
  legend.key         = element_rect(fill = "white", color = NA),
  legend.key.size    = unit(12, "pt"),
  legend.margin      = margin(0, 0, 8, 0),
  legend.spacing.x   = unit(6, "pt")
)


df_long2 <- df %>%
  pivot_longer(
    cols      = c(attitude_score, barrier_score),
    names_to  = "score_type",
    values_to = "score"
  ) %>%
  mutate(score_type = recode(score_type,
                             "attitude_score" = "Attitude toward research",
                             "barrier_score"  = "Barriers to research")) %>%
  pivot_longer(
    cols      = c(gender, university, residence, academic_year),
    names_to  = "variable",
    values_to = "group"
  ) %>%
  mutate(
    variable = recode(variable,
                      "gender"        = "Gender",
                      "university"    = "University",
                      "residence"     = "Residence",
                      "academic_year" = "Academic year"),
    variable = factor(variable,
                      levels = c("Gender", "Academic year", "Residence", "University"))
  )


p <- ggplot(df_long2, aes(x = group, y = score, fill = score_type, color = score_type)) +
  
  # Violin layer
  geom_violin(
    alpha    = 0.25,
    trim     = FALSE,
    linewidth = 0.3,
    position = position_dodge(0.85)
  ) +
  
  # Boxplot layer
  geom_boxplot(
    width          = 0.18,
    outlier.shape  = NA, #SHOW THROUGH JITTER
    outlier.size   = 1.2,
    outlier.alpha  = 0.4,
    outlier.stroke = 0.3,
    fatten         = 2.5,       # thicker median line
    linewidth      = 0.45,
    position       = position_dodge(0.85),
    color          = "black",
    fill           = "white",
    alpha          = 0.7
  ) +
  # Jitter — raw individual observations
  geom_jitter(
    aes(color = score_type),
    width         = 0.12,
    alpha         = 0.18,
    size          = 0.7,
    #position      = position_dodge(0.85),
    show.legend   = FALSE
  ) +
  # Median dot
  stat_summary(
    fun      = median,
    geom     = "point",
    shape    = 21,
    size     = 2.2,
    color    = "black",
    fill     = "white",
    position = position_dodge(0.85)
  ) +
  
  # Colors
  scale_fill_manual(
    values = c(
      "Attitude toward research" = "#4C72B566",
      "Barriers to research"     = "#DD126181"
    )
  ) +
  scale_color_manual(
    values = c(
      "Attitude toward research" = "#4C72B566",
      "Barriers to research"     = "#DD126181"
    )
  ) +
  
  # Y axis limits with breathing room
  scale_y_continuous(
    limits = c(1, 5.2),
    breaks = seq(1, 5, by = 0.5),
    expand = c(0, 0)
  ) +
  
  # Facets
  facet_wrap(
    ~ variable,
    scales  = "free_x",
    nrow    = 2
  ) +
  
  # Labels
  labs(
    title    = "Attitude toward research and perceived barriers among medical students",
    subtitle = "Comparison across demographic variables  |  n = 626",
    x        = NULL,
    y        = "Mean Likert score (1–3)",
    fill     = NULL,
    color    = NULL,
    caption  = "Box shows IQR; thick line = median; whiskers = 1.5×IQR; dots = outliers"
  ) +
  
  # Angle x-axis text for all panels
  my_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9))

p

ggsave(
  filename = "figure_professional.jpg",
  plot     = p,
  width    = 16,
  height   = 7,
  dpi      = 300,
  units    = "in"
)

showtext_end()





# ------------------------------------------------------------
# 3. Master figure — violin + box + jitter + median dot
#    Faceted by demographic variable (2 × 2 grid)
# ------------------------------------------------------------
my_theme <- theme(
  
  # --- Background ---
  plot.background   = element_rect(fill = "white", color = NA),
  panel.background  = element_rect(fill = "white", color = NA),
  plot.margin       = margin(20, 25, 15, 15),
  
  # --- Grid ---
  panel.grid.major  = element_line(color = "#EAEAEA", linewidth = 0.35),
  panel.grid.minor  = element_blank(),
  
  # --- Borders ---
  panel.border      = element_blank(),
  axis.line.x       = element_line(color = "#333333", linewidth = 0.45),
  axis.line.y       = element_line(color = "#333333", linewidth = 0.45),
  axis.ticks        = element_line(color = "#333333", linewidth = 0.35),
  axis.ticks.length = unit(3, "pt"),
  
  # --- Title & subtitle ---
  plot.title        = element_text(family = "CENSCBK", face = "bold",
                                   size = 15, color = "#111111",
                                   margin = margin(b = 5), hjust = 0),
  plot.subtitle     = element_text(family = "CENSCBK", face = "plain",
                                   size = 11, color = "#555555",
                                   margin = margin(b = 12), hjust = 0),
  plot.caption      = element_text(family = "CENSCBK", face = "plain",
                                   size = 9, color = "#777777",
                                   hjust = 1, margin = margin(t = 10)),
  
  # --- Axis labels ---
  axis.title.x      = element_text(family = "CENSCBK", face = "plain",
                                   size = 12, color = "#222222",
                                   margin = margin(t = 10)),
  axis.title.y      = element_text(family = "CENSCBK", face = "plain",
                                   size = 12, color = "#222222",
                                   margin = margin(r = 10)),
  
  # --- Tick labels ---
  axis.text.x       = element_text(family = "CENSCBK", face = "plain",
                                   size = 10, color = "#333333"),
  axis.text.y       = element_text(family = "CENSCBK", face = "plain",
                                   size = 10, color = "#333333"),
  
  # --- Facet strips ---
  strip.background  = element_rect(fill = "#F0F0F0", color = NA),
  strip.text        = element_text(family = "CENSCBK", face = "bold",
                                   size = 11, color = "#222222",
                                   margin = margin(5, 5, 5, 5)),
  
  # --- Legend ---
  legend.position    = "top",
  legend.direction   = "horizontal",
  legend.title       = element_text(family = "CENSCBK", face = "bold",
                                    size = 11, color = "#222222"),
  legend.text        = element_text(family = "CENSCBK", face = "plain",
                                    size = 10, color = "#333333"),
  legend.background  = element_rect(fill = "white", color = NA),
  legend.key         = element_rect(fill = "white", color = NA),
  legend.key.size    = unit(12, "pt"),
  legend.margin      = margin(0, 0, 8, 0),
  legend.spacing.x   = unit(6, "pt")
)


df_long2 <- df %>%
  pivot_longer(
    cols      = c(attitude_score, barrier_score),
    names_to  = "score_type",
    values_to = "score"
  ) %>%
  mutate(score_type = recode(score_type,
                             "attitude_score" = "Attitude toward research",
                             "barrier_score"  = "Barriers to research")) %>%
  pivot_longer(
    cols      = c(gender, university, residence, academic_year),
    names_to  = "variable",
    values_to = "group"
  ) %>%
  mutate(
    variable = recode(variable,
                      "gender"        = "Gender",
                      "university"    = "University",
                      "residence"     = "Residence",
                      "academic_year" = "Academic year"),
    variable = factor(variable,
                      levels = c("Gender", "Academic year", "Residence", "University"))
  )


p <- ggplot(df_long2, aes(x = group, y = score, fill = score_type, color = score_type)) +
  
  # Violin layer
  geom_violin(
    alpha    = 0.25,
    trim     = FALSE,
    linewidth = 0.3,
    position = position_dodge(0.85)
  ) +
  
  # Boxplot layer
  geom_boxplot(
    width          = 0.18,
    outlier.shape  = NA, #SHOW THROUGH JITTER
    outlier.size   = 1.2,
    outlier.alpha  = 0.4,
    outlier.stroke = 0.3,
    fatten         = 2.5,       # thicker median line
    linewidth      = 0.45,
    position       = position_dodge(0.85),
    color          = "black",
    fill           = "white",
    alpha          = 0.7
  ) +
  # Jitter — raw individual observations
  geom_jitter(
    aes(color = score_type),
    width         = 0.12,
    alpha         = 0.18,
    size          = 0.7,
    #position      = position_dodge(0.85),
    show.legend   = FALSE
  ) +
  # Median dot
  stat_summary(
    fun      = median,
    geom     = "point",
    shape    = 21,
    size     = 2.2,
    color    = "black",
    fill     = "white",
    position = position_dodge(0.85)
  ) +
  
  # Colors
  scale_fill_manual(
    values = c(
      "Attitude toward research" = "#4C72B566",
      "Barriers to research"     = "#DD126181"
    )
  ) +
  scale_color_manual(
    values = c(
      "Attitude toward research" = "#4C72B566",
      "Barriers to research"     = "#DD126181"
    )
  ) +
  
  # Y axis limits with breathing room
  scale_y_continuous(
    limits = c(1, 5.2),
    breaks = seq(1, 5, by = 0.5),
    expand = c(0, 0)
  ) +
  
  # Facets
  facet_wrap(
    ~ variable,
    scales  = "free_x",
    nrow    = 2
  ) +
  
  # Labels
  labs(
    title    = "Attitude toward research and perceived barriers among medical students",
    subtitle = "Comparison across demographic variables  |  n = 626",
    x        = NULL,
    y        = "Mean Likert score (1–3)",
    fill     = NULL,
    color    = NULL,
    caption  = "Box shows IQR; thick line = median; whiskers = 1.5×IQR; dots = outliers"
  ) +
  
  # Angle x-axis text for all panels
  my_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9))

p

ggsave(
  filename = "figure_professional.jpg",
  plot     = p,
  width    = 16,
  height   = 7,
  dpi      = 300,
  units    = "in"
)

showtext_end()



