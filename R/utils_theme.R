y_theme <- theme(
  
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
