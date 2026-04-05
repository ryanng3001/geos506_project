### viz_Figure2.R ##############################################################
# By: Ryan Ng | Date: 5 Apr 2026 ###############################################

# Creates stacked bar graph of elasticities for 20 matrices  
################################################################################

# read data 
elasinfo <- read.csv("elasticityresults.csv")

# reshape data
elasticity_long <- elasinfo %>%
  pivot_longer(
    cols = s_s:a_a,
    names_to = "transition",
    values_to = "elasticity"
  ) %>%
  mutate(
    matrix_id = paste(Site, TransYear, sep = "_"),
    transition = recode(
      transition,
      a_s = "Fecundity",
      j_j = "Juv. survival",
      a_a = "Adult survival",
      s_j = "Seedling to Juv.",
      s_a = "Seedling to Adult",
      j_a = "Juv. to Adult",
      a_j = "Adult to Juv.",
      s_s = "s_s",
      j_s = "j_s"
    ),
    transition = factor(
      transition,
      levels = c(
        "j_s",
        "Fecundity",
        "s_s",
        "Juv. survival",
        "Adult survival",
        "Seedling to Juv.",
        "Seedling to Adult",
        "Juv. to Adult",
        "Adult to Juv."
      )
    )
  )

# stacked bar plot
ggplot(elasticity_long, aes(x = matrix_id, y = elasticity, fill = transition)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "j_s" = "#FFFFFF",
      "Fecundity" = "#3A7D8C",
      "s_s" = "#E6A79B",
      "Juv. survival" = "#D17C6B",
      "Adult survival" = "#9F4A3A",
      "Seedling to Juv." = "#F2D6A2",
      "Seedling to Adult" = "#D9A441",
      "Juv. to Adult" = "#7A5A14",
      "Adult to Juv." = "#9E9E9E"
    ),
    breaks = c(
      "Fecundity",
      "Juv. survival",
      "Adult survival",
      "Seedling to Juv.",
      "Seedling to Adult",
      "Juv. to Adult",
      "Adult to Juv."
    )
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  labs(
    x = "",
    y = "Elasticity",
    fill = "Transition"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.margin = margin(l = 25, r = 10, t = 10, b = 10),
    panel.background = element_rect(fill = "#f5f3ef", color = NA),
    plot.background  = element_rect(fill = "#f5f3ef", color = NA),
    legend.background = element_rect(fill = "#f5f3ef", color = NA),
    axis.title = element_text(size = 20),  
    axis.text  = element_text(size = 12),   
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
  )




