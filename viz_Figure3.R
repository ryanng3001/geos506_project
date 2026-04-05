### viz_Figure3.R ##############################################################
# By: Ryan Ng | Date: 5 Apr 2026 ###############################################

# Plot elasticity (a_s, a_a, j_j) vs. suit/ lat w/ linear regression 
################################################################################

elasinfo <- read.csv("elasticityresults.csv")

# prepare regression datasets 
# 1: a_s vs latitude
reg_r1c2 <- elasinfo %>%
  filter(
    TransYear != 1516,
    !(Site == "Uinta" & TransYear == 1314),
    !(Site == "Bountiful" & TransYear == 1415))
# 2: a_a vs latitude
reg_aa <- elasinfo
# 3: j_j vs latitude
reg_jj <- elasinfo

# reusable plotting function for one panel 
make_panel <- function(point_data, reg_data, xvar, yvar, xlab, ylab, overall_col="black", overall_lty="dashed") {
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  ggplot(point_data, aes(x = !!xvar, y = !!yvar,
                         colour = Range,
                         shape = factor(TransYear))) +
    geom_point(size = 3) +
    # overall regression
    geom_smooth(
      data = reg_data,
      aes(x = !!xvar, y = !!yvar),
      method = "lm", se = FALSE,
      inherit.aes = FALSE,
      colour = overall_col,
      linetype = overall_lty,
      linewidth=0.8) +
    # center regression
    geom_smooth(
      data = reg_data %>% filter(Range == "Center"),
      aes(x = !!xvar, y = !!yvar),
      method = "lm", se = FALSE,
      inherit.aes = FALSE,
      colour = "#b23a6f",
      linetype = "dashed",
      linewidth = 0.8) +
    # north regression
    geom_smooth(
      data = reg_data %>% filter(Range == "Edge"),
      aes(x = !!xvar, y = !!yvar),
      method = "lm", se = FALSE,
      inherit.aes = FALSE,
      colour = "#2f5d50",
      linetype = "dashed",
      linewidth = 0.8) +
    scale_colour_manual(
      values = c(
        "Center" = "#b23a6f",
        "Edge" = "#2f5d50")) +
    scale_shape_manual(
      values = c(16, 17, 15),  # or whatever shapes you want
      labels = c(
        "1314" = "2013–2014",
        "1415" = "2014–2015",
        "1516" = "2015–2016")) +
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f5f3ef", color = NA),
      plot.background  = element_rect(fill = "#f5f3ef", color = NA),
      legend.background = element_rect(fill = "#f5f3ef", color = NA),
    ) +
    labs(
      x = xlab,
      y = ylab,
      colour = "Range",
      shape = "Census Year")}

p1 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_r1c2,
  xvar = latitude,
  yvar = a_s,
  xlab = "Latitude (°)",
  ylab = "Elasticity of fecundity",
  overall_col = "black",
  overall_lty = "solid")

p2 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_aa,
  xvar = latitude,
  yvar = a_a,
  xlab = "Latitude (°)",
  ylab = "Elasticity of adult survival",)

p3 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_jj,
  xvar = latitude,
  yvar = j_j,
  xlab = "Latitude (°)",
  ylab = "Elasticity of juv. survival",)

# combined plot 
combined_plot <- (p1 | p2 | p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")
combined_plot

## AIC stuff 
# m_as_suit <- lm(a_s ~ suitability, data = reg_r1c1)
# m_as_lat  <- lm(a_s ~ latitude, data = reg_r1c2)
# AIC(m_as_suit, m_as_lat)
# 
# m_aa_suit <- lm(a_a ~ suitability, data = reg_aa)
# m_aa_lat  <- lm(a_a ~ latitude, data = reg_aa)
# AIC(m_aa_suit, m_aa_lat)
# 
# m_jj_suit <- lm(j_j ~ suitability, data = reg_jj)
# m_jj_lat  <- lm(j_j ~ latitude, data = reg_jj)
# AIC(m_jj_suit, m_jj_lat)
