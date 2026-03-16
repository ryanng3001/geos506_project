elasinfo <- read.csv("elasticityresults.csv")

# prepare regression datasets 
# row1col1: a_s vs suitability
reg_r1c1 <- elasinfo %>%
  filter(
    TransYear != 1516,
    !(Site == "Uinta" & TransYear == 1415),
    !(Site == "Bountiful" & TransYear == 1314))
# row1col2: a_s vs latitude
reg_r1c2 <- elasinfo %>%
  filter(
    TransYear != 1516,
    !(Site == "Uinta" & TransYear == 1314),
    !(Site == "Bountiful" & TransYear == 1415))
# row2col1 + row2col2: a_a vs suitability / latitude
reg_aa <- elasinfo
# row3col1 + row3col2: j_j vs suitability / latitude
reg_jj <- elasinfo %>%
  filter(
    !(Site == "Swan" & TransYear == 1516),
    !(Site == "Nordic Center" & TransYear == 1415),
    !(Site == "Wolverine" & TransYear == 1314))

# reusable plotting function for one panel 
make_panel <- function(point_data, reg_data, xvar, yvar, xlab, ylab) {
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
      colour = "black") +
    # center regression
    geom_smooth(
      data = reg_data %>% filter(Range == "Center"),
      aes(x = !!xvar, y = !!yvar),
      method = "lm", se = FALSE,
      inherit.aes = FALSE,
      colour = "#3A7D8C",
      linetype = "dashed",
      linewidth = 0.8) +
    # north regression
    geom_smooth(
      data = reg_data %>% filter(Range == "North"),
      aes(x = !!xvar, y = !!yvar),
      method = "lm", se = FALSE,
      inherit.aes = FALSE,
      colour = "#9F4A3A",
      linetype = "dashed",
      linewidth = 0.8) +
    scale_colour_manual(
      values = c(
        "Center" = "#3A7D8C",
        "North" = "#9F4A3A")) +
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.minor = element_blank()) + 
    labs(
      x = xlab,
      y = ylab,
      colour = "Range",
      shape = "Census Year")}

# create each subplot 
p1 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_r1c1,
  xvar = suitability,
  yvar = a_s,
  xlab = NULL,
  ylab = "e(fecundity)")

p2 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_r1c2,
  xvar = latitude,
  yvar = a_s,
  xlab = NULL,
  ylab = NULL)

p3 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_aa,
  xvar = suitability,
  yvar = a_a,
  xlab = NULL,
  ylab = "e(adult survival))")

p4 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_aa,
  xvar = latitude,
  yvar = a_a,
  xlab = NULL,
  ylab = NULL)

p5 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_jj,
  xvar = suitability,
  yvar = j_j,
  xlab = "Suitability",
  ylab = "e(juvenile survival))")

p6 <- make_panel(
  point_data = elasinfo,
  reg_data   = reg_jj,
  xvar = latitude,
  yvar = j_j,
  xlab = "Latitude",
  ylab = NULL)

# combined plot 
combined_plot <- ((p1 | p2) /
                    (p3 | p4) /
                    (p5 | p6)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

combined_plot


