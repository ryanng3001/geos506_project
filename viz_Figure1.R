### viz_Figure1.R ##############################################################
# By: Ryan Ng | Date: 5 Apr 2026 ###############################################

# Creates plot of latitude vs. suitability for the 7 study sites 
################################################################################

metainfo <- read.csv("metainfo.csv")

# linear regression for latitude x suitability 
lm_lat_suit <- lm(suitability ~ latitude, data = metainfo)
summary(lm_lat_suit)
ggplot(metainfo, aes(x = latitude, y = suitability, color = Range)) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1),
              linetype = "dashed", color = "black") +  
  geom_point(size = 5) +                          
  scale_color_manual(values = c("Center" = "#b23a6f",
                                "Edge" = "#2f5d50")) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#f5f3ef", color = NA),
    plot.background  = element_rect(fill = "#f5f3ef", color = NA),
    legend.position = c(0.05, 0.05),   
    legend.justification = c(0, 0),   
    legend.background = element_rect(fill = "#f5f3ef", color = NA),
    axis.title = element_text(size = 20),  
    axis.text  = element_text(size = 18),   
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 20)
  ) +
  labs(
    x = "Latitude (°)",
    y = "Suitability",
    color = "Range"
  )

