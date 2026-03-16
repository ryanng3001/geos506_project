### 04_viz1_suitlat ############################################################
# By: Ryan Ng | Date: 15 Mar 2026 ##############################################

# Creates plot of latitude vs. suitability for the 7 study sites 
################################################################################

metainfo <- read.csv("metainfo.csv")

# linear regression for latitude x suitability 
lm_lat_suit <- lm(latitude ~ suitability, data = metainfo)
summary(lm_lat_suit)
# plot  
ggplot(metainfo, aes(x = suitability, y = latitude, shape = Range)) +
  geom_point(size = 3.5) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1)) + # se = TRUE plots 95% CI
  theme_classic() +
  labs(
    x = "Suitability",
    y = "Latitude",
    shape = "Range")
