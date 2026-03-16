### 05_regressions #############################################################
# By: Ryan Ng | Date: 15 Mar 2026 ##############################################

# Perform simple linear regression on elasticity (a_s, a_a, j_j) and suit/ lat
################################################################################

elasinfo <- read.csv("elasticityresults.csv")

# filter data for regression 
elas_subset <- elasinfo[
  # elasinfo$TransYear != 1516 &
    (elasinfo$Range == "North") &
    !(elasinfo$Site == "Wolverine" & elasinfo$TransYear == 1314) &
    !(elasinfo$Site == "Swan" & elasinfo$TransYear == 1516) &
    !(elasinfo$Site == "Nordic Center" & elasinfo$TransYear == 1415),]
# elas_subset <- elasinfo[elasinfo$Range == "Center",]
                          
# linear regression: elasticity vs suitability
model_suit <- lm(a_a ~ suitability, data = elas_subset) ## CHANGE 
summary(model_suit)

# linear regression: elasticity vs latitude
model_lat <- lm(a_a ~ latitude, data = elas_subset)     ## CHANGE
summary(model_lat)

