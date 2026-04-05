### 02_buildmatrix #############################################################
# By: Ryan Ng | Date: 5 Apr 2026 ###############################################

# Build matrix for each site and each census (i.e. total 20)
# Stores matrix elements in 'matrixsummary.csv' 

# Note1: The code is not looped for all observations - I did that manually
################################################################################

# A. subset data based on site and census year 
subset <- demodata %>%
  filter(Site == "Providence", TransYear == 1516) ## CHANGE THIS 

# B. replace NA in total seed production to 0; create col for fertility 
subset <- subset %>%
  mutate(
    fec.totseed = ifelse(is.na(fec.totseed), 0, fec.totseed),
    fert = 0)

# C. calculate fertility as the individual contribution to seedlings in t+1
n_seedlings_next <- sum(subset$lifestageNext == "seedling", na.rm = TRUE)
adult_seed_output <- ifelse(
  !is.na(subset$lifestage) & subset$lifestage == "adult",
  subset$fec.totseed, 0)
total_seed_output <- sum(adult_seed_output, na.rm = TRUE)
if (total_seed_output > 0 && n_seedlings_next > 0) {
  subset$fert <- adult_seed_output / total_seed_output * n_seedlings_next} else {
    subset$fert <- 0}
subset$fert[is.na(subset$fert)] <- 0

# D. calculate fate
subset <- subset %>%
  mutate(
    fate = as.character(lifestageNext),
    fate = ifelse(is.na(fate), "dead", fate),
    fate = factor(fate, levels = c("seedling", "juvenile", "adult", "dead")))

# E. build matrix
stages <- c("seedling", "juvenile", "adult")
A_subset <- projection.matrix(
  subset,
  stage = lifestage,
  fate = fate,
  fertility = fert,
  sort = stages)

A_subset
lambda(A_subset)

