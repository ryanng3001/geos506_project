### 07_LTRE ####################################################################
# By: Ryan Ng | Date: 15 Mar 2026 ##############################################

# Perform a fixed design LTRE with Central pop as reference and Northern pop as 
# "treatment". Bootstrap to account for uncertainty between sites and years. 
# Also creates a plot. 
################################################################################

# A. data preparation 
# A.1 reconstruct matrices
matrixresults <- read.csv("matrixresults.csv")
matrix_list <- list()
for(i in 1:nrow(matrixresults)){
  row <- matrixresults[i, ]
  mat <- matrix(
    c(row$s_s, row$j_s, row$a_s,
      row$s_j, row$j_j, row$a_j,
      row$s_a, row$j_a, row$a_a),
    nrow = 3,
    byrow = TRUE)
  rownames(mat) <- c("seedling_t1", "juvenile_t1", "adult_t1")
  colnames(mat) <- c("seedling_t", "juvenile_t", "adult_t")
  # name each matrix: Range_Site_TransYear_Matrix
  mat_name <- paste(row$Range, row$Site, row$TransYear, "Matrix", sep = "_")
  matrix_list[[mat_name]] <- mat}

# A.2 split matrices by range (Center/ North)
# split matrices by range 
center_mats <- matrix_list[grep("^Center_", names(matrix_list))]  # 9 
north_mats  <- matrix_list[grep("^North_", names(matrix_list))]   # 11

# A.3 function to calculate a mean matrix, apply to Center and North
mean_matrix <- function(mat_list){
  Reduce("+", mat_list) / length(mat_list)}
center_mean <- mean_matrix(center_mats)
north_mean  <- mean_matrix(north_mats)

# A.4 calculate lambda and delta_lambda 
lambda_center <- lambda(center_mean)
lambda_north  <- lambda(north_mean)
delta_lambda <- lambda_north - lambda_center

# B. bootstrapping (sample w/ replacement from site means)
# B.1 determine mean site matrix across years 
site_info <- unique(matrixresults[, c("Range", "Site")])
site_matrix_list <- list() # build mean matrix for each site  
for(i in 1:nrow(site_info)){
  this_range <- site_info$Range[i]
  this_site  <- site_info$Site[i]
  site_rows <- matrixresults[matrixresults$Range == this_range &
                               matrixresults$Site == this_site, ]
  # rebuild each yearly matrix for this site
  temp_list <- list()
  for(j in 1:nrow(site_rows)){
    row <- site_rows[j, ]
    mat <- matrix(
      c(row$s_s, row$j_s, row$a_s,
        row$s_j, row$j_j, row$a_j,
        row$s_a, row$j_a, row$a_a),
      nrow = 3,
      byrow = TRUE)
    rownames(mat) <- c("seedling_t1", "juvenile_t1", "adult_t1")
    colnames(mat) <- c("seedling_t", "juvenile_t", "adult_t")
    temp_list[[j]] <- mat}
  # average across years within this site
  site_mean_mat <- mean_matrix(temp_list)
  # save
  site_name <- paste(this_range, this_site, "SiteMean", sep = "_")
  site_matrix_list[[site_name]] <- site_mean_mat}
# split site-level matrices by range
center_sites <- site_matrix_list[grep("^Center_", names(site_matrix_list))]
north_sites  <- site_matrix_list[grep("^North_", names(site_matrix_list))]

# B.2 observed LTRE based on site mean matrices (for reference; not BS yet)
center_obs <- mean_matrix(center_sites)
north_obs  <- mean_matrix(north_sites)
lambda_center_obs <- lambda(center_obs)
lambda_north_obs  <- lambda(north_obs)
delta_lambda_obs  <- lambda_north_obs - lambda_center_obs
sens_center_obs <- sensitivity(center_obs)
delta_A_obs <- north_obs - center_obs
ltre_obs <- delta_A_obs * sens_center_obs

# B.3 perform bootstrap 
set.seed(123)   # for reproducibility
n_boot <- 1000  # number of bootstrap iterations
# result matrix: row rep replicate; col rep delta lambda and LTRE contrib
boot_results <- data.frame(
  delta_lambda = numeric(n_boot),
  s_s = numeric(n_boot),
  j_s = numeric(n_boot),
  a_s = numeric(n_boot),
  s_j = numeric(n_boot),
  j_j = numeric(n_boot),
  a_j = numeric(n_boot),
  s_a = numeric(n_boot),
  j_a = numeric(n_boot),
  a_a = numeric(n_boot))
for(b in 1:n_boot){
  center_sample <- sample(center_sites,
                          size = length(center_sites),
                          replace = TRUE)
  north_sample <- sample(north_sites,
                         size = length(north_sites),
                         replace = TRUE)
  # calculate bootstrap mean matrix for each range
  center_boot <- mean_matrix(center_sample)
  north_boot  <- mean_matrix(north_sample)
  # calculate lambda difference
  lambda_center_boot <- lambda(center_boot)
  lambda_north_boot  <- lambda(north_boot)
  delta_lambda_boot  <- lambda_north_boot - lambda_center_boot
  # fixed LTRE
  sens_center_boot <- sensitivity(center_boot)
  delta_A_boot     <- north_boot - center_boot
  ltre_boot        <- delta_A_boot * sens_center_boot
  # store results 
  boot_results$delta_lambda[b] <- delta_lambda_boot
  boot_results$s_s[b] <- ltre_boot[1,1]
  boot_results$j_s[b] <- ltre_boot[1,2]
  boot_results$a_s[b] <- ltre_boot[1,3]
  boot_results$s_j[b] <- ltre_boot[2,1]
  boot_results$j_j[b] <- ltre_boot[2,2]
  boot_results$a_j[b] <- ltre_boot[2,3]
  boot_results$s_a[b] <- ltre_boot[3,1]
  boot_results$j_a[b] <- ltre_boot[3,2]
  boot_results$a_a[b] <- ltre_boot[3,3]}

# B.4 prepare results
transitions <- c("s_s", "j_s", "a_s",
                 "s_j", "j_j", "a_j",
                 "s_a", "j_a", "a_a")
# observed contributions as a named vector
obs_contrib <- c(
  s_s = ltre_obs[1,1],
  j_s = ltre_obs[1,2],
  a_s = ltre_obs[1,3],
  s_j = ltre_obs[2,1],
  j_j = ltre_obs[2,2],
  a_j = ltre_obs[2,3],
  s_a = ltre_obs[3,1],
  j_a = ltre_obs[3,2],
  a_a = ltre_obs[3,3])
boot_summary <- data.frame(
  transition = transitions,
  observed = NA,
  boot_mean = NA,
  lower_95 = NA,
  upper_95 = NA,
  sign_consistency = NA)
for(i in 1:length(transitions)){
  tr <- transitions[i]
  x  <- boot_results[[tr]]
  boot_summary$observed[i]  <- obs_contrib[tr]
  boot_summary$boot_mean[i] <- mean(x, na.rm = TRUE)
  boot_summary$lower_95[i]  <- quantile(x, 0.025, na.rm = TRUE)
  boot_summary$upper_95[i]  <- quantile(x, 0.975, na.rm = TRUE)
  # proportion of replicates with same sign as observed
  if(obs_contrib[tr] > 0){
    boot_summary$sign_consistency[i] <- mean(x > 0, na.rm = TRUE)
  } else if(obs_contrib[tr] < 0){
    boot_summary$sign_consistency[i] <- mean(x < 0, na.rm = TRUE)
  } else {
    boot_summary$sign_consistency[i] <- NA}}
boot_summary

# B.5 plot results 
# reorder transitions by observed contribution
# set order + labels + colours
boot_summary$transition <- factor(
  boot_summary$transition,
  levels = c("a_a", "s_a", "j_a", "s_j", "j_j", "j_s", "s_s", "a_s", "a_j")
)

boot_summary$transition_label <- factor(
  boot_summary$transition,
  levels = c("a_a", "s_a", "j_a", "s_j", "j_j", "j_s", "s_s", "a_s", "a_j"),
  labels = c(
    "Adult survival",
    "Seedling to adult",
    "Juv. to adult",
    "Seedling to juv.",
    "Juv. survival",
    "Juv. to seedling",
    "Seedling survival",
    "Fecundity",
    "Adult to juv."
  )
)

boot_summary$group_col <- ifelse(
  boot_summary$transition %in% c("a_s", "a_a", "j_j"),
  as.character(boot_summary$transition),
  "other"
)
boot_summary_plot <- boot_summary %>%
  filter(!transition %in% c("j_s", "s_s", "a_j"))
ggplot(boot_summary_plot, aes(x = transition_label, y = observed, colour = group_col)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.15) +
  geom_point(size = 5) +
  scale_colour_manual(
    values = c(
      "a_s" = "black",   # change if you want
      "a_a" = "black",   # change if you want
      "j_j" = "black",   # change if you want
      "other" = "grey70"
    ),
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = "LTRE contribution (Δλ = -0.16)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 22, hjust = 1),
    panel.background = element_rect(fill = "#f5f3ef", color = NA),
    plot.background  = element_rect(fill = "#f5f3ef", color = NA),
    axis.title = element_text(size = 18),  
    axis.text  = element_text(size = 18),   
  )

