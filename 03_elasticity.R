### 03_elasticity ##############################################################
# By: Ryan Ng | Date: 5 Apr 2026 ###############################################

# Calculate elasticity for each of the 20 matrices
# Stores elasticity in 'elasticityresults.csv' 
################################################################################

matrixresults <- read.csv("matrixresults.csv")

# reconstruct matrices 
matrix_list <- list()
for(i in 1:nrow(matrixresults)){
  row <- matrixresults[i, ]
  mat <- matrix(
    c(row$s_s, row$j_s, row$a_s,
      row$s_j, row$j_j, row$a_j,
      row$s_a, row$j_a, row$a_a),
    nrow = 3,
    byrow = TRUE)
  # name each matrix: Range_Site_TransYear_Matrix
  mat_name <- paste(row$Range, row$Site, row$TransYear, "Matrix", sep = "_")
  matrix_list[[mat_name]] <- mat}

# determine lambda 
lambda_values <- sapply(matrix_list, function(A) {lambda(A)})

# determine elasticity and save as .csv file 
elasticity_df <- do.call(rbind, lapply(names(matrix_list), function(mat_name) {
  A <- matrix_list[[mat_name]] # matrix 
  E <- elasticity(A)           # elasticity 
  parts <- strsplit(mat_name, "_")[[1]]
  data.frame(
    Range = parts[1],
    Site = parts[2],
    TransYear = parts[3],
    s_s = E[1, 1], j_s = E[1, 2], a_s = E[1, 3], 
    s_j = E[2, 1], j_j = E[2, 2], a_j = E[2, 3],
    s_a = E[3, 1], j_a = E[3, 2], a_a = E[3, 3])}))
write.csv(elasticity_df, "elasticityresults.csv", row.names = FALSE)
