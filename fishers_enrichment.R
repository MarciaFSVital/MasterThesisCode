setwd("/Users/marcia/Library/CloudStorage/OneDrive-UniversidadedeLisboa/MARCIA")
library(stats)
library(vcd)

# Load the data
results_df <- read.csv("enrichment_results.csv")
filtered_drivers_df <- read.csv("driver_stats.csv", row.names = "driver")  # Set driver as index

# Calculate the number of drivers in each category
num_drivers_D0 <- nrow(subset(filtered_drivers_df, group == 'non_sign'))
num_drivers_D1 <- nrow(subset(filtered_drivers_df, group == 'neg'))
num_drivers_D2 <- nrow(subset(filtered_drivers_df, group == 'pos'))
num_drivers_D3 <- nrow(subset(filtered_drivers_df, group == 'more_pos'))
num_drivers_D4 <- nrow(subset(filtered_drivers_df, group == 'more_neg'))
num_drivers_D5 <- nrow(subset(filtered_drivers_df, group == 'neutral'))

# Combine all driver counts into a single vector
num_drivers_D <- c(num_drivers_D0, num_drivers_D1, num_drivers_D2, num_drivers_D3, num_drivers_D4, num_drivers_D5)

# Initialize an empty list to store results
data_list <- list()

# Loop over each term to perform Fisher's exact test
for (term in 1:nrow(results_df)) {
  values_df <- as.numeric(results_df[term, c("non_sign", "neg", "pos", "more_pos", "more_neg", "neutral")])
  remaining_values <- num_drivers_D - values_df
  matrix_2x6 <- rbind(values_df, remaining_values)
  
  fisher_results <- fisher.test(matrix_2x6, simulate.p.value = TRUE)
  
  # Store the results in the list
  data_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    p.value = fisher_results$p.value
  )
}

# Create a DataFrame from the Fisher test results
fisher_df <- do.call(rbind, lapply(data_list, as.data.frame))
fisher_df$adjusted.p.value <- p.adjust(fisher_df$p.value, method = "BH")

# Initialize lists for Chi-Square and Cramer's V results
chi_square_list <- list()
cramers_v_list <- list()

# Loop to perform Chi-Square test and calculate Cramer's V
for (term in 1:nrow(results_df)) {
  values_df <- as.numeric(results_df[term, c("non_sign", "neg", "pos", "more_pos", "more_neg", "neutral")])
  remaining_values <- num_drivers_D - values_df
  matrix_2x6 <- rbind(values_df, remaining_values)
  
  chi_square_result <- chisq.test(matrix_2x6, simulate.p.value = TRUE)
  chi_square_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    chi_square_p.value = chi_square_result$p.value
  )
  
  cramers_v <- assocstats(matrix_2x6)$cramer
  cramers_v_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    cramers_v = cramers_v
  )
}

# Create DataFrames for Chi-Square and Cramer's V results
chi_square_df <- do.call(rbind, lapply(chi_square_list, as.data.frame))
cramers_v_df <- do.call(rbind, lapply(cramers_v_list, as.data.frame))

# Merge all results into a final DataFrame
final_results <- merge(fisher_df, chi_square_df, by = c("Source", "Name"))
final_results <- merge(final_results, cramers_v_df, by = c("Source", "Name"))

# Save the final results to a CSV file
write.csv(final_results, "combined_statistical_results.csv", row.names = FALSE)