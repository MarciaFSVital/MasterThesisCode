# We employed Fisher’s Exact Test to determine whether the frequency of each enriched term within each category were randomly 
# distributed or biased toward specific categories.
# Fisher’s Exact Test is particularly well-suited for this analysis due to its ability to handle small sample sizes and categorical data.

# Check and install required packages if missing
required_packages <- c("stats", "vcd")

# Function to install missing packages
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Install and load all required packages
lapply(required_packages, install_if_missing)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Set the working directory to where your script is located 
setwd("/Users/marcia/Library/CloudStorage/OneDrive-UniversidadedeLisboa/MARCIA")

# First, verify files exist and print helpful messages
data_files <- c("enrichment_results.csv", "filtered_drivers.csv")
for (file in data_files) {
  file_path <- file.path("data", file)
  if (!file.exists(file_path)) {
    stop(paste("Error:", file, "not found in data directory. Make sure to run the Python script first to generate the required files."))
  }
}

# Load the data
results_df <- read.csv("data/enrichment_results.csv")
filtered_drivers_df <- read.csv("data/filtered_drivers.csv", row.names = "driver") # Set driver as index

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

set.seed(42)  # Add before statistical tests to ensure consistent random sampling

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

# Count significant results (adjusted p-value < 0.05)
significant_count <- sum(final_results$adjusted.p.value < 0.05)
print(paste("Number of significant terms (adjusted p-value < 0.05):", significant_count))

# Save the final results to a CSV file
write.csv(final_results, "data/combined_statistical_results.csv", row.names = FALSE)