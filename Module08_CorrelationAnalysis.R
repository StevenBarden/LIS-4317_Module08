# --------------------------------------------------------------------
# SECTION 1: COURSE AND ASSIGNMENT DETAILS
# --------------------------------------------------------------------
# Course      : LIS 4317 - Introduction to Visual Analytics
# Assignment  : Module 08 - Correlation Analysis and ggplot2
# URL         : https://usflearn.instructure.com/courses/1934094/discussion_topics/12570796
# Filename    : Module08_CorrelationAnalysis.R
# Purpose     : Perform correlation analysis and visualize data using ggplot2
# Author      : Steven Barden
# Email       : StevenBarden@usf.edu
# Created     : 2025-03-13-0312.42 (EST)  # <<— Never changes
# Updated     : 2025-03-13-0436.12 (EST)  # <<— Changes every full output
# License     : The Unlicense  # <<— Locked in as per our original joke
# Description : This script explores correlations using scatter plots and ggplot2.
#               It follows the professor's format closely for accurate grading.
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# SECTION 2: ENVIRONMENT SETUP
# --------------------------------------------------------------------

# Set Working Directory to current directory (.)
base_loc <- "."
tryCatch({
  setwd(base_loc)
}, error = function(e) {
  stop("Error setting the working directory: ", e$message)
})

# Display Working Directory
tryCatch({
  print(getwd())
}, error = function(e) {
  stop("Error retrieving the working directory: ", e$message)
})

# Load Required Libraries
required_packages <- c("ggplot2", "dplyr", "tidyverse", "corrplot")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

lapply(required_packages, install_if_missing)

# --------------------------------------------------------------------
# SECTION 3: DATA IMPORT AND CLEANING
# --------------------------------------------------------------------

# Load the mtcars dataset from the current directory
data_path <- file.path(".", "mtcars.csv")

data <- tryCatch({
  read.csv(data_path)
}, error = function(e) {
  stop("Error loading dataset: File missing or unreadable at ", data_path)
})

# Ensure data isn't empty
if (nrow(data) == 0) {
  stop("Error: Loaded dataset is empty.")
}

# Convert variables to factors if necessary
data$cyl <- as.factor(data$cyl)
data$gear <- as.factor(data$gear)

# Remove missing values
data <- na.omit(data)

# Display summary statistics
summary(data)

# --------------------------------------------------------------------
# SECTION 4: CORRELATION ANALYSIS
# --------------------------------------------------------------------

# Compute correlation matrix for numerical variables safely
cor_matrix <- tryCatch({
  cor(data[, sapply(data, is.numeric)], use = "complete.obs")
}, error = function(e) {
  stop("Error computing correlation matrix: ", e$message)
})

# Ensure correlation matrix isn't empty
if (nrow(cor_matrix) == 0 || ncol(cor_matrix) == 0) {
  stop("Error: Correlation matrix calculation failed (empty result).")
}

# Print the correlation matrix
print(cor_matrix)

# Visualize correlation matrix
tryCatch({
  corrplot::corrplot(cor_matrix, method = "color", tl.cex = 0.8)
}, error = function(e) {
  stop("Error generating correlation plot: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 5: SCATTER PLOTS USING GGPLOT2
# --------------------------------------------------------------------

# Ensure required columns exist before plotting
required_vars <- c("hp", "mpg", "wt", "cyl", "gear")
missing_vars <- setdiff(required_vars, names(data))
if (length(missing_vars) > 0) {
  stop("Error: Missing required columns: ", paste(missing_vars, collapse = ", "))
}

# Scatter plot for mpg vs hp with darker green background
scatter_hp_mpg <- tryCatch({
  ggplot(data, aes(x = hp, y = mpg)) +
    geom_point(aes(color = cyl), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = "Horsepower vs MPG",
         x = "Horsepower",
         y = "Miles Per Gallon (MPG)") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#D3D3D3", color = "#4F4F4F"))  # Medium-Light Gray with Dark Gray Border
  
}, error = function(e) {
  stop("Error generating scatter plot for mpg vs hp: ", e$message)
})

# Scatter plot for wt vs mpg with darker green background
scatter_wt_mpg <- tryCatch({
  ggplot(data, aes(x = wt, y = mpg)) +
    geom_point(aes(color = gear), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Car Weight vs MPG",
         x = "Weight (1000 lbs)",
         y = "Miles Per Gallon (MPG)") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#D3D3D3", color = "#4F4F4F"))  # Medium-Light Gray with Dark Gray Border
}, error = function(e) {
  stop("Error generating scatter plot for wt vs mpg: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 6: EXPORT RESULTS
# --------------------------------------------------------------------

# Save the correlation heatmap as an image
cor_matrix_plot_path <- file.path(".", "correlation_matrix.png")

tryCatch({
  png(cor_matrix_plot_path, width = 800, height = 600)
  corrplot::corrplot(cor_matrix, method = "color", tl.cex = 0.8)
  dev.off()
}, error = function(e) {
  stop("Error saving correlation matrix heatmap: ", e$message)
})

# Function to rename existing files before saving new ones
rename_existing_file <- function(filepath) {
  if (file.exists(filepath)) {
    abs_filepath <- normalizePath(filepath, winslash = "/")  # Convert to absolute path
    timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M-%S")
    file_ext <- tools::file_ext(filepath)
    file_base <- tools::file_path_sans_ext(basename(filepath))  # Get filename without path
    new_name <- file.path(dirname(abs_filepath), paste0("z_", file_base, "-", timestamp, ".", file_ext))
    file.rename(abs_filepath, new_name)
  }
}

# Paths for output files
cor_matrix_path <- file.path(".", "correlation_matrix.csv")
scatter_hp_path <- file.path(".", "scatter_hp_mpg.png")
scatter_wt_path <- file.path(".", "scatter_wt_mpg.png")

# Rename existing files if they exist
rename_existing_file(cor_matrix_path)
rename_existing_file(scatter_hp_path)
rename_existing_file(scatter_wt_path)

# Save the correlation matrix
tryCatch({
  write.csv(cor_matrix, cor_matrix_path)
}, error = function(e) {
  stop("Error saving correlation matrix: ", e$message)
})

# Save scatter plots
tryCatch({
  ggsave(filename = scatter_hp_path, plot = scatter_hp_mpg, width = 7, height = 5)
}, error = function(e) {
  stop("Error saving scatter plot: scatter_hp_mpg.png - ", e$message)
})

tryCatch({
  ggsave(filename = scatter_wt_path, plot = scatter_wt_mpg, width = 7, height = 5)
}, error = function(e) {
  stop("Error saving scatter plot: scatter_wt_mpg.png - ", e$message)
})

# --------------------------------------------------------------------
# SECTION 7: CONCLUSION
# --------------------------------------------------------------------

# Print message indicating script completion
cat("\nAnalysis complete. Correlation matrix and scatter plots saved in", getwd(), "\n")
