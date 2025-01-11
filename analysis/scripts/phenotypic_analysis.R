# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tibble)
library(ggfortify)
library(factoextra)

# Define file paths (data directory in the Git repository)
data_dir <- "./data"
file_path <- file.path(data_dir, "mean_value.xlsx")

# Read data from Excel sheets
sheets <- excel_sheets(file_path)
data_list <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(file_path, sheet = .x))

# Combine control data (processed data directory)
control_data <- bind_rows(
  data_list$mean_control_2017 %>% mutate(Year = 2017),
  data_list$mean_control_2018 %>% mutate(Year = 2018)
)

# Combine drought data
drought_data <- bind_rows(
  data_list$mean_drought_2017 %>% mutate(Year = 2017),
  data_list$mean_drought_2018 %>% mutate(Year = 2018)
)

# Add condition columns
control_data <- control_data %>% mutate(Condition = "Control")
drought_data <- drought_data %>% mutate(Condition = "Drought")

# Combine all data
all_data <- bind_rows(control_data, drought_data)

# Reshape data for plotting (analysis/scripts directory)
morphological_features <- names(all_data)[3:(ncol(all_data) - 2)]  # Exclude Genotype, Year, Condition

all_data_long <- all_data[-1] %>%  # Exclude S.No.
  pivot_longer(cols = all_of(morphological_features), names_to = "Feature", values_to = "Value")

# Calculate max values for significance annotations
max_values <- all_data_long %>%
  group_by(Feature, Condition) %>%
  summarize(max_value = max(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Feature) %>%
  summarize(y_pos = max(max_value) * 0.95)  # Space above max value for annotations

# Merge with original data for plotting
all_data_long <- all_data_long %>%
  left_join(max_values, by = "Feature")

# Create boxplots with significance annotations
boxplot_fig1 <- ggplot(all_data_long, aes(x = Condition, y = Value, fill = Condition)) +
  geom_boxplot() +
  geom_text(aes(x = 1.5, y = y_pos, label = "**"), size = 6, vjust = 0) +
  facet_wrap(~ Feature, scales = "free", strip.position = "left") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 0),
    legend.position = "top",
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_blank(),
    legend.key.size = unit(1, "cm")
  )

# Save the plot (results directory)
results_dir <- "./analysis/results"
ggsave(file.path(results_dir, "boxplot_fig1.jpeg"), plot = plot_fig1, width = 9, height = 10, dpi = 600, bg = "white")

# Perform Principal Component Analysis (PCA)
# Reshape data for PCA
numeric_columns <- all_data %>% 
  select(GYPP, BY, HI, DFF, DM, PH, RL, TPP, DRW, SF, No.GPP, `1000SW`, PL, FLL, FLW, BLL, BLW, RWC, CT, SPAD)

# Perform PCA
pca_result <- prcomp(numeric_columns, scale. = TRUE)

# Plot PCA results
pca_fig2 <- fviz_pca_biplot(pca_result, 
                geom.ind = "point", 
                pointshape = 21, 
                pointsize = 2.5, 
                fill.ind = all_data$Condition, 
                palette = c("red", "blue"), 
                addEllipses = TRUE, 
                legend.title = list(fill = "Condition"),
                col.var = "contrib", 
                gradient.cols = c("blue", "yellow", "red"), 
                repel = TRUE) +
  theme(legend.position = "top") +
  labs(title = "PCA of Morphological Features", 
       x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"), 
       y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)"))

# Save PCA plot
ggsave(file.path(results_dir, "PCA_plot_biplot.png"), plot = pca_fig2, width = 7, height = 6, dpi = 600, bg = "white")
