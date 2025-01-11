setwd("C:\\Users\\aku048\\OneDrive - Bihar Agricultural University\\shared\\pkanalysis\\heatmap")

library(pheatmap)
library(RColorBrewer)
library(dplyr)

# Read data from Excel sheets
file_path_dti <- "Final Relative mean and Drought tolerance indices.csv"
data <- read.csv(file_path_dti)

# Extract the "Rank of genotypes" data
rnk_genotypes <- data %>% 
  filter(!is.na(Genotypes)) %>%    # Remove rows without Genotype values (if applicable)
  select(-`S.No.`)                # Remove the "S. No." column if present

# Prepare the data for pheatmap (remove Genotypes column and use it as rownames)
rnk_matrix <- as.data.frame(rnk.genotypes)
rownames(rnk_matrix) <- rnk_matrix$Genotypes
rnk_matrix <- rnk_matrix[,-1]  # Remove the Genotypes column

# Create heatmap with hierarchical clustering
heatmap_fig3 <- pheatmap(
  rnk_matrix,
  cluster_rows = TRUE,   # Cluster genotypes
  cluster_cols = TRUE,   # Cluster traits
  color = colorRampPalette(brewer.pal(n = 5, name = 'RdBu'))(50),  # Color scheme
  fontsize_row = 10,     # Adjust font size for row labels
  fontsize_col = 10,     # Adjust font size for column labels
  legend = TRUE,
  legend_breaks = c(min(rnk_matrix), max(rnk_matrix)),
  legend_labels = c("Low Rank", "High Rank")
)

# Save the heatmap
ggsave("figures/heatmap_fig3.jpeg", plot = heatmap_fig3, width = 9, height = 10, dpi = 600, bg = "white")
