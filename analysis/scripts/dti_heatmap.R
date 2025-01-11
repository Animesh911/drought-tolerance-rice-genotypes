setwd("C:\\Users\\aku048\\OneDrive - Bihar Agricultural University\\shared\\pkanalysis\\heatmap")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Read data from Excel sheets
file_path_dti <- "Final Relative mean and Drought tolerance indices.xls"

# Read each sheet into a list of data frames
sheets <- excel_sheets(file_path_dti)
data_list <- sheets %>% 
  set_names() %>% 
  map(~ read_excel(file_path_dti, sheet = .x))


#read_xlsx("Final Relative mean and Drought toleranc indices.xls", sheet = 'Rank of genotypes')

#Rank of genotypes

rnk.genotypes <- data_list$`Rank of genotypes` %>% 
  select(-`S. No.`)

#Try one
# Convert the data into long format
rnk_long <- rnk.genotypes %>%
  pivot_longer(cols = -Genotypes, names_to = "Traits", values_to = "Rank")
# Create a heatmap using ggplot2
ggplot(rnk_long, aes(x = Traits, y = Genotypes, fill = Rank)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "navyblue", high = "#AA4A44") +  # Green for low rank, red for high
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Genotypes vs Traits",
       x = "Traits", y = "Genotypes", fill = "Rank")


#Try two, success
# Load required packages
library(pheatmap)
library(RColorBrewer)

# Prepare the data for pheatmap (remove Genotypes column and use it as rownames)
rnk_matrix <- as.data.frame(rnk.genotypes)
rownames(rnk_matrix) <- rnk_matrix$Genotypes
rnk_matrix <- rnk_matrix[,-1]  # Remove the Genotypes column

# Create heatmap with hierarchical clustering
heatmap_fig3 <- pheatmap(rnk_matrix,
         cluster_rows = TRUE,   # Cluster genotypes
         cluster_cols = TRUE,   # Cluster traits
         color = colorRampPalette(brewer.pal(n = 5, name = 'RdBu'))(50),  # Color scheme   c("firebrick4","blue4"),bias=0.5
         fontsize_row = 10,       # Adjust font size for row labels
         fontsize_col = 10)       # Adjust font size for column labels

#add legend heading as "Rank", x axis title as "trait" on top, y axis on left title as "genotypes"

ggsave("heatmap_fig3.jpeg", plot = heatmap_fig3, width = 9, height = 10, dpi = 600, bg="white")


##########