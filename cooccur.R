# Set encoding and library paths
options(encoding = "UTF-8")
.libPaths(c("/projappl/project_2005433/project_rpackages_4.2.1", .libPaths()))
libpath <- .libPaths()[1]

# Load necessary libraries and check versions
library(cooccur); packageVersion("cooccur")
library(visNetwork); packageVersion("visNetwork")
library(dplyr); packageVersion("dplyr")
library(tidyr); packageVersion("tidyr")
library(VennDiagram)
library(grid)

# Set working directory
setwd("/scratch/project_2005433/KisunPokharel/projects/meilahti17/")

# Read sample information table
st <- read.table("meilahti17_cooccurrence/sample_table.txt", header = TRUE, sep = "\t")
st <- data.frame(st)  # Convert to data frame for consistency
dim(st)
print(st)

# Read occurrence data
output_data <- read.table("meilahti17_cooccurrence/data_table.txt", header = TRUE, sep = "\t")
dim(output_data)
head(output_data)

# Extract taxonomy information
tax <- output_data %>% select(1:6)
head(tax)

# Define taxonomic ranks
phylum <- tax[, 1]
class <- tax[, 2]
order <- tax[, 3]
family <- tax[, 4]
genus <- tax[, 5]
species <- tax[, 6]

# Extract sample data without taxonomy
dt <- output_data %>% select(-c(1:6))
head(dt)
dim(dt)

# Function to merge duplicate row names by summing values into one row
merge_duplicates <- function(mat) {
  row_names <- rownames(mat)
  unique_names <- unique(row_names)
  new_mat <- matrix(nrow = 0, ncol = ncol(mat))
  colnames(new_mat) <- colnames(mat)
  
  for(name in unique_names) {
    indices <- which(row_names == name)
    if(length(indices) > 1) {
      # Sum the rows for duplicates into one row
      merged_row <- colSums(mat[indices, ])
      new_mat <- rbind(new_mat, ifelse(merged_row > 0, 1, 0))
      rownames(new_mat)[nrow(new_mat)] <- name
    } else {
      # If no duplicates, just add the row
      new_mat <- rbind(new_mat, ifelse(mat[indices, ] > 0, 1, 0))
      rownames(new_mat)[nrow(new_mat)] <- name
    }
  }
  return(new_mat)
}

# Analysis by Family. If you want to use different level of taxa just replace family with what level you want. Eg., genus, phylum, etc.
## Urban West
uw <- st %>% filter(urban_suburban == "Urban", west_east == "West")
uw_id <- uw[, 1]
uw_dt <- dt %>% select(all_of(uw_id))
head(uw_dt)

# Prepare binary matrix
uw_dt_bin <- apply(uw_dt, 2, function(x) ifelse(x > 0, 1, x))
rownames(uw_dt_bin) <- family

# Clean up matrix
uw_dt_bin <- uw_dt_bin[!is.na(rownames(uw_dt_bin)), ]
dim(uw_dt_bin)

# Merge duplicates
uw_dt_bin <- merge_duplicates(uw_dt_bin)
dim(uw_dt_bin)

# Filter out zero-sum rows
uw_dt_bin1 <- uw_dt_bin[rowSums(uw_dt_bin) != 0, ]
dim(uw_dt_bin1)
head(uw_dt_bin1)

# Co-occurrence analysis
c_uw <- cooccur(uw_dt_bin1, spp_names = TRUE)
summary(c_uw)
pc_uw <- print(c_uw)
nrow(pc_uw)

# Save results
write.csv(pc_uw, file = "cooccur_uw_family.csv")

# Prepare visualization data
n_uw <- data.frame(id = 1:nrow(uw_dt_bin1), label = rownames(uw_dt_bin1), shadow = TRUE)
dim(n_uw)
e_uw <- data.frame(
  from = pc_uw$sp1,
  to = pc_uw$sp2,
  color = ifelse(pc_uw$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
  dashes = ifelse(pc_uw$p_lt <= 0.05, TRUE, FALSE)
)

# Visualize network
uw_nicely <- visNetwork(nodes = n_uw, edges = e_uw) %>% visIgraphLayout(layout = "layout_nicely")
visSave(uw_nicely, file = "uw_family_nicely.html")
uw_kk <- visNetwork(nodes = n_uw, edges = e_uw) %>% visIgraphLayout(layout = "layout_with_kk")
visSave(uw_kk, file = "uw_family_kk.html")

## Suburban West
sw <- st %>% filter(urban_suburban == "Suburban", west_east == "West")
sw_id <- sw[, 1]
sw_dt <- dt %>% select(all_of(sw_id))
head(sw_dt)

# Prepare binary matrix
sw_dt_bin <- apply(sw_dt, 2, function(x) ifelse(x > 0, 1, x))
rownames(sw_dt_bin) <- family

# Clean up matrix
sw_dt_bin <- sw_dt_bin[!is.na(rownames(sw_dt_bin)), ]
dim(sw_dt_bin)

# Merge duplicates
sw_dt_bin <- merge_duplicates(sw_dt_bin)
dim(sw_dt_bin)

# Filter out zero-sum rows
sw_dt_bin1 <- sw_dt_bin[rowSums(sw_dt_bin) != 0, ]
dim(sw_dt_bin1)
head(sw_dt_bin1)

# Co-occurrence analysis
c_sw <- cooccur(sw_dt_bin1, spp_names = TRUE)
summary(c_sw)
pc_sw <- print(c_sw)
nrow(pc_sw)

# Save results
write.csv(pc_sw, file = "cooccur_sw_family.csv")

# Prepare visualization data
n_sw <- data.frame(id = 1:nrow(sw_dt_bin1), label = rownames(sw_dt_bin1), shadow = TRUE)
dim(n_sw)
e_sw <- data.frame(
  from = pc_sw$sp1,
  to = pc_sw$sp2,
  color = ifelse(pc_sw$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
  dashes = ifelse(pc_sw$p_lt <= 0.05, TRUE, FALSE)
)

# Visualize network
sw_nicely <- visNetwork(nodes = n_sw, edges = e_sw) %>% visIgraphLayout(layout = "layout_nicely")
visSave(sw_nicely, file = "sw_family_nicely.html")
sw_kk <- visNetwork(nodes = n_sw, edges = e_sw) %>% visIgraphLayout(layout = "layout_with_kk")
visSave(sw_kk, file = "sw_family_kk.html")

## Urban East
ue <- st %>% filter(urban_suburban == "Urban", west_east == "East")
ue_id <- ue[, 1]
ue_dt <- dt %>% select(all_of(ue_id))
head(ue_dt)

# Prepare binary matrix
ue_dt_bin <- apply(ue_dt, 2, function(x) ifelse(x > 0, 1, x))
rownames(ue_dt_bin) <- family

# Clean up matrix
ue_dt_bin <- ue_dt_bin[!is.na(rownames(ue_dt_bin)), ]
dim(ue_dt_bin)

# Merge duplicates
ue_dt_bin <- merge_duplicates(ue_dt_bin)
dim(ue_dt_bin)

# Filter out zero-sum rows
ue_dt_bin1 <- ue_dt_bin[rowSums(ue_dt_bin) != 0, ]
dim(ue_dt_bin1)
head(ue_dt_bin1)

# Co-occurrence analysis
c_ue <- cooccur(ue_dt_bin1, spp_names = TRUE)
summary(c_ue)
pc_ue <- print(c_ue)
nrow(pc_ue)

# Save results
write.csv(pc_ue, file = "cooccur_ue_family.csv")

# Prepare visualization data
n_ue <- data.frame(id = 1:nrow(ue_dt_bin1), label = rownames(ue_dt_bin1), shadow = TRUE)
dim(n_ue)
e_ue <- data.frame(
  from = pc_ue$sp1,
  to = pc_ue$sp2,
  color = ifelse(pc_ue$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
  dashes = ifelse(pc_ue$p_lt <= 0.05, TRUE, FALSE)
)

# Visualize network
ue_nicely <- visNetwork(nodes = n_ue, edges = e_ue) %>% visIgraphLayout(layout = "layout_nicely")
visSave(ue_nicely, file = "ue_family_nicely.html")
ue_kk <- visNetwork(nodes = n_ue, edges = e_ue) %>% visIgraphLayout(layout = "layout_with_kk")
visSave(ue_kk, file = "ue_family_kk.html")

## Suburban East
se <- st %>% filter(urban_suburban == "Suburban", west_east == "East")
se_id <- se[, 1]
se_dt <- dt %>% select(all_of(se_id))
head(se_dt)

# Prepare binary matrix
se_dt_bin <- apply(se_dt, 2, function(x) ifelse(x > 0, 1, x))
rownames(se_dt_bin) <- family

# Clean up matrix
se_dt_bin <- se_dt_bin[!is.na(rownames(se_dt_bin)), ]
dim(se_dt_bin)

# Merge duplicates
se_dt_bin <- merge_duplicates(se_dt_bin)
dim(se_dt_bin)

# Filter out zero-sum rows
se_dt_bin1 <- se_dt_bin[rowSums(se_dt_bin) != 0, ]
dim(se_dt_bin1)
head(se_dt_bin1)

# Co-occurrence analysis
c_se <- cooccur(se_dt_bin1, spp_names = TRUE)
summary(c_se)
pc_se <- print(c_se)
nrow(pc_se)

# Save results
write.csv(pc_se, file = "cooccur_se_family.csv")

# Prepare visualization data
n_se <- data.frame(id = 1:nrow(se_dt_bin1), label = rownames(se_dt_bin1), shadow = TRUE)
dim(n_se)
e_se <- data.frame(
  from = pc_se$sp1,
  to = pc_se$sp2,
  color = ifelse(pc_se$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
  dashes = ifelse(pc_se$p_lt <= 0.05, TRUE, FALSE)
)

# Visualize network
se_nicely <- visNetwork(nodes = n_se, edges = e_se) %>% visIgraphLayout(layout = "layout_nicely")
visSave(se_nicely, file = "se_family_nicely.html")
se_kk <- visNetwork(nodes = n_se, edges = e_se) %>% visIgraphLayout(layout = "layout_with_kk")
visSave(se_kk, file = "se_family_kk.html")

# Unique Families Analysis
# Extract row names for each subset
rnse <- rownames(se_dt_bin1)
rnsw <- rownames(sw_dt_bin1)
rnue <- rownames(ue_dt_bin1)
rnuw <- rownames(uw_dt_bin1)

# Find unique items
unique_se <- setdiff(rnse, c(rnsw, rnue, rnuw))
unique_sw <- setdiff(rnsw, c(rnse, rnue, rnuw))
unique_ue <- setdiff(rnue, c(rnse, rnsw, rnuw))
unique_uw <- setdiff(rnuw, c(rnse, rnsw, rnue))

# Format data for CSV
df_length <- max(length(unique_se), length(unique_sw), length(unique_ue), length(unique_uw))
data <- data.frame(
  "Suburban East" = c(unique_se, rep("", df_length - length(unique_se))),
  "Suburban West" = c(unique_sw, rep("", df_length - length(unique_sw))),
  "Urban East" = c(unique_ue, rep("", df_length - length(unique_ue))),
  "Urban West" = c(unique_uw, rep("", df_length - length(unique_uw)))
)

# Save to CSV
write.csv(data, "unique_items_family.csv", row.names = FALSE, na = "")

# Venn Diagram for Family Overlaps
sets <- list(
  "Suburban East" = rnse,
  "Suburban West" = rnsw,
  "Urban East" = rnue,
  "Urban West" = rnuw
)

# Create Venn Diagram
pdf("Venn_4way_family.pdf")
venn_plot <- venn.diagram(
  x = sets,
  filename = NULL,
  output = FALSE,
  
  # Visual settings
  lwd = 0,  # No border lines
  lty = "blank",
  fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
  
  cex = 0.8,
  fontface = "bold",
  fontfamily = "sans",
  
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.fontfamily = "sans",
  rotation.degree = 0,
  margin = 0.2
)

grid.draw(venn_plot)  # Render the Venn diagram
dev.off()  # Close the PDF file
