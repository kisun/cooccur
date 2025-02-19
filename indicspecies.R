options(encoding = "UTF-8")

library(indicspecies); packageVersion("indicspecies")
library(vegan); packageVersion("vegan")
library(dplyr); packageVersion("dplyr")
library(tidyr); packageVersion("tidyr")

# Set working directory
setwd("/scratch/project_2005433/KisunPokharel/projects/meilahti17/")

# Function to merge duplicate rows
merge_duplicates <- function(mat) {
  unique_names <- unique(rownames(mat))
  new_mat <- do.call(rbind, lapply(unique_names, function(name) {
    colSums(mat[rownames(mat) == name, , drop = FALSE])
  }))
  rownames(new_mat) <- unique_names
  new_mat
}

# Read sample info
st <- read.table("meilahti17_cooccurrence/sample_table.txt", header=TRUE, sep = "\t")
groups <- with(st, interaction(urban_suburban, west_east, sep = "-"))
st$groups <- groups
# Read species data
output_data <- read.table("meilahti17_cooccurrence/data_table.txt", header = TRUE, sep = "\t")
output_data
# Prepare species matrix
dt <- data.matrix(output_data %>% select(-c(1:6)))


####### Phylum ############
rownames(dt) <- output_data$Phylum
dtp <- dt[!is.na(rownames(dt)), ]

# Merge duplicates
dtp_m <- merge_duplicates(dtp)

# Align samples
common_samples <- intersect(colnames(dtp_m), st$adele_code)
dtp_m <- dtp_m[, common_samples]

# Transpose matrix
dtp_m_t <- t(dtp_m)
groups <- st$groups[match(common_samples, rownames(dtp_m_t))]

# Indicator species analysis
ind_dtp <- multipatt(dtp_m_t, groups, func = "r.g", control = how(nperm = 9999))
summary(ind_dtp)

##########Class ############
rownames(dt) <- output_data$Class
dtc <- dt[!is.na(rownames(dt)), ]

# Merge duplicates
dtc_m <- merge_duplicates(dtc)

# Align samples
common_samples <- intersect(colnames(dtc_m), st$adele_code)
dtc_m <- dtc_m[, common_samples]

# Transpose matrix
dtc_m_t <- t(dtc_m)
groups <- st$groups[match(common_samples, rownames(dtc_m_t))]

# Indicator species analysis
ind_dtc <- multipatt(dtc_m_t, groups, func = "r.g", control = how(nperm = 9999))
summary(ind_dtc)
############Order ############
rownames(dt) <- output_data$Order
dto <- dt[!is.na(rownames(dt)), ]

# Merge duplicates
dto_m <- merge_duplicates(dto)

# Align samples
common_samples <- intersect(colnames(dto_m), st$adele_code)
dto_m <- dto_m[, common_samples]

# Transpose matrix
dto_m_t <- t(dto_m)
groups <- st$groups[match(common_samples, rownames(dto_m_t))]

# Indicator species analysis
ind_dto <- multipatt(dto_m_t, groups, func = "r.g", control = how(nperm = 9999))
summary(ind_dto)
#########Family ############
rownames(dt) <- output_data$Family
dtf <- dt[!is.na(rownames(dt)), ]

# Merge duplicates
dtf_m <- merge_duplicates(dtf)

# Align samples
common_samples <- intersect(colnames(dtf_m), st$adele_code)
dtf_m <- dtf_m[, common_samples]

# Transpose matrix
dtf_m_t <- t(dtf_m)
groups <- st$groups[match(common_samples, rownames(dtf_m_t))]

# Indicator species analysis
ind_dtf <- multipatt(dtf_m_t, groups, func = "r.g", control = how(nperm = 9999))
summary(ind_dtf)
######## Genus ###########
rownames(dt) <- output_data$Genus
dtg <- dt[!is.na(rownames(dt)), ]

# Merge duplicates
dtg_m <- merge_duplicates(dtg)

# Align samples
common_samples <- intersect(colnames(dtg_m), st$adele_code)
dtg_m <- dtg_m[, common_samples]

# Transpose matrix
dtg_m_t <- t(dtg_m)
groups <- st$groups[match(common_samples, rownames(dtg_m_t))]

# Indicator species analysis
ind_dtg <- multipatt(dtg_m_t, groups, func = "r.g", control = how(nperm = 9999))
summary(ind_dtg)

###########Species #############
rownames(dt) <- output_data$Species
dts <- dt[!is.na(rownames(dt)), ]

# Merge duplicates
dts_m <- merge_duplicates(dts)

# Align samples
common_samples <- intersect(colnames(dts_m), st$adele_code)
dts_m <- dts_m[, common_samples]

# Transpose matrix
dts_m_t <- t(dts_m)
groups <- st$groups[match(common_samples, rownames(dts_m_t))]

# Indicator species analysis
ind_dts <- multipatt(dts_m_t, groups, func = "r.g", control = how(nperm = 9999))
summary(ind_dts)

