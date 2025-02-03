options(encoding = "UTF-8")
.libPaths(c("/projappl/project_2005433/project_rpackages_4.2.1", .libPaths()))
libpath <- .libPaths()[1]

library(cooccur); packageVersion("cooccur")
library(visNetwork); packageVersion("visNetwork")
library(dplyr); packageVersion("dplyr")
library(tidyr); packageVersion("tidyr")

setwd("/scratch/project_2005433/KisunPokharel/projects/meilahti17/")

#read in sample information table (st = sample table)
st<-read.table("meilahti17_cooccurrence/sample_table.txt", header=TRUE, sep = "\t")
dim(st)
st
st <- data.frame(st)
#read in the output file that has all occurrence data
output_data<-read.table("meilahti17_cooccurrence/data_table.txt", header = TRUE, sep = "\t")
dim(output_data)

head(output_data)
#create a subset of taxonomy information
tax<-output_data %>% select(1:6)
head(tax)
phylum <- tax[,1]
class <- tax[, 2]
order<-tax[,3]
family<-tax[,4]
genus<-tax[,5]
species<-tax[,6]

#create a subset of all samples and data (dt = data table)
dt<-output_data %>% select(-c(1:6))


########## WEST ##############

##  Urban west (uw)
#create subset of sample information to get all the urban part of the cities that belong to "west" block of countries.
uw <- (st %>% filter(urban_suburban == "Urban", west_east == "West"))
uw
uw_id <- uw[,1]
uw_id
uw_dt <- dt %>% select(all_of(uw_id))
head(uw_dt)
dim(uw_dt)

##########GENUS###########

#prepare binary matrix
uw_dt_bin<-apply(uw_dt, 2, function(x) ifelse(x >0, 1, x))
rownames(uw_dt_bin)<-genus
uw_dt_bin<-uw_dt_bin[!is.na(rownames(uw_dt_bin)),]
head(uw_dt_bin)
dim(uw_dt_bin)

# Run cooccurrence analysis
c_uw <- print(cooccur(uw_dt_bin, spp_names = TRUE))

write.csv(c_uw, file = "cooccur_uw_genus.csv")
head(c_uw)

# Prepare nodes and edges for urban west visualization
n_uw <- data.frame(id = 1:nrow(uw_dt), label = rownames(uw_dt_bin), shadow = TRUE)
e_uw <- data.frame(from = c_uw$sp1,
                           to = c_uw$sp2,
                           color = ifelse(c_uw$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
                           dashes = ifelse(c_uw$p_lt <= 0.05, TRUE, FALSE))

uw_nicely <- visNetwork(nodes = n_uw, edges = e_uw) %>% visIgraphLayout(layout = "layout_nicely")
visSave(uw_nicely, file = "uw_nicely.html")
uw_kk <- visNetwork(nodes = n_uw, edges = e_uw) %>% visIgraphLayout(layout = "layout_with_kk")
visSave(uw_kk, file = "uw_kk.html")
uw_fr <- visNetwork(nodes = n_uw, edges = e_uw) %>% visIgraphLayout(layout = "layout_with_fr")
visSave(uw_fr, file = "uw_fr.html")


##  suburban west (sw)
#create subset of sample information to get all the suburban part of the cities that belong to "west" block of countries.
sw <- st %>% filter(urban_suburban == "Suburban", west_east == "West")
#get all samples in main data table that belong to sw
sw
sw_id <- sw[,1]
sw_id
sw_dt <- dt %>% select(all_of(sw_id))
head(sw_dt)
dim(sw_dt)


#prepare binary matrix
sw_dt_bin<-apply(sw_dt, 2, function(x) ifelse(x >0, 1, x))
rownames(sw_dt_bin)<-genus
sw_dt_bin<-sw_dt_bin[!is.na(rownames(sw_dt_bin)),]
head(sw_dt_bin)
# Run cooccurrence analysis
c_sw <- print(cooccur(sw_dt_bin, spp_names = TRUE))

write.csv(c_sw, file = "cooccur_sw.csv")
head(c_sw)

# Prepare nodes and edges for suburban west visualization
n_sw <- data.frame(id = 1:nrow(sw_dt), label = rownames(sw_dt_bin), shadow = TRUE)
e_sw <- data.frame(from = c_sw$sp1,
                              to = c_sw$sp2,
                              color = ifelse(c_sw$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
                              dashes = ifelse(c_sw$p_lt <= 0.05, TRUE, FALSE))

visNetwork(nodes = n_sw, edges = e_sw) %>% visIgraphLayout(layout = "layout_nicely")
visNetwork(nodes = n_sw, edges = e_sw) %>% visIgraphLayout(layout = "layout_with_kk")
visNetwork(nodes = n_sw, edges = e_sw) %>% visIgraphLayout(layout = "layout_with_fr")


########## EAST ##############

##  Urban East
# Create a subset of sample information to get all the urban parts of the cities that belong to the "east" block of countries.
ue <- st %>% filter(urban_suburban == "Urban", west_east == "East")
head(ue)
ue_id <- ue[,1]
ue_id
ue_dt <- dt %>% select(all_of(ue_id))
head(ue_dt)
dim(ue_dt)

# Prepare binary matrix
ue_dt_bin <- apply(ue_dt, 2, function(x) ifelse(x > 0, 1, x))
rownames(ue_dt_bin) <- genus
ue_dt_bin<-ue_dt_bin[!is.na(rownames(ue_dt_bin)),]
head(ue_dt_bin)
dim(ue_dt_bin)

# Run co-occurrence analysis
c_ue <- print(cooccur(ue_dt_bin, spp_names = TRUE))

# Save co-occurrence results to CSV
write.csv(c_ue, file = "cooccur_ue.csv", row.names = FALSE)
head(c_ue)

# Prepare nodes and edges for urban east visualization
n_ue <- data.frame(id = 1:nrow(ue_dt), label = rownames(ue_dt_bin), shadow = TRUE)
e_ue <- data.frame(from = c_ue$sp1,
                           to = c_ue$sp2,
                           color = ifelse(c_ue$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
                           dashes = ifelse(c_ue$p_lt <= 0.05, TRUE, FALSE))

# Visualize Urban East Network
visNetwork(nodes = n_ue, edges = e_ue) %>% visIgraphLayout(layout = "layout_nicely")
visNetwork(nodes = n_ue, edges = e_ue) %>% visIgraphLayout(layout = "layout_with_kk")
visNetwork(nodes = n_ue, edges = e_ue) %>% visIgraphLayout(layout = "layout_with_fr")

##  Suburban East
# Create a subset of sample information to get all the suburban parts of the cities that belong to the "east" block of countries.
se <- st %>% filter(urban_suburban == "Suburban", west_east == "East")
head(se)
se_id <- se[,1]
se_id
se_dt <- dt %>% select(all_of(se_id))
head(se_dt)
dim(se_dt)

# Prepare binary matrix
se_dt_bin <- apply(se_dt, 2, function(x) ifelse(x > 0, 1, x))
rownames(se_dt_bin) <- genus
se_dt_bin<-se_dt_bin[!is.na(rownames(se_dt_bin)),]
head(se_dt_bin)

# Run co-occurrence analysis for suburban east
c_se <- print(cooccur(se_dt_bin, spp_names = TRUE))

# Save co-occurrence results to CSV
write.csv(c_se, file = "cooccur_se.csv", row.names = FALSE)
head(c_se)

# Prepare nodes and edges for suburban east visualization
n_se <- data.frame(id = 1:nrow(se_dt), label = rownames(se_dt_bin), shadow = TRUE)
e_se <- data.frame(from = c_se$sp1,
                   to = c_se$sp2,
                   color = ifelse(c_se$p_lt <= 0.05, "#B0B2C1", "#3C3F51"),
                   dashes = ifelse(c_se$p_lt <= 0.05, TRUE, FALSE))

# Visualize Suburban East Network
visNetwork(nodes = n_se, edges = e_se) %>% visIgraphLayout(layout = "layout_nicely")
visNetwork(nodes = n_se, edges = e_se) %>% visIgraphLayout(layout = "layout_with_kk")
visNetwork(nodes = n_se, edges = e_se) %>% visIgraphLayout(layout = "layout_with_fr")

