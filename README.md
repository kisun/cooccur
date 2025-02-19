# cooccur

Script/s used for cooccurrence analysis. The data/table typically needs some preprocessing prior to running cooccur. Here's how the data was preprocessed.

1. Exclude uncharacterized families i.e., those with “NA” as family name 
2. Combine multiple entries of same family into one 
3. Use binary matrix for cooccurrence analysis (either 0 for absence or 1 for presence)
4. Exclude all entries that have 0 counts across all samples
 

Read more about co-occurrence and cooccur r package [here](https://www.jstatsoft.org/article/view/v069c02)

### cooccur.R 
This script performs cooccurrence analysis at family level (can be adapted to any levels) and visualize the results. 


In addition, it will generate a venn diagram and you can also get a list of unique families for each group.

### cooccur.sh
You can use this to submit cooccur.R as a batch job in slurm environment eg., in csc's taito.


