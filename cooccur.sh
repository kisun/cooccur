#!/bin/bash -l
#SBATCH --job-name=cooccur_g
#SBATCH --account=project_2005433
#SBATCH --output=output_%j.txt
#SBATCH --error=errors_%j.txt
#SBATCH --partition=longrun
#SBATCH --time=12-00:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=120000

# Load r-env-singularity
module load r-env-singularity

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
    sed -i '/TMPDIR/d' ~/.Renviron
fi

# Specify a temp folder path
echo "TMPDIR=/scratch/project_2005433/KisunPokharel/projects/meilahti17" >> ~/.Renviron

# Run the R script
srun singularity_wrapper exec Rscript --no-save cooccur.R
