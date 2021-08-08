#!/bin/bash

# # Working on setting up for Pearcey
# # Each node has 10 cores. 325 nodes have 128GB memory and 500GB disk, 16 have 512GB mem and 100GB disk

# # See OneNote 'Basic HPC workflow to build on' from 9 Feb 2021 for more detail about connection and file transfer


#SBATCH --time=2:00:00 # request time (walltime, not compute time)
#SBATCH --mem=128GB # request memory
#SBATCH --nodes=1 # number of nodes
#SBATCH --ntasks-per-node=10 # This is the cores per node # If I want 20 cpus on 2 nodes, for ex, use --nodes=2 --ntasks-per-node=10

#SBATCH -o test_%A_%a.out # Standard output
#SBATCH -e test_%A_%a.err # Standard error

# timing
begin=`date +%s`

# That really might be a bit absurd if we're not going to use all of it.
# Should really test just running off scratch directly

module load R/4.0.2

Rscript hpc_wrap.R "Scripts/DataProcessing/testingandscratch/inunSpeedTest.R"

# Copy to ruby datastore. copying to the {lw-mer} on bowen is at /datasets/work/lw-mer/work/galen_holt, but need to sort this out a bit better
# cp -rp testout.rdata /datastore/hol436/HPC_testing


end=`date +%s`
elapsed=`expr $end - $begin`

echo Time taken for code: $elapsed

# below here is a bunch of notes about slurm array setups
# Leaving the notes below from EWKR, since we will likely do somethign withthe slurm array task ID stuff

# To run on the cluster, call this file with <sbatch --array=*RANGE* clustRunner_SuperFASI.sh> RANGE determines which runs, although there are some internal checks as well:
# 1-4 is the overall, including consumer comparisons sbatch --array=1-4 clustRunner_SuperFASI.sh
# 5-8 is the within-consumer sbatch --array=5-8 clustRunner_SuperFASI.sh
# 9-13 is the within habitat and consumer individual models (usually not run) sbatch --array=9-13 clustRunner_SuperFASI.sh
# All is sbatch --array=1-13 clustRunner_SuperFASI.sh
# Most common sbatch --array=1-8 clustRunner_OvensFA.sh

# For some reason I get a bunch of errors about not being able to run conda activate Bio from this shell, but then everything works, so I'll just ignore it I guess

# Looks like there are sbatch ways to run arrays of runs, etc. Or could do what I did with streams, and write a script that then calls sbatch scriptname.sh for a bunch of scripts.
# begin=`date +%s`
# Start miniconda; not sure why
# conda activate Bio

# Run the Rscript. Can I potentially call separate scripts to separate nodes? Or CPUs?
# Could probably make this more generic and pass the script to run to the sbatch call, but it's probably nice to have separate scripts to keep track
# Rscript clusterwrapperPar.R "/foodwebestimation/mixRunnerOFA.R" ${SLURM_ARRAY_TASK_ID}

# conda deactivate

# end=`date +%s`
# elapsed=`expr $end - $begin`

# echo Time taken: $elapsed