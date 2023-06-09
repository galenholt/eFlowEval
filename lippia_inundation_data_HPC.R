# script paralleling lippia_inundation_data.qmd, but for the HPC, which needs a script

# ugh this needs cleanup
library(here)
library(tidyverse)
library(sf)
library(stars)
library(foreach)
library(doFuture)
library(doRNG)

source('directorySet.R')

registerDoFuture()

plan(list(tweak(batchtools_slurm,
                template = "batchtools.slurm.tmpl",
                resources = list(time = 5,
                                 ntasks.per.node = 64, 
                                 mem.per.cpu = "8GB",
                                 job.name = 'NewName')),
          multicore))





