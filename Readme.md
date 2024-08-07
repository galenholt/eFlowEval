# eFlowEval modeling framework repository

This repository contains the code for the eFlowEval modelling framework. It is built primarily in R, but is not (currently) an R package, and contains some shell scripts to manage running on SLURM HPCs.

To get set up, clone this repository, and use {renv} to establish the package environment `renv::restore()` to ensure there are no package conflicts.

## Structure

The `directorySet.R` script does some setup work to establish standard input and output directories, and make some other configuration changes depending on whether the eFlowEval framework is running on local Windows machines or remote HPCs at the CSIRO. It is likely to need to be edited to use in new environments.

The core functionality is contained as a package, with functions in `/R`. These are then called by notebooks. Bespoke functions can be built for the response models, which can be provided here or by the user.

The most up-to-date example of this workflow is in `galenholt/SRA`, and this directory is in the process of being updated to that system.

The code structure consists of data processing code for the driver data, located in `/Scripts/DataProcessing`, which are typically functions to process each set of driver data. The data itself is typically large and so located elsewhere (with locations as set in `directorySet.R`). Sources for original data are provided in Supplementary Material. Because each response model needs different data or processes data in different ways, the functions in `Scripts/DataProcessing` are bespoke for each model while using the same fundamental backbone functions and format.

The code will generate a `/datOut` folder, where processed data is saved, with location defined in `directorySet.R`.

Stricture and other response relationships (e.g. metabolism) are defined with scripts and functions in `/Strictures`. Like the dataprocessing, these functions are bespoke, capturing the particular responses of each group to the data while using the same fundamental backbone functions and format.The processed responses are then saved to a `/strictOut` directory the code creates.

Figures and other synthesis of the output are prepared in `/Scripts/plotting`, and organised and produced in Quarto notebooks in `/notebooks`.

## Data

The data is expected to be in a directory outside the repo, (set as `datDir` in `directorySet.R`). Currently, the data is all open-source (ANAE layers, soil temp from MODIS/NASA, soil moisture from AWRA-L (Australian Bureau of Meteorology), and some simple spatial layers for the Murray-Darling Basin and RAMSAR wetland sites, with citations in Supplementary Material. As used, the data sits in a directory at CSIRO and is available on request.

## Processing

Most processing is designed to occur either locally or on an HPC running a SLURM job scheduler. The SLURM approach has changed through the life of the project. The most up-to-date approach is in `galenholt/SRA`, which dispenses with all but one SLURM script. And so the SLURM scripts in `/SLURM` here are deprecated but kept for reproducibility. The HPC process would likely need to be altered for other HPC environments.

The current flow is to have a method that works both locally or on an HPC. I want to have R control the HPC parallelisation through foreach and future, and so use foreach loops with %dofuture% and modify the `plan`.

That requires a central control process to spawn subsidiary runs.

So, the thinking is

Use `any_R.sh` as the control process. It should point to the file to run. But because we use notebooks, we need to `knitr::purl` them to R scripts. So any_R.sh calls `run_r_hpc.R`, which purls a notebook or passes through a script, and then runs it. Then, that script should start a bunch of jobs.

HPCs often have to have some set of packages already installed that need compiled C libraries (especially sf). To access those, we need to add to libPaths, but that doesn't propagate through {future}s if it's done in a script. So, in the .Rprofile, add

```         
if (grepl('^HPCNAME', Sys.info()["nodename"])) {
  renvpaths <- .libPaths()
  .libPaths(new = c(renvpaths,'/path/to/hpc/R/library' ))
}
```

where you get the path to the HPC R library by opening R outside the renv and typing `.libPaths()`.

### Typical run

Once everything's set up, use something like

```         
sbatch any_R.sh run_r_hpc.R "MER_data_processing/notebook_with_processing.qmd"
```

To start a master process in run_r_hpc that then fires off sub-slurms (presumably) in notebook_with_processing.qmd.

## Contact

For more information, contact Galen Holt, g.holt\@deakin.edu.au.
