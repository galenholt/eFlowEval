# eFlowEval modelling framework repository

This repository contains the code for the eFlowEval modelling framework. It is built primarily in R, but is not (currently) an R package, and contains some shell scripts to manage running on SLURM HPCs.

To get set up, clone this repository, and use {renv} to establish the package environment `renv::restore()` to ensure there are no package conflicts.

## Structure

The `directorySet.R` script does some setup work to establish standard input and output directories, and make some other configuration changes depending on whether the eFlowEval framework is running on local Windows machines or remote HPCs at the CSIRO. It is likely to need to be edited to use in new environments.

The code structure consists of data processing code for the driver data, located in `/Scripts/DataProcessing`. The data itself is located elsewhere (with locations as set in `directorySet.R`). Sources for original data are provided in Supplementary Material. Because each response model needs different data or processes data in different ways, the scripts in `Scripts/DataProcessing` are bespoke for each model.

The code will generate a `/datOut` folder, where processed data is saved, with location defined in `directorySet.R`.

Stricture and other response relationships (e.g. metabolism) are defined with scripts and functions in `/Strictures`. The processed responses are then saved to a `/strictOut` directory the code creates.

Figures and other synthesis of the output are prepared in `/Scripts/plotting`, and organised and produced in Quarto notebooks in `/notebooks`.

## Processing

Most processing is designed to occur either locally or on an HPC running a SLURM job scheduler. The SLURM approach has changed through the life of the project, and so the SLURM scripts in `/SLURM` differ a bit depending on when they were created. In general, they tend to chunk the data into catchments, and then into 100 chunks within catchments. The `*Fails.R` scripts in `/Strictures` and `/Scripts/DataProcessing` generate SLURM scripts and other shell scripts to control the chunking and identify missing chunks. The HPC process would likely need to be altered for other HPC environments.

## Data

The data is expected to be in a directory outside the repo, (set as `datDir` in `directorySet.R`). Currently, the data is all open-source (ANAE layers, soil temp from MODIS/NASA, soil moisture from AWRA-L (Australian Bureau of Meteorology), and some simple spatial layers for the Murray-Darling Basin and RAMSAR wetland sites, with citations in Supplementary Material. As used, the data sits in a directory at CSIRO and is available on request.

## Contact

For more information, contact Galen Holt, g.holt\@deakin.edu.au.