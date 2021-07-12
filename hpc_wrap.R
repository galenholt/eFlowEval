# HPC wrapper

args <- commandArgs()
print(args)

# For now, let me pass an argument about where we want the data to be for testing what's fastest
dataWhere <- 'MER' # args[7]

# source('directorySet.R')
# What got passed? Setting up for passing which script to run in the sh

source(args[6])
