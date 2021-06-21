# HPC wrapper
source('directorySet.R')
# What got passed? Setting up for passing which script to run in the sh
args <- commandArgs()
print(args)
source(args[6])