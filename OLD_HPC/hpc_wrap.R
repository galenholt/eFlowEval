# HPC wrapper

args <- commandArgs()
print('args are:')
print(args)
print('end args')

# For now, let me pass an argument about where we want the data to be for testing what's fastest
dataWhere <- 'MER'

# This is silly to do here, since the arg order changes depending on the shell
# script. but it will break code to move it.
summaryFun <- args[7] # source('directorySet.R')
# What got passed? Setting up for passing which script to run in the sh

source(args[6])
