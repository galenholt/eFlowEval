# HPC wrapper

args <- commandArgs()
print(args)

# For now, let me pass an argument about where we want the data to be for testing what's fastest
# dataWhere <- 'MER' # args[7]
# summaryFun <- args[7]
# source('directorySet.R')
# What got passed? Setting up for passing which script to run in the sh


library(doFuture)
registerDoFuture()

# Cluster
print('sessionInfo')
print(sessionInfo())

registerDoFuture()
print('available workers: ')
print(availableWorkers())

print('makeNodePSOCK.setup.strategy is ')
print(parallelly:::getOption2("parallelly.makeNodePSOCK.setup_strategy", "parallel"))

print('trying to set up the cluster dry run')
print(parallelly::makeClusterPSOCK(workers = availableWorkers(), dryrun = TRUE))


print('now try the plan inside withCalling')
write.to.log <- function(x) {
  print(x) # Let's try this- it should just print directly?
}
withCallingHandlers(plan(cluster), error = function(e) { write.to.log(sys.calls()) })

# This and the above both fail
print('try to set up the cluster for real without setting plan()')
print(parallelly::makeClusterPSOCK(workers = availableWorkers()))

# print('now try the plan alone')
plan(cluster) # On windows

print('made the cluster. now try the script where it does something')
# source(args[6])
