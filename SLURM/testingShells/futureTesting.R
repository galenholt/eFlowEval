# trying to sort out auto-arraying and different plan()s

library(dplyr)
library(tibble)
library(doFuture)
registerDoFuture()

# plans to test
# plan(sequential)
# plan(multisession)
# plan(multicore)
# plan(cluster)
# plan(future.batchtools::batchtools_slurm) # Spits out each future into slurm nodes
# # # Probably better to do 
# plan(list(future.batchtools::batchtools_slurm, multisession)) # and include an outer layer of futures around catchment or something.
# # basically, according to 
# # https://future.batchtools.futureverse.org/ 


# I think each should be done separately, but let's build in a core test

# Turn off warnings because of the rand
oldw <- getOption("warn")
options(warn = -1)

# core function
testfun <- function() {
  a <- rnorm(1000)
  b <- matrix(rnorm(1000*1000), nrow = 1000)
  c <- mean(a %*% b)
  return(mean(c))
}

# What do I want to check?
# Time, definitely
# Use of all cores on one node
# Use of multiple nodes
# Propagation to new nodes

# so, how do I do that? would be good to print some things to .out
# available workers
# and avaialable cores
# WHICH worker for each task- can I get that?
# Node
# timing of each foreach



# SEQUENTIAL --------------------------------------------------------------

print('SEQUENTIAL')
plan(sequential)

print('available Workers:')
print(availableWorkers())

print('available Cores:')
print(availableCores(methods = 'Slurm'))

# base R process id
print('Main PID:')
print(Sys.getpid())

# i/o is different than processing. maybe look at ech?


system.time(processtest <- foreach(i = 1:100, .combine = bind_rows) %dopar% {
  internaltime <- system.time(testout <- testfun())
  d <- Sys.getpid()
  e <- tibble(loop = i, pid = d, result = testout, time = internaltime[3])
})

# options(warn = oldw)

print('PIDs in the loop')
print(processtest)

print('Unique processes')
print(unique(processtest$pid))

# MULTISESSION --------------------------------------------------------------

print('MULTISESSION')
plan(multisession(workers = availableCores(methods = 'Slurm')))

print('available Workers:')
print(availableWorkers())

print('available Cores:')
print(availableCores(methods = 'Slurm'))

# base R process id
print('Main PID:')
print(Sys.getpid())

# i/o is different than processing. maybe look at ech?


system.time(processtest <- foreach(i = 1:100, .combine = bind_rows) %dopar% {
  internaltime <- system.time(testout <- testfun())
  d <- Sys.getpid()
  e <- tibble(loop = i, pid = d, result = testout, time = internaltime[3])
})

# options(warn = oldw)

print('PIDs in the loop')
print(processtest)

print('Unique processes')
print(unique(processtest$pid))


# MULTICORE --------------------------------------------------------------

print('multicore')
plan(multicore(workers = availableCores(methods = 'Slurm')))

print('available Workers:')
print(availableWorkers())

print('available Cores:')
print(availableCores(methods = 'Slurm'))

# base R process id
print('Main PID:')
print(Sys.getpid())

# i/o is different than processing. maybe look at ech?


system.time(processtest <- foreach(i = 1:100, .combine = bind_rows) %dopar% {
  internaltime <- system.time(testout <- testfun())
  d <- Sys.getpid()
  e <- tibble(loop = i, pid = d, result = testout, time = internaltime[3])
})

# options(warn = oldw)

print('PIDs in the loop')
print(processtest)

print('Unique processes')
print(unique(processtest$pid))
















# # IO
# scriptOut <- file.path(datOut, 'HPCTESTING')
# if (!dir.exists(scriptOut)) {dir.create(scriptOut, recursive = TRUE)}
# 
# iotest <- foreach(i = 1:100) %dopar% {
#   a <- rnorm(10000)
#   save(a, file = file.path(scriptOut, paste0('TESTFILE_', as.character(i))))
# }

# OK, so that should let me do things. Need to test it does
# Then, need to set something up to sort out how it propagates the breakup into
# array jobs once I can get that working at all.






