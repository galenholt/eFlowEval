# Set up directory locations
# directorySet <- function() {
#   # this function 
# }
print('opened directoryset\n')
print(Sys.info())
print('\n')

# Rather than a functtion, I want this to set a few things, so building as a script instead
# Set directory structure for pearcey
if (grepl('^pearcey', Sys.info()["nodename"])) {
  # Let's assume I cp from flush/scratch/whatever into JOBDIR at the start of the job.
  datDir <-  file.path(Sys.getenv('SCRATCH1DIR'), 'dataBase')
  datOut <- file.path(Sys.getenv('SCRATCH1DIR'), 'datOut')
  # datDir <-  file.path(Sys.getenv('JOBDIR'), 'dataBase')
  # datOut <- file.path(Sys.getenv('JOBDIR'), 'datOut')
  print('in the grepl\n')
  print(paste0('datOut is ', datOut, '\n'))
  source(".Rprofile")
} else if (grepl('^Windows', Sys.info()["sysname"])) {
  myhome <- str_remove(path.expand("~"), "/Documents")
  datDir <- file.path(myhome, "Deakin University/QAEL - MER/Model/dataBase") # "C:/Users/Galen/Deakin University/QAEL - MER/Model/dataBase"
  
  datOut <- "datOut"
}

print('make it through the ifs')
# Make the out directory, in case it doesn't exist
if (!dir.exists(datOut)) {dir.create(datOut, recursive = TRUE)}
# The in directory has to exist, or there won't be anything to use
