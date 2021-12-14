# getting packages to work on the HPC is terrible. Let's try basically bypassing
# renv for anything that's already there. For the mment ignore versions; they'll
# be all goofed up in the main HPC library too

# Though maybe it'd be better to just put this in .Rprofile, since all it does is set libpaths?
# source(".Rprofile")
renvtxt <- readLines('renv.lock')

# Parse the renv lock to package level
renvpks <- renvtxt[grep("\"Package\": ", renvtxt)]
renvpks <- gsub("\"Package\": ", replacement = '', x = renvpks)
renvpks2 <- gsub("([\"\\, ])", replacement = '', x = renvpks)

renvpks2 

print('libPaths in renvHPC are now')
print(.libPaths())

# packages that aren't installed yet
notinstalled <- renvpks2[which(!(renvpks2 %in% installed.packages()))]


print('packages not installed are')
print(notinstalled)

# stop('stopping to check here')

# There's an issue with packages installed from github. I could sort out
# programatically, but doesn't matter right now
if ('calecopal' %in% notinstalled) {
  # Need to do this programatically, but for now
  notinstalled <- notinstalled[notinstalled != 'calecopal']
  devtools::install_github("an-bui/calecopal")
}

# I could just do this the old rbrary way I guess too, and bypass renv here
# If notinstalled is length 0, this tries a full renv::install() of everything.
if (length(notinstalled) > 0) {
  renv::install(notinstalled)
}

# where do we want to put this? a standalone helper, or source it every time as
# a check for updates to renv.lock?
