source("renv/activate.R")

if (grepl('^pearcey', Sys.info()["nodename"])) {
# options(renv.config.external.libraries = "/apps/R/4.0.2/lib64/R/library")
# options(renv.config.sandbox.enabled = FALSE)

 oldpaths <- .libPaths() # Gets the renv libs
 .libPaths(new = c(oldpaths[1],'/apps/R/4.0.2/lib64/R/library' ))
} else if (grepl('^petrichor', Sys.info()["nodename"]) | grepl('^c', Sys.info()["nodename"])) {
oldpaths <- .libPaths() # Gets the renv libs
 .libPaths(new = c(oldpaths[1],'/apps/R/4.0.5/lib64/R/library' ))
}

