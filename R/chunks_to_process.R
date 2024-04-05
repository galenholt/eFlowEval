#' Find the chunks that we need to process
#'
#' @param out_dir outer directory for all output data
#' @param dataname name of the dataset (this will be the next dir in)
#' @param summaryFun name of the summary function (the next dir in, enabling multiple summaries of same data)
#' @param nchunks number of chunks. If > 1, data goes into a 'catchment/chunked/...' directory. If 1, does not chunk, data goes into the summaryFun directory
#' @param catchment names of catchments or other units. Default 'all' forces all catchments in the basin.
#' @param returnForR logical, default TRUE, return a tibble with catchment and chunk columns
#' @param produce_sh logical default FALSE, produce an .sh file if TRUE
#' @param filetype '.rds' (default), or '.rdata' for backwards compatibility
#'
#' @return a tibble or invisible, depending on returnForR
#' @export
#'
chunks_to_process <- function(out_dir,
                  dataname,
                  summaryFun,
                  nchunks = 100,
                  catchment = 'all',
                  extraname = NULL,
                  returnForR = TRUE,
                  produce_sh = FALSE,
                  filetype = '.rds') {

  if ('all' %in% catchment) {
    catchment <- c("Avoca", "BarwonDarling", "BorderRivers", "Broken", "Campaspe",
                  "Castlereagh", "CentralMurray", "CondamineBalonne",
                  "EdwardWakool", "Goulburn", "Gwydir", "Kiewa", "Lachlan",
                  "Loddon", "LowerDarling", "LowerMurray", "Macquarie", "MittaMitta",
                  "Murrumbidgee", "Namoi", "Ovens", "Paroo", "UpperMurray", "Warrego", "Wimmera")
  }

  # Where to look for the files
  if (!is.null(extraname)) {
    sumextra <- paste0(summaryFun, '/', extraname)
  } else {
    sumextra <- summaryFun
  }
  fundir <- file.path(out_dir, dataname, sumextra)
  chunkdir <- file.path(fundir, 'chunked')

  # Create the directory structure?
  for (d in 1:length(catchment)){
    if (!dir.exists(file.path(chunkdir, catchment[d]))) {
      dir.create(file.path(chunkdir, catchment[d]), recursive = TRUE)
    }
  }


  # Get the expected full set
  # The paths here are relative to fundir.
  full_set <- tidyr::expand_grid(catchment, chunk = 1:nchunks) |>
    dplyr::mutate(filename = dplyr::case_when(nchunks == 1 ~ paste0(catchment, '_', summaryFun, filetype),
                                              nchunks > 1 ~ file.path('chunked', catchment,
                                                                      paste0(catchment, '_', summaryFun, '_', chunk, filetype))))

  # Get the existing files
  existing <- list.files(fundir, recursive = TRUE)

  # Find the sets that are done or not done
  done_set <- full_set[full_set$filename %in% existing, ]
  todo_set <- full_set[!full_set$filename %in% existing, ]

  if (nrow(todo_set) == 0) {
    rlang::inform(c("All processing has finished!"))
  } else {
    rlang::inform(c("Processing to do:",
                    "i" = "Total chunks: ",
                    glue::glue("{nrow(todo_set)}"),
                    "i" = "Catchments: ",
                    glue::glue("{unique(todo_set$catchment)}\n"),
                    "\n")
                    )
  }

  if (produce_sh) {
    todolist <- todo_set |>
      dplyr::select(-filename) |>
      tidyr::nest(.by = catchment)

    missinglist <- todolist$data |> setNames(todolist$catchment)
    build_sh_file(misslist = missinglist, summaryFun = summaryFun)
  }

  if (returnForR) {
    return(todo_set)
  } else {
    return(invisible())
  }



}

#' make an sh file for the HPC from a list of catchments and chunks
#'
#' @param misslist list with names being catchments, and each having a numeric vector of chunks to run
#' @param summaryFun function for the summary
#' @param runImmediate logical FALSE- immediately start the sbatch
#' @param lengthOrChunk 'short', 'long' or 'chunk'. Short and long are times, 'chunk' makes subchunks, and is rarely used or maintained.
#'
#' @return nothing, makes a file
#' @export
#'
build_sh_file <- function(misslist, summaryFun, runImmediate = FALSE, lengthOrChunk = c('short', 'long')) {
    # If we're using this to support the HPC, we need to create the shell script
    missnames <- names(misslist)
    headline <- "#!/bin/bash"
    head2 <- "\n"


    # Short runs
    if ('short' %in% lengthOrChunk) {
      # For each catchment, I need to set up the chunk in a list
      lineslist <- foreach::foreach(cn = 1:length(missnames), .inorder = TRUE, .combine = c) %do% {
        l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
        l2 <- stringr::str_c("echo 'start' $thiscatch")
        misschars <- stringr::str_flatten(misslist[[cn]], collapse = ",")
        l3 <- stringr::str_c("sbatch -J $thiscatch --array=", misschars, " all", summaryFun, "SLURM.sh $thiscatch")
        l4 <- "sleep 2"
        l5 <- "\n"
        thislist <- list(l1, l2, l3, l4, l5)
      }

      filechars <- c(headline, head2, unlist(lineslist))

      # Hpc says file isn't an option
      # readr::write_lines(filechars, file = 'missingTemps.sh')

      writeLines(filechars, con = paste0('missing', summaryFun, '.sh'))

      # Do I want to have R actually run this too?
      if (runImmediate) {
        system2(command = 'bash', args = paste0('missing', summaryFun, '.sh'))
      }

    }

    # Just run longer
    if ('long' %in% lengthOrChunk) {
      # For each catchment, I need to set up the chunk in a list
      lineslist <- foreach::foreach(cn = 1:length(missnames), .inorder = TRUE, .combine = c) %do% {
        l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
        l2 <- stringr::str_c("echo 'start' $thiscatch")
        misschars <- stringr::str_flatten(misslist[[cn]], collapse = ",")
        l3 <- stringr::str_c("sbatch -J $thiscatch --array=", misschars, " all", summaryFun, "SLURMLong.sh $thiscatch")
        l4 <- "sleep 2"
        l5 <- "\n"
        thislist <- list(l1, l2, l3, l4, l5)
      }

      filechars <- c(headline, head2, unlist(lineslist))

      # Hpc says file isn't an option
      # readr::write_lines(filechars, file = 'missingTemps.sh')

      writeLines(filechars, con = paste0('missing', summaryFun, 'long.sh'))

      # Do I want to have R actually run this too?
      if (runImmediate) {
        system2(command = 'bash', args = paste0('missing', summaryFun, 'long.sh'))
      }

    }

    # Make the subchunk script
    if ('chunk' %in% lengthOrChunk) {
      # This generates an absurd number of runs. I think usually run the above first, and then only do this if necessary.
      # For each catchment, I need to set up the chunk in a list, but ALSO need to loop over the missings to sub-chunk
      lineslistSUB <- foreach::foreach(cn = 1:length(missnames)) %:%
        foreach::foreach(sc = 1:length(misslist[[cn]]), .inorder = TRUE, .combine = c) %do% {
          l1 <- stringr::str_c("thiscatch='", missnames[cn],"'")
          l1.5 <- stringr::str_c("thiscchunk='", misslist[[cn]][sc],"'")
          l2 <- stringr::str_c("echo 'start' $thiscatch $thischunk")

          l3 <- stringr::str_c("sbatch -J $thiscatch --array=1-100",
                               " all", summaryFun, "TempSLURMchunk.sh $thiscatch $thischunk")
          l4 <- "sleep 2"
          l5 <- "\n"
          thislist <- list(l1, l1.5, l2, l3, l4, l5)
        }

      filecharsSUB <- c(headline, head2, unlist(lineslistSUB))
      writeLines(filecharsSUB, con = paste0('missing', summaryFun, 'Chunk.sh'))

      # I *should* be able to recurse this relatively easily, but hopefully won't need to.

      # Do I want to have R actually run this too?
      if (runImmediate) {
        system2(command = 'bash', args = paste0('missing', summaryFun, 'Chunk.sh'))
      }

    }
  }
