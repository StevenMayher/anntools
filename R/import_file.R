#' @import utils read.csv
#' @import utils read.csv2
#' @import tools file_ext
#' @import utils read.table



import_file = function(file = NULL,
                       dir = getwd(),
                       ...){




  if(!is.null(file)){
    if(file.exists(file.path(dir, file))){


      if(tools::file_ext(file.path(dir, file)) == "rds"){
        readRDS(file = file.path(dir, file), ...)


      }else if(tools::file_ext(file.path(dir, file)) == "txt"){
        try(utils::read.table(file = file.path(dir, file), ...), silent = TRUE)
        try(utils::read.csv(file = file.path(dir, file), ...))
        # try(utils::read.csv2(file = file.path(dir, file), ...))

      }else if(tools::file_ext(file.path(dir, file)) == "csv"){
        utils::read.csv(file = file.path(dir, file), ...)


      }else{
        stop("No method defined for importing a %s file - please provide a file of a supported format (rds, txt, csv).")
      }

    }else{
      stop(sprintf("No file named %s found in directory %s - please specify an existing file.", file, dir))
    }

  }else{
    stop("Please provide a file ('file') for importing.")
  }

}




