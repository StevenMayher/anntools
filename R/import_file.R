#' @import data.table
#' @importFrom tools file_ext
#' @importFrom utils read.table read.csv


import_file = function(file = NULL,
                       dir = getwd(),
                       method = NULL,
                       suppress_warnings = FALSE,
                       ...){

  tryCatch(
    {
      y = file.path(dir, file)

      if(method == "readRDS"){
        readRDS(file = y, ...)
      }else if(method == "fread"){
        fread(file = y, ...)
      }else if(method == "read.table"){
        read.table(file = y, ...)
      }else if(method == "read.csv"){
        read.csv(file = y, ...)
      }else if(method == "read.csv2"){
        read.csv2(file = y, ...)
      }else{
        stop()
      }
    },
    error = function(cond){
      if(is.null(file)){
        message(sprintf("The following error occurred when attempting to import '%s/%s':", dir, file))
        message("No file provided - please provide a file via 'file' for importing.")
      }else if(!dir.exists(dir)){
        message(sprintf("The following error occurred when attempting to import '%s/%s':", dir, file))
        message(sprintf("Cannot find directory %s - please provided an existing directory.", dir))
      }else if(!file.exists(file.path(dir, file))){
        message(sprintf("The following error occurred when attempting to import '%s/%s':", dir, file))
        message(sprintf("No file named %s found in directory '%s' - please specify an existing file.", file, dir))
      }else if(is.null(method)){
        message(sprintf("The following error occurred when attempting to import '%s/%s':", dir, file))
        message("No import method provided via 'method'. Please specify a valid import method via 'method' and try again.")
      }else if(!(method %in% c("readRDS", "fread", "read.table", "read.csv", "read.csv2"))){
        message(sprintf("The following error occurred when attempting to import '%s/%s':", dir, file))
        message(sprintf("No import method defined for method '%s' - please provide an existing import method.", method))
      }else{
        message(sprintf("The following error occurred when attempting to import '%s/%s':", dir, file))
        message(cond)
        message("Please adjust import options to accommodate error and try importing again.")
      }
    },
    warning = function(cond){
      if(!suppress_warnings){
        message(sprintf("The following warning(s) occurred when importing '%s/%s':", dir, file))
        message(cond)
      }
    }
  )

  ## FASTEST VERSION - NO ERROR HANDLING ##
  # y = file.path(dir, file)
  #
  # if(method == "readRDS"){
  #   readRDS(file = y, ...)
  # }else if(method == "fread"){
  #   fread(file = y, ...)
  # }else if(method == "read.table"){
  #   read.table(file = y, ...)
  # }else if(method == "read.csv"){
  #   read.csv(file = y, ...)
  # }else if(method == "read.csv2"){
  #   read.csv2(file = y, ...)
  # }else{
  #   stop()
  # }

  # if(!is.null(file)){
  #
  #   x = file_ext(file.path(dir, file))
  #   y = file.path(dir, file)
  #
  #   if(file.exists(y)){




      # if(x == "rds"){
      #   readRDS(file = y, ...)
      #
      #
      # }else if(x == "txt"){
      #   if(method == "fread"){
      #     fread(file = y, ...)
      #   }else if(method == "read.table"){
      #     read.table(file = y, ...)
      #   }else if(method == "read.csv"){
      #     read.csv(file = y, ...)
      #   }else if(method == "read.csv2"){
      #     read.csv2(file = y, ...)
      #   }
      #
      #
      # }else if(x == "csv"){
      #   read.csv(file = y, ...)


  #     }else{
  #       stop(sprintf("No method defined for importing a %s file - please provide a file of a supported format (rds, txt, csv).", x))
  #     }
  #
  #   }else{
  #     stop(sprintf("No file named %s found in directory %s - please specify an existing file.", file, dir))
  #   }
  #
  # }else{
  #   stop("Please provide a file ('file') for importing.")
  # }

}




