#'
#'
#'
#'
#' @export


import_matrix = function(file = NULL,
                         dir = NULL,
                         as.data.matrix = TRUE,        # Logical, defaults to TRUE. Will coerce the imported object to a data matrix via base::data.matrix(). If you'd like to just import the file and perform formatting separately, select FALSE.
                         suppress_warnings = FALSE,
                         ...){



  z = function(){

    if(!is.null(file)){
      if(dir.exists(dir)){

        if(file.exists(file.path(dir, file))){

          matrix = NULL

          if(...length() > 0){
            matrix = do.call(import_file, c(list(file = file, dir = dir), ...))
          }else{
            matrix = import_file(file = file, dir = dir)
          }
          if(as.data.matrix){
            matrix = format_matrix(mat = matrix)
          }
        }
      }

      if(!dir.exists(dir)){
        stop(sprintf("Directory %s not detected - please specify an existing file directory.", dir))

      }else if(!file.exists(file.path(dir, file))){
        stop(sprintf("Matrix file %s not detected in %s directory - please specify the correct file and directory.", file))

      }else{
        if(...length() > 0){
          matrix = do.call(import_file, c(list(file = file, dir = dir), ...))

        }else{
          matrix = import_file(file = file, dir = dir)
        }
      }

      if(as.data.matrix){
        matrix = format_matrix(mat = matrix)
      }

      return(matrix)

    }else{
      stop("No matrix file provided - please provide a matrix file for importing.")
    }
  }

  if(suppress_warnings){
    suppressWarnings(z())
  }else{
    z()
  }


}
