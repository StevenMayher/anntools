#'
#'
#'
#'
#' @export


import_matrix = function(file = NULL,
                         dir = getwd(),
                         method = NULL,
                         format_matrix = TRUE,        # Logical, defaults to TRUE Will coerce the imported object to a data matrix via anntools::format_matrix(). If you'd like to just import the file and perform formatting separately, select FALSE.
                         suppress_warnings = FALSE,
                         convert_to = "double",
                         rownames.force = NA,
                         rownames = NULL,
                         colnames = NULL,
                         ...){

  if(...length() > 0){
    matrix = do.call(import_file, c(list(file = file, dir = dir, method = method, suppress_warnings = suppress_warnings), ...))
  }else{
    matrix = import_file(file = file, dir = dir, method = method, suppress_warnings = suppress_warnings)
  }

  if(format_matrix){

    format_matrix(mat = matrix,
                  convert_to = convert_to,
                  rownames.force = rownames.force,
                  rownames = rownames,
                  colnames = colnames,
                  suppress_warnings = suppress_warnings)

  }else{
    return(matrix)
  }

  # if(format_matrix){
  #   tryCatch(
  #     {
  #       matrix = format_matrix(mat = matrix,
  #                              convert_to = convert_to,
  #                              rownames.force = rownames.force,
  #                              rownames = rownames,
  #                              colnames = colnames,
  #                              suppress_warnings = suppress_warnings)
  #
  #     },
  #     error = function(cond){
  #       message("Error in 'format_matrix()':")
  #     },
  #     warning = function(cond){
  #
  #     }
  #   )



  # if(suppress_warnings){
  #   if(...length() > 0){
  #     matrix = suppressWarnings(do.call(import_file, c(list(file = file, dir = dir, method = method), ...)))
  #
  #   }else{
  #     matrix = suppressWarnings(import_file(file = file, dir = dir, method = method))
  #
  #   }
  #   if(as.data.matrix){
  #     matrix = suppressWarnings(format_matrix(mat = matrix))
  #   }
  # }else{
  #   if(...length() > 0){
  #     matrix = do.call(import_file, c(list(file = file, dir = dir, method = method), ...))
  #
  #   }else{
  #     matrix = import_file(file = file, dir = dir, method = method)
  #
  #   }
  #   if(as.data.matrix){
  #     matrix = format_matrix(mat = matrix)
  #   }
  # }
  #
  # return(matrix)

  # z = function(){
  #
  #   if(!is.null(file)){
  #     if(dir.exists(dir)){
  #
  #       if(file.exists(file.path(dir, file))){
  #
  #         matrix = NULL
  #
  #         if(...length() > 0){
  #           matrix = do.call(import_file, c(list(file = file, dir = dir, method = method), ...))
  #         }else{
  #           matrix = import_file(file = file, dir = dir, method = method)
  #         }
  #         if(as.data.matrix){
  #           matrix = format_matrix(mat = matrix)
  #         }
  #       }
  #     }
  #
  #     if(!dir.exists(dir)){
  #       stop(sprintf("Directory %s not detected - please specify an existing file directory.", dir))
  #
  #     }else if(!file.exists(file.path(dir, file))){
  #       stop(sprintf("Matrix file %s not detected in %s directory - please specify the correct file and directory.", file))
  #
  #     }else{
  #       if(...length() > 0){
  #         matrix = do.call(import_file, c(list(file = file, dir = dir, method = method), ...))
  #
  #       }else{
  #         matrix = import_file(file = file, dir = dir, method = method)
  #       }
  #     }
  #
  #     if(as.data.matrix){
  #       matrix = format_matrix(mat = matrix)
  #     }
  #
  #     return(matrix)
  #
  #   }else{
  #     stop("No matrix file provided - please provide a matrix file for importing.")
  #   }
  # }
  #
  # if(suppress_warnings){
  #   suppressWarnings(z())
  # }else{
  #   z()
  # }

}
