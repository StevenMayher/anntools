#' @export


import_annot = function(file = NULL,
                        dir = getwd(),
                        method = NULL,
                        suppress_warnings = FALSE,
                        ...){

  if(...length() > 0){
    do.call(import_file, c(list(file = file, dir = dir, method = method, suppress_warnings = suppress_warnings), ...))
  }else{
    import_file(file = file, dir = dir, method = method, suppress_warnings = suppress_warnings)
  }



  # tryCatch(
  #   {
  #     if(...length() > 0){
  #       do.call(import_file, c(list(file = file, dir = dir, method = method), ...))
  #     }else{
  #       import_file(file = file, dir = dir, method = method)
  #     }
  #
  #   },
  #   error = function(cond){
  #     message(cond)
  #   },
  #   warning = function(cond){
  #     if(!suppress_warnings){
  #       message(cond)
  #     }
  #   }
  # )

  # if(suppress_warnings){
  #   if(...length() > 0){
  #     annot_obj = suppressWarnings(do.call(import_file, c(list(file = file, dir = dir, method = method), ...)))
  #
  #   }else{
  #     annot_obj = suppressWarnings(import_file(file = file, dir = dir, method = method))
  #   }
  #
  # }else{
  #   if(...length() > 0){
  #     annot_obj = do.call(import_file, c(list(file = file, dir = dir, method = method), ...))
  #
  #   }else{
  #     annot_obj = import_file(file = file, dir = dir, method = method)
  #   }
  #
  #   if(length(unique(lengths(annot_obj))) != 1){
  #     warning("Fields of different lengths detected in annotation file.")
  #   }
  #
  # }


  # z = function(){
  #
  #
  #
    # if(!is.null(file)){
    #
    #   if(dir.exists(dir)){
    #     if(file.exists(file.path(dir, file))){
    #
    #       if(...length() > 0){
    #         annot_obj = do.call(import_file, c(list(file = file, dir = dir, method = method), ...))
    #
    #       }else{
    #         annot_obj = import_file(file = file, dir = dir, method = method)
    #       }
    #
    #       if(length(unique(lengths(annot_obj))) != 1){
    #         warning("Fields of different lengths detected in annotation file - please correct file.")
    #       }
    #
    #       return(annot_obj)
    #
    #
    #
    #
    #     }else{
    #       stop(sprintf("Annotation file %s not detected in %s directory - please specify the correct file and directory.", file))
    #     }
    #   }else{
    #     stop(sprintf("Directory %s not detected - please specify an existing file directory.", dir))
    #   }
    # }else{
    #   stop("No annotation dataframe file provided - please provide an annotation file for importing.")
    # }
  # }
  #
  # if(suppress_warnings){
  #   suppressWarnings(z())
  # }else{
  #   z()
  # }
}

