#'


import_annot_internal = function(file = NULL,
                                 dir = getwd(),
                                 method = NULL,
                                 suppress_warnings = FALSE,
                                 import_opts = NULL
                                 ){

  if(!is.null(import_opts)){
    do.call(import_file, c(list(file = file, dir = dir, method = method, suppress_warnings = suppress_warnings), import_opts))
  }else{
    import_file(file = file, dir = dir, method = method, suppress_warnings = suppress_warnings)
  }
}


#'

import_matrix_internal = function(file = NULL,
                                  dir = getwd(),
                                  method = NULL,
                                  format_matrix = TRUE,        # Logical, defaults to TRUE Will coerce the imported object to a data matrix via anntools::format_matrix(). If you'd like to just import the file and perform formatting separately, select FALSE.
                                  suppress_warnings = FALSE,
                                  convert_to = "double",
                                  rownames.force = NA,
                                  rownames = NULL,
                                  colnames = NULL,
                                  import_opts = NULL
                                  ){

  if(!is.null(import_opts)){
    matrix = do.call(import_file, c(list(file = file, dir = dir, method = method, suppress_warnings = suppress_warnings), import_opts))
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
}
