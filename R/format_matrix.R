#' @importFrom DescTools StripAttr
#' @import data.table
#' @importFrom tibble is_tibble
#' @export




format_matrix = function(mat = NULL,
                         # type = NULL,
                         convert_to = "double",
                         rownames.force = NA,
                         rownames = NULL,
                         colnames = NULL,
                         suppress_warnings = FALSE
                         ){

  tryCatch(
    {
      if(is.matrix(mat)){
        if(is.character(mat)){
          matrix = data.matrix(frame = mat, rownames.force = rownames.force)
        }else{
          matrix = mat
        }
      }else if(data.table::is.data.table(mat) || is.data.frame(mat) || tibble::is_tibble(mat)){
        matrix = data.matrix(frame = mat, rownames.force = rownames.force)
      }else if(is.list(mat) && collapse::ldepth(mat) == 1){
        matrix = data.table::data.table(data.table::rbindlist(list(mat)))
        matrix = data.matrix(frame = matrix, rownames.force = rownames.force)
      }else{
        stop()
      }


      if(convert_to != typeof(matrix)){
        if(convert_to == "single"){
          mode(matrix) = "single"

        }else if(convert_to == "double"){
          storage.mode(matrix) = "double"
        }else if(convert_to == "integer"){
          storage.mode(matrix) = "integer"
        }
      }else if(convert_to != "single"){
        if(!is.null(attr(matrix, "Csingle"))){
          matrix = DescTools::StripAttr(matrix, attr_names = "Csingle")
        }
      }

      if(!is.null(rownames) && length(rownames) == dim(matrix)[1]){
        dimnames(matrix)[[1]] = rownames
      }

      if(!is.null(colnames) && length(colnames) == dim(matrix)[2]){
        dimnames(matrix)[[2]] = colnames
      }

    },
    error = function(cond){
      if(is.null(mat)){
        message("Error in 'format_matrix()':")
        message("No object provided - please specify an object via argument 'mat' to format.")
      }else if(!(data.table::is.data.table(mat)) && !is.data.frame(mat) && !(tibble::is_tibble(mat)) && !(is.list(mat)) && !is.matrix(mat)){
        message("Error in 'format_matrix()':")
        message("Invalid object type - please provide a valid data structure to coerce.")
      }else if(is.list(mat) && collapse::ldepth(mat) != 1){
        message("Error in 'format_matrix()':")
        message(sprintf("List object '%s' has too many sub lists.", deparse(substitute(mat))))
      }else{
        message("Error in 'format_matrix()':")
        message(cond)
      }
    },
    warning = function(cond){
      if(!suppress_warnings){
        message("Warning in 'format_matrix()':")
        message(cond)
      }
    }
  )


  # if(!is.null(mat)){
  #
  #   if(!suppress_warnings){
  #
  #
  #     if(!is.matrix(mat)){
  #       x = deparse(substitute(mat))
  #
  #       if(is.data.frame(mat) || tibble::is_tibble(mat)){      # || is.array(mat)
  #
  #         if(is.data.frame(mat)){
  #           warning(sprintf("%s is a dataframe - will attempt to convert %s to a matrix.", x, x), call. = FALSE)
  #
  #         }else if(tibble::is_tibble(mat)){
  #           warning(sprintf("%s is a tibble - will attempt to convert %s to a matrix.", x, x), call. = FALSE)
  #         }
  #
  #         matrix = data.matrix(frame = mat, rownames.force = rownames_force)
  #
  #
  #       }else{
  #         stop(sprintf("%s not formatted as a coercible object - cannot convert %s into a matrix.", x, x), call. = FALSE)
  #       }
  #
  #
  #
  #     }else{
  #       warning(sprintf("%s already formatted as a matrix - no conversion necessary.", x), call. = FALSE)
  #       matrix = mat
  #     }
  #   }else{
  #     if(!is.matrix(mat)){
  #
  #       if(is.data.frame(mat) || tibble::is_tibble(mat)){      # || is.array(mat)
  #         matrix = data.matrix(frame = mat, rownames.force = rownames_force)
  #
  #       }else{
  #         x = deparse(substitute(mat))
  #         stop(sprintf("%s not formatted as a coercible object - cannot convert %s into a matrix.", x, x), call. = FALSE)
  #       }
  #
  #     }else{
  #       matrix = mat
  #     }
  #   }
  #
  #   if(!is.null(rownames)){
  #     dimnames(matrix)[[1]] <- rownames
  #   }
  #
  #   if(!is.null(colnames)){
  #     dimnames(matrix)[[2]] <- colnames
  #   }
  #
  #   return(matrix)
  #
  # }else{
  #   stop("No matrix provided - please provide a matrix to format.")
  # }


  # if(!is.matrix(mat)){
  #   if(data.table::is.data.table(mat) || is.data.frame(mat) || tibble::is_tibble(mat)){
  #     matrix = data.matrix(frame = mat, rownames.force = rownames.force)
  #   }else if(is.list(mat) && collapse::ldepth(mat) == 1){
  #     matrix = data.table::data.table(rbindlist(list(mat)))
  #     matrix = data.matrix(frame = matrix, rownames.force = rownames.force)
  #   }else{
  #     stop()
  #   }
  # }else{
  #   matrix = mat
  # }

  # if(convert_to != typeof(matrix)){
  #   if(convert_to == "single"){
  #     if(typeof(matrix) != "double" || !attr(matrix, "Csingle")){
  #       mode(matrix) = "single"
  #     }
  #   }else if(convert_to == "double"){
  #     if(!is.null(attr(matrix, "Csingle"))){
  #
  #     }
  #   }
  # }else if(!is.null(attr(matrix, "Csingle"))){
  #
  # }


  # z = function(){
  #   if(!is.null(mat)){
  #
  #     x = deparse(substitute(mat))
  #
  #     if(!is.matrix(mat)){
  #
  #       if(is.data.frame(mat) || tibble::is_tibble(mat)){      # || is.array(mat)
  #
  #         if(is.data.frame(mat)){
  #           warning(sprintf("%s is a dataframe - will attempt to convert %s to a matrix.", x, x), call. = FALSE)
  #
  #         }else if(tibble::is_tibble(mat)){
  #           warning(sprintf("%s is a tibble - will attempt to convert %s to a matrix.", x, x), call. = FALSE)
  #         }
  #
  #         matrix = data.matrix(frame = mat, rownames.force = rownames_force)
  #
  #
  #       }else{
  #         stop(sprintf("%s not formatted as a coercible object - cannot convert %s into a matrix.", x, x), call. = FALSE)
  #       }
  #
  #
  #
  #     }else{
  #       warning(sprintf("%s already formatted as a matrix - no conversion necessary.", x), call. = FALSE)
  #       matrix = mat
  #     }
  #
  #
  #     if(!is.null(rownames)){
  #       dimnames(matrix)[[1]] <- rownames
  #     }
  #
  #     if(!is.null(colnames)){
  #       dimnames(matrix)[[2]] <- colnames
  #     }
  #
  #     return(matrix)
  #
  #   }else{
  #     stop("No matrix provided - please provide a matrix to format.")
  #   }
  # }
  #
  # if(suppress_warnings){
  #     suppressWarnings(z())
  #   }else{
  #     z()
  #   }

}
