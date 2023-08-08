#'
#'
#'
#'
#' @export




format_matrix = function(mat = NULL,
                         type = NULL,
                         # convert_to = "numeric",
                         rownames_force = NA,
                         rownames = NULL,
                         colnames = NULL,
                         suppress_warnings = FALSE
                         ){


  z = function(){
    if(!is.null(mat)){

      x = deparse(substitute(mat))

      if(!is.matrix(mat)){

        if(is.data.frame(mat) || tibble::is_tibble(mat)){      # || is.array(mat)

          if(is.data.frame(mat)){
            warning(sprintf("%s is a dataframe - will attempt to convert %s to a matrix.", x, x), call. = FALSE)

          }else if(tibble::is_tibble(mat)){
            warning(sprintf("%s is a tibble - will attempt to convert %s to a matrix.", x, x), call. = FALSE)
          }

          matrix = data.matrix(frame = mat, rownames.force = rownames_force)


        }else{
          stop(sprintf("%s not formatted as a coercible object - cannot convert %s into a matrix.", x, x), call. = FALSE)
        }



      }else{
        warning(sprintf("%s already formatted as a matrix - no conversion necessary.", x), call. = FALSE)
        matrix = mat
      }


      if(!is.null(rownames)){
        dimnames(matrix)[[1]] <- rownames
      }

      if(!is.null(colnames)){
        dimnames(matrix)[[2]] <- colnames
      }

      return(matrix)

    }else{
      stop("No matrix provided - please provide a matrix to format.")
    }
  }

  if(suppress_warnings){
      suppressWarnings(z())
    }else{
      z()
    }

}
