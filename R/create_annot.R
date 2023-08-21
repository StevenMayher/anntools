#' Annotation object creation and alteration
#'
#' @description
#' 'create_annot' enables the creation and modification of annotation objects.
#'
#' @param mat Used to specify a matrix object with dimension names. If you have a matrix object in R that contains the dimension names you'd like to use for the new annotation object's IDs, you may provide the object name here. See  Use 'mat' to specify a matrix object in R, or 'mat_file' to specify the name of a file containing a matrix with dimension names for importing. See Details section for more information.
#' @param mat_file,mat_dir If you wish to specify
#'
#' @details
#' The purpose of this function is to output a new annotation object. New objects can be created by supplying a matrix with appropriate dimension names, a pre-existing annotation file, both, or from scratch using this function.
#'
#'
#'
#' @export


create_annot = function(mat = NULL,
                        mat_file = NULL,
                        mat_dir = getwd(),
                        mat_import_method = NULL,
                        format_matrix = TRUE,        # Logical, defaults to TRUE Will coerce the imported object to a data matrix via anntools::format_matrix(). If you'd like to just import the file and perform formatting separately, select FALSE.
                        convert_to = "double",
                        rownames.force = NA,
                        rownames = NULL,
                        colnames = NULL,
                        mat_import_opts = NULL,   # If you have additional arguments you'd like to pass to importing of a matrix, provide the options here as a list(). See import_matrix() for details.
                        annot_type = NULL,         # May be either "vars" or "obs". Necessary if using a matrix file / object - select "vars" (i.e. variables) if you would like to create an annotation dataset for the column variables, and "obs" (i.e. observations) if you wish to create one for the row observations.
                        annot_obj = NULL,
                        annot_file = NULL,
                        annot_dir = getwd(),
                        annot_import_method = NULL,
                        annot_import_opts = NULL,   # If you have additional arguments you'd like to pass to importing of a matrix, provide the options here as a list(). See import_annot() for file details
                        annot_join = NULL,
                        remove_missing = FALSE,
                        IDs = NULL,
                        pseudotime_labs = NULL,
                        remove_labs_pattern = NULL, # If you wish to remove a pattern in the labels you've provided, supply the pattern here. Argument(s) will be substituted into gsub() - see gsub() for details.
                        drop_labs_containing = NULL,
                        select_labs_containing = NULL,
                        pseudotimes = NULL,         # Single numeric or integer list containing pseudotime information for each sample. If named list, name will be used as annotation field header - otherwise will be named "Pseudotime"
                        remove_pseudotimes_pattern = NULL,
                        drop_pseudotimes_containing = NULL,
                        select_pseudotimes_containing = NULL,
                        # pseudotime_map = NULL,      # Would you like to generate a pseudotime vector by ? If so provide a named numeric vector here, where the names correspond to the cell names provided, and the numbers correspond to the number of days.
                        color_by = NULL,
                        color_palette = NULL,
                        # color_by_order = NULL,
                        select_annot_cols = c(FALSE, FALSE, FALSE, FALSE),   # Logical vector - use if you'd like to specify a column from the provided annotation object to serve as the specified field in the newly generated annotation table. Provide the name of the column as a single length character vector in the appropriate field, and provide a logical vector of the same length as the number of arguments'IDs', 'pseudotime_labs', 'pseudotimes', 'color_by', or any of the generic arguments refer to a column name? If so, provide a logical vector  Controls how the 'IDs', 'pseudotime_labs', 'pseudotimes', and generic '...' arguments are interpreted. Assessed only if the argument is  provided in 'IDs', 'pseudotime_labs', or 'pseudotimes'
                        preserve_names = c(FALSE, FALSE, FALSE, FALSE),      # Logical vector - if you've specified a column from a provided annotation object to serve as one of the specified fields above in the newly generated annotation table, would you like to keep the original column names as they are in the table, or rename them? The default behavior renames them to the defaults shown in 'annot_col_names', which can be adjusted as desired, however you may set any or all of these to TRUE for each one you'd like to preserve. Alternatively, you can just enter a single TRUE or FALSE if you'd like all columns to follow the same logic.
                        annot_col_names = c("ID", "Pseudotime Labels", "Pseudotimes", "Pseudotime Colors"),
                        replace_annot_cols = c(FALSE, FALSE, FALSE),  # Logical vector - logic for determining behavior when a named list is provided that conflicts with a column name in a provided annotation object. If TRUE, the named list will replace the annotation column with the list provided, retaining the name of the annotation column / list name as opposed to renaming the list to "ID". Otherwise, the named list is renamed to "ID", and the annotation object will be  First three vector entries refer to 'IDs', 'psedutime_labs', and 'pseudotimes' respectively - may extend vector with additional entries for specifying the behaivor of generic arguments if desired.
                        return_as = "default",
                        suppress_warnings = FALSE,
                        ...
                        ){


  # FAIL CHECKS #



  ## STOP - FAIL IF NO MATRIX OBJ/FILE, ANNOTATION OBJ/FILE, OR ID VECTOR IS PROVIDED ##

  if (is.null(mat) && is.null(mat_file) && is.null(annot_obj) && is.null(annot_file) && is.null(IDs)) {
    stop("Please provide either a matrix with IDs variable / observation names (via either 'mat' or 'mat_file' & 'mat_dir'), an input file containing said IDs (via either 'annot_obj' or 'annot_file'), or a character vector with the appropriate IDs ('IDs').")
  }

  ## STOP - FAIL IF BOTH A MATRIX AND MATRIX FILE ARE PROVIDED ##
  if(!is.null(mat) && !is.null(mat_file)){
    stop("Please provide a matrix object ('matrix') or a matrix file name and directory ('matrix_file' & 'matrix_dir'), not both.")
  }


  # IMPORT FILES IF PROVIDED #

  ## IMPORT COUNT MAT FILE ##
  if(!is.null(mat_file)){
    matrix_import = import_matrix_internal(file = mat_file,
                                           dir = mat_dir,
                                           method = mat_import_method,
                                           format_matrix = format_matrix,
                                           convert_to = convert_to,
                                           rownames.force = rownames.force,
                                           rownames = rownames,
                                           colnames = colnames,
                                           suppress_warnings = suppress_warnings,
                                           import_opts = mat_import_opts
                                           )
  }else{
    matrix_import = NULL
  }


  ## IMPORT ANNOTATION FILE IF PROVIDED ##
  if(!is.null(annot_file)){
    annot_import = import_annot_internal(file = annot_file,
                                         dir = annot_dir,
                                         method = annot_import_method,
                                         suppress_warnings = suppress_warnings,
                                         import_opts = annot_import_opts)
    # if(!is.null(annot_import_opts)){
    #   annot_import = import_annot_internal(file = annot_file,
    #                                        dir = annot_dir,
    #                                        method = annot_import_method,
    #                                        suppress_warnings = suppress_warnings,
    #                                        import_opts = annot_import_opts)
    # }else{
    #   annot_import = import_annot_internal(file = annot_file,
    #                                        dir = annot_dir,
    #                                        method = annot_import_method,
    #                                        suppress_warnings = suppress_warnings)
    # }
  }else{
    annot_import = NULL
  }




  # FINALIZE INPUT FILES / OBJECTS FOR USE #

  ## ASSIGN PROVIDED MATRIX FILE / OBJECT TO UNIQUE OBJECT NAME FOR DOWNSTREAM USE ##
  if(!is.null(matrix_import)){
    matrix = mat_import

  }else if(!is.null(mat)){
    matrix = mat

  }else{
    matrix = NULL
  }


  ## FORMAT ANNOTATION OBJECT AND / OR ANNOTATION IMPORT OBJECT IF PROVIDED ##
  if(!is.null(annot_obj)){
    annot_object = as.list(annot_object)
  }else{
    annot_object = NULL
  }

  if(!is.null(annot_import)){
    annot_import = as.list(annot_import)
  }else{
    annot_import = NULL
  }


  ## ASSESS ANNOTATION OBJECTS IF PROVIDED & INSTANCE ANNOT_BASE ##
  if(!is.null(annot_import) && !is.null(annot_object)){
    stop("Please provide an annotated object via file importation or as an active R object, not both.")
  }else if(!is.null(annot_import) && is.null(annot_object)){
    annot_base = annot_import
    rm(annot_object, annot_import)
  }else if(is.null(annot_import) && !is.null(annot_object)){
    annot_base = annot_object
    rm(annot_object, annot_import)
  }else{
    annot_base = NULL
  }


  ## IF ANNOTATED DATA FRAME OBJECT IS PROVIDED, DOUBLE-CHECK ALL FIELDS ARE THE SAME LENGTH ##
  if(!is.null(annot_base)){
    if(length(unique(lengths(annot_base))) != 1){
      stop("Dimension mis-match - check annotation file / object for missing data.")
    }

    x = unique(lengths(annot_base))
    y = names(annot_base)
  }

  ## ASSESS PRESERVE NAMES INPUT ##
  if(length(preserve_names) == 1){
    if(isTRUE(preserve_names)){
      preserve_names = c(TRUE, TRUE, TRUE)
    }else if(isFALSE(preserve_names)){
      preserve_names = c(FALSE, FALSE, FALSE)
    }else{
      stop("'preserve_names' must be either a single logical, or a logical vector with a length of 3.")
    }
  }

  ## ASSESS `select_annot_cols` INPUT ##
  if(length(select_annot_cols) == 1){
    if(isTRUE(select_annot_cols)){
      select_annot_cols = c(TRUE, TRUE, TRUE, TRUE)
    }else if(isFALSE(select_annot_cols)){
      select_annot_cols = c(FALSE, FALSE, FALSE, FALSE)
    }else{
      stop("'select_annot_cols' must be either a single logical, or a logical vector with a length of 4.")
    }
  }

  ## ASSESS 'replace_annot_cols' INPUT ##
  if(length(replace_annot_cols) == 1){
    if(isTRUE(replace_annot_cols)){
      replace_annot_cols = c(TRUE, TRUE, TRUE, TRUE)
    }else if(isFALSE(replace_annot_cols)){
      replace_annot_cols = c(FALSE, FALSE, FALSE, FALSE)
    }else{
      stop("'replace_annot_cols' must be either a single logical, or a logical vector with a length of 4.")
    }
  }

  # CREATE ANNOTATION DATA FRAME FIELDS #

  ## WHEN ONLY ANNOTATION FILE / OBJECT IS PROVIDED ##
  if(!is.null(annot_base) && is.null(matrix)){

    ##### IDs #####
    #### LOGIC ####
    if(!is.null(IDs)){


      if(is.list(IDs)){
        if(collapse::ldepth(IDs) == 1 && length(IDs) == 1){

          if(lengths(IDs) == x){

            if(!is.null(names(IDs))){
              if(names(IDs) %in% y){ # If annotation object contains a field name matching the provided lists name
                if(replace_annot_cols[1]){           # If replace annotation columns on matching names is TRUE, replace the field name in the output with the list
                  ID = IDs
                  annot_base = annot_base[!grepl(names(IDs), y)]
                }else{                               # Otherwise preserve both columns
                  ID = IDs
                }
              }else{                                 # If no conflicting names, preserve list name as annotation column name
                ID = IDs
              }
            }else{                                   # If no annotation name given, assign "ID" as list name. If an annotation field with the name "ID" already exists, the object or file name will be appended to the column using code found near the end of this script.
              ID = IDs
              names(ID) = annot_col_names[1]
            }
          }

        }


      }else if(is.vector(IDs)){
        if(!is.null(names(IDs))){
          IDs = unname(IDs)
        }


        if(length(IDs) == 1){
          if(select_annot_cols[1]){
            if(IDs %in% y){
              ID = annot_base[IDs]
              annot_base = annot_base[!grepl(IDs, y)]

              if(!preserve_names[1]){
                names(ID) = annot_col_names[1]
              }

            }
          }else if(length(IDs) == x){
            ID = list(IDs)
            names(ID) = annot_col_names[1]
          }
        }else if(length(IDs) != 1 && length(IDs) == x){
          ID = list(IDs)
          names(ID) = annot_col_names[1]
        }
      }
    }


    #### FAILURE MESSAGES ####
    # if(is.null(IDs)){
    #   stop(sprintf("Please provide an ID field either by providing a field name from %s in IDs or a vector of the same length of the annotated dataframe %s in IDs.", annot_file, annot_file))
    # }
    #
    # if(is.list(IDs)){
    #   if(collapse::ldepth(IDs) > 1){
    #     stop("Overly nested list object - please provide either a vector or a named / unnamed listed vector.")
    #   }
    #   if(length(IDs) != 1){
    #     stop("Too many lists in 'IDs' - please ensure 'IDs' is either a single named / unnamed listed vector or a vector with the appropriate number of elements.")
    #   }
    #
    # }
    #
    # if(length(IDs) != dim(annot_base)[1]){
    #   if(length(IDs) != 1){
    #     stop(sprintf("Dimension mismatch - length of supplied ID vector in 'IDs' does not match the length of fields in the suplied annotation file %s.", annot_file))
    #
    #   }else if(!(IDs %in% y)){
    #     stop(sprintf("No field named %s found in %s, and annotation file %s is larger than one entry. Please provide a vector in IDs that's either the same length as %s, or a name of a column in %s for use as the ID field of the new annotation data frame.", IDs, annot_file, annot_file, annot_file, annot_file))
    #   }
    # }


    ##### PSEUDOTIME LABELS #####
    #### LOGIC ####
    if(!is.null(pseudotime_labs)){

      if(is.list(pseudotime_labs)){
        if(collapse::ldepth(pseudotime_labs) == 1 && length(pseudotime_labs) == 1){

          if(lengths(pseudotime_labs) == x){

            if(!is.null(names(pseudotime_labs))){
              if(names(pseudotime_labs) %in% y){
                if(replace_annot_cols[2]){
                  Labels = pseudotime_labs
                  annot_base = annot_base[!grepl(names(pseudotime_labs), y)]
                }else{
                  Labels = pseudotime_labs
                }
              }else{
                Labels = pseudotime_labs
              }
            }else{
              Labels = pseudotime_labs
              names(Labels) = annot_col_names[2]
            }
          }

        }



      }else if(is.vector(pseudotime_labs)){
        if(!is.null(names(pseudotime_labs))){
          pseudotime_labs = unname(pseudotime_labs)
        }

        if(length(pseudotime_labs) == 1){
          if(select_annot_cols[2]){
            if(pseudotime_labs %in% y){
              Labels = annot_base[pseudotime_labs]
              annot_base = annot_base[!grepl(pseudotime_labs, y)]

              if(!preserve_names[2]){
                names(Labels) = annot_col_names[2]
              }

            }
          }else if(length(pseudotime_labs) == x){
            Labels = list(pseudotime_labs)
            names(Labels) = annot_col_names[2]
          }
        }else if(length(pseudotime_labs) != 1 && length(pseudotime_labs) == x){
          Labels = list(pseudotime_labs)
          names(Labels) = annot_col_names[2]
        }
      }

    }else{
      Labels = NULL
    }

    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotime_labs)){
    #
    #
    #   if(is.list(pseudotime_labs)){
    #
    #     if(length(pseudotime_labs) != 1){
    #       stop("Too many lists in 'pseudotime_labs' - please ensure 'pseudotime_labs' is a single named/unnamed list or vector containing the appropriate number of pseudotime labels.")
    #
    #     }else if(!is.null(names(pseudotime_labs))){
    #       if(lengths(pseudotime_labs) != dim(annot_base)[1]){
    #         stop("Dimension mismatch - 'pseudotime_labs' and annotated dataframe have differing lengths. Please check both for missing/duplicate entries and try again.")
    #
    #       }else if(names(pseudotime_labs) %in% y){
    #         stop(sprintf("Name conflict - annotation object already contains field named %s - either rename annotation field, or select new name for 'pseudotime_labs' field.", names(pseudotime_labs)))
    #       }
    #
    #     }else if(lengths(pseudotime_labs) != dim(annot_base)[1]){
    #       stop("Dimension mismatch - 'pseudotime_labs' and annotated dataframe have differing lengths. Please check both for missing/duplicate entries and try again.")
    #
    #     }
    #
    #
    #   }else if(is.vector(pseudotime_labs)){
    #
    #     if(length(pseudotime_labs) != dim(annot_base)[1]){
    #
    #       if(length(pseudotime_labs) != 1){
    #         stop("Dimension mismatch - 'pseudotime_labs' and annotated dataframe have differing lengths. Please check both for missing/duplicate entries and try again.")
    #
    #       }else{
    #         if(!(pseudotime_labs %in% y)){
    #           stop(sprintf("No field named %s in the provided annotation object - please provide either a list or vector of the same length as the annotation matrix, or the name of a field in the annotation matrix.", pseudotime_labs))
    #
    #         }
    #       }
    #
    #     }
    #   }else{
    #     stop("'pseudotime_labs' not a list or vector - please provide either a list or character vector for 'pseudotime_labs' for labeling the pseudotime numeric 'pseudotimes'.")
    #   }
    # }else if(!is.null(pseudotimes) && is.null(pseudotime_labs)){
    #   stop("'pseudotime_labs' list or vector not provided - please provide either a list or character vector for 'pseudotime_labs' for labeling the pseudotime numeric 'pseudotimes'.")
    # }


    ##### PSEUDOTIMES #####
    #### LOGIC ####
    if(!is.null(pseudotimes)){

      if(is.list(pseudotimes)){
        if(collapse::ldepth(pseudotimes) == 1 && length(pseudotimes) == 1){

          if(lengths(pseudotimes) == x){

            if(is.numeric(unlist(pseudotimes, use.names = FALSE))){

              if(!is.null(names(pseudotimes))){
                if(names(pseudotimes) %in% y){
                  if(replace_annot_cols[3]){
                    Pseudotime = pseudotimes
                    annot_base = annot_base[!grepl(names(pseudotimes), y)]
                  }else{
                    Pseudotime = pseudotimes
                  }
                }else{
                  Pseudotime = pseudotimes
                }
              }else{
                Pseudotime = pseudotimes
                names(Pseudotime) = annot_col_names[3]
              }

            }
          }

        }


      }else if(is.vector(pseudotimes)){
        if(!is.null(names(pseudotimes))){
          pseudotimes = unname(pseudotimes)
        }

        if(length(pseudotimes) == 1){
          if(select_annot_cols[3]){
            if(pseudotimes %in% y){
              if(is.numeric(unlist(annot_base[pseudotimes], use.names = FALSE))){
                Pseudotime = annot_base[pseudotimes]
                annot_base = annot_base[!grepl(pseudotimes, y)]

                if(!preserve_names[3]){
                  names(Pseudotime) = annot_col_names[3]
                }

              }
            }
          }else if(length(pseudotimes) == x){
            if(is.numeric(pseudotimes)){
              Pseudotime = list(pseudotimes)
              names(Pseudotime) = annot_col_names[3]
            }
          }
        }else if(length(pseudotimes) != 1 && length(pseudotimes) == x){
          if(is.numeric(pseudotimes)){
            Pseudotime = list(pseudotimes)
            names(Pseudotime) = annot_col_names[3]
          }
        }
      }
    }else{
      Pseudotime = NULL
    }


    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotimes)){
    #
    #   if(is.list(pseudotimes)){
    #
    #     if(length(pseudotimes) != 1){
    #       stop("Too many lists in 'pseudotimes' - please ensure 'pseudotimes' is a single named or unnamed numeric / integer list containing the appropriate time points for the sample data.")
    #
    #     }else if(!is.numeric(unlist(pseudotimes, use.names = FALSE))){
    #       stop("'pseudotimes' list is not a numeric list - please enter a numeric list for 'pseudotimes'.")
    #
    #     }else if(lengths(pseudotimes) != x){
    #       stop("Dimension mismatch - 'pseudotimes' list and annotated dataframe have differing lengths. Please check both for missing/duplicate entries and try again.")
    #
    #     }else if(!is.null(names(pseudotimes))){
    #       if(names(pseudotimes) %in% y){
    #         stop(sprintf("Name conflict - annotation object already contains field named %s - either rename annotation field, or select new name for 'pseudotimes' field.", names(pseudotimes)))
    #       }
    #     }
    #   }else{
    #     stop("'pseudotimes' not a list - please provide either a named or unnamed numeric list for 'pseudotimes'.")
    #
    #   }
    #
    # }


  }

  ## WHEN ONLY COUNT MATRIX FILE / OBJECT IS PROVIDED ##
  if(is.null(annot_base) && !is.null(matrix)){
    if(annot_type == "vars"){
      x = dim(matrix)[[2]]
    }else if(annot_type == "obs"){
      x = dim(matrix)[[1]]
    }


    ##### IDs #####
    #### LOGIC ####
    if(annot_type == "vars"){

      if(!is.null(dimnames(matrix)[[2]])){
        ID = list(dimnames(matrix)[[2]])
        names(ID) = annot_col_names[1]
      }

    }else if(annot_type == "obs"){

      if(!is.null(dimnames(matrix)[[1]])){
        ID = list(dimnames(matrix)[[1]])
        names(ID) = annot_col_names[1]
      }
    }




    #### FAILURE MESSAGES ####
    # if(!is.null(IDs)){
    #   stop("Please provide either a count matrix with appropriate dimension names to serve as annotation IDs, or a vector / list of appropriate length for this same purpose in 'IDs', not both.")
    # }
    #
    # if(annot_type == "vars"){
    #   if(is.null(dimnames(matrix)[[2]])){
    #     stop("Provided count matrix does not have variable (i.e. column) names to use as annotation IDs. Please add column names to count matrix, or include the IDs to be used as a list in 'IDs'.")
    #   }
    #
    # }else if(annot_type == "obs"){
    #   if(is.null(dimnames(matrix)[[1]])){
    #     stop("Provided count matrix does not have observation (i.e. row) names to use as annotation IDs. Please add column names to count matrix, or include the IDs to be used as a list in 'IDs'.")
    #   }
    #
    # }else{
    #   stop("Please specify which axis of the count matrix you'd like to make the annotations dataframe for - \"vars\" for variables (i.e. column data) or \"obs\" for observations (i.e. row data).")
    # }
    ##### PSEUDOTIME LABELS #####
    #### LOGIC ####
    if(!is.null(pseudotime_labs)){

      if(is.list(pseudotime_labs)){
        if(collapse::ldepth(pseudotime_labs) == 1 && length(pseudotime_labs) == 1){


          if(annot_type == "vars"){

            if(lengths(pseudotime_labs) == dim(matrix)[[2]]){
              if(!is.null(names(pseudotime_labs))){
                Labels = pseudotime_labs

              }else{
                Labels = pseudotime_labs
                names(Labels) = annot_col_names[2]
              }
            }

          }else if(annot_type == "obs"){

            if(lengths(pseudotime_labs) == dim(matrix)[[1]]){
              if(!is.null(names(pseudotime_labs))){
                Labels = pseudotime_labs
              }else{
                Labels = pseudotime_labs
                names(Labels) = annot_col_names[2]
              }
            }
          }
        }


      }else if(is.vector(pseudotime_labs)){
        if(!is.null(names(pseudotime_labs))){
          pseudotime_labs = unname(pseudotime_labs)
        }

        if(annot_type == "vars"){

          if(length(pseudotime_labs) == dim(matrix)[[2]]){
            Labels = list(pseudotime_labs)
            names(Labels) = annot_col_names[2]
          }

        }else if(annot_type == "obs"){

          if(length(pseudotime_labs) == dim(matrix)[[1]]){
            Labels = list(pseudotime_labs)
            names(Labels) = annot_col_names[2]
          }
        }
      }
    }else{
      Labels = NULL
    }

    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotime_labs)){
    #
    #   if(is.list(pseudotime_labs)){
    #
    #     if(length(pseudotime_labs) != 1){
    #       stop("Too many lists in 'pseudotime_labs' - please ensure 'pseudotime_labs' is a single named or unnamed list containing the appropriate number of pseudotime labels.")
    #
    #     }else{
    #       if(annot_type == "vars"){
    #         if(lengths(pseudotime_labs) != dim(matrix)[[2]]){
    #           stop("Dimension mismatch - 'pseudotime_labs' and count matrix variables (i.e. columns) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #         }
    #
    #       }else if(annot_type == "obs"){
    #         if(lengths(pseudotime_labs) != dimnames(matrix)[[1]]){
    #           stop("Dimension mismatch - 'pseudotime_labs' and count matrix observations (i.e. rows) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #         }
    #
    #       }else{
    #         stop("Please specify which axis of the count matrix you'd like to make the annotations dataframe for - \"vars\" for variables (i.e. column data) or \"obs\" for observations (i.e. row data).")
    #       }
    #     }
    #
    #   }else if(is.vector(pseudotime_labs)){
    #
    #     if(annot_type == "vars"){
    #       if(length(pseudotime_labs) != dim(matrix)[[2]]){
    #         stop("Dimension mismatch - 'pseudotime_labs' and count matrix variables (i.e. columns) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #       }
    #
    #     }else if(annot_type == "obs"){
    #       if(length(pseudotime_labs) != dim(matrix)[[1]]){
    #         stop("Dimension mismatch - 'pseudotime_labs' and count matrix observations (i.e. rows) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #       }
    #
    #     }else{
    #       stop("Please specify which axis of the count matrix you'd like to make the annotations dataframe for - \"vars\" for variables (i.e. column data) or \"obs\" for observations (i.e. row data).")
    #     }
    #
    #   }else{
    #     stop("'pseudotime_labs' not a list or vector - please provide either a list or character vector for 'pseudotime_labs' for labeling the pseudotime numeric 'pseudotimes'.")
    #   }
    #
    # }else if(!is.null(pseudotimes) && is.null(pseudotime_labs)){
    #   stop("'pseudotime_labs' list or vector not provided - please provide either a list or character vector for 'pseudotime_labs' for labeling the pseudotime numeric 'pseudotimes'.")
    # }
    ##### PSEUDOTIMES #####
    #### LOGIC ####
    if(!is.null(pseudotimes)){

      if(is.list(pseudotimes)){
        if(collapse::ldepth(pseudotimes) == 1 && length(pseudotimes) == 1){

          if(is.numeric(unlist(pseudotimes, use.names = FALSE))){

            if(annot_type == "vars"){

              if(lengths(pseudotimes) == dim(matrix)[[2]]){
                if(!is.null(names(pseudotimes))){
                  Pseudotime = pseudotimes

                }else{
                  Pseudotime = pseudotimes
                  names(Pseudotime) = annot_col_names[3]
                }
              }

            }else if(annot_type == "obs"){

              if(lengths(pseudotimes) == dim(matrix)[[1]]){
                if(!is.null(names(pseudotimes))){
                  Pseudotime = pseudotimes

                }else{
                  Pseudotime = pseudotimes
                  names(Pseudotime) = annot_col_names[3]
                }
              }
            }

          }

        }


      }else if(is.vector(pseudotimes)){
        if(!is.null(names(pseudotimes))){
          pseudotimes = unname(pseudotimes)
        }

        if(is.numeric(pseudotimes)){

          if(annot_type == "vars"){

            if(length(pseudotimes) == dim(matrix)[[2]]){
              Pseudotime = list(pseudotimes)
              names(Pseudotime) = annot_col_names[3]
            }

          }else if(annot_type == "obs"){

            if(length(pseudotimes) == dim(matrix)[[1]]){
              Pseudotime = list(pseudotimes)
              names(Pseudotime) = annot_col_names[3]
            }
          }

        }

      }
    }else{
      Pseudotime = NULL
    }



    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotimes)){
    #
    #   if(is.list(pseudotimes)){
    #     if(length(pseudotimes) != 1){
    #       stop("Too many lists in 'pseudotimes' - please ensure 'pseudotimes' is a single named or unnamed numeric / integer list containing the appropriate time points for the sample data.")
    #
    #     }else if(!is.numeric(unlist(pseudotimes, use.names = FALSE))){
    #       stop("'pseudotimes' list is not a numeric list - please enter a numeric list for 'pseudotimes'.")
    #
    #     }else{
    #       if(annot_type == "vars"){
    #         if(lengths(pseudotimes) != dimnames(matrix)[[2]]){
    #           stop("Dimension mismatch - 'pseudotimes' and count matrix variables (i.e. columns) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #         }
    #
    #       }else if(annot_type == "obs"){
    #         if(lengths(pseudotimes) != dimnames(matrix)[[1]]){
    #           stop("Dimension mismatch - 'pseudotimes' and count matrix observations (i.e. rows) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #         }
    #
    #       }else{
    #         stop("Please specify which axis of the count matrix you'd like to make the annotations dataframe for - \"vars\" for variables (i.e. column data) or \"obs\" for observations (i.e. row data).")
    #       }
    #
    #     }
    #
    #   }else{
    #     stop("'pseudotimes' not a list - please provide either a named or unnamed numeric list for 'pseudotimes'.")
    #   }
    #
    # }


  }


  ### WHEN BOTH ARE PROVIDED ###
  if(!is.null(annot_base) && !is.null(matrix)){

    ### VERIFY THAT COUNT MATRIX AND ANNOTATION OBJECT HAVE MATCHING DIMENSIONS (CORRECTING IF REQUESTED), AND REORDER ANNOTATION MATRIX TO MATCH INDEX ORDER OF MATRIX ###
    if(annot_type == "vars"){

      if(!is.null(dimnames(matrix)[[2]])){
        z = dimnames(matrix)[[2]]

        if(dim(matrix)[[2]] != x){
          if(remove_missing){


            if(!select_annot_cols[1]){
              select_annot_cols[1] = TRUE
            }


            if(dim(matrix)[[2]] < x){

              if(!is.list(IDs) && is.vector(IDs) && length(IDs) == 1 && IDs %in% names(annot_base)){
                idx = eval(parse(text = sprintf("stats::na.omit(match(z, table = annot_base$%s))", IDs)))
                annot_base = lapply(annot_base, "[", idx)

                if(unique(lengths(annot_base)) != dim(matrix)[[2]]){
                  stop("Dimension mismatch - attempt to reduce annotation object to matrix variables faild. Check matrix variable names and annotation IDs for correct syntax and try again.")
                }
              }else{
                stop("Cannot remove missing annotation observations - please provide a valid ID column name from the provided annotation object via 'IDs' to use to compare against the provided matrix's varable names.")
              }
            }else{
              stop(sprintf("Annotation object %s contains less observations than the provided matrix - add missing observation(s) to provided annotation object or subset matrix to match provided annotation data and try again."))
            }
          }else{
            if(dim(matrix)[[2]] > x){
              stop("Dimension mismatch - the number of variables in the provided matrix is greater than the number of observations in the provided annotation object - either subset the matrix to match provided annotation object or append missing observations to annotation object and try again.")
            }else if(dim(matrix)[[2]] < x){
              stop("Dimension mismatch - annotation object contains more observations than the number of variables in the provided matrix - double-check that correct objects have been provided, and if necessary subet annotation object to match matrix by setting 'remove_missing' to TRUE.")
            }
          }

        }else if(!is.list(IDs) && is.vector(IDs) && length(IDs) == 1 && IDs %in% names(annot_base)){
          idx = eval(parse(text = sprintf("stats::na.omit(match(z, table = annot_base$%s))", IDs)))
          annot_base = lapply(annot_base, "[", idx)

          if(dim(annot_base)[1] != dim(matrix)[[2]]){
            stop("Dimension mismatch - check matrix variable names and annotation IDs for correct syntax and try again.")
          }

        }
      }else{
        stop("Matrix is missing variable names - please provide a matrix that has dimension names for its variables, or adjust the import options to properly import them.")
      }
    }else if(annot_type == "obs"){

      if(!is.null(dimnames(matrix)[[1]])){
        z = dimnames(matrix)[[1]]

        if(dim(matrix)[[1]] != x){
          if(remove_missing){


            if(!select_annot_cols[1]){
              select_annot_cols[1] = TRUE
            }


            if(dim(matrix)[[1]] < x){

              if(!is.null(IDs) && !is.list(IDs) && is.vector(IDs) && length(IDs) == 1 && IDs %in% names(annot_base)){
                idx = eval(parse(text = sprintf("na.omit(match(z, table = annot_base$%s))", IDs)))
                annot_base = lapply(annot_base, "[", idx)

                if(unique(lengths(annot_base)) != dim(matrix)[[1]]){
                  stop("Dimension mismatch - attempt to reduce annotation object to matrix observations faild. Check matrix observation names and annotation IDs for correct syntax and try again.")
                }
              }else{
                stop("Cannot remove missing annotation observations - please provide a valid ID column name from the provided annotation object via 'IDs' to use to compare against the provided matrix's observation names.")
              }
            }else{
              stop(sprintf("Annotation object %s contains less observations than the provided matrix - add missing observation(s) to provided annotation object or subset count matrix to match provided annotation data and try again."))
            }
          }else{
            if(dim(matrix)[[1]] > x){
              stop("Dimension mismatch - the number of observations in the provided matrix is greater than the number of observations in the provided annotation object - either subset the matrix to match provided annotation object or append missing observations to annotation object and try again.")
            }else{
              stop("Dimension mismatch - annotation object contains more observations than the number of observations in the provided matrix - double-check that correct objects have been provided, and if necessary subet annotation object to match matrix by setting 'remove_missing' to TRUE.")
            }
          }
        }else if(!is.list(IDs) && is.vector(IDs) && length(IDs) == 1 && IDs %in% names(annot_base)){
          idx = eval(parse(text = sprintf("stats::na.omit(match(z, table = annot_base$%s))", IDs)))
          annot_base = lapply(annot_base, "[", idx)

          if(dim(annot_base)[1] != dim(matrix)[[1]]){
            stop("Dimension mismatch - check matrix observation names and annotation IDs for correct syntax and try again.")
          }
        }else{
          stop("Matrix is missing observation names - please provide a matrix that has dimension names for its observations, or adjust the import options to properly import them.")
        }
      }
    }else if(is.null(annot_type) || annot_type != "uns"){
      stop("Please specify a valid annotation structure type to generate via 'annot_type' - see documentation for valid options.")
    }



    ##### IDs #####
    #### LOGIC ####
    # if(is.null(IDs)){
    #   message("'IDs' field is NULL - will attempt to pull annotation IDs from count matirx dimnames.")
    #
    #   if(annot_type == "vars"){
    #
    #     if(!is.null(dimnames(matrix)[[2]])){
    #       ID = list(dimnames(matrix)[[2]])
    #       names(ID) = annot_col_names[1]
    #     }
    #
    #   }else if(annot_type == "obs"){
    #
    #     if(!is.null(dimnames(matrix)[[1]])){
    #       ID = list(dimnames(matrix)[[1]])
    #       names(ID) = annot_col_names[1]
    #     }
    #   }

    if(!is.null(IDs)){

      if(is.list(IDs)){
        if(collapse::ldepth(IDs) == 1 && length(IDs) == 1){

          if(lengths(IDs) == x){

            if(!is.null(names(IDs))){
              if(names(IDs) %in% y){ # If annotation object contains a field name matching the provided lists name
                if(replace_annot_cols[1]){           # If replace annotation columns on matching names is TRUE, replace the field name in the output with the list
                  ID = IDs
                  annot_base = annot_base[!grepl(names(IDs), y)]
                }else{                               # Otherwise preserve both columns
                  ID = IDs
                }
              }else{                                 # If no conflicting names, preserve list name as annotation column name
                ID = IDs
              }
            }else{                                   # If no annotation name given, assign "ID" as list name. If an annotation field with the name "ID" already exists, the object or file name will be appended to the column using code found near the end of this script.
              ID = IDs
              names(ID) = annot_col_names[1]
            }
          }

        }


      }else if(is.vector(IDs)){
        if(!is.null(names(IDs))){
          IDs = unname(IDs)
        }

        if(length(IDs) == 1){
          if(select_annot_cols[1]){
            if(IDs %in% y){
              ID = annot_base[IDs]
              annot_base = annot_base[!grepl(IDs, y)]

              if(!preserve_names[1]){
                names(ID) = annot_col_names[1]
              }

            }
          }else if(length(IDs) == x){
            ID = list(IDs)
            names(ID) = annot_col_names[1]
          }
        }else if(length(IDs) != 1 && length(IDs) == x){
          ID = list(IDs)
          names(ID) = annot_col_names[1]
        }
      }

    }


    #### FAILURE MESSAGES ####
    # if(is.null(IDs)){
    #
    #
    #   if(annot_type == "vars"){
    #     if(dim(matrix)[[2]] != dim(annot_base)[[1]]){
    #       stop("Count matrix varialbes (i.e. columns) and annotation dataframe entry lengths do not match - please check annotation dataframe object for missing variables.")
    #
    #
    #     }else if(is.null(dimnames(matrix)[[2]])){
    #       stop("Provided count matrix does not have variable (i.e. column) names to use as annotation IDs. Please add column names to count matrix, or include the IDs to be used as a list in 'IDs'.")
    #     }
    #
    #
    #   }else if(annot_type == "obs"){
    #     if(dim(matrix)[[1]] != dim(annot_base)[[1]]){
    #       stop("Count matrix observations (i.e. rows) and annotation dataframe entry lengths do not match - please check annotation dataframe object for missing variables.")
    #
    #
    #     }else if(is.null(dimnames(matrix)[[1]])){
    #       stop("Provided count matrix does not have observation (i.e. row) names to use as annotation IDs. Please add column names to count matrix, or include the IDs to be used as a list in 'IDs'.")
    #     }
    #
    #
    #   }else{
    #     stop("Please specify at least one of the following:\n 1.) An axis of the count matrix you'd like to make the annotations dataframe for - \"vars\" for variables (i.e. column data) or \"obs\" for observations (i.e. row data).\n\n 2.) ")
    #   }
    #
    # }else if(!is.null(IDs)){
    #
    #   if(length(IDs) != dim(annot_base)[1]){
    #     if(length(IDs) != 1){
    #       stop(sprintf("Dimension mismatch - length of supplied ID vector in 'IDs' does not match the length of fields in the suplied annotation file %s.", annot_file))
    #
    #
    #
    #     }else{
    #       if(!(IDs %in% y)){
    #         stop(sprintf("No field named %s found in %s, and annotation file %s is larger than one entry. Please provide a vector in IDs that's either the same length as %s, or a name of a column in %s for use as the ID field of the new annotation data frame.", IDs, annot_file, annot_file, annot_file, annot_file))
    #
    #       }
    #     }
    #   }
    # }
    ##### PSEUDOTIME LABELS #####
    #### LOGIC ####
    if(!is.null(pseudotime_labs)){

      if(is.list(pseudotime_labs)){
        if(collapse::ldepth(pseudotime_labs) == 1 && length(pseudotime_labs) == 1){

          if(lengths(pseudotime_labs) == x){

            if(!is.null(names(pseudotime_labs))){
              if(names(pseudotime_labs) %in% y){
                if(replace_annot_cols[2]){
                  Labels = pseudotime_labs
                  annot_base = annot_base[!grepl(names(pseudotime_labs), y)]
                }else{
                  Labels = pseudotime_labs
                }
              }else{
                Labels = pseudotime_labs
              }
            }else{
              Labels = pseudotime_labs
              names(Labels) = annot_col_names[2]
            }
          }
        }


      }else if(is.vector(pseudotime_labs)){
        if(!is.null(names(pseudotime_labs))){
          pseudotime_labs = unname(pseudotime_labs)
        }

        if(length(pseudotime_labs) == 1){
          if(select_annot_cols[2]){

            if(pseudotime_labs %in% y){
              Labels = annot_base[pseudotime_labs]

              if(!is.vector(remove_labs_pattern)){

                annot_base = annot_base[!grepl(pseudotime_labs, y)]
                if(!preserve_names[2]){
                  names(Labels) = annot_col_names[2]
                }

              }else{
                names(Labels) = annot_col_names[2]

              }
            }
          }else if(length(pseudotime_labs) == x){
            Labels = list(pseudotime_labs)
            names(Labels) = annot_col_names[2]
          }
        }else if(length(pseudotime_labs) == x){
          Labels = list(pseudotime_labs)
          names(Labels) = annot_col_names[2]
        }
      }

    }else{
      Labels = NULL
    }


    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotime_labs)){
    #
    #   if(is.list(pseudotime_labs)){
    #
    #     if(length(pseudotime_labs) != 1){
    #       stop("Too many lists in 'pseudotime_labs' - please ensure 'pseudotime_labs' is a single named or unnamed list containing the appropriate number of pseudotime labels.")
    #     }
    #
    #     if(lengths(pseudotime_labs) != x){
    #       stop("Dimension mismatch - 'pseudotime_labs' and annotated dataframe have differing lengths. Please check both for missing/duplicate entries and try again.")
    #     }
    #
    #     if(!is.null(names(pseudotime_labs))){
    #       if(names(pseudotime_labs) %in% y){
    #         stop(sprintf("Name conflict - annotation object already contains field named %s - either rename annotation field, or select new name for 'pseudotime_labs' field.", names(pseudotime_labs)))
    #
    #       }
    #     }
    #
    #     if(is.null(IDs)){   # IF 'IDs' IS NULL, THE IDs WILL BE TAKEN FROM THE COUNT MATRIX FIELD - CAN PLAN ACCORDINGLY
    #
    #       if(annot_type == "vars"){
    #         if(lengths(pseudotime_labs) != dimnames(matrix)[[2]]){
    #           stop("Dimension mismatch - 'pseudotime_labs' and count matrix variables (i.e. columns) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #         }
    #
    #       }else if(annot_type == "obs"){
    #         if(lengths(pseudotime_labs) != dimnames(matrix)[[1]]){
    #           stop("Dimension mismatch - 'pseudotime_labs' and count matrix observations (i.e. rows) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #         }
    #
    #       }else{
    #         stop("Please specify which axis of the count matrix you'd like to make the annotations dataframe for - \"vars\" for variables (i.e. column data) or \"obs\" for observations (i.e. row data).")
    #
    #       }
    #
    #     }
    #
    #   }else{
    #     stop("'pseudotime_labs' not a list - please provide either a named or unnamed character list for 'pseudotime_labs'.")
    #   }
    #
    #
    # }

    ##### PSEUDOTIMES #####
    #### LOGIC ####
    if(!is.null(pseudotimes)){

      if(is.list(pseudotimes)){
        if(collapse::ldepth(pseudotimes) == 1 && length(pseudotimes) == 1){

          if(lengths(pseudotimes) == x){

            if(is.numeric(unlist(pseudotimes, use.names = FALSE))){

              if(!is.null(names(pseudotimes))){
                if(names(pseudotimes) %in% y){
                  if(replace_annot_cols[3]){
                    Pseudotime = pseudotimes
                    annot_base = annot_base[!grepl(names(pseudotimes), y)]
                  }else{
                    Pseudotime = pseudotimes
                  }
                }else{
                  Pseudotime = pseudotimes
                }
              }else{
                Pseudotime = pseudotimes
                names(Pseudotime) = annot_col_names[3]
              }

            }
          }

        }


      }else if(is.vector(pseudotimes)){
        if(!is.null(names(pseudotimes))){
          pseudotimes = unname(pseudotimes)
        }

        if(length(pseudotimes) == 1){
          if(select_annot_cols[3]){
            if(pseudotimes %in% y){

              if(is.vector(remove_pseudotimes_pattern)){
                Pseudotime = annot_base[pseudotimes]

              }else if(is.numeric(unlist(annot_base[pseudotimes], use.names = FALSE))){
                Pseudotime = annot_base[pseudotimes]
                annot_base = annot_base[!grepl(pseudotimes, y)]

                if(!preserve_names[3]){
                  names(Pseudotime) = annot_col_names[3]
                }
              }
            }
          }else if(length(pseudotimes) == x){
            if(is.numeric(pseudotimes)){
              Pseudotime = list(pseudotimes)
              names(Pseudotime) = annot_col_names[3]
            }
          }
        }else if(length(pseudotimes) != 1 && length(pseudotimes) == x){
          if(is.numeric(pseudotimes)){
            Pseudotime = list(pseudotimes)
            names(Pseudotime) = annot_col_names[3]
          }
        }
      }
    }else{
      Pseudotime = NULL
    }



    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotimes)){
    #
    #   if(is.list(pseudotimes)){
    #
    #
    #     if(length(pseudotimes) != 1){
    #       stop("Too many lists in 'pseudotimes' - please ensure 'pseudotimes' is a single named or unnamed numeric / integer list containing the appropriate time points for the sample data.")
    #
    #
    #     }else if(!is.numeric(unlist(pseudotimes, use.names = FALSE))){
    #       stop("'pseudotimes' list is not a numeric list - please enter a numeric list for 'pseudotimes'.")
    #
    #
    #     }else if(lengths(pseudotimes) != x){
    #       stop("Dimension mismatch - 'pseudotimes' and annotated dataframe have differing lengths. Please check both for missing/duplicate entries and try again.")
    #
    #
    #     }else if(!is.null(names(pseudotimes))){
    #       if(names(pseudotimes) %in% y){
    #         stop(sprintf("Name conflict - annotation object already contains field named %s - either rename annotation field, or select new name for 'pseudotime_labs' field.", names(pseudotimes)))
    #
    #       }
    #     }
    #
    #     if(is.null(IDs)){   # IF 'IDs' IS NULL, THE IDs WILL BE TAKEN FROM THE COUNT MATRIX FIELD - CAN PLAN ACCORDINGLY
    #
    #       if(annot_type == "vars"){
    #         if(lengths(pseudotimes) != dimnames(matrix)[[2]]){
    #           stop("Dimension mismatch - 'pseudotimes' and count matrix variables (i.e. columns) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #         }
    #
    #       }else if(annot_type == "obs"){
    #         if(lengths(pseudotimes) != dimnames(matrix)[[1]]){
    #           stop("Dimension mismatch - 'pseudotimes' and count matrix observations (i.e. rows) have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #         }
    #
    #       }else{
    #         stop("Please specify which axis of the count matrix you'd like to make the annotations dataframe for - \"vars\" for variables (i.e. column data) or \"obs\" for observations (i.e. row data).")
    #
    #       }
    #
    #     }
    #
    #   }else{
    #     stop("'pseudotimes' not a list - please provide either a named or unnamed numeric list for 'pseudotimes'.")
    #   }
    #
    #
    # }


  }


  ### WHEN NEITHER ARE PROVIDED ###
  if(is.null(annot_base) && is.null(matrix)){


    ##### IDs #####
    #### LOGIC ####
    if(!is.null(IDs)){


      if(is.list(IDs)){
        if(collapse::ldepth(IDs) == 1){
          if(!is.null(names(IDs))){
            ID = IDs
          }else{
            ID = IDs
            names(ID) = annot_col_names[1]
          }
        }



      }else if(is.vector(IDs)){
        if(!is.null(names(IDs))){
          IDs = unname(IDs)
        }

        ID = list(IDs)
        names(ID) = annot_col_names[1]
      }
    }


    #### FAILURE MESSAGES ####
    # if(is.null(IDs)){
    #   stop("Please specify either an ID vector / list, or provide a count matrix with appropriate dimension names or annotation data frame that contains the sample IDs. If provideing an annotation data frame, please specify the name of the field that will serve as the ID field in the 'IDs' option as a single character vector.")
    # }
    ##### PSEUDOTIME LABELS #####
    #### LOGIC ####
    if(!is.null(pseudotime_labs)){

      if(is.list(pseudotime_labs)){
        if(collapse::ldepth(pseudotime_labs) == 1 && length(pseudotime_labs) == 1){

          if(lengths(pseudotime_labs) == lengths(ID)){
            if(!is.null(names(pseudotime_labs))){
              Labels = pseudotime_labs

            }else{
              Labels = pseudotime_labs
              names(Labels) = annot_col_names[2]
            }
          }


        }
      }else if(is.vector(pseudotime_labs)){

        if(!is.null(names(pseudotime_labs))){
          pseudotime_labs = unname(pseudotime_labs)
        }

        if(length(pseudotime_labs) == lengths(ID)){
          Labels = list(pseudotime_labs)
          names(Labels) = annot_col_names[2]
        }

      }

    }else{
      Labels = NULL
    }



    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotime_labs)){
    #
    #   if(is.list(pseudotime_labs)){
    #
    #     if(length(pseudotime_labs) != 1){
    #       stop("Too many lists in 'pseudotime_labs' - please ensure 'pseudotime_labs' is a single named or unnamed list containing the appropriate number of pseudotime labels.")
    #
    #     }else if(lengths(pseudotime_labs) != lengths(IDs)){
    #       stop("Dimension mismatch - 'pseudotime_labs' and supplied IDs (i.e. 'IDs') have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #     }
    #
    #   }else if(is.vector(pseudotime_labs)){
    #
    #
    #
    #
    #   }else{
    #     stop("'pseudotime_labs' not a list - please provide either a named or unnamed character list for 'pseudotime_labs'.")
    #   }
    #
    # }
    ##### PSEUDOTIMES #####
    #### LOGIC ####
    if(!is.null(pseudotimes)){

      if(is.list(pseudotimes)){
        if(collapse::ldepth(pseudotimes) == 1 && length(pseudotimes) == 1){

          if(is.numeric(unlist(pseudotimes, use.names = FALSE))){

            if(lengths(pseudotimes) == lengths(ID)){
              if(!is.null(names(pseudotimes))){
                Pseudotime = pseudotimes

              }else{
                Pseudotime = pseudotimes
                names(Pseudotime) = annot_col_names[3]
              }
            }
          }
        }

      }else if(is.vector(pseudotimes)){

        if(!is.null(names(pseudotimes))){
          pseudotimes = unname(pseudotimes)
        }

        if(length(pseudotimes) == lengths(ID)){
          if(is.numeric(pseudotimes)){
            Pseudotime = list(pseudotimes)
            names(Pseudotime) = annot_col_names[3]
          }
        }
      }
    }else{
      Pseudotime = NULL
    }



    #### FAILURE MESSAGES ####
    # if(!is.null(pseudotimes)){
    #
    #   if(is.list(pseudotimes)){
    #     if(length(pseudotimes) != 1){
    #       stop("Too many lists in 'pseudotimes' - please ensure 'pseudotimes' is a single named or unnamed numeric / integer list containing the appropriate time points for the sample data.")
    #
    #     }else if(!is.numeric(unlist(pseudotimes, use.names = FALSE))){
    #       stop("'pseudotimes' list is not a numeric list - please enter a numeric list for 'pseudotimes'.")
    #
    #     }else if(lengths(pseudotimes) != lengths(IDs)){
    #       stop("Dimension mismatch - 'pseudotimes' and supplied IDs (i.e. 'IDs') have differing lengths. Please check for both missing/duplicate entries and try again.")
    #
    #     }
    #
    #   }else{
    #     stop("'pseudotimes' not a list - please provide either a named or unnamed numeric list for 'pseudotimes'.")
    #   }
    #
    # }

  }

  ##### POTENTIAL PARALLELIZING CODE #####
  # ## CREATE LIST FOR PARALLELIZING OPERATIONS IN FOR LOOP ##
  # if(!is.null(pseudotimes) && !is.null(pseudotime_map)){
  #   stop("Provide pseudotime list 'pseudotimes' or a named list 'pseudotime_map', not both. See 'sc_annot_data()' help information for more details.")
  # }else if(is.null(pseudotime_labs) && !is.null(pseudotime_map)){
  #   stop("Please provide labels for matching 'pseudotime_map' values to. See help for details.")
  # }else if(!is.null(color_palette) && is.null(color_by)){
  #   stop("Please provide a metric to use to determine how to apply the color palette.")
  # }
  #
  # annot_list = NULL
  #
  # if(!is.null(IDs)){
  #   annot_list = c(annot_list, "IDs")
  # }
  #
  # if(!is.null(pseudotime_labs)){
  #   annot_list = c(annot_list, "IDs")
  # }
  #
  # if(!is.null(pseudotime_labs)){
  #   annot_list = c(annot_list, "pseudotime_labs")
  # }
  #
  # if(!is.null(pseudotimes)){
  #   annot_list = c(annot_list, "pseudotime")
  # }
  #
  # if(!is.null(pseudotime_map)){
  #   annot_list = c(annot_list, "pseudotime_map")
  # }
  #
  # if(!is.null(color_palette)){
  #   annot_list = c(annot_list, "color_palette")
  # }
  #
  # if(!is.null(color_by)){
  #   annot_list = c(annot_list, "color_by")
  # }
  #
  # if(!is.null(...)){
  #   annot_list = c(annot_list, ...names())
  # }
  #
  #
  # ## FOR LOOP ##
  # for(i in annot_list){
  #
  #
  #
  # }
  ##### END OF POTENTIAL PARALLELIZING CODE #####


  ## START TO CREATE ANNOTATION LIST OBJECT ##
  annot_data = NULL

  ### IDs ###
  annot_data = c(annot_data, ID)

  ### LABELS ###
  if(!is.null(Labels)){

    #### REMOVE PATTERN(S) ####
    if(is.vector(remove_labs_pattern)){
      for(i in remove_labs_pattern){
        Labels = gsub(pattern = i, replacement = '', perl = FALSE, x = unlist(Labels))
      }
      Labels = as.vector(Labels)
      Labels = list(Labels)
      names(Labels) = annot_col_names[2]
    }

    #### DROP BY PATTERN ####
    if(is.vector(drop_labs_containing)){

      for(i in drop_labs_containing){
        idx = !grepl(pattern = i, unlist(Labels))
        Labels = lapply(Labels, "[", idx)
        annot_data = lapply(annot_data, "[", idx)
        if(!is.null(annot_base)){
          annot_base = lapply(annot_base, "[", idx)
        }
        if(!is.null(Pseudotime)){
          Pseudotime = lapply(Pseudotime, "[", idx)
        }
      }
    }

    #### SELECT BY PATTERN ####
    if(is.vector(select_labs_containing)){

      for(i in select_labs_containing){
        idx = grepl(pattern = i, unlist(Labels))
        Labels = lapply(Labels, "[", idx)
        annot_data = lapply(annot_data, "[", idx)
        if(!is.null(annot_base)){
          annot_base = lapply(annot_base, "[", idx)
        }
        if(!is.null(Pseudotime)){
          Pseudotime = lapply(Pseudotime, "[", idx)
        }
      }
    }

    annot_data = c(annot_data, Labels)
  }

  if(!is.null(Pseudotime)){

    ### REMOVE PATTERN(S) ###
    if(is.vector(remove_pseudotimes_pattern)){
      for(i in remove_pseudotimes_pattern){
        Pseudotime = gsub(pattern = i, replacement = '', perl = FALSE, x = unlist(Pseudotime))
      }

      # Pseudotime = as.numeric(Pseudotime)
      Pseudotime = list(Pseudotime)
      names(Pseudotime) = annot_col_names[3]
    }

    ### DROP BY PATTERN ###
    if(is.vector(drop_pseudotimes_containing)){

      for(i in drop_pseudotimes_containing){
        idx = !grepl(pattern = i, unlist(Pseudotime))
        Pseudotime = lapply(Pseudotime, "[", idx)
        annot_data = lapply(annot_data, "[", idx)
        if(!is.null(annot_base)){
         annot_base =  lapply(annot_base, "[", idx)
        }
      }
    }

    ### SELECT BY PATTERN ###
    if(is.vector(select_pseudotimes_containing)){

      for(i in select_pseudotimes_containing){
        idx = grepl(pattern = i, unlist(Pseudotime))
        Pseudotime = lapply(Pseudotime, "[", idx)
        annot_data = lapply(annot_data, "[", idx)
        if(!is.null(annot_base)){
          annot_base = lapply(annot_base, "[", idx)
        }
      }
    }

    annot_data = c(annot_data, Pseudotime)
    pseudotime_idx = length(annot_data)
  }else{
    pseudotime_idx = NULL
  }

  ## SETUP COLORS ##
  if(!is.null(color_palette)){
    if(is.null(color_by)){
      if(!is.null(Labels)){
        colors_key = ann_color_palette(color_palette = color_palette,
                                       color_by = Labels)

        # if(!is.null(color_by_order)){
        #   colors_key = ann_color_palette(color_palette = color_palette,
        #                                  color_by = Labels,
        #                                  order = color_by_order)
        # }else{
        #   colors_key = ann_color_palette(color_palette = color_palette,
        #                                  color_by = Labels)
        # }

        color_list = list(colors_key[match(unlist(Labels), table = names(colors_key))])
        names(color_list) = annot_col_names[4]

      }else{
        stop("Please provide either Pseudotime labels via 'pseudotime_labs' or another annotation metric name via 'color_by' to generate a vector for coloring the results by.")
      }

    }else{
      if(!is.null(annot_base)){
        if(length(color_by) == 1){
          if(color_by %in% names(annot_base)){
            colors_key = ann_color_palette(color_palette = color_palette,
                                           color_by = unique(unlist(annot_base[[color_by]])))

            # if(!is.null(color_by_order)){
            #   colors_key = ann_color_palette(color_palette = color_palette,
            #                                  color_by = unique(unlist(annot_base[[color_by]])),
            #                                  order = color_by_order)
            # }else{
            #   colors_key = ann_color_palette(color_palette = color_palette,
            #                                  color_by = unique(unlist(annot_base[[color_by]])))
            # }

            color_list = list(colors_key[match(unique(annot_base[[color_by]]), table = names(colors_key))])
            names(color_list) = sprintf("%s Colors", color_by)


          }else{
            stop(sprintf("No field named %s found in annotation object.", color_by))
          }
        }else{
          stop("Please provide a single character vector naming a field in the annotation object to color by, or leave null to color by Pseudotime Labels if provided via 'pseudotime_labs'.")
        }
      }else{

      }

    }


  }else{
    if(!is.null(color_by)){
      stop("Please provide a color palette via 'color_palette'.")
    }else{
      color_list = NULL
    }
  }

  ## BIND COLOR LIST TO ANNOTATION OBJECT IF GENERATED ##
  if(!is.null(color_list)){
    annot_data = c(annot_data, color_list)
  }



  ## VALIDATE ADDITIONALLY PROVIDED ARGUMENTS ... ##
  if(...length() >= 1){
    reserved_names = c(names(ID))

    if(!is.null(Labels)){
      reserved_names = c(reserved_names, names(Labels))
    }
    if(!is.null(Pseudotime)){
      reserved_names = c(reserved_names, names(Pseudotime))
    }
    if(!is.null(color_list)){
      reserved_names = c(reserved_names, names(color_list))
    }


    for(i in 1:...length()){
      if(length(...elt(i)) != lengths(ID)){
        stop(sprintf("Argument %s is not the same length as the number of IDs - please check argument '%s' and 'IDs'.", ...names()[[i]], ...names()[[i]]))
      }else if(...names()[i] %in% reserved_names){
        stop(sprintf("Name conflict - rename '%s' so it does not conflict with the 'IDs', 'pseudotime_labs', 'pseudotimes', and / or colors field names. See function details for default name values.", ...names()[[i]]))
      }else if(!is.null(annot_base)){
        if(isTRUE(replace_annot_cols[3+i]) && ...names()[i] %in% y){
          stop(sprintf("Name conflict - column named %s already exists in the provided annotated data frame. If you'd like to replace this field, set 'replace_annot_cols[%s]' to TRUE - otherwise, either rename the argument or annotation column name.", ...names()[[i]], i))
        }
      }

      a = list(...elt(i))
      names(a) = ...names()[i]
      annot_data = c(annot_data, a)

    }

  }

  if(!is.null(annot_base)){
    annot_data = c(annot_data, annot_base)
  }

  ## IF ANNOTATED DATA FRAME OBJECT IS PROVIDED - CHECK COLUMN NAMES FOR CONFLICTS ##

  for(i in 1:length(names(annot_data))){
    if(duplicated(names(annot_data))[i]){
      if(!is.null(annot_file)){
        names(annot_data)[i] = sprintf("%s.%s", names(annot_data)[i], file_path_sans_ext(annot_file, compression = TRUE))
      }else if(!is.null(annot_obj)){
        names(annot_data)[i] = sprintf("%s.%s", names(annot_data)[i], deparse(substitute(annot_obj)))
      }else{
        stop("Resulting annotation object has duplicate variable names - check provided generic names.")
      }
    }
  }

  # ## SUBSET ANNOTATION OBJECT ##
  #
  # ### DROP BY LABELS PATTERN ###
  # if(is.vector(drop_labs_containing)){
  #
  #   for(i in drop_labs_containing){
  #     idx = !grepl(pattern = i, unlist(Labels))
  #     Labels = lapply(Labels, "[", idx)
  #     annot_data = lapply(annot_data, "[", idx)
  #   }
  # }
  #
  # ### SELECT BY LABELS PATTERN ###
  # if(is.vector(select_labs_containing)){
  #
  #   for(i in select_labs_containing){
  #     idx = grepl(pattern = i, unlist(Labels))
  #     Labels = lapply(Labels, "[", idx)
  #     annot_data = lapply(annot_data, "[", idx)
  #   }
  # }
  #
  # ### DROP BY PSEUDOTIME PATTERN ###
  # if(is.vector(drop_pseudotimes_containing)){
  #
  #   for(i in drop_pseudotimes_containing){
  #     idx = !grepl(pattern = i, unlist(Pseudotime))
  #     Pseudotime = lapply(Pseudotime, "[", idx)
  #     annot_data = lapply(annot_data, "[", idx)
  #   }
  # }
  #
  # ### SELECT BY PATTERN ###
  # if(is.vector(select_pseudotimes_containing)){
  #
  #   for(i in select_pseudotimes_containing){
  #     idx = grepl(pattern = i, unlist(Pseudotime))
  #     Pseudotime = lapply(Pseudotime, "[", idx)
  #     annot_data = lapply(annot_data, "[", idx)
  #   }
  # }


  ## SET PSEUDOTIME TO NUMERIC IF PROVIDED ##
  if(!is.null(pseudotime_idx)){
    annot_data[[pseudotime_idx]] = as.numeric(annot_data[[pseudotime_idx]])
  }


  ## RETURN ANNOTATION OBJECT ##
  if(return_as == "default"){
    if(annot_type == "vars" || annot_type == "obs"){
      annot_data = data.table::data.table(data.table::rbindlist(list(annot_data)))


    }else if(annot_type != "uns"){
      stop("Invalid annotation type - please designate the annotation type you'd like to generate.")
    }

    return(annot_data)

  }else if(return_as == "list"){
    return(annot_data)

  }else if(return_as == "data frame"){
    annot_data = data.frame(data.table::rbindlist(list(annot_data)), check.names = FALSE)
    return(annot_data)

  }else if(return_as == "data table"){
    annot_data = data.table::data.table(data.table::rbindlist(list(annot_data)), check.names = FALSE)
  }

}
