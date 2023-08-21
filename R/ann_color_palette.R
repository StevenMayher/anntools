#' @export




ann_color_palette = function(color_palette = NULL,
                             color_by = NULL){


  if(!is.null(color_palette)){
    if(!is.null(color_by)){

      ## IF 'COLOR_PALETTE' IS A LIST - UNLIST TO VECTOR ##
      if(is.list(color_palette)){
        color_palette = unlist(color_palette)
      }

      ## IF 'COLOR_BY' IS A LIST - UNLIST TO VECTOR ##
      if(is.list(color_by)){
        color_by = unlist(color_by)
      }

      color_by = unique(color_by)

      # if(!is.null(order)){
      #   if(length(order) == length(color_by)){
      #     if(is.character(order)){
      #       idx = match(order, table = color_by)
      #     }else{
      #       idx = order
      #     }
      #     color_by = color_by[idx]
      #   }
      # }


      ## LOGIC FOR EACH PACKAGE AND PALETTE ##
      if(color_palette[1] == 'ggsci'){
        common_colors = sprintf("stats::setNames(ggsci::%s(palette = color_palette[3])(length(unique(color_by)))[as.numeric(as.factor(unique(color_by)))], unique(color_by))", color_palette[2])
        return(eval(parse(text = common_colors)))

      }else if(color_palette[1] == 'RColorBrewer'){
        common_colors = stats::setNames(RColorBrewer::brewer.pal(n = length(unique(color_by)), name = color_palette[2])[as.numeric(as.factor(unique(color_by)))], unique(color_by))
        return(common_colors)

      }else if(color_palette[1] == 'grDevices'){
        common_colors = stats::setNames(grDevices::palette.colors(n = length(unique(color_by)), palette = color_palette[2])[as.numeric(as.factor(unique(color_by)))], unique(color_by))
        return(common_colors)

      }

    }else{
      stop("Please provide a metric to color by via 'color_by'.")
    }

  }else{
    if(is.null(color_by)){
      stop("Please provide the package name and appropriate arguments to specify a color palette via either a list or vector in 'color_palette', and a list or vector to color by via 'color_by'.")
    }else{
      stop("Please provide the package name and appropriate arguments to specify a color palette via either a list or vector in 'color_palette'.")
    }
  }

}
