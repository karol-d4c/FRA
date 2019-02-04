#### GetWaveColors ####
GetWaveColorsRescalingObject <-
  function(
    pallete.fun = NULL,
    class.list,
    colors.n = 100,
    colors.min.diff = 2,
    ...
  ){
    if(is.null(class.list)){
      stop("No classes defined")
    }
    class.list <- sort(class.list)

    if(is.null(pallete.fun)){
      pallete.fun <- GenerateWavePalleteFunction()
    }
    class.max <- max(class.list)
    class.min <- min(class.list)

    class.list.top <- class.list[-1]
    class.list.bottom <- class.list[-length(class.list)]
    class.diff.min <- min(class.list.top - class.list.bottom)

    colors.n <- max(
      colors.n,
      round(colors.min.diff*(class.max - class.min)/class.diff.min + 1))
    pallete <- pallete.fun(colors.n)

    a <- (colors.n - 1)/(class.max - class.min)
    b <- colors.n  - a*class.max

    color.fun <- function(class_){
      pallete[round(a*class_+b)]
    }
    return(
      list(
        color.fun = color.fun,
        colors.n = colors.n,
        a = a,
        b = b,
        pallete  = pallete
      )
    )
  }

GetWaveColorsDataFrame <-
  function(
    pallete.fun = NULL,
    class.list,
    ...
  ){

    if(is.null(class.list)){
      stop("No classes defined")
    }
    class.list <- sort(class.list)

    color.rescaling.object <-
      GetWaveColorsRescalingObject( pallete.fun = pallete.fun,
                                    class.list = class.list,
                                    ...)
    colors.df <-
      data.frame(
        class = class.list,
        color = sapply(class.list,
                       color.rescaling.object$color.fun),
        stringsAsFactors = FALSE)
    return(colors.df)
  }

#### GetPalleteColorsList ####
GetPalleteColorsList <-
  function(...){
    pallete.list <- list()
    pallete.list[["blue_red"]] <-
      c(rev(
        grDevices::rainbow(
          n = 50,
          start=grDevices::rgb2hsv(grDevices::col2rgb('cyan'))[1],
          end=grDevices::rgb2hsv(grDevices::col2rgb('blue'))[1])),
        rev(grDevices::rainbow(
          n = 50,
          start=grDevices::rgb2hsv(grDevices::col2rgb('red'))[1],
          end=grDevices::rgb2hsv(grDevices::col2rgb('yellow'))[1])))
    pallete.list[["blue_green_red"]] <-
      c(rev(
        grDevices::rainbow(
          n = 100,
          start=grDevices::rgb2hsv(grDevices::col2rgb('red'))[1],
          end=grDevices::rgb2hsv(grDevices::col2rgb('blue'))[1])))
    pallete.list[["bcgyr"]] <-
      gplots::col2hex(c("blue","cyan", "green", "yellow", "red"))
    pallete.list[["yellow_blue"]] <-
      c(rev(
        grDevices::rainbow(
          n = 100,
          start=grDevices::rgb2hsv(data.frame(c(255,255,217)))[1],
          end=grDevices::rgb2hsv(data.frame(c(8,29,88)))[1])))
    pallete.list[["orange"]] <-
      c(rev(
        grDevices::rainbow(
          n = 100,
          start=grDevices::rgb2hsv(data.frame(c(102,37,6)))[1],
          end=grDevices::rgb2hsv(data.frame(c(255,255,229)))[1])))
    pallete.list[["yellow_red"]] <-
      c(rev(
        grDevices::rainbow(
          n = 100,
          start=grDevices::rgb2hsv(data.frame(c(128,0,38)))[1],
          end=grDevices::rgb2hsv(data.frame(c(255,255,204)))[1])))
    return(pallete.list)
  }

#### GetPalleteColors ####
GetPalleteColors <-
  function(
   pallete = "virdis",
   pallete.args =
     list(),
   ...
  ){
    if(!is.list(pallete.args)){
      stop("pallete.args should be a list of argumnets")
    }
    if(length(pallete) == 1) {
       if(is.character(pallete)){
        if(pallete == "virdis"){
          if(length(pallete.args) == 0){
            pallete.args <-
              list(n = 1000,
                   option = "D",
                   end = 1)
          }
          colors.list <-
            do.call(what = viridis::viridis,
                  args = pallete.args)
        } else {
          pallete.list <-
            GetPalleteColorsList()
          tryCatch(
            expr = {
            pallete.name <-
              match.arg(pallete,
                       names(pallete.list))
            colors.list <-
              pallete.list[[pallete.name]]
            }, error =
              function(e){
                stop(paste("There is no pallete ", pallete))
              })
        }
    } else if( is.function(pallete))
      colors.list <-
        do.call(what = pallete,
              args = pallete.args)
    } else {
      colors.list <-
        pallete
    }
    return(colors.list)
}


GetPalleteFun <-
  function(
    colors.range.list = NULL,
    ...
  ){
    #pallete.fun <-
    grDevices::colorRampPalette(colors = colors.range.list) %>%
      return()
  }


GetLevelColors <-
  function(
    levels,
    levels.names = levels,
    ...
  ){
    if(all(is.null(levels)) | all(is.na(levels))){
      stop("No levels defined")
    } else if (!all(is.numeric(levels)) & !all(is.numeric(levels.names))){
      stop("Signal levels should be numeric")
    } else if (length(levels) != length(levels.names)){
      levels.names <- levels
      warnings("Names of signal levels should have the same size as signal levels")
    }


    pallete.fun <-
      GetPalleteFun(
        colors.range.list = GetPalleteColors(#))
          ...
        ),
        ...
      )

    if(!is.function(pallete.fun)){
      stop("Improper colors pallete. Try default version")
    }

    color.rescaling.object <-
      GetWaveColorsRescalingObject(
      pallete.fun = pallete.fun,
      class.list = levels,
      ...)

    colors <- sapply(levels,
                             color.rescaling.object$color.fun)
    names(colors) <- levels.names
    return(colors)
  }
