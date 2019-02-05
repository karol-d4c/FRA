#' GetWaveColorsRescalingObject
#'
#' @description Internal function that specify function which returns color for signal level.
#'
#' @param pallete.fun function, declared as \code{function(n){ }} that returns n colors with increasing intensity
#' @param class.list numeric vector, specify levels of color sensitivity
#' @param colors.n numeric, number of colors in range, \code{default = 100}
#' @param colors.min.diff numeric, minimal difference between colors ids in colors range, \code{default = 2}
#'
#' @return function, declared as \code{function(level) {}}, that return color for level
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
      pallete.fun <-
        GenerateWavePalleteFunction()
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
    return(color.fun)
  }



#' GetPalleteColorsList
#' @description This function return list of internal colors palletes
#'
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

#' GetPalleteColors
#'
#' @description This function return colors pallete according to internal parameters. In default return virdis pallete.
#'
#' @param pallete specify colors pallete; pallete can be chosen from set
#' \code{c("virdis", "blue_red", "blue_green_red", "yellow_red", "orange", "bcgyr")} or as a vector of colors that will be
#' passed to the function \code{GetPalleteFun} to create new pallete
#' @param pallete.args list, arguments to pallete (for instance to virdis function)
#'
#' @return vector of colors pallete in RGB
#'
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

#' GetPalleteFun
#'
#' @description  These functions return functions that interpolate a set of given colors to create
#' new color palettes (like topo.colors) and color ramps, functions that map the interval [0, 1] to colors (like grey).
#'
#' @param colors.range.list colors to interpolate; must be a valid argument to col2rgb().
#'
#' @return character vector of colors in RGB
#'
GetPalleteFun <-
  function(
    colors.range.list = NULL,
    ...
  ){
    #pallete.fun <-
    grDevices::colorRampPalette(colors = colors.range.list) %>%
      return()
  }

#' GetLevelColors
#'
#' @description Returns colors pallete for signals defined in \code{levels}
#'
#' @param levels numeric vector
#' @param levels.names character vector, names of signals
#' @inheritDotParams GetPalleteColors
#'
#' @return vector of colors for signal levels
#'
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

    color.fun <-
      GetWaveColorsRescalingObject(
      pallete.fun = pallete.fun,
      class.list = levels,
      ...)

    colors <- sapply(levels, color.fun)
    names(colors) <- levels.names
    return(colors)
  }
