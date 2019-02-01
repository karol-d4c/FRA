#' GetPlotTheme
#' @param theme_fun \code{ggthemes} or \code{ggplot2} theme function. Default set to \code{theme_itrc}.
#' @param theme_fun.args list of parameters to \code{theme_fun}
#'
GetPlotTheme <-
  function(
    theme_fun = theme_itrc,
    theme_fun.args = list(),
    ...
  ){
  do.call(
    what = theme_fun,
    args = theme_fun.args) %>%
    return()
}
