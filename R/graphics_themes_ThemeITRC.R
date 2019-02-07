#' theme_itrc
#'
#' @description This function specify theme used in the ITRC package
#'
#' @param  theme.base_size = 12,
#' @param  theme.base_family = "sans",
#' @param  theme.title_size = 36,
#' @param  theme.text_size = 3*theme.title_size/4,
#' @param  theme.margins = c(1,1, 1, 1),
#' @param  legend.position = "right",
#'
#' @return ggthemes::theme_foundation object
#'
#' @export
theme_itrc <-  function(
  theme.base_size = 9,
  theme.base_family = "sans",
  theme.title_size = 12,
  theme.text_size = 9,
  theme.text.axes_size = 9,
  theme.text.strip_size = 9,
  theme.text.legend_size = 12,
  theme.margins = c(1,1, 1, 1),
  legend.position = "right",
  ...
)
{
  theme.args <-
    list(
      line = ggplot2::element_line(),
      rect = ggplot2::element_rect(
        fill = "#F0F0F0",
          #ggthemes::ggthemes_data$fivethirtyeight$value[3],
        linetype = 0,
        colour = NA),
      plot.title = ggplot2::element_text(
        colour = "#3C3C3C",#ggthemes::ggthemes_data$fivethirtyeight$value[1],
        vjust = 0.5,
        hjust = 0.5,
        size  = theme.title_size,
        face  = "bold"),
      text = ggplot2::element_text(
        colour = "#000000"
          #ggthemes::ggthemes_data$fivethirtyeight$value[1] previously
        ),
      axis.title =
        ggplot2::element_text(
          size = theme.title_size),
      axis.title.y =
        ggplot2::element_text(
          angle = 90
          ),
      axis.text =
        ggplot2::element_text(
          size  = theme.text.axes_size,
          face = "bold"),
      axis.text.x =
        ggplot2::element_text(
          vjust = 0.5,
          hjust = 0.5,
          angle = 90),
      axis.ticks = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(),
      axis.line.y = ggplot2::element_blank(),
      legend.position = legend.position,
      legend.title =
        ggplot2::element_text(
          size = theme.text.legend_size
        ),
      legend.background =
        ggplot2::element_rect(fill = "white"),
      panel.grid =
        ggplot2::element_line(colour = NULL),
      panel.grid.major =
        ggplot2::element_line(
          colour = "#D2D2D2"
          # ggthemes::ggthemes_data$fivethirtyeight$value[2]
          ),
      panel.grid.minor =
        ggplot2::element_blank(),
      panel.background =
        ggplot2::element_rect(fill = "white"),
      # plot.title = ggplot2::element_text(hjust = 0, size = rel(1.75), face = "bold"),
      plot.margin =
        ggplot2::unit(theme.margins, "lines"),
      plot.background =
        ggplot2::element_rect(fill = "white"),
      strip.background =
        ggplot2::element_rect(fill = "white"),
      strip.text =
        ggplot2::element_text(
          size= theme.text.strip_size,
          face="bold",
          vjust = 0.5,
          lineheight = theme.text_size*3
        )
    )

  theme.dots.args <- list(...)
  if(length(theme.dots.args) > 0){
    theme.combine.args <-
      append(
        x = theme.dots.args,
        values = theme.args[-which(names(theme.args) %in% names(theme.dots.args))])
  } else {
    theme.combine.args <- theme.args
  }

  ggthemes::theme_foundation(
    base_size = theme.base_size,
    base_family = theme.base_family) +
    do.call(what = ggplot2::theme,
            args = theme.combine.args)
}

