#' theme_scrc.confusion_matrix
#'
#' @description This function specify theme used in the scrc package
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
theme_scrc.confusion_matrix <-
  function (theme.base_size = 12,
                                         theme.base_family = "sans",
                                         theme.title_size = 36,
                                         theme.text_size = 3*theme.title_size/4,
                                         theme.margins = c(1,1, 1, 1),
                                         legend.position="right",
                                         ...)
{
  (ggthemes::theme_foundation(
    base_size = theme.base_size,
    base_family = theme.base_family) +
     ggplot2::theme(line = ggplot2::element_line(),
                    rect = ggplot2::element_rect(fill =
                                                   ggthemes::ggthemes_data$fivethirtyeight$value[3],
                                                 linetype = 0, colour = NA),
                    plot.title = ggplot2::element_text(colour =
                                                         ggthemes::ggthemes_data$fivethirtyeight$value[1],
                                                       vjust = 1, hjust = 0.5,
                                                       size=theme.title_size,
                                                       face="bold"),
                    text = ggplot2::element_text(colour = ggthemes::ggthemes_data$fivethirtyeight$value[1]),
                    #axis.title = ggplot2::element_blank(),
                    axis.title = ggplot2::element_text(size= 16,
                                                       face="bold",
                                                       vjust = 0.5),
                    #axis.title.y = ggplot2::element_text(angle=90),
                    #axis.text.y = ggplot2::element_blank(),
                    axis.text = ggplot2::element_blank(),#ggplot2::element_text(size=theme.text_size, face="bold"),
                    #axis.text.x = ggplot2::element_text(angle = 90),
                    axis.ticks = ggplot2::element_blank(),
                    axis.line.x = ggplot2::element_blank(),
                    axis.line.y = ggplot2::element_blank(),
                    legend.position=legend.position,
                    legend.background = ggplot2::element_rect(fill = "white"),

                    panel.grid = ggplot2::element_line(colour = NULL),
                    panel.grid.major =
                      ggplot2::element_line(colour = ggthemes::ggthemes_data$fivethirtyeight$value[2]),
                    panel.grid.minor =
                      ggplot2::element_blank(),
                    panel.background = ggplot2::element_rect(fill = "white"),
                    # plot.title = ggplot2::element_text(hjust = 0, size = rel(1.75), face = "bold"),
                    plot.margin = ggplot2::unit(theme.margins, "lines"),
                    plot.background = ggplot2::element_rect(fill = "white"),
                    strip.background = ggplot2::element_rect(fill = "white"),
                    #strip.text.y = ggplot2::element_text(angle = 180),
                    strip.text =
                      ggplot2::element_text(size= 12,
                                            face="bold",
                                            vjust = 0.5#,lineheight = theme.text_size*3
                      )))
}
