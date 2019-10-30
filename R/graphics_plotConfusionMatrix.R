#' plotITRCWaves
#'
#' @description This functions return ggplot2 figure that visualise Information
#'  Theoretic Response Curves and specificity of cellular response to particular signals.
#'
#' @param model ITRCModel object return by ITRC function
#' @param confusion.signal signal for which you want to plot confusion matrix, default \code{confusion.signal == max(signal)}
#' @param title_ character, specify title of plot, default \code{"Information Theoretic Response Curve"}
#' @param xlab_ character, label of x axes, default \code{"States number"}
#' @param ylab_ character, label of y axes and legend title, default \code{"Signal levels"}
#' @details TODO important
#' @export
plotCofusionMatrix <-
  function(
    model,
    confusion.signal = NULL,
    save.plot = FALSE,
    title_ = "",
    ylab_ ="Specific Stimulation Level",
    xlab_ = "Stimulation Level",
    ...
  ){

    if(is.null(model)){
      stop("model must be an object of class ITRCModel")
    } else if(class(model) != "ITRCModel"){
      stop("model must be an object of class ITRCModel")
    }

    if(is.null(model$confusion.table)){
      stop("model must be an object of class ITRCModel and should contain confusion.table")
    }

    x_ = "1"
    y_ = "prob"
    fill_ = "fill"

    # # ggplot2::model$confusion.table
    # # df.confusion.fill <- df.confusion$fill
    # # names(df.confusion.fill) <- df.confusion$fill
    #
    # summary_vars <-syms(experiments.df$response.cols)
    #
    # # Wrap the summary variables with mean()
    # summary_vars <- purrr::map(summary_vars, function(var) {
    #   expr(fun.summarise(!!var, na.rm = TRUE))
    # })
    #
    # # Prefix the names with `avg_`
    # names(summary_vars) <- experiments.df$response.protein
    confusion.signal.syms <- quo(max.signal == max(!!sym(model$signal)))
    if(!is.null(confusion.signal)){
      if(is.numeric(confusion.signal)){
        confusion.signal.syms <- quo(max.signal == confusion.signal)
      }
    }



    df.confusion <-
      model$confusion.table %>%
      dplyr::filter(!!confusion.signal.syms) %>%
      dplyr::mutate(
        fill =
          dplyr::if_else(!!quo(!!sym(model$signal) == !!sym(model$class)),
                         ggthemes::canva_palettes[["Subdued and proffesional"]][2],
                         ggthemes::canva_palettes[["Subdued and proffesional"]][1]))

    fill.values <- df.confusion$fill
    names(fill.values) <- fill.values

    ggplot2::ggplot(
        df.confusion,
        mapping =
          ggplot2::aes_string(
            x = x_,
            y = y_,
            fill = fill_
          )
      ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::facet_grid(paste(model$signal,model$class, sep = "~"),
                          switch = "both"
      ) +
      ggplot2::coord_polar(theta = "y", start = 0) +
      ggplot2::ylim(c(0,1)) +
      theme_itrc.confusion_matrix(theme.title_size = 12) +
      ggplot2::ylab(ylab_) +
      ggplot2::xlab(xlab_) +
      ggplot2::ggtitle(title_) +
      ggplot2::scale_fill_manual(values = fill.values,
                                 guide = "none"
                                 ) %>%
    return()
  }
