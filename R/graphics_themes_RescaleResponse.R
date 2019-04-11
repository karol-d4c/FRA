#' rescaleDataToITRC.Params
#' @param model ITRCModel
#' @param data data to rescale
#' @param variable.to.compare variable.to.compare TODO
#' @param data_raw_min data_raw_min 0 or min of data
rescaleDataToITRC.Params <-
  function(
    model = NULL,
    data,
    variable.to.compare,
    # type_,
    # type.response_,
    data_raw_min = NULL,
    signal.max = NULL,
    ...
  ){
    if(is.null(data)){
      data <- model$rc.sum
    }

    if(is.null(variable.to.compare)){
      variable.to.compare <- model$response[1]
    }

    if(is.null(signal.max)){
      signal.max <-
        (model$itrc %>%
           dplyr::filter_(paste(model$signal, "==",
                                "max(", model$signal, ")")))[[model$signal]]
    }

    data_states_min <- min((model$itrc %>%
                              dplyr::filter(itrc == min(itrc)))[["itrc"]])
    data_states_max <- max((model$itrc %>%
                              dplyr::filter_(paste(model$signal, "<=", signal.max)) %>%
                              dplyr::filter(itrc == max(itrc)))[["itrc"]])

    if(is.null(data_raw_min)){
      data_raw_min <-
        min((data %>%
               dplyr::filter_(
                 paste("`", variable.to.compare, "`", "==",
                       "min(", "`",variable.to.compare,"`",
                       ")", sep = "")))[[variable.to.compare]])
    }
    data_raw_max <-
      max((data %>%
             dplyr::filter_(paste(model$signal, "<=", signal.max)) %>%
             dplyr::filter_(
               paste("`", variable.to.compare, "`", "==",
                     "max(", "`",variable.to.compare,"`",
                     ")", sep = "")))[[variable.to.compare]])
    a <- (data_states_min - data_states_max)/(data_raw_min - data_raw_max)
    return(list(a = a,
                b = data_states_max - a*data_raw_max))
  }

#' rescaleDataToITRC.DataFrame
#' @param model ITRCModel
#' @param data data to rescale
#' @param variable.to.compare variable.to.compare TODO
#' @param variable.rescaled variable.rescaled TODO
#' @inheritDotParams rescaleDataToITRC.Params
#' @export
rescaleDataToITRC.DataFrame <-
  function(
    model,
    data = NULL,
    variable.to.compare = NULL,
    variable.rescaled = NULL,
    variable.to.rescale = NULL,
    # logStim.0 = 0.001,
    # rcc.waves.logStim.0 = NULL,
    # logfun = log,
    data_raw_min = NULL,
    a = NULL,
    b = NULL,
    ...
  ){
    if(is.null(data)){
      stop("data must be defined")
    } else if(!("data.frame" %in% class(data))){
      stop("data should be data.frame or data.table")
    }

    if(is.null(variable.to.compare)){
      stop("variable.to.compare must be defined")
    } else if(length(variable.to.compare) != 1 |
              !(variable.to.compare %in% colnames(data))
    ) {
      stop("variable.to.compare must be colname of data.frame data")
    }

    if(is.null(a) | is.null(b)){
      rescale.params <-
        rescaleDataToITRC.Params(
          model = model,
          data = data,
          variable.to.compare = variable.to.compare,
          data_raw_min = data_raw_min,
          ...
        )

      a <- rescale.params$a
      b <- rescale.params$b
    }

    if(is.null(variable.rescaled)){
      variable.rescaled <- variable.to.compare
      variable.to.rescale <- variable.to.compare
    }


    data %>%
      dplyr::mutate_(
        .dots =
          setNames(object = paste("`", variable.to.rescale, "`", "*a + b", sep = ""),
                   nm = variable.rescaled
          )) %>%
      reshape2::melt(id.vars = model$signal,
                     measure.vars = variable.rescaled,
                     value.name = "response_rescaled") ->
      #dplyr::mutate(type = "rescaled") ->
      data.rescaled

    return(
      list(
        a = a,
        b = b,
        data.rescaled = data.rescaled
      )
    )
  }
