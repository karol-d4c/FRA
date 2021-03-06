GetLogRegParameters <-
  function(
    data,
    model,
    signal.list = NULL,
    signal.factor.column = "X",
    response.factor.column = "Y",
    ...
  ){
    data %>%
      dplyr::filter_(paste(model$signal, "%in%", "signal.list")) ->
      data

    response.cols <-
      which(colnames(data) %in%
              model$response)
    response.factor <- paste(response.factor.column,
                             model$response,
                             sep = "_")
    colnames(data)[response.cols] <- response.factor

    data %>%
      dplyr::rename_(factor_signal_obj = model$signal) %>%
      dplyr::mutate(
        factor_signal_obj = factor_signal_obj#paste(signal.factor.column, factor_signal_obj, sep = "_")
      ) %>%
      dplyr::rename_(
        .dots = setNames(nm = signal.factor.column,
                         object = "factor_signal_obj")) %>%
      dplyr::select_(
        paste("c(",
              signal.factor.column,
              ",",
              paste(response.factor, collapse = ","),
              ")")) ->
      data

    return(
      list(
        formula_string = paste(signal.factor.column,
                               paste(response.factor, collapse = "+"),
                               sep = "~"),
        data = data,
        signal = signal.factor.column,
        response = response.factor
      )
    )
  }
