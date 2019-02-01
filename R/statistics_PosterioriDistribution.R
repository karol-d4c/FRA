# fitted(lr_model)
#
# a jezeli potrzebujesz jedynie parametrow diagnostycznych klasyfikatora to mozesz uzyc
#
# confusionMatrix(lr_model)
####
PreapreLogRegDataFrame <-
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
#
# # Probability of proper discrimiantion
# PosterioriDistribution <-
#   function(
#     data,
#     stims.comp = NULL,
#     signal.comp = NULL,
#     cc_maxit = 100,
#     lr_maxit = 1000,
#     MaxNWts = 5000,
#     ...
#   ){
#     if(is.null(stims.comp)){
#       stims.comp <-
#         (data %>%
#            dplyr::distinct_(paste(stimulation.col)))[[stimulation.col]]
#     }
#
#     df.res <- PreapreLogRegDataFrame(
#       data = data,
#       stims.comp = stims.comp,
#       signal.comp = signal.comp,
#       ...)
#
#     lr_model <- nnet::multinom(formula = df.res$formula_string,
#                                data = df.res$data[,
#                                                   c(df.res$signal,
#                                                     df.res$response)],
#                                na.action = na.omit,
#                                maxit = lr_maxit,
#                                MaxNWts = MaxNWts)
#     attr(x =  lr_model$terms, ".Environment") <- NULL
#
#     lr.fit <- fitted(object = lr_model)
#     if(length(stims.comp) == 2){
#       lr.fit <- cbind(lr.fit, 1- lr.fit[,1])
#     }
#
#     return(
#       list(
#         posterior = lr.fit,
#         model = lr_model
#       ))
#   }
#
# # Probability of proper discrimiantion
# ProbabilityOfDiscrimination <-
#   function(
#     data,
#     stims.comp = NULL,
#     stimulation.col = "Stim",
#     signal = "signal",
#     response = "intensity",
#     formula_string  = paste(signal, response, sep = "~"),
#     cc_maxit = 100,
#     lr_maxit = 1000,
#     MaxNWts = 5000,
#     lr.fit = NULL
#   ){
#     if(is.null(lr.fit)){
#       lr.fit <- PosterioriDistribution(
#         data = data,
#         stims.comp = stims.comp,
#         stimulation.col = stimulation.col,
#         signal = signal,
#         response = response,
#         formula_string  = formula_string,
#         cc_maxit = cc_maxit,
#         lr_maxit = lr_maxit,
#         MaxNWts = 5000
#       )
#     }
#     return(mean(apply(lr.fit, 1, max)))
#   }
#
#
# #### CalculateArimotoRenyiChannelCapacity ####
# CalculateArimotoRenyiChannelCapacity <-
#   function(
#     data,
#     stims.comp = NULL,
#     stimulation.col = "Stim",
#     signal = "signal",
#     response = "intensity",
#     formula_string  = paste(signal, response, sep = "~"),
#     cc_maxit = 100,
#     lr_maxit = 1000,
#     MaxNWts = 5000,
#     lr.fit = NULL
#   ){
#     if(is.null(lr.fit)){
#       lr.fit <- PosterioriDistribution(
#         data = data,
#         stims.comp = stims.comp,
#         stimulation.col = stimulation.col,
#         signal = signal,
#         response = response,
#         formula_string  = formula_string,
#         cc_maxit = cc_maxit,
#         lr_maxit = lr_maxit,
#         MaxNWts = 5000
#       )
#     }
#     if(is.null(stims.comp)){
#       stims.comp <- (data %>% dplyr::distinct_(stimulation.col))[[stimulation.col]]
#     }
#     M <- length(stims.comp)
#     log2(M*mean(apply(lr.fit, 1, max)))
#   }
