#' CalculateConfuison
#'
#' @description This function calculates confusiuon waves from confusion matrix.
#' model need to have confusion matrix attribute
#'
CalculateConfusion <-
  function(model,
           signal.max = NULL,
           confusion.signal.max = signal.max,
           ...){
    signal.list <-
      (model$confusion.table %>%
         dplyr::distinct_(model$signal) %>%
         dplyr::arrange_(model$signal))[[model$signal]]
    if(!is.null(confusion.signal.max) & length(signal.list) > 1){
      signal.list <- signal.list[signal.list <= confusion.signal.max]
    }
    if(length(signal.list) <= 1){
      stop("There must be at last two sigals to plot ITRC waves")
    }
    if(is.null(confusion.signal.max)){
      confusion.signal.max <- max(signal.list)
    }

    model$confusion.waves <-
      CalculateConfusionWaves(
        model = model,
        signal.list = signal.list,
        signal.max  = confusion.signal.max)
    model$confusion.waves.polygon <-
      CalculateConfusionWavesPolygons(
        model = model,
        confusion.waves = model$confusion.waves,
        signal.list = signal.list)
    return(model)
  }
