#' CalculateConfuison
#'
#' @description This function calculates confusiuon waves from confusion matrix.
#' model need to have confusion matrix attribute
#'
CalculateConfusion <-
  function(model,
           ...){
    signal.list <-
      (model$confusion.matrix %>%
         dplyr::distinct_(model$signal) %>%
         dplyr::arrange_(model$signal))[[model$signal]]
    model$confusion.waves <-
      CalculateConfusionWaves(
        model = model,
        signal.list = signal.list)
    model$confusion.waves.polygon <-
      CalculateConfusionWavesPolygons(
        model = model,
        confusion.waves = model$confusion.waves,
        signal.list = signal.list)
    return(model)
  }
