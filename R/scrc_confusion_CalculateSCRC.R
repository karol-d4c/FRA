CalculateSCRC <-
  function(
    model,
    cols.list,
    ...
  ){

    model$confusion.bootstrap.table %>%
      dplyr::filter_(
        paste(model$signal,
              "==",
              model$class)
      ) ->
      confusion.bootstrap.diagonal.table

    foreach::foreach(scrc.i =
                       1:nrow(confusion.bootstrap.diagonal.table)) %do% {

      scrc_ <- confusion.bootstrap.diagonal.table[scrc.i,]
      prob_ <-
        (confusion.bootstrap.diagonal.table %>%
           dplyr::filter(
             max.signal == scrc_[["max.signal"]],
             bootstrap ==  scrc_[["bootstrap"]],
             computation ==  scrc_[["computation"]]
             ) %>%
           dplyr::filter_(
             paste(model$signal,
                   "<=",
                   scrc_[[model$signal]])
           ) %>%
           dplyr::summarise(prob.bootstrap = sum(prob.bootstrap)))[["prob.bootstrap"]]
      scrc_ %>%
        dplyr::mutate(prob.bootstrap = prob_)
    }  %>%
      do.call(what = rbind,
              args = .) %>%
      dplyr::group_by_(
        model$signal,
        "max.signal"
      ) %>%
      dplyr::summarise(
        prob.mean = mean(prob.bootstrap),
        prob.sd = sd(prob.bootstrap)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange_("max.signal",
                      model$signal) %>%
      dplyr::mutate(scrc = prob.mean) ->
      model$scrc.table

    model$scrc.table[1,] %>%
      dplyr::mutate_all(.funs = function(x){0}) %>%
      dplyr::mutate(scrc = 1,
                    prob.mean = 1) %>%
      rbind(model$scrc.table) %>%
      dplyr::ungroup() ->
      model$scrc.table

    model$scrc.table %>%
      dplyr::group_by_(
        model$signal
      ) %>%
      dplyr::summarise(
        scrc =
          max(prob.mean)
      ) ->
      model$scrc

    max.scrc <- 1
    for(i in 1:nrow(model$scrc)) {
      max.scrc <- max(max.scrc, model$scrc[i,]$scrc)
      model$scrc[i,]$scrc <- max.scrc
    }

    return(model)
  }
