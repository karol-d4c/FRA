CalculateITRC <-
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

    foreach::foreach(itrc.i =
                       1:nrow(confusion.bootstrap.diagonal.table)) %do% {

      itrc_ <- confusion.bootstrap.diagonal.table[itrc.i,]
      prob_ <-
        (confusion.bootstrap.diagonal.table %>%
           dplyr::filter(
             max.signal == itrc_[["max.signal"]],
             bootstrap ==  itrc_[["bootstrap"]]) %>%
           dplyr::filter_(
             paste(model$signal,
                   "<=",
                   itrc_[[model$signal]])
           ) %>%
           dplyr::summarise(prob.bootstrap = sum(prob.bootstrap)))[["prob.bootstrap"]]
      itrc_ %>%
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
      dplyr::mutate(itrc = prob.mean) ->
      model$itrc.table

    model$itrc.table[1,] %>%
      dplyr::mutate_all(.funs = function(x){0}) %>%
      dplyr::mutate(itrc = 1,
                    prob.mean = 1) %>%
      rbind(model$itrc.table) %>%
      dplyr::ungroup() ->
      model$itrc.table

    model$itrc.table %>%
      dplyr::group_by_(
        model$signal
      ) %>%
      dplyr::summarise(
        itrc =
          max(prob.mean)
      ) ->
      model$itrc

    max.itrc <- 1
    for(i in 1:nrow(model$itrc)) {
      max.itrc <- max(max.itrc, model$itrc[i,]$itrc)
      model$itrc[i,]$itrc <- max.itrc
    }

    return(model)
  }
