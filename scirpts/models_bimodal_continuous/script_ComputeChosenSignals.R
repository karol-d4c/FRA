computePairs <-
  function(data,
           signal,
           sample,
           response,
           parallel_cores){
    quo.minmax <- quo(!!sym(signal) != min(!!sym(signal)) & !!sym(signal) != max(!!sym(signal)))
    signal.list <- c((data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!quo.minmax))[[signal]])
    signal.min <- (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!sym(signal) == min(!!sym(signal))))[[signal]]
    signal.max <- (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!sym(signal) == max(!!sym(signal))))[[signal]]

    foreach(signal_ = signal.list) %do% {
      if(!is.na(signal_)){
        signal.list_ <- c(signal.min, signal_, signal.max)
      }
      # computeCC(data =
      #             data %>% dplyr::filter(!!quo(!!sym(signal) %in% signal.list_)),
      #           signal = signal,
      #           sample = sample,
      #           response = response,
      #           parallel_cores = parallel_cores) %>%
      #   dplyr::mutate(method = "shannon-capacity")  %>%
      #   dplyr::mutate(Stim = signal_)->
      #   df.pair.cc

      (ITRC(data =
                  data %>% dplyr::filter(!!quo(!!sym(signal) %in% signal.list_)),
                signal = signal,
                response = response,
                bootstrap.number = 64,
                parallel_cores = parallel_cores))$itrc %>%
        dplyr::filter(!!quo(!!sym(signal) == max(!!sym(signal)))) %>%
        dplyr::mutate(Stim = signal_) %>%
        dplyr::mutate(method = "renyi-min-capacity") ->
        df.pair.renyi

      # rbind(df.pair.cc,
      #       df.pair.renyi) ->
      #   df.pair
      return(df.pair.renyi)
  } %>%
      do.call(
        what = rbind,
        args = .
      ) ->
      df.pair
return(df.pair)
  }


#### pair computation ####
df.itrc_cc.bimodal <-
  computePairs(
    data = data.raw.bimodal,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores
  )  %>%
  dplyr::mutate(type = "bimodal")


df.itrc_cc.cont <-
  computePairs(
    data = data.raw.cont,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores
  )  %>%
  dplyr::mutate(type = "cont")

df.itrc_cc.bimodal %>%
  rbind(df.itrc_cc.cont)  ->
  df.itrc_cc


saveRDS(object = df.itrc_cc, file = paste(output.path, "capacity_threes.rds", sep = "/"))
# df.itrc_cc %>%
#   rbind(.,
#         data.frame(itrc  = c(1,1),
#                    Stim  = c(0,0),
#                    type  = c("bimodal", "cont"),
#                    method = "shannon-capacity")) ->
#   df.itrc_cc
