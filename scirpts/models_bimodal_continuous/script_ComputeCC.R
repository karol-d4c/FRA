computeCC <-
  function(data,
           signal,
           sample,
           response,
           parallel_cores){
    slemi.model <- CapacityLogReg::capacity_logreg_main(
      dataRaw = data,
      signal = signal,
      response = response,
      glmnet_cores = parallel_cores,
      graphs = FALSE,
      model_out = FALSE,
      dataout = FALSE,
      cc_maxit = 100,
      lr_maxit = 1000,
      MaxNWts = 5000)
    quo.max <- quo(!!sym(signal) == max(!!sym(signal)))
    names(quo.max) <- signal
    data.table(itrc = 2^slemi.model$cc,
               Stim = (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!quo.max))[[signal]][[1]])
  }


computeITRC_CC <-
  function(data,
           signal,
           sample,
           response,
           parallel_cores){
    quo.min <- quo(!!sym(signal) != min(!!sym(signal)))
    names(quo.min) <- signal
    signal.list <- (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!quo.min))[[signal]]

    foreach(signal_ = signal.list) %do% {
      computeCC(data =
                  data %>% dplyr::filter(!!quo(!!sym(signal) <= signal_)),
                signal = signal,
                sample = sample,
                response = response,
                parallel_cores = parallel_cores)
    } %>%
      do.call(
        what = rbind,
        args = .
      ) ->
      df.itrc_cc
    return(df.itrc_cc)
  }

####
df.itrc_cc.bimodal <-
  computeITRC_CC(
    data = data.raw.bimodal,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores
  )  %>%
  dplyr::mutate(type = "bimodal")


df.itrc_cc.cont <-
  computeITRC_CC(
    data = data.raw.cont,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores
  )  %>%
  dplyr::mutate(type = "cont")

df.itrc_cc.bimodal %>%
  rbind(df.itrc_cc.cont) %>%
  dplyr::mutate(method = "shannon-capacity") ->
  df.itrc_cc

df.itrc_cc %>%
  rbind(.,
        data.frame(itrc  = c(1,1),
                   Stim  = c(0,0),
                   type  = c("bimodal", "cont"),
                   method = "shannon-capacity")) ->
  df.itrc_cc
