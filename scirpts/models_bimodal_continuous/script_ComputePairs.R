computeIncreasingPairs <-
  function(data,
           signal,
           sample,
           response,
           parallel_cores){
    signal.min <- (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!sym(signal) == min(!!sym(signal))))[[signal]]
    signal.list <- (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!sym(signal) != min(!!sym(signal))))[[signal]]

    foreach(signal_ = signal.list) %do% {
      if(!is.na(signal_)){
        signal.list_ <- c(signal.min, signal_)
      }
      computeCC(data =
                  data %>% dplyr::filter(!!quo(!!sym(signal) %in% signal.list_)),
                signal = signal,
                sample = sample,
                response = response,
                parallel_cores = parallel_cores) %>%
        dplyr::mutate(method = "shannon-capacity")  %>%
        dplyr::mutate(Stim = signal_)->
        df.pair.cc

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

      rbind(df.pair.cc,
            df.pair.renyi) ->
        df.pair
      return(df.pair)
    } %>%
      do.call(
        what = rbind,
        args = .
      ) ->
      df.pair
    return(df.pair)
  }


#### pair computation ####
df.itrc_cc.pair.bimodal <-
  computeIncreasingPairs(
    data = data.raw.bimodal,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores
  )  %>%
  dplyr::mutate(type = "bimodal")
df.itrc_cc.pair <- df.itrc_cc.pair.bimodal

df.itrc_cc.pair.cont <-
  computeIncreasingPairs(
    data = data.raw.cont,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores
  )  %>%
  dplyr::mutate(type = "cont")

df.itrc_cc.pair.bimodal %>%
  rbind(df.itrc_cc.pair.cont)  ->
  df.itrc_cc.pair


saveRDS(object = df.itrc_cc.pair, file = paste(output.path, "capacity_pairs.rds", sep = "/"))
# df.itrc_cc %>%
#   rbind(.,
#         data.frame(itrc  = c(1,1),
#                    Stim  = c(0,0),
#                    type  = c("bimodal", "cont"),
#                    method = "shannon-capacity")) ->
#   df.itrc_cc


df.itrc_cc.pair %>%
  dplyr::left_join(
    df.itrc_cc.pair  %>%
      dplyr::filter(method == "renyi-min-capacity") %>%
      dplyr::mutate(error =  1- itrc/2) %>%
      dplyr::select(Stim, type, error),
    by = c("Stim", "type")) ->
  df.itrc_cc.pair

g.pairs <-
  ggplot(data = df.itrc_cc.pair,
       mapping = aes(x = 1 - error,
                     y = (itrc),
                     group = interaction(method, type),
                     color = interaction(method, type))) +
  geom_point() +
  geom_line() +
  ITRC::theme_itrc() +
  xlim(c(0.5,1)) +
  ylim(c(1,2)) +
  xlab("Probability of correct discrimiantion") +
  ylab("capacity")

ggsave(filename = paste(output.path, "pairs.pdf", sep = "/"), plot = g.pairs, width = 8, height = 6)

