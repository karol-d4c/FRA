#### log-normal distribution - increading mean -> increasing sd ####
sample_ModelManuscript1D <-
  function(
    output.path = "resources/output/model/manuscript/",
    time = 1,
    n = 1000,
    #stim.list = c(seq(from = 0, length.out = 100, to = 1), seq(from = 1, length.out = 100, to = 200))[c(1,2,10,50,101,102,103,200)],
    stim.list = c(0,0.01, 0.1, 0.25, 0.5, 1, 2 ,3, 5, 100),
    #stim.list = c(0,100),
    alpha = 7.5,
    beta = 0.3,
    gamma = -0.75,
    delta = 5,
    #twosignals close
    fun.distr.params =
      function(
        x,
        alpha,
        beta,
        gamma,
        delta,
        ...){
        mean.signal <- alpha*x/(1+x) + delta
        sd.signal <- beta*(mean.signal) + gamma
        #mean.signal <- 2*mean.signal
        return(
          list( mean.signal = mean.signal,
                sd.signal = sd.signal))
      },
    ...)
  {
    fun.distr.params_ <- function(x){fun.distr.params(x = x,
                                                      alpha = alpha,
                                                      beta = beta,
                                                      gamma = gamma,
                                                      delta = delta)}
    params.list <- fun.distr.params_(x = stim.list)
    data.raw.list <- foreach::foreach(signal = 1:length(stim.list)) %do% {
      data.frame(intensity = rnorm(n = n,
                                   mean = params.list$mean.signal[signal],
                                   sd =  params.list$sd.signal[signal]),
                 Stim = stim.list[signal],
                 Time = time,
                 CellID2  = paste(stim.list[signal], 1:n, sep = "_"))
    }
    data.raw <- do.call(what = rbind,
                        args = data.raw.list) %>%
      data.table()

    stim.list <- (data.raw %>% dplyr::distinct(Stim))[["Stim"]]
    signal.list <- paste("X", stim.list, sep = "")
    data.raw$signal <- paste("X", data.raw$Stim, sep = "")
    data.raw$signal <- factor(
      x = data.raw$signal,
      levels = signal.list)

    dir.create(output.path, recursive = TRUE)
    saveRDS(object =
              list(
                fun.distr.params = fun.distr.params_,
                alpha = alpha,
                beta = beta,
                gamma = gamma,
                data.raw = data.raw,
                n = n,
                stim.list = stim.list),
            file = paste(output.path, "data.rds", sep = "/"))
    return(data.raw)
  }
