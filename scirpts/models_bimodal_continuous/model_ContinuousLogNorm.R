

### MVN continuous model sampling
### ###
#### sample_ModelLogNorm ###
sample_ModelLogNorm <-
  function(
    output.path,
    mean.signal,
    sd.signal,
    n = 10000,
    n.train = 1000,
    n.test = 1000,
    time = 1,
    ...
  )
  {
    data.raw.list <- foreach::foreach(signal = 1:length(mean.signal)) %do% {
      data.frame(intensity = rnorm(n = n, mean = mean.signal[signal], sd =  sd.signal[signal]),
                 Stim = signal,
                 Time = time,
                 CellID2  = paste(signal, 1:n, sep = "_"))
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

    density.estimation.limits <-
      NULL #DensityEstimationLimits(data.long = data.raw)

    dir.create(output.path, recursive = TRUE)
    saveRDS(object =
              list(
                data.raw = data.raw,
                n = n,
                stim.list = stim.list,
                density.estimation.limits = density.estimation.limits,
                n.train = n.train,
                n.test = n.test),
            file = paste(output.path, "data.rds", sep = "/"))
    return(data.raw)
  }
#### log-normal distribution - increading mean -> increasing sd ####
sample_ModelContinuousLogNorm <-
  function(
    output.path = "resources/output/model/1d/continuous_logmvn/",
    by_ = 1.25,
    stims.num = 65,
    #twosignals close
    fun.distr.params =
      function(
        n.modes = 2,
        ...){
        mean.signal <- seq(from = 20, to = 40, length.out =  stims.num)#/2
        sd.signal <- (mean.signal - min(mean.signal))*0.45 + 5

        # mean.signal <- seq(from = 20, to = 60, length.out =  stims.num)#/2
        # sd.signal <- mean.signal*0 + 5

        #mean.signal <- 2*mean.signal
        return(
          list( mean.signal = mean.signal,
                sd.signal = sd.signal))
      },
    # #twosignals imposed
    # fun.distr.params =
    #   function(
    #     n.modes = 2,
    #     ...){
    #     mean.signal <- seq(from = 20, to = 100, length.out =  stims.num)#/2
    #     sd.signal <- (mean.signal - min(mean.signal))*0.45 + 5
    #     #mean.signal <- 2*mean.signal
    #     return(
    #       list( mean.signal = mean.signal,
    #             sd.signal = sd.signal))
    #   },
    ## two signals distinct
    # fun.distr.params =
    # function(
    #   by_,
    #   stims.num,
    #   ...){
    #   mean.signal <- seq(from = 20, to = 100, length.out =  stims.num)/2
    #   sd.signal <- (mean.signal - min(mean.signal))*0.45 + 5
    #   mean.signal <- 2*mean.signal
    #   return(
    #     list( mean.signal = mean.signal,
    #           sd.signal = sd.signal))
    # },
    # # distinct
    # function(
    #   by_,
    #   stims.num,
    #   ...){
    #   mean.signal <- seq(from = 20, to = 100, length.out = stims.num)/2
    #   sd.signal <- (mean.signal - min(mean.signal))*0.1 + 2.5
    #   mean.signal <- 2*mean.signal
    #   return(
    #     list( mean.signal = mean.signal,
    #           sd.signal = sd.signal))
    # },
    ...)
  {
    distr.params <-
      fun.distr.params(
        by_ = by_,
        stims.num = stims.num,
        ...
      )
    data.raw <- sample_ModelLogNorm(
      output.path = output.path,
      mean.signal = distr.params$mean.signal,
      sd.signal = distr.params$sd.signal,
      ...
    )
    saveRDS(object = list(fun.distr.params = fun.distr.params, by_ = by_),
            file =  paste(output.path, "model_specification.rds", sep = "/") )
    return(data.raw)
  }


#### generate ####
#
# data.raw <- res$data.raw
# stim.list <- res$stim.list
# n <- res$n
# test.limits <-
#   DefaultTestLimits(data.long = data.raw)
# graphics.args <- GetGraphicsArgs()
#
# output.time.path <- paste(output.path, by_, sep = "/")
# res <- readRDS(paste(output.path, "data.rds", sep = "/"))
# data.raw <- res$data.raw
# stim.list <- res$stim.list
# n <- res$n
# test.limits <-
#   DefaultTestLimits(data.long = data.raw)
# graphics.args <- GetGraphicsArgs()
# data.raw <- sample_ModelContinuousLogNorm()
#
#
# ggplot(data = data.raw %>% dplyr::filter(Stim %in% c(1,65))) +
#   geom_density(mapping = aes(x = intensity, color = factor(Stim), group = factor(Stim))) +
#   xlim(c(test.limits$min, test.limits$max))
#
#
# g.data <-  ggplot(data = data.raw) +
#   geom_density(mapping = aes(x = intensity, color = factor(Stim), group = factor(Stim))) +
#   xlim(c(test.limits$min, test.limits$max)) +
#   do.call(what = graphics.args$theme.fun, args = graphics.args$theme.args)
#
# do.call(what = ggsave,
#         args = append(graphics.args$ggsave.args,
#                       list(
#                         filename = paste(output.path, "data.pdf", sep = "/"),
#                         plot = g.data)
#         ))
#
# g.density.list <- foreach::foreach(stim_ = stim.list) %do% {
#   tryCatch({
#     ggplot(data = data.raw %>% dplyr::filter(Stim <= stim_)) +
#       geom_density(mapping = aes(x = intensity, color = factor(Stim), group = factor(Stim))) +
#       xlim(c(test.limits$min, test.limits$max)) +
#       ggtitle(stim_) +
#       do.call(what = graphics.args$theme.fun, args = graphics.args$theme.args)
#   }, error = function(e){print(e)})
# }
#
# g.density <- marrangeGrob(g.density.list, ncol = 1, nrow = 1)
# do.call(what = ggsave,
#         args = append(graphics.args$ggsave.args,
#                       list(
#                         filename = paste(output.path, "data_density.pdf", sep = "/"),
#                         plot = g.density)
#                       ))
#
#
#
# #### normal pairwise error ####
# # output.path <- "resources/output/normal_1d/time_point/pairwise/mean_changes_thicker/"
# # by_ <- 0.1
# # mean.signal <- seq(from = 0, to = 20, by = by_)
# # sd.signal <- rep(x = 2, times = length(mean.signal))
# #
# # #### three_distr ####
# #
# # output.path <- "resources/output/normal_1d/three_distributions_continous_increasing/"
# # by_ <- 1.25
# # mean.signal <- seq(from = 20, to = 100, by = by_)
# # sd.signal <- (mean.signal - 20)*0.5 + 5
# # n <- 10000
# # n.train <- 1000
# # n.test <- 1000
# #
# # data.raw.list <- foreach::foreach(signal = 1:length(mean.signal)) %do% {
# #   data.frame(intensity = rnorm(n = n, mean = mean.signal[signal], sd =  sd.signal[signal]),
# #              Stim = signal,
# #              Time = by_,
# #              CellID2  = paste(signal, 1:n, sep = "_"))
# # }
# # data.raw <- do.call(what = rbind,
# #                     args = data.raw.list) %>%
# #   data.table()
# #
# # stim.list <- (data.raw %>% dplyr::distinct(Stim))[["Stim"]]
# # test.limits <-
# #   DefaultTestLimits(data.long = data.raw)
# #
# # dir.create(output.path, recursive = TRUE)
# # saveRDS(object = list(data.raw = data.raw, n = n, stim.list = stim.list, test.limits = test.limits, n.train = n.train, n.test = n.test),
# #         file = paste(output.path, "data.rds", sep = "/"))
# #
# # g <- ggplot(data = data.raw) +
# #   geom_density(mapping = aes(x = intensity, color = factor(Stim), group = factor(Stim))) +
# #   xlim(c(test.limits$min, test.limits$max))
# #
# # ggsave(filename = paste(output.path, "data.pdf", sep = "/"), plot = g, useDingbats = FALSE)
# #
# # #### ####
# # res <- readRDS(paste(output.path, "data.rds", sep = "/"))
# # ggsave(filename = paste(output.path, "data.pdf", sep = "/"), plot = g, width = 12, height = 8, useDingbats = FALSE)
#
# ####  read data  ####
# # output.time.path <- paste(output.path, by_, sep = "/")
# # res <- readRDS(paste(output.time.path, "data.rds", sep = "/"))
# # data.raw <- res$data.raw
# # stim.list <- res$stim.list
# # n <- res$n
# # test.limits <-
# #   DefaultTestLimits(data.long = data.raw)
#
# #### ####
# # g <- ggplot(data = data.raw %>% dplyr::filter(Stim < 27)) +
# #   geom_boxplot(mapping = aes(x = factor(Stim), y = intensity, group = Stim, fill = factor(Stim)))
# # ggsave(filename = paste(output.path, "data_27.pdf", sep = "/"), plot = g, width = 12, height = 8, useDingbats = FALSE)
# #### ####
# output.time.path <- paste(output.path, "18", sep = "/")
# df <- readDataFrames(
#   path = output.time.path,
#   sub.path.list = stim.list,
#   pattern = "channel_capacity.csv",
#   add.sub = TRUE)
# df$states <- 2^df$cc
# df$stim <- df$sub
# #### response curve ####
# g.list<- list()
# g.list[["log"]] <- ggplot(data = df %>%
#                             dplyr::group_by(stim, type) %>%
#                             dplyr::summarise(cc.mean = mean(cc), cc.sd = sd(cc))) +
#   geom_errorbar(mapping =
#                   aes(
#                     x = log10(stim),
#                     ymin = cc.mean - cc.sd,
#                     ymax = cc.mean + cc.sd,
#                     group = interaction(stim,type),
#                     color = type
#                   )) +
#   geom_line(mapping = aes(x = log10(stim),
#                           y = cc.mean,
#                           group = interaction(stim,type),
#                           color = type)) +
#   ylim(c(0,0.5))
#
# g.list[["linear"]] <- ggplot(data = df %>%
#                                dplyr::group_by(stim, type) %>%
#                                dplyr::summarise(cc.mean = mean(cc), cc.sd = sd(cc))) +
#   geom_errorbar(mapping =
#                   aes(
#                     x = stim,
#                     ymin = cc.mean - cc.sd,
#                     ymax = cc.mean + cc.sd,
#                     group = interaction(stim,type),
#                     color = type
#                   )) +
#   geom_line(mapping = aes(x = stim,
#                           y = cc.mean,
#                           group = interaction(stim,type),
#                           color = type)) +
#   ylim(c(0,0.5))
#
# g.list[["factor"]] <- ggplot(data = df %>%
#                                dplyr::group_by(stim, type) %>%
#                                dplyr::summarise(cc.mean = mean(cc), cc.sd = sd(cc))) +
#   geom_errorbar(mapping =
#                   aes(
#                     x = factor(stim),
#                     ymin = cc.mean - cc.sd,
#                     ymax = cc.mean + cc.sd,
#                     group = interaction(stim,type),
#                     color = type
#                   )) +
#   geom_line(mapping = aes(x = factor(stim),
#                           y = cc.mean,
#                           group = interaction(stim,type),
#                           color = type)) +
#   ylim(c(0,0.5))
# g <- marrangeGrob(g.list, ncol = 1, nrow = 1)
# ggsave(filename = paste(output.path, "response_curve.pdf", sep = "/"), plot = g, width = 12, height = 8, useDingbats = FALSE)
#
#
# #### response curve states ####
# g.list<- list()
# g.list[["log"]] <- ggplot(data = df %>%
#                             dplyr::group_by(stim, type) %>%
#                             dplyr::summarise(states.mean = mean(states), states.sd = sd(states))) +
#   # geom_errorbar(mapping =
#   #                 aes(
#   #                   x = log10(stim),
#   #                   ymin = cc.mean - cc.sd,
#   #                   ymax = cc.mean + cc.sd,
#   #                   group = interaction(stim,type),
#   #                   color = type
#   #                 )) +
#   geom_line(mapping = aes(x = log10(stim),
#                           y = states.mean,
#                           # group = interaction(stim,type),
#                           color = type)) +
#   geom_point(mapping = aes(x = log10(stim),
#                            y = states.mean,
#                            # group = interaction(stim,type),
#                            color = type)) +
#   ylim(c(1,4))
#
# g.list[["linear"]] <-ggplot(data = df %>%
#                               dplyr::group_by(stim, type) %>%
#                               dplyr::summarise(states.mean = mean(states), states.sd = sd(states))) +
#   # geom_errorbar(mapping =
#   #                 aes(
#   #                   x = log10(stim),
#   #                   ymin = cc.mean - cc.sd,
#   #                   ymax = cc.mean + cc.sd,
#   #                   group = interaction(stim,type),
#   #                   color = type
#   #                 )) +
#   geom_line(mapping = aes(x = stim,
#                           y = states.mean,
#                           #group = interaction(stim,type),
#                           color = type)) +
#   geom_point(mapping = aes(x = stim,
#                            y = states.mean,
#                            # group = interaction(stim,type),
#                            color = type)) +
#   ylim(c(1,4))
#
# g.list[["factor"]] <- ggplot(data = df %>%
#                                dplyr::group_by(stim, type) %>%
#                                dplyr::summarise(states.mean = mean(states), states.sd = sd(states))) +
#   # geom_errorbar(mapping =
#   #                 aes(
#   #                   x = log10(stim),
#   #                   ymin = cc.mean - cc.sd,
#   #                   ymax = cc.mean + cc.sd,
#   #                   group = interaction(stim,type),
#   #                   color = type
#   #                 )) +
#   geom_line(mapping = aes(x = factor(stim),
#                           y = states.mean,
#                           #group = interaction(stim,type),
#                           color = type)) +
#   geom_point(mapping = aes(x = factor(stim),
#                            y = states.mean,
#                            # group = interaction(stim,type),
#                            color = type)) +
#   ylim(c(1,4))
# g <- marrangeGrob(g.list, ncol = 1, nrow = 1)
# ggsave(filename = paste(output.time.path, "response_curve_states.pdf", sep = "/"), plot = g, width = 12, height = 8, useDingbats = FALSE)
# #### optimal distribution ####
# g.list <- foreach::foreach(stim_ = 10) %do% {
#   tryCatch({
#
#     output.comp <- paste(output.time.path, stim_, "bootstrap", sep = "/")
#
#     df <- readDataFrames(
#       path = output.comp,
#       sub.path.list = stim.list,
#       pattern = "optimal_distribution.csv",
#       add.sub = TRUE)
#     if(!is.null(df)){
#       df.group <- df %>% dplyr::group_by(X, type, stim) %>% dplyr::summarise(P_X = mean(P_X))
#       ggplot(df.group) +
#         geom_bar(mapping = aes(x = factor(stim), fill = type, y = P_X), position = "dodge", stat = "identity")
#     } else {
#       return()
#     }
#   }, error = function(e){print(e)})
# }
#
# g <- marrangeGrob(grobs = g.list, ncol = 1, nrow = 1)
# ggsave(filename = paste(output.time.path, "optimal_distribution.pdf", sep = "/"), plot = g, width = 12, height = 8, useDingbats = FALSE)
