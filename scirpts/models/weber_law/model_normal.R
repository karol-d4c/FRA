#### libraries ####
library(foreach)
library(ITRC)
library(dplyr)
library(data.table)
library(ggplot2)
#### computation parameters ####
output.path <- "resources/model/weber_law/normal/"
parallel_cores = 8
bootstrap = TRUE
bootstrap.number = 8
bootstrap.sample_size = 1000
bootstrap.test.sample <- 4
path.model <- paste(output.path, "model.rds", sep = "/")

#### normal distribution ####
# n <- 1000
# m <- 20
#
# #### normal distribution -- Y ~ log(c) ####
# norm_mu <- 10
# norm_mu_delta <- 1
# norm_sigma <- 1
# norm_mu.list <-
#   seq(from = norm_mu,
#       by = norm_mu_delta,
#       length.out = m)
# foreach(i = 1:m) %do% {
#   data.frame(
#     response =
#         rnorm(n = n,
#               mean = norm_mu.list[i],
#               sd = norm_sigma),
#     signal = ifelse(i == 1, 0, exp(i - 1)))
# } %>%
#   do.call(what = rbind,
#           args = .) ->
#   data.norm
# data.norm$sample <- 1:nrow(data.norm)
# ggplot(data.norm, aes(x = response, group = signal)) + geom_density()
#
# model.norm <-
#   ITRC::ITRC(
#     data = data.norm,
#     signal = "signal",
#     response = "response",
#     sample = "sample",
#     parallel_cores = parallel_cores,
#     bootstrap.number = bootstrap.number,
#     bootstrap = bootstrap,
#     bootstrap.sample_size = bootstrap.sample_size,
#     bootstrap.test.sample = bootstrap.test.sample
#   )
#
#
# ITRC::plotITRCWaves(
#   model = model.norm,
#   rescale.fun = function(x){log2(x)})
#
#
# #### lognormal distribution ####
#
#
# n <- 1000
# m <- 100
# lognorm_m <- 10
# lognorm_m_delta <- 1
# lognorm_v_sqrt <- 5
# lognorm_m.list <-
#   seq(from = lognorm_m,
#       by = lognorm_m_delta,
#       length.out = m)
# foreach(i = 1:m) %do% {
#   data.frame(
#     response =
#       exp(
#         rnorm(n = n,
#           mean = mu.fun(m = lognorm_m.list[i], v = lognorm_v_sqrt^2),
#           sd = sigma.fun(m = lognorm_m.list[i], v = lognorm_v_sqrt^2))),
#     signal = i-1)#ifelse(i == 1, 0, 2^(i - 1)))
# } %>%
#   do.call(what = rbind,
#           args = .) ->
#   data.lognorm
# ggplot(data.lognorm, aes(x = response, group = signal)) + geom_density()
# model.lognorm <-
#   ITRC(
#     data = data.lognorm,
#     signal = "signal",
#     response = "response",
#     parallel_cores = parallel_cores,
#     bootstrap.number = bootstrap.number,
#     bootstrap = bootstrap,
#     bootstrap.sample_size = bootstrap.sample_size,
#     bootstrap.test.sample = bootstrap.test.sample
#   )
#
#
# ITRC::plotITRCWaves(
#   model = model.lognorm,
#   rescale.fun = function(x){log2(x)})
#
#
# n <- 1000
# m <- 10
# norm_mu <- 10
# norm_mu_delta <- 1
# norm_sigma <- 1
# norm_mu.list <-
#   seq(from = norm_mu,
#       by = norm_mu_delta,
#       length.out = m)
# foreach(i = 1:m) %do% {
#   data.frame(
#     response =
#       rnorm(n = n,
#             mean = norm_mu.list[i],
#             sd = norm_sigma),
#     signal = ifelse(i == 1, 0, 2^(i - 1)))
# } %>%
#   do.call(what = rbind,
#           args = .) ->
#   data
#
# model <-
#   ITRC(
#     data = data,
#     signal = "signal",
#     response = "response",
#     parallel_cores = parallel_cores,
#     bootstrap.number = bootstrap.number,
#     bootstrap = bootstrap,
#     bootstrap.sample_size = bootstrap.sample_size,
#     bootstrap.test.sample = bootstrap.test.sample
#   )
#
#
# ITRC::plotITRCWaves(
#   model = model,
#   rescale.fun = function(x){log2(x)})
# #### ###
#### ####
a <- 20
k <- 5
m <- 20
x <- round(seq(from = 0, to = 2, length.out = m + 1), digits = 2)


y.fun.mm <-
  function(
    x,
    a = 20,
    l = 5
  ){
    a*(x^k/(1+x^k))
  }
y.fun.linear <-
  function(
    x,
    a = 20
  ){
    a*x
  }

y.fun.log <-
  function(
    x,
    a = 20){
    a*log(x)
  }

sample.norm.fun <-
  function(
    x,
    y,
    sd,
    n,
    m
  ){
    foreach(i = 1:m) %do% {
      data.frame(
        response =
          rnorm(n = n,
                mean = y[i],
                sd = sd),
        signal = x[i])
    } %>%
      do.call(what = rbind,
              args = .)  %>%
      return()
  }

sample.lognorm.fun <-
  function(
    x,
    y,
    v_sqrt,
    n,
    m,
    epsilon = 1
  ){
    mu.fun <-
      function(
        m,
        v){
        log(m / sqrt(1 + v/(m^2)))
      }
    sigma.fun <-
      function(
        m,
        v){
        sqrt(log(1+ v/(m^2)))
      }

    foreach(i = 1:m) %do% {
      data.frame(
        response =
          exp(
            rnorm(n = n,
                  mean =
                    mu.fun(m = y[i] + epsilon,
                           v = v_sqrt^2),
                  sd =
                    sigma.fun(m = y[i] + epsilon,
                              v = v_sqrt^2))),
        signal = x[i])
    } %>%
      do.call(what = rbind,
              args = .) %>%
      return()
  }


#### linear norm ####
sigma <- 5
m <- 40
n <- 1000
x <- round(seq(from = 0, to = 2, length.out = m + 1), digits = 2)
data <-
  sample.norm.fun(
    x = x,
    #y = y.fun.linear(x = x + 10),
    #y = y.fun.log(x = x + 10),
    y = y.fun.mm(x = x),
    sd = sigma,
    n = n,
    m = m
  )
#data$sample <- 1:nrow(data)
bootstrap.number <- 8
ggplot(data, aes(x = response, group = signal)) + geom_density()#  + coord_cartesian(xlim = c(0,20))
model <-
  ITRC::ITRC(
    data = data,
    signal = "signal",
    response = "response",
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap.sample_size = bootstrap.sample_size,
    bootstrap.test.sample = bootstrap.test.sample
  )

data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(signal) %>%
  dplyr::summarise(var = var(response),
                mean = mean(response))

ITRC::plotITRCWaves(
  model = model,
  rescale.fun = function(x){log2(x)})

#model.list <- list()
#model.list[["logC-norm"]] <- model
#model.list[["linC-norm"]] <- model
#model.list[["MMC-norm"]] <- model


# dir.create(output.path, recursive = TRUE)
# saveRDS(object = model.list, file = paste(output.path, "model_list.rds", sep = '/'))


#### log norm ####
output.path <- "resources/model/weber_law/lognormal/"
sigma <- 5
m <- 80
n <- 1000
x <- round(seq(from = 0, to = 50, length.out = m + 1), digits = 2)
data <-
  sample.lognorm.fun(
    x = x,
    #y = y.fun.linear(x = x + 1, a = 1),
    y = y.fun.log(x = x + 1, a = 1),
    #y = y.fun.mm(x = x),
    v_sqrt = sigma,
    n = n,
    m = m
  )
#data$sample <- 1:nrow(data)



(data %>%
  dplyr::ungroup() %>%
  dplyr::group_by(signal) %>%
  dplyr::summarise(var = var(response),
                   mean = mean(response)))[["var"]]


bootstrap.number <- 2
bootstrap.test.sample <- NULL
ggplot(data, aes(x = response, group = signal)) + geom_density()  + coord_cartesian(xlim = c(0,20))
model <-
  ITRC::ITRC(
    data = data,
    signal = "signal",
    response = "response",
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap.sample_size = bootstrap.sample_size,
    bootstrap.test.sample = bootstrap.test.sample
  )

ITRC::plotITRCWaves(
  model = model,
  rescale.fun = function(x){log2(x)})

#model.list.log <- list()
#model.list.log[["logC-norm"]] <- model
#model.list.log[["linC-norm"]] <- model
#model.list.log[["MMC-norm"]] <- model


# dir.create(output.path, recursive = TRUE)
# saveRDS(object = model.list, file = paste(output.path, "model_list.rds", sep = '/'))
