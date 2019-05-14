#### libraries ####
library(foreach)
library(ITRC)
library(dplyr)
library(data.table)
#### computation parameters ####
output.path <- "resources/model/weber_law/normal/"
parallel_cores = 2
bootstrap = TRUE
bootstrap.number = 2
bootstrap.sample_size = 1000
bootstrap.test.sample <- 4
path.model <- paste(output.path, "model.rds", sep = "/")

#### normal distribution ####

n <- 1000
m <- 10
norm_mu <- 10
norm_mu_delta <- 1
norm_sigma <- 1
norm_mu.list <-
  seq(from = norm_mu,
      by = norm_mu_delta,
      length.out = m)
foreach(i = 1:m) %do% {
  data.frame(
    response =
        rnorm(n = n,
              mean = norm_mu,
              sd = norm_sigma),
    signal = ifelse(i == 1, 0, 2^(i - 1)))
} %>%
  do.call(what = rbind,
          args = .) ->
  data.norm

model.norm <-
  ITRC(
    data = data.norm,
    signal = "signal",
    response = "response",
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap = bootstrap,
    bootstrap.sample_size = bootstrap.sample_size,
    bootstrap.test.sample = bootstrap.test.sample
  )


ITRC::plotITRCWaves(
  model = model.lognorm,
  rescale.fun = function(x){log2(x)})


n <- 10000
m <- 10
norm_mu <- 10
norm_mu_delta <- 1
norm_sigma <- 2.5
norm_mu.list <-
  seq(from = norm_mu,
      by = norm_mu_delta,
      length.out = m)
foreach(i = 1:m) %do% {
  data.frame(
    response =
      rnorm(n = n,
            mean = norm_mu.list[i],
            sd = norm_sigma),
    signal = ifelse(i == 1, 0, 2^(i - 1)))
} %>%
  do.call(what = rbind,
          args = .) ->
  data.norm
ggplot(data.norm, aes(x = response, group = signal)) + geom_density()
model.norm <-
  ITRC(
    data = data.norm,
    signal = "signal",
    response = "response",
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap = bootstrap,
    bootstrap.sample_size = bootstrap.sample_size,
    bootstrap.test.sample = bootstrap.test.sample
  )


ITRC::plotITRCWaves(
  model = model.norm,
  rescale.fun = function(x){log2(x)})


#### lognormal distribution ####

mu.fun <-
  function(m,v){
    log(m / sqrt(1 + v/(m^2)))
  }

sigma.fun <-
  function(m,v){
    sqrt(log(1+ v/(m^2)))
  }

n <- 1000
m <- 100
lognorm_m <- 10
lognorm_m_delta <- 1
lognorm_v_sqrt <- 5
lognorm_m.list <-
  seq(from = lognorm_m,
      by = lognorm_m_delta,
      length.out = m)
foreach(i = 1:m) %do% {
  data.frame(
    response =
      exp(
        rnorm(n = n,
          mean = mu.fun(m = lognorm_m.list[i], v = lognorm_v_sqrt^2),
          sd = sigma.fun(m = lognorm_m.list[i], v = lognorm_v_sqrt^2))),
    signal = i-1)#ifelse(i == 1, 0, 2^(i - 1)))
} %>%
  do.call(what = rbind,
          args = .) ->
  data.lognorm
ggplot(data.lognorm, aes(x = response, group = signal)) + geom_density()
model.lognorm <-
  ITRC(
    data = data.lognorm,
    signal = "signal",
    response = "response",
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap = bootstrap,
    bootstrap.sample_size = bootstrap.sample_size,
    bootstrap.test.sample = bootstrap.test.sample
  )


ITRC::plotITRCWaves(
  model = model.lognorm,
  rescale.fun = function(x){log2(x)})
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
