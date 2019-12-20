### ###
### script continuous
### ###

#### ####
library(ITRC)
library(doParallel)
library(tidyverse)
library(data.table)

source("scirpts/models_bimodal_continuous//model_ContinuousLogNorm.R")


force.recalulation <-
  FALSE
model.name <- "continuous"
#output.path <- "resources/output/model_bimodal_continuous/1d/2019-10-30-bimodal_logmvn_bootstrap//"
output.path <- "resources/output/model_bimodal_continuous/1d/2019-12-17"
output.cont.path <- paste("resources/output/model_bimodal_continuous/1d/2019-12-17", model.name, sep = "/")
dir.create(output.bimodal.path, recursive = TRUE)

# fun.distr.params =
#   function(
#     n.modes = 2,
#     ...){
#     mean.signal <- seq(from = 20, to = 100, length.out =  n.modes)#/2
#     sd.signal   <-  c(5,5)
#     #mean.signal <- 2*mean.signal
#     return(
#       list( mean.signal = mean.signal,
#             sd.signal = sd.signal))
#   }
stims.num = 11
gray.start <- 0
gray.end <- 0.5
n = 10000
data.raw.cont <-sample_ModelContinuousLogNorm(output.path = output.path, n = n, stims.num = stims.num)#, fun.distr.params = fun.distr.params, stim.list = 0:4)
data.raw.cont %>% dplyr::mutate(Stim = Stim - 1) -> data.raw.cont

g.data.cont <-
  ggplot(data = data.raw.cont,
         mapping = aes(x = intensity, group = signal, color = signal)) +
  geom_density(alpha = 0.5) +
  SysBioSigTheme::theme_sysbiosig() +
  scale_color_manual(values = rev(gray.colors(n = stims.num, start = gray.start, end = gray.end)),
                     guide = FALSE) +
  xlab("response intensity") +
  ylab("frequency") +
  ggtitle("continuous model")
####
signal <- "Stim"
sample <- "CellID2"
response <- "intensity"
parallel_cores = 8
bootstrap = TRUE
bootstrap.number = 16
bootstrap.sample_size = 1000
path.model <- paste(output.cont.path, "model_pair.rds", sep = "/")

model.pair.cont <-
  ITRC::ITRC(
    data = data.raw.cont %>% dplyr::filter(Stim == max(Stim)|Stim == min(Stim)),
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap = bootstrap,
    bootstrap.sample_size = bootstrap.sample_size
  )

saveRDS(object = model.pair.cont, file = path.model)
#### slemi pair ####
slemi.model.pair.cont <-
  CapacityLogReg::capacity_logreg_main(
    dataRaw = data.raw.cont %>% dplyr::filter(Stim == max(Stim)|Stim == min(Stim)),
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
data.table(itrc = 2^slemi.model.pair.cont$cc,
           Stim = (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!quo.max))[[signal]][[1]]) ->
  slemi.model.pair.cont.df
#### ####
signal <- "Stim"
sample <- "CellID2"
response <- "intensity"
parallel_cores = 8
bootstrap = TRUE
bootstrap.number = 16
bootstrap.sample_size = 1000
path.model <- paste(output.cont.path, "model.rds", sep = "/")

model.cont <-
  ITRC::ITRC(
    data = data.raw.cont,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap = bootstrap,
    bootstrap.sample_size = bootstrap.sample_size
  )

saveRDS(object = model.cont, file = path.model)


#### slemi pair ####
slemi.model.cont <-
  CapacityLogReg::capacity_logreg_main(
    dataRaw = data.raw.cont,
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
data.table(itrc = 2^slemi.model.cont$cc,
           Stim = (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!quo.max))[[signal]][[1]]) ->
  slemi.model.cont.df
#### ###
g.waves <-
  ITRC::plotITRCWaves(
    model = model.cont)
