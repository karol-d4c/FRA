### ###
### script bimodal
### ###

#### ####
library(ITRC)
library(doParallel)
library(tidyverse)
library(CapacityLogReg)
library(data.table)
source("scirpts/models_bimodal_continuous//model_BimodalLogNorm.R")


### ###
### Continuous Log MVN model script
### ###
force.recalulation <- FALSE
model.name <- "bimodal"
output.path <- "resources/output/model_bimodal_continuous/1d/2019-10-30-bimodal_logmvn_bootstrap//"
output.path <- "resources/output/model_bimodal_continuous/1d/2019-12-17"
output.bimodal.path <- paste("resources/output/model_bimodal_continuous/1d/2019-12-17", model.name, sep = "/")
dir.create(output.bimodal.path, recursive = TRUE)
bootstrap.num = 10000
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
n = 10000
stims.num = 11
gray.start <- 0
gray.end <- 0.5
data.raw.bimodal <-sample_ModelBimodalLogNorm(output.path = output.path, n = n, stims.num = stims.num)#, fun.distr.params = fun.distr.params, stim.list = 0:4)
data.raw.bimodal %>% dplyr::mutate(Stim = Stim - 1) -> data.raw.bimodal
data.raw.bimodal %>% data.table::data.table() -> data.raw.bimodal

g.data.bimodal <-
  ggplot(data = data.raw.bimodal,
         mapping = aes(x = intensity, group = signal, color = signal)) +
  geom_density(alpha = 0.5) +
  SysBioSigTheme::theme_sysbiosig() +
  scale_color_manual(values = rev(gray.colors(n = stims.num, start = gray.start, end = gray.end)),
                     guide = FALSE) +
  xlab("response intensity") +
  ylab("frequency") +
  ggtitle("bimodal model")


signal <- "Stim"
sample <- "CellID2"
response <- "intensity"
parallel_cores = 8
bootstrap = TRUE
bootstrap.number = 16
bootstrap.sample_size = 10000
path.model <- paste(output.bimodal.path, "model_pair.rds", sep = "/")

model.pair.bimodal <-
  ITRC::ITRC(
    data = data.raw.bimodal%>% dplyr::filter(Stim == max(Stim)|Stim == min(Stim)),
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap = bootstrap,
    bootstrap.sample_size = bootstrap.sample_size
  )

saveRDS(object = model.pair.bimodal, file = path.model)
#### sle mi pair ####
slemi.model.pair.bimodal <-
  CapacityLogReg::capacity_logreg_main(
    dataRaw = data.raw.bimodal%>% dplyr::filter(Stim == max(Stim)|Stim == min(Stim)),
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
data.table(itrc = 2^slemi.model.pair.bimodal$cc,
           Stim = (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!quo.max))[[signal]][[1]]) ->
  slemi.model.pair.bimodal

#### ####


signal <- "Stim"
sample <- "CellID2"
response <- "intensity"
parallel_cores = 8
bootstrap = TRUE
bootstrap.number = 32
bootstrap.sample_size = 10000
path.model <- paste(output.bimodal.path, "model.rds", sep = "/")

model.bimodal <-
  ITRC::ITRC(
    data = data.raw.bimodal,
    signal = signal,
    sample = sample,
    response = response,
    parallel_cores = parallel_cores,
    bootstrap.number = bootstrap.number,
    bootstrap = bootstrap,
    bootstrap.sample_size = bootstrap.sample_size
  )

saveRDS(object = model.bimodal, file = path.model)

#### slemi all ####
slemi.model.bimodal <-
  CapacityLogReg::capacity_logreg_main(
    dataRaw = data.raw.bimodal,
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
data.table(itrc = 2^slemi.model.bimodal$cc,
           Stim = (data %>% dplyr::distinct(!!sym(signal)) %>%  dplyr::filter(!!quo.max))[[signal]][[1]]) ->
  slemi.model.bimodal.df


#### ####
g.waves <-
  ITRC::plotITRCWaves(
    model = model.bimodal)

g.waves <-
  ITRC::plotITRCWaves(
    model = model.cont)

g <- ITRC::plotCofusionMatrix(
  model = model.bimodal)

ggplot2::ggsave(
  filename = paste(output.path, "confusion_matrix_overlapping.pdf", sep = "/"),
  plot = g,
  width = 6,
  height = 6,
  useDingbats = FALSE
)

