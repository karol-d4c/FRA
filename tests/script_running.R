# data = data.nfkb
# signal = "signal"
#
# response <- colnames(data.nfkb)[-c(1,2)]
# parallel_cores <- 8
# bootstrap <- TRUE
# bootstrap.number = 4
#
# model <-
#   ITRC(data = data.nfkb,
#             signal = "signal",
#             sample = "sample",
#             response = response,
#             parallel_cores = parallel_cores,
#             bootstrap.number = bootstrap.number,
#             bootstrap = bootstrap)
#
#
#
# model$bootstrap.samples.df
#
#
# g <- plotITRCWaves(model = model,
#                    rescale.fun = "log"
#                    # rescale.fun = function(x){log(x = x, base = 10)}
#                    # rescale.fun = function(x){x}
#                    )
# g
