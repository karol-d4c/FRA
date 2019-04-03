#### cytof ####
parallel_cores <- 4
bootstrap <- TRUE
bootstrap.number = 4
version <- "2018-09-10-RR18_011"
version.analysis <- paste(version, "2018-10-24", sep = "_")
folder.path <- "/media/knt/1624BB0024BAE1C31/KN/cytof/resources/"
input.path <- paste(folder.path, "input", version, "/",sep = "/")
data.path <- paste(input.path, "data_norm/", "/", sep = "")



ifns.list <- list.dirs(data.path, full.names = FALSE, recursive = FALSE)

ifn_ <- ifns.list[1]

data.list <-
  readRDS(
    file =
      paste(
        data.path,
        ifn_,
        "data_cells_list.rds", sep = "/"))
cells.list <- rev(names(data.list)[-c(1)])

data.list$nk_cells %>% data.table::data.table()

cell_ <- "b_cells"
data.value.colnames <- sort(colnames(data.list[[cell_]])[which(stringr::str_detect(colnames(data.list[[cell_]]), "pSTAT"))])
data.var.colnames <- c(
  #"CellID2",
  "Stim")

data.wide <- data.list[[cell_]] %>%
  # dplyr::mutate(CellID2 =
  #                 paste(ifn_,
  #                       Stim, sep = "_")) %>%
  dplyr::mutate(Event_Time = Time) %>%
  dplyr::select(-Time) %>%
  dplyr::select_(paste("c(",
                       paste(
                         c(data.var.colnames,
                           data.value.colnames), collapse = ","), ")")) %>%
  data.table::data.table()
data.wide$CellID2 <-  paste(data.wide$CellID2,
                            1:nrow(data.wide),
                            sep = "_")


model.cytof <-
  ITRC(data = data.wide %>% dplyr::filter(Stim != 50),
       signal = "Stim",
       sample = "CellID2",
       response = data.value.colnames,
       parallel_cores = parallel_cores,
       bootstrap.number = bootstrap.number,
       bootstrap = bootstrap)






g.cytof <- plotITRCWaves(model = model.cytof ,
                         #                   rescale.fun = "log"
                         rescale.fun = function(x){log(x = x/25, base = 10)}, ylimits_ = c(1, 3.5)
                         # rescale.fun = function(x){x}
)
g.cytof


data.itrc.cytof <- data.wide
devtools::use_data(data.itrc.cytof,
                   overwrite = TRUE)
