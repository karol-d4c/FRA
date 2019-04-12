for(file_ in list.files("R")){
  source(file = paste("R", file_, sep = "/"))
}
