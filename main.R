library(tidyverse)
library(APCalign)

source("R/havplot.R")

havplot <-
  load_havplot("../../../data/havplot/data") %>%
  correct_havplot()

apc <- load_taxonomic_resources()

output <-
  havplot %>%
  compile_havplot() %>%
  align_taxonomy_havplot(apc) %>%
  arrange_havplot()

saveRDS(havplot, "output/havplot.rds")
