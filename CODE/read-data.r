# this script will read in data and save it as .rds file

library(tidyverse)

full_data <- map_dfr(data_dir, ~{read_csv(paste0("data/", .))})

write_rds(full_data, 'full-data.rds')