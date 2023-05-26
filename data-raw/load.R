# Loading data with uuids

library(tidyverse)

gun_id <- read_csv(here::here("data-raw", "guns.csv"))

agent_id <- read_csv(here::here("data-raw", "agents.csv"))

map_id <- read_csv(here::here("data-raw", "maps.csv"))

usethis::use_data(c(gun_id, agent_id, map_id), overwrite = TRUE)

