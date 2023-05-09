# Loading data with uuids

library(tidyverse)

gun_id <- read_csv(here::here("data-raw", "gun_uuid.csv"))
usethis::use_data(gun_id, overwrite = TRUE)

agent_id <- read_csv(here::here("data-raw", "agents.csv"))
usethis::use_data(agent_id, overwrite = TRUE)
