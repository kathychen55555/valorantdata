# Loading gun uuids

library(tidyverse)

df_id <- read_csv(here::here("data-raw", "gun_uuid.csv"))
agent_id <- read_csv(here::here("data-raw", "agents.csv"))
