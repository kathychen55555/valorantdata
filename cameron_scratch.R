gun_stats <- data.frame(gun_name = gun_id$gun)

weapon_types <- vector()
fire_rates <- vector()
mag_sizes <- vector()
equip_times <- vector()
reload_times <- vector()

for (gun in gun_id$uuid) {
  api <- httr::GET(paste("https://valorant-api.com/v1/weapons/",
                         gun,
                         sep = ""))
  gunInfo <- jsonlite::fromJSON(rawToChar(api$content))
  weapon_types <- append(weapon_types, gunInfo$data$shopData$category)
  fire_rates <- append(fire_rates, gunInfo$data$weaponStats$fireRate)
  mag_sizes <- append(mag_sizes, gunInfo$data$weaponStats$magazineSize)
  equip_times <- append(equip_times, gunInfo$data$weaponStats$equipTimeSeconds)
  reload_times <- append(reload_times, gunInfo$data$weaponStats$reloadTimeSeconds)
}

gun_stats$weapon_type <- weapon_types
gun_stats$fire_rate <- fire_rates
gun_stats$mag_size <- mag_sizes
gun_stats$equip_times <- equip_times
gun_stats$reload_times <- reload_times

plotly::plot_ly(data = gun_stats, x = ~fire_rate, type = "box", boxpoints = "all",
                       pointpos = 0.0, text = ~gun_name, name = "Fire Rate Statistics")

