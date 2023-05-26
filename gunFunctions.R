#' Gets basic stats about inputted gun
#'
#' @param gun name of gun in a string
#'
#' @return A data frame of information about the gun
#'
#' @export
getGunStats <- function(gun) {

  gunInfo <- gunSetUp(gun)
  if (class(gunInfo) == "character") {
    stop(gunInfo)
  }

  df <- data.frame(gunInfo$data$weaponStats[1:8])
  df$gunName <- stringr::str_to_title(gun)
  df$wallPenetration <- gsub(".*::","",df$wallPenetration)
  df <- df[ , c("gunName", names(df)[names(df) != "gunName"])]

  return(df)
}

#' Gets damage stats about inputted gun
#'
#' @param gun name of gun in a string
#'
#' @return A data frame of damage information about the gun
#'
#'
#'
#' @export
getGunDamage <- function(gun) {

  gunInfo <- gunSetUp(gun)
  if (class(gunInfo) == "character") {
    stop(gunInfo)
  }

  return(data.frame(gunInfo$data$weaponStats$damageRanges))
}

# helper function
gunSetUp <- function(gun) {

  if (class(gun) != "character") {
    return("Function must take in the gun name in form of a string")
  }
  else if (!(stringr::str_to_title(gun) %in% gun_id$gun)) {
    return("Function must take in a gun from the game VALORANT")
  }

  api <- httr::GET(paste("https://valorant-api.com/v1/weapons/",
                         gun_id[gun_id$gun == stringr::str_to_title(gun),]$uuid,
                         sep = ""))
  return(jsonlite::fromJSON(rawToChar(api$content)))
}

#' Creates a visualization about all guns in respect to a certain statistic
#'
#' @param gun a statistic in a string ("fireRate", "magazineSize", "equipTimeSeconds", "reloadTimeSeconds")
#'
#' @return A boxplot of all of the values for each gun
#'
#' @import dplyr
#'
#' @export
allGunStats <- function(statistic) {

  api <- httr::GET("https://valorant-api.com/v1/weapons/")

  info <- jsonlite::fromJSON(rawToChar(api$content))$data$weaponStats[1:17,][[statistic]]

  df_gun <- data.frame(gun_name <- gun_id$gun, info <- info)

  plot <- gunInfo %>%
    plotly::plot_ly(x = ~info, type = "box", boxpoints = "all", pointpos = 0.0,
                    text = ~gun_name, name = "Gun Statistics") %>%
    plotly::layout(xaxis=list(title=statistic))

  return(plot)

}









#' Creates a visualization about all guns in respect to a certain statistic
#'
#' @param wallpenetrationlevel ("Low", "Medium", "High")
#'
#' @return box plot of wall penetration levels and their corresponding fire rates of all guns
#'
#' @import dplyr
#'
#' @export
gunFireLevel <- function(wall_pen_level) {
  gun_stats <- data.frame(gun_name = gun_id$gun)
  fire_rates <- vector()
  wall_pen_level <- vector()

  for (gun in gun_id$uuid) {
    api <- httr::GET(paste("https://valorant-api.com/v1/weapons/",
                           gun,
                           sep = ""))
    gunInfo <- jsonlite::fromJSON(rawToChar(api$content))

    fire_rates <- append(fire_rates, gunInfo$data$weaponStats$fireRate)
    wall_pen_level <- append(wall_pen_level, gunInfo$data$weaponStats$wallPenetration)
  }

  gun_stats$fire_rate <- fire_rates
  gun_stats$wall_pen_level <- wall_pen_level


  gun_stats$wall_pen_level <- gsub("EWallPenetrationDisplayType::", "", gun_stats$wall_pen_level)



  gun_stats$wall_pen_level <- factor(gun_stats$wall_pen_level, levels = c("Low", "Medium", "High"))

  plot <- plot_ly(data = gun_stats,
                  x = ~fire_rate,
                  y = ~wall_pen_level,
                  type = "box",
                  color = ~wall_pen_level,
                  showlegend = FALSE,
                  boxpoints = "all",
                  pointpos = 0.0)


  plot <- layout(plot,
                 xaxis = list(title = "Fire Rate"),
                 yaxis = list(title = "Wall Penetration Level",
                              categoryorder = "array",
                              categoryarray = c("Low", "Medium", "High")),
                 title = "Gun Fire Rate on Wall Penetration Level")

  return(plot)

}

gunFireLevel(wall_pen_level = "High")



