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
#' @param gun a statistic in a string (i.e. "fireRate", "magazineSize", "equipTimeSeconds", "reloadTimeSeconds")
#'
#' @return A boxplot of all of the values for each gun
#'
#' @import dplyr
#'
#' @export
allGunStats <- function(statistic) {

  if (class(statistic) != "character") {
    stop("Function must take in the gun statistic in form of a string")
  }
  else if (!(statistic %in% c("fireRate", "magazineSize", "equipTimeSeconds", "reloadTimeSeconds"))) {
    stop("Function must take in a property a gun has. This includes fireRate, magazineSize, equipTimeSeconds, and reloadTimeSeconds.")
  }

  api <- httr::GET("https://valorant-api.com/v1/weapons/")

  info <- jsonlite::fromJSON(rawToChar(api$content))$data$weaponStats[1:17,][[statistic]]

  df_gun <- data.frame(gun_name <- gun_id$gun, info <- info)

  plot <- df_gun %>%
    plotly::plot_ly(x = ~info, type = "box", boxpoints = "all", pointpos = 0.0,
                    text = ~gun_name, name = "Gun Statistics") %>%
    plotly::layout(xaxis=list(title=statistic))

  return(plot)

}

#' Creates a visualization about all guns in respect to a certain statistic
#'
#' @param wall_pen_level ("Low", "Medium", "High")
#'
#' @return box plot of wall penetration levels and their corresponding fire rates of all guns
#'
#' @import dplyr
#'
#' @export
gunFireLevel <- function(wall_pen_level) {

  if (class(wall_pen_level) != "character") {
    stop("Function must take in a gun penetration level in the form of a string")
  }
  else if (!(wall_pen_level %in% c("Low", "Medium", "High"))) {
    stop("Function must take in a wall penetration level that a gun has. This includes Low, Medium, High")
  }

  api <- httr::GET("https://valorant-api.com/v1/weapons/")

  info <- jsonlite::fromJSON(rawToChar(api$content))$data$weaponStats[1:17,]

  info <- info %>%
    mutate(gunName = gun_id$gun) %>%
    filter(wallPenetration == paste0("EWallPenetrationDisplayType::", stringr::str_to_title(wall_pen_level)))

  plot <- plotly::plot_ly(data = info,
                          x = ~fireRate,
                          type = "box",
                          color = ~wallPenetration,
                          text = ~gunName,
                          showlegend = FALSE,
                          boxpoints = "all",
                          pointpos = 0.0)


  plot <- plotly::layout(plot,
                         xaxis = list(title = "Fire Rate"),
                         yaxis = list(title = "Wall Penetration Level",
                                      categoryorder = "array",
                                      categoryarray = c("Low", "Medium", "High")),
                         title = "Gun Fire Rate on Wall Penetration Level")

  return(plot)
}
