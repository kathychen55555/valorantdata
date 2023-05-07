#' Gets basic stats about inputted gun
#'
#' @param gun name of gun
#'
#' @return A data frame of information about the gun
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
#' @export
getGunStats <- function(gun) {

  uuid = df_id[gun == gun,]$uuid

  api = GET(paste("https://valorant-api.com/v1/weapons", uuid, sep = ""))
  gunInfo = fromJSON(rawToChar(api$content))

  df = data.frame(gunInfo$data$weaponStats[1:9])
  df$gunName = gun
  df$wallPenetration = gsub(".*::","",df$wallPenetration)
  df$feature = gsub(".*::","",df$feature)

  return(df)
}

#' Gets damage stats about inputted gun
#'
#' @param gun name of gun
#'
#' @return A data frame of damage information about the gun
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
#' @export
getGunDamage <- function(gun) {
  uuid = df_id[gun == gun,]$uuid

  api = GET(paste("https://valorant-api.com/v1/weapons", uuid, sep = ""))
  gunInfo = fromJSON(rawToChar(api$content))

  data.frame(gunInfo$data$weaponStats$damageRanges)
}
