#' Gets map information about inputted map
#'
#' @param map name of map in a string
#'
#' @return A dataframe that lists all of the locations on the map
#'
#' @export
getMapInfo <- function(map) {
  if (class(map) != "character") {
    stop("Function must take in the map name in form of a string")
  }
  else if (!(stringr::str_to_title(gun) %in% map_id$map)) {
    stop("Function must take in a map from the game VALORANT")
  }
  api <- httr::GET(paste("https://valorant-api.com/v1/maps/",
                        map_id[map_id$map == stringr::str_to_title(map),]$uuid,
                        sep = ""))
  mapInfo <- jsonlite::fromJSON(rawToChar(api$content))$data$callouts

  names(mapInfo) <- c("Name", "Location", "coordinate")

  return(mapInfo)
}
