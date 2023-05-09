#' Gets basic stats about inputted gun
#'
#' @param gun name of gun in a string
#'
#' @return A data frame of information about the gun
#'
#' @export
getGunStats <- function(gun) {
  if (class(gun) == "character") {
    return("Function must take in the gun name in form of a string")
  }
  else if (stringr::str_to_title(gun) %in% df_id$gun) {
    return("Function must take in a gun from the game VALORANT")
  }

  api <- httr::GET(paste("https://valorant-api.com/v1/weapons/",
                         df_id[df_id$gun == stringr::str_to_title(gun),]$uuid,
                         sep = ""))
  gunInfo <- jsonlite::fromJSON(rawToChar(api$content))

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
#' @export
getGunDamage <- function(gun) {
  if (class(gun) == "character") {
    return("Function must take in the gun name in form of a string")
  }
  else if (stringr::str_to_title(gun) %in% df_id$gun) {
    return("Function must take in a gun from the game VALORANT")
  }

  api <- httr::GET(paste("https://valorant-api.com/v1/weapons/",
                         df_id[df_id$gun == stringr::str_to_title(gun),]$uuid,
                         sep = ""))
  gunInfo <- jsonlite::fromJSON(rawToChar(api$content))

  return(data.frame(gunInfo$data$weaponStats$damageRanges))
}

#' Gets description of inputted agent
#'
#' @param agent name of agent (capitalized)
#'
#' @return A string that is an agent's description
#'
#' @export

getAgentInfo <- function(agent) {
  if (class(agent) == "character") {
    return("Function must take in agent name in the form of a string")
  }

  else if (stringr::str_to_title(agent) %in% agent_id$agent) {
    return("Function must take in an agent from the game VALORANT")
  }

  api = httr::GET(paste("https://valorant-api.com/v1/agents/",
                  agent_id[agent_id$agent == stringr::str_to_title(agent),]$uuid,
                  sep = ""))
  agentInfo = jsonlite::fromJSON(rawToChar(api$content))

  agentInfo$data$description
}
