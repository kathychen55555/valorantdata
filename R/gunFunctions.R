#' Gets basic stats about inputted gun
#'
#' @param gun name of gun in a string
#'
#' @return A data frame of information about the gun
#'
#' @export
getGunStats <- function(gun) {
<<<<<<< HEAD
  guns <- c("Odin",
            "Ares",
            "Vandal",
            "Bulldog",
            "Phantom",
            "Judge",
            "Bucky",
            "Frenzy",
            "Classic",
            "Ghost",
            "Sheriff",
            "Shorty",
            "Operator",
            "Guardian",
            "Marshal",
            "Spectre",
            "Stinger")

  uuids <- c("63e6c2b6-4a8e-869c-3d4c-e38355226584",
             "55d8a0f4-4274-ca67-fe2c-06ab45efdf58",
             "9c82e19d-4575-0200-1a81-3eacf00cf872",
             "ae3de142-4d85-2547-dd26-4e90bed35cf7",
             "ee8e8d15-496b-07ac-e5f6-8fae5d4c7b1a",
             "ec845bf4-4f79-ddda-a3da-0db3774b2794",
             "910be174-449b-c412-ab22-d0873436b21b",
             "44d4e95c-4157-0037-81b2-17841bf2e8e3",
             "29a0cfab-485b-f5d5-779a-b59f85e204a8",
             "1baa85b4-4c70-1284-64bb-6481dfc3bb4e",
             "e336c6b8-418d-9340-d77f-7a9e4cfe0702",
             "42da8ccc-40d5-affc-beec-15aa47b42eda",
             "a03b24d3-4319-996d-0f8c-94bbfba1dfc7",
             "4ade7faa-4cf1-8376-95ef-39884480959b",
             "c4883e50-4494-202c-3ec3-6b8a9284f00b",
             "462080d1-4035-2937-7c09-27aa2a5c27a7",
             "f7e1b454-4ad4-1063-ec0a-159e56b58941")

  gun_data <- data.frame(gun = guns, uuid = uuids)

  if (class(gun) == "character") {
=======
  if (class(gun) != "character") {
>>>>>>> e3d23bdfe88a2473f59dbe582a866f7afd9cf53c
    return("Function must take in the gun name in form of a string")
  }
  else if (!(stringr::str_to_title(gun) %in% gun_id$gun)) {
    return("Function must take in a gun from the game VALORANT")
  }

  api <- httr::GET(paste("https://valorant-api.com/v1/weapons/",
                         gun_id[gun_id$gun == stringr::str_to_title(gun),]$uuid,
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
  if (class(gun) != "character") {
    return("Function must take in the gun name in form of a string")
  }
  else if (!(stringr::str_to_title(gun) %in% gun_id$gun)) {
    return("Function must take in a gun from the game VALORANT")
  }

  api <- httr::GET(paste("https://valorant-api.com/v1/weapons/",
                         gun_id[gun_id$gun == stringr::str_to_title(gun),]$uuid,
                         sep = ""))
  gunInfo <- jsonlite::fromJSON(rawToChar(api$content))

  return(data.frame(gunInfo$data$weaponStats$damageRanges))
}
<<<<<<< HEAD

#' Gets description of inputted agent
#'
#' @param agent name of agent (capitalized)
#'
#' @return A string that is an agent's description
#'
#' @export

getAgentInfo <- function(agent) {
  uuids <- c("5f8d3a7f-467b-97f3-062c-13acf203c006",
           "f94c3b30-42be-e959-889c-5aa313dba261",
           "6f2a04ca-43e0-be17-7f36-b3908627744d",
           "117ed9e3-49f3-6512-3ccf-0cada7e3823b",
           "320b2a48-4d9b-a075-30f1-1f93a9b638fa",
           "1e58de9c-4950-5125-93e9-a0aee9f98746",
           "707eab51-4836-f488-046a-cda6bf494859",
           "eb93336a-449b-9c1b-0a54-a891f7921d69",
           "9f0d8ba9-4140-b941-57d3-a7ad57c6b417",
           "7f94d92c-4234-0a36-9646-3a87eb8b5c89",
           "569fdd95-4d10-43ab-ca70-79becc718b46",
           "a3bfb853-43b2-7238-a4f1-ad90e9e46bcc",
           "8e253930-4c05-31dd-1b6c-968525494517",
           "add6443a-41bd-e414-f6ad-e58d267f4e95")

  agents <- c("Breach",
             "Raze",
             "Skye",
             "Cypher",
             "Sova",
             "Killjoy",
             "Viper",
             "Phoenix",
             "Brimstone",
             "Yoru",
             "Sage",
             "Reyna",
             "Omen",
             "Jett")

  agent_data <- data.frame(agent = agents, uuid = uuids)

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
=======
>>>>>>> e3d23bdfe88a2473f59dbe582a866f7afd9cf53c
