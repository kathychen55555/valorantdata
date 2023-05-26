#' Gets description of inputted agent
#'
#' @param agent name of agent in a string
#'
#' @return A string that is an agent's description
#'
#' @export
getAgentInfo <- function(agent) {

  agentInfo <- agentSetUp(agent)

  if (class(agentInfo) == "character") {
    stop(agentInfo)
  }

  return(agentInfo$data$description)
}

#' Gets description of inputted agent's abilities
#'
#' @param agent name of agent in a string
#'
#' @return A data frame of agent info
#'
#' @export
getAgentAbilities <- function(agent) {

  agentInfo <- agentSetUp(agent)

  if (class(agentInfo) == "character") {
    stop(agentInfo)
  }

  return(data.frame(agentInfo$data$abilities)[,1:3])
}

# helper function
agentSetUp <- function(agent) {
  if (class(agent) != "character") {
    return("Function must take in agent name in the form of a string")
  }

  else if (!(stringr::str_to_title(agent) %in% agent_id$agent)) {
    return("Function must take in an agent from the game VALORANT.")
  }

  api <- httr::GET(paste("https://valorant-api.com/v1/agents/",
                         agent_id[agent_id$agent == stringr::str_to_title(agent),]$uuid,
                         sep = ""))

  return(jsonlite::fromJSON(rawToChar(api$content)))
}
