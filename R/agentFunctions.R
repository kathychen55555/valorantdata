#' Gets description of inputted agent
#'
#' @param agent name of agent (capitalized)
#'
#' @return A string that is an agent's description
#'
#' @export
getAgentInfo <- function(agent) {
  if (class(agent) != "character") {
    return("Function must take in agent name in the form of a string")
  }

  else if (!(stringr::str_to_title(agent) %in% agent_id$agent)) {
    return("Function must take in an agent from the game VALORANT")
  }

  api = httr::GET(paste("https://valorant-api.com/v1/agents/",
                        agent_id[agent_id$agent == stringr::str_to_title(agent),]$uuid,
                        sep = ""))
  agentInfo = jsonlite::fromJSON(rawToChar(api$content))

  agentInfo$data$description
}
