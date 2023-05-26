#' Cluster analysis on player kills vs. damage to see if patterns exist for type of gun.
#'
#' @param username Player username
#' @param tagline Player's tagline (don't include #)
#' @param region Region of player
#' @param filter Filter by gamemode (i.e competitive)
#' @param centers Number of centers in the cluster analysis. Default to 4 for each type of loadout (pistol, semi, eco, full)
#'
#' @return a plot with cluster analysis
#'
#' @import dplyr
#'
#' @export
gunClusterAnalysis <- function(username, tagline, region, filter = "competitive", centers = 4) {

  api <- httr::GET(paste(paste("https://api.henrikdev.xyz/valorant/v3/matches",
                               tolower(region), username, tagline, sep = "/"), "?filter=",
                         tolower(filter), sep=""))

  df_rounds <- jsonlite::fromJSON(rawToChar(api$content))$data$rounds

  ign <- paste(username, tagline, sep="#")

  df <- data.frame()

  for (i in 1:5) {

    temp <- data.frame()

    for (j in 1:length(df_rounds[[i]]$player_stats)) {

      round <- df_rounds[[i]]$player_stats[[j]] %>%
        mutate(gun_name = df_rounds[[i]]$player_stats[[j]]$economy$weapon$name) %>%
        filter(player_display_name == ign) %>%
        select(gun_name, kills, damage)

      temp <- rbind(temp, round)
    }

    df <- rbind(df, temp)
  }

  df <- df %>%
    group_by(gun_name) %>%
    summarize(avg_kills = mean(kills), avg_damage = mean(damage))

  my_kmeans <- df %>%
    select(avg_kills, avg_damage) %>%
    mutate_all(~scale(.)) %>%
    kmeans(centers = centers)

  plot <- df %>%
    mutate(cluster = as.character(sort(my_kmeans$cluster))) %>%
    plotly::plot_ly(type = "scatter", x = ~avg_kills, y = ~avg_damage, color = ~cluster, text = ~gun_name) %>%
    plotly::layout(title = paste(as.character(centers), "Means Clustering of Average Kills and Damage Per Round"),
                   xaxis = list(title = "Average Kills Per Round"), yaxis = list(title = "Average Damage per Round"),
                   legend = list(title = list(text = "Cluster")))

  return(plot)

}



