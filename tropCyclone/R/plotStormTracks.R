
#' Plot a map of storm tracks for a selection of storms
#'
#' Given a vector of storm id from the hurdat dataset,
#' return a map of the United States which
#' contains county and state boundaries and the storm track of the storms
#' in the given vector.
#'
#' @param id storm id found in hurdat$id
#' @return a map containing storm tracks
#'
#' @examples
#' map <- plotStormTracks(c("AL051896", "AL011851"))
#' print(map)
#' @export
plotStormTracks <- function(storm_id){
  #states map
  states <- map_data("state")

  #making hurdat compatible with states (change dataframe to hurdat_2)
  hurdat_2 <- hurdat %>%
    mutate(longitude = ifelse(hemisphereWE == "E", longitude*1, longitude*-1)) %>%
    mutate(latitude = ifelse(hemisphereNS == "N", latitude*1, latitude*-1))

  hurdat_2 <- hurdat_2 %>%
    mutate(group = 75)

  hurdat_2$date <- as.Date(hurdat_2$date, format = "%Y%m%d")

  hurdat_2 <- hurdat_2 %>%
    rename("Date" = "date")

  #base map of United States
  p <- ggplot(data=states, aes(x=long, y=lat, group = group)) +
    geom_polygon(color = "black", fill = "white") +
    #guides(fill= "none") +
    ggtitle('Storm Map') +
    labs(x = "Longitude", y = "Latitude") +
    xlim(-130, 10) +
    coord_fixed(1.3)
  #loop over all the storms and plot
  colorDict <- data.frame()
  for(i in 1:length(storm_id)){
    p <- p + geom_point(data = hurdat_2 %>%
                         filter(id == storm_id[i]),
                       aes(x = longitude, y = latitude, group = group, color = id), size = 2)
    colorDict <- rbind(colorDict, c(storm_id[i], randomColor()))
  }
  colors <- as.character(colorDict[,2])
  names(colors) <- as.character(colorDict[,1])
  p <- p +
    scale_color_manual(name = "Key",
                       breaks = names(colors),
                       values = colors) +
    theme(legend.text = element_text(size = 6),
          legend.key.size = unit(.25,'cm'),
          legend.key.width = unit(.25,'cm'),
          legend.key.height = unit(.25,'cm'))
  return(p)
}











