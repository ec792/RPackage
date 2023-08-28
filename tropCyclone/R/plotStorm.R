#' Plot a hurricane from the hurdat dataset
#'
#' Given a row from the hurdat dataset,
#' return a map of the United States which
#' contains county and state boundaries and the potential
#' hurricane view based off the maximum radii 
#' for each wind speed
#' 
#' The graph was calculated based off of 
#' an elliptical arc with the radii as reference
#'
#' @param row of hurdat database (indexed directly from hurdat)
#' @return a map containing a graph of the hurricane from the row
#'
#' @examples
#' #plot Hurricane Katrina around when it makes landfall
#' plotStorm(hurdat[c("46347"),])
#' 
#' @export
plotStorm <- function(rowHurdat){
  #states map
  states <- map_data("state")
  
  stormLat <- rowHurdat$latitude * ifelse(rowHurdat$hemisphereNS == "N", 1, -1)
  stormLong <- rowHurdat$longitude * ifelse(rowHurdat$hemisphereWE == "E", 1, -1)
  
  #collect the new points for each knot
  ordinal <- c("NE", "SE", "SW", "NW")
  degrees <- c(45, 315, 225, 135)
  points34 <- data.frame()
  for (i in 13:16){
    a <- rowHurdat[1, i]
    if (i == 16){
      b <- rowHurdat[1, 13]
    }
    else{
      b <- rowHurdat[1, i + 1]
    }
    points34 <- rbind(points34, ellipseParam(a, b, stormLat, stormLong, degrees[i - 12]))
  }
  points34$group <- 80
  
  points50 <- data.frame()
  for (i in 17:20){
    a <- rowHurdat[1, i]
    if (i == 20){
      b <- rowHurdat[1, 17]
    }
    else{
      b <- rowHurdat[1, i + 1]
    }
    points50 <- rbind(points50, ellipseParam(a, b, stormLat, stormLong, degrees[i - 16]))
  }
  points50$group <- 81
  
  points64 <- data.frame()
  for (i in 21:24){
    a <- rowHurdat[1, i]
    if (i == 24){
      b <- rowHurdat[1, 21]
    }
    else{
      b <- rowHurdat[1, i + 1]
    }
    points64 <- rbind(points64, ellipseParam(a, b, stormLat, stormLong, degrees[i - 20]))
  }
  points64$group <- 82
  
  allPoints <- rbind(points34, points50, points64)
  
  p <- ggplot(data=states, aes(x=long, y=lat, group = group)) +
    geom_polygon(color = "black", fill = "white") +
    guides(fill= "none") +
    ggtitle('Storm Map') +
    labs(x = "Longitude", y = "Latitude")
  coord_fixed(1.3)
  p <- p + geom_point(data = allPoints,
                      aes(x = long, y = lat, group = group), color = "red", size = 0.5)
  return(p)
}

#given the nm and the degree,
#this helper function will compute the point that is 
#that given distance away from a given point
nmToLatLong <- function(nm, latitude, longitude, degree){
  #here the distance traveled lat or long is the same
  #since the ordinal distance represents a isosceles right triangle
  nmLong <- nm * cos(degree * pi/180)
  nmLat <- nm * sin(degree * pi/180)
  
  #nm To Lat is 60nm per one degree lat
  lat <- nmLat / 60
  
  #here we assume that the Earth is spherical and so
  #the distance traveled by 1 degree longitude is proportional 
  #to the cosine of latitude, one degree lat is 60nm
  #formula: p = 60 cos(latitude) nm; so 1 degree long = p nm
  p <- 60 * cos(latitude * pi/180)
  long <- nmLong / p
  
  lat <- latitude + lat
  long <- longitude + long
  #return a data frame of the coordinates of the new point
  return(data.frame(lat, long))
}

#make a parametric equation that gives us the elliptical arc
#using our two radius (in lat long) as the a and b
ellipseParam <- function(a, b, lat, long, startDegree){
  coords <- data.frame()
  for (i in 0:90){
    degree <- startDegree - i
    nm <- sqrt((a * cos(i * pi/180))^2 + (b * sin(-i * pi/180))^2)
    coords <- rbind(coords, nmToLatLong(nm, lat, long, degree))
  }
  return(coords)
}