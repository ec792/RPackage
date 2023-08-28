
#' Determine whether a given storm made landfall in the U.S.
#'
#' Given the storm id from the hurdat dataset, return a boolean indicating whether or not
#' the storm made landfall, by checking whether the coordinates in the hurdat dataset
#' is in the U.S. boundaries as given by the maps package.
#'
#' @param id storm id found in hurdat$id
#' @return a boolean indicating whether the storm made landfall
#'
#' @examples
#' #check if Hurricane Katrina (2005) made landfall
#' madeLandfall("AL122005")
#'
#' @export
madeLandfall <- function(id){
  #this function will take in a hurricane id, subset it from the best track dataset
  #then use the us boundaries (main) given by maps and the sp package
  #to see if the points are in the polygon made by the points of the boundaries
  us <- map_data("usa")

  #we take a subset of the hurdat by the given storm id
  subset <- hurdat[which(hurdat$id == id), ]

  latNS <- ifelse(subset$hemisphereNS == "N", 1, -1)
  longWE <- ifelse(subset$hemisphereWE == "E", 1, -1)

  #use the boundaries and point in polygon to see if it made landfall
  ans <- sum(point.in.polygon(subset$longitude * longWE, subset$latitude * latNS, us$long, us$lat))
  if (ans > 0){
    return(TRUE)
  }
  return(FALSE)
}
