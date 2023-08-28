
#' Interpolate Actual Latitude and Longitude Data into 30-min Intervals
#'
#' Given the storm id from the hurdat dataset, return a matrix containing information
#' about the interpolated latitude and longitude values of that storm, where the
#' interpolation takes place in 30 minute intervals between the start and end time
#' of the storm.
#'
#' @param id storm id found in hurdat$id
#' @return a matrix containing the following columns
#' \itemize{
#'   \item \code{@context} metadata
#'   \item \code{intervals} a vector of the time stamps in 30-min intervals in POSIXct
#'   \item \code{latitude} latitude in decimal degrees north (positive for northern hemisphere)
#'   \item \code{longitude} longitude in decimal degrees east (positive for eastern hemisphere)
#' }
#' @examples
#' track <- stormTrack("AL041932")
#' print(track)
#' @export
stormTrack <- function(id){
  #we take a subset of the hurdat by the given storm id
  subset <- hurdat[which(hurdat$id == id), ]

  #combine the date and time data in POSIXct
  dateTime <- as.POSIXct(paste(subset$date, subset$time), format = "%Y%m%d %H%M")

  #to mark pos/neg for N/S and E/W each
  latNS <- ifelse(subset$hemisphereNS == "N", 1, -1)
  longWE <- ifelse(subset$hemisphereWE == "E", 1, -1)

  #now we can build our matrix using the intervals from our helper function
  tracker <- incrementsBetween(subset)
  tracker <- cbind(tracker, rep(NA, nrow(tracker)), rep(NA, nrow(tracker)))
  colnames(tracker) <- c("intervals", "latitude", "longitude")

  #we add the actual value to the points
  for (i in 1:length(dateTime)){
    tracker[tracker$intervals == dateTime[i], 2] <- subset$latitude[i] * latNS[i]
    tracker[tracker$intervals == dateTime[i], 3] <- subset$longitude[i] * longWE[i]
  }

  #now we use a built in linear interpolation function to fill in the rest
  tracker$latitude <- approx(dateTime, subset$latitude * latNS, xout = tracker$intervals)$y
  tracker$longitude <- approx(dateTime, subset$longitude * longWE, xout = tracker$intervals)$y

  #finally we return the matrix
  return(tracker)
}

#this helper function will track how many 30 min increments of the storm with the given id
incrementsBetween <- function(subset){
  #get the start time
  startTime <- as.POSIXct(paste(subset$date[1], subset$time[1]), format = "%Y%m%d %H%M")

  #get the end time
  endTime <- as.POSIXct(paste(subset$date[nrow(subset)], subset$time[nrow(subset)]), format = "%Y%m%d %H%M")

  return(data.frame(seq.POSIXt(startTime, endTime, by = "30 mins")))
}
