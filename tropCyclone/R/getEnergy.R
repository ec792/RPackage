
#' Get the Accumulated Cyclone Energy of a Given Storm
#'
#' Given the storm id from the hurdat dataset, return the accumulated cyclone energy (ACE)
#' defined in https://en.wikipedia.org/wiki/Accumulated_cyclone_energy.
#' The formula is ACE = 1e-4 * sum(maximum velocities^2), where the
#' maximum velocities are estimated sustained wind speed in knots
#' measured at six-hour intervals
#'
#' @param id storm id found in hurdat$id
#' @return a numeric representing the ACE of a given storm
#'
#' @examples
#' getEnergy("AL101869")
#' @export
getEnergy <- function(id){
  #the formula is given by the sum of squares of the cyclone's maximum
  #sustained winds, tracked every six hours. the total is then divided
  #by 10,000 to make it manageable

  #we take a subset of the hurdat by the given storm id
  subset <- hurdat[which(hurdat$id == id), ]

  #we need to get the data for every six hours, so omit data for
  #that is not measured at 0000,0600,1200, and 1800.
  subset <- subset[which(as.numeric(subset$time) %% 600 == 0), ]

  #use the formula
  return(10^-4 * sum(subset$maxWind ^ 2))
}
