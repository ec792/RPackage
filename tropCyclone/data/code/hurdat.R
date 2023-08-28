#my code from ex07
dat <- read.csv("hurdat2-1851-2022-040723.txt", header = FALSE)

dat
hurdat <- data.frame( matrix(NA, 0, ncol(dat)+1) )

#get list of indices that contain the id and name
inds <- which(substr( dat[, 1], 1, 2 ) == "AL" )
for (i in 1:length(inds)){
  fromIndex <- inds[i]
  if (i == length(inds)){
    toIndex <- nrow(dat) + 1
  }
  else{
    toIndex <- inds[i + 1]
  }
  #write a data frame that holds the name and id and adds them to the rows of interest
  subset <- cbind(rep(dat[fromIndex, 1], toIndex - fromIndex - 1),
                  rep(dat[fromIndex, 2], toIndex - fromIndex - 1),
                  dat[(fromIndex + 1):(toIndex - 1), ])
  hurdat <- rbind(hurdat, subset)
}


#rewrite to replace NA
hurdat[hurdat == -999] <- NA
hurdat[hurdat == -99] <- NA

#numeric latitude and hemisphereNS
hurdat <- cbind(hurdat[,1:6],
                as.numeric(substr(hurdat$V5,1,5)),
                substr(hurdat$V5,6,6),
                hurdat[,8:23])

substr(hurdat$V6,1,6)
#numeric longitude and hemisphereWE
hurdat <- cbind(hurdat[,1:8],
                as.numeric(substr(hurdat$V6,1,6)),
                substr(hurdat$V6,7,7),
                hurdat[,10:24])

#add meaningful column names
colnames(hurdat) <- c("id",
                      "name",
                      "date",
                      "time",
                      "recordID",
                      "status",
                      "latitude",
                      "hemisphereNS",
                      "longitude",
                      "hemisphereWE",
                      "maxWind",
                      "minPressure",
                      "34windNE",
                      "34windSE",
                      "34windSW",
                      "34windNW",
                      "50windNE",
                      "50windSE",
                      "50windSW",
                      "50windNW",
                      "64windNE",
                      "64windSE",
                      "64windSW",
                      "64windNW",
                      "radius")
