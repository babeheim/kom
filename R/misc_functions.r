
cartesian_map <- function(lat1, lat2, long1, long2, units="km"){

    x <- haversine(lat1, lat1, long1, long2, units=units)
    tar <- which(long1 < long2)
    if(length(tar)>0) x[tar] <- (-1)*x[tar]
    y <- haversine(lat1, lat2, long1, long1, units=units)
    tar <- which(lat1 < lat2)
    if(length(tar)>0) y[tar] <- (-1)*y[tar]
    output <- list(x=x, y=y)
    output

}


haversine <- function(lat1, lat2, long1, long2, units="km"){

    R <- 6371

    phi1 <- lat1*(2*pi)/360 # lat1 in radians
    phi2 <- lat2*(2*pi)/360  #lat2 in radians
    lambda1 <- long1*(2*pi)/360
    lambda2 <- long2*(2*pi)/360

    delta.phi <- phi2 - phi1  # lat2 - lat1 in radians
    delta.lambda <- lambda2 - lambda1  # in radians

    a <- sin(delta.phi/2)*sin(delta.phi/2) + cos(phi1)*cos(phi2)*sin(delta.lambda/2)*sin(delta.lambda/2)
    c <- 2*atan2(sqrt(a), sqrt(1-a))
    distance <- R*c

    if(units=="m") distance <- distance*1000

    return(distance)

}


identical_set <- function(x,y){

 # new = not element wise

  c1 <- length(x)==length(y)
  c2 <- all(x %in% y)
  c3 <- all(y %in% x)

  c1 & c2 & c3

}

