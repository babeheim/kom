
# neighbor_finder

rm(list=ls())

library(testthat)

library(devtools)
devtools::load_all()
library(kom)

data(hortVillage)

preg <- hortVillage$people
hreg <- hortVillage$households
vreg <- hortVillage$villages

hreg$ray_dist <- haversine(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)], units="m")
hreg$x_coord <- cartesian_map(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)], units="m")$x
hreg$y_coord <- cartesian_map(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)], units="m")$y

# errors in the dataset

n <- 1000

hreg[is.na(hreg$x_coord),]

drop <- which(is.na(hreg$lat) | is.na(hreg$long) | duplicated(hreg[,c("lat", "long")]))

if(length(drop) > 0){
 bad_houses <- hreg$household[drop]
 hreg <- hreg[-drop,]
 preg <- preg[-which(preg$household %in% bad_houses),]
}

# # pid not in pop_reg
# my_pid <- "12344545"
# neighbor_finder(my_pid, preg, hreg, dist_radius=5000)


# # pid in pop_reg, but household is NA
# my_pid <- sample(preg$pid, 1)
# preg1 <- preg
# preg1$household[preg1$pid==my_pid] <- NA
# neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)

# # pid in pop_reg, but household is not in house_reg
# my_pid <- sample(preg$pid, 1)
# preg1 <- preg
# preg1$household[preg1$pid==my_pid] <- "asdf"
# neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)


# # random NA's in pid
# my_pid <- NA
# neighbor_finder(my_pid, preg, hreg, dist_radius=5000)





test_that("bad pid not accepted", {
  my_pid <- 1
  expect_error(neighbor_finder(my_pid, preg, hreg, dist_radius=5000))
})

test_that("random NA's in pop_reg$household", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    preg1 <- preg
    kill_entries <- sample(which(!preg1$pid %in% my_pid), 5)
    preg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    preg2 <- preg1[-which(is.na(preg1$household)),]
    output2 <- neighbor_finder(my_pid, preg2, hreg, dist_radius=5000)
    identical_set(output1, output2)
  })
  expect_true(all(x))
})

test_that("popreg has only one entry", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    preg1 <- preg
    kill_entries <- sample(which(!preg1$pid %in% my_pid))
    preg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    preg2 <- preg1[-which(is.na(preg1$household)),]
    output2 <- neighbor_finder(my_pid, preg2, hreg, dist_radius=5000)
    identical_set(output1, output2)
  })
  expect_true(all(x))
})

test_that("random NA's in pop_reg$village and pop_reg$household", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[which(preg$pid==my_pid)]
    preg1 <- preg
    kill_entries <- sample(which(!preg1$pid %in% my_pid), 5)
    preg1$village[kill_entries] <- NA
    preg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    preg2 <- preg1[-which(is.na(preg1$village)),]
    output2 <- neighbor_finder(my_pid, preg2, hreg, dist_radius=5000)
    identical_set(output1, output2)
  })
  expect_true(all(x))
})

test_that("only person from village is ego", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    village_folk <- which(preg$pid != my_pid & preg$village == my_village)
    preg1 <- preg[-village_folk,]
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    length(output1)==0
  })
  expect_true(all(x))
})

test_that("random NA's in house_reg$household", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_household <- preg$household[preg$pid==my_pid]
    hreg1 <- hreg
    kill_entries <- sample(which(!hreg1$household %in% my_household), 5)
    hreg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg, hreg1, dist_radius=5000)
    hreg2 <- hreg1[-which(is.na(hreg1$household)),]
    output2 <- neighbor_finder(my_pid, preg, hreg2, dist_radius=5000)
    identical_set(output1, output2)
  })
  expect_true(all(x))
})

test_that( "random NA's in house_reg$village", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_household <- preg$household[preg$pid==my_pid]
    hreg1 <- hreg
    kill_entries <- sample(which(!hreg1$household %in% my_household), 5)
    hreg1$village[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg, hreg1, dist_radius=5000)
    hreg2 <- hreg1[-which(is.na(hreg1$village)),]
    output2 <- neighbor_finder(my_pid, preg, hreg2, dist_radius=5000)
    identical_set(output1, output2)
  })
  expect_true( all(x) )
})

test_that( "all neighbors actually live in this community" , {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    test <- neighbor_finder(my_pid, preg, hreg, dist_radius=5000)
    all(test %in% preg$pid[preg$village==my_village])
  })
  expect_true( all(x) )
})

test_that( "too-short distance returns character(0)" , {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    test <- neighbor_finder(my_pid, preg, hreg, dist_radius=1)
    length(test)==0
  })
  expect_true( all(x) )
})

test_that("warning if search radius too close", {
  my_pid <- 1508
  expect_warning(neighbor_finder(my_pid, preg, hreg, dist_radius=1))
})

test_that("warning if search radius too close", {
  my_pid <- 1508
  expect_warning(neighbor_finder(my_pid, preg, hreg, units="km", dist_radius=0.001))
})

# hreg$x_coord and hreg$y_coord are in meters
# so, if we specify km for the neighbor_finder, need to times by 1000

test_that("same results if I change the units from m to km", {

  x <- replicate(n, {
    my_dist <- sample(1:5000, 1)
    my_pid <- sample(preg$pid, 1)
    in_km <- neighbor_finder(my_pid, preg, hreg, units="km", dist_radius=my_dist/1000)
    in_m <- neighbor_finder(my_pid, preg, hreg, units="m", dist_radius=my_dist)
    identical_set(in_km, in_m)
    # expect_equal(in_km, in_m)
  })

  expect_true( all(x) )

})

test_that("no warning for 2 km", {
  for(i in 1:nrow(preg)){
    my_pid <- preg$pid[i]
    expect_silent(neighbor_finder(my_pid, preg, hreg, units="km", dist_radius=2))
  }
})

test_that("no warning for 2000m", {
  for(i in 1:nrow(preg)){
    my_pid <- preg$pid[i]
    expect_silent(neighbor_finder(my_pid, preg, hreg, units="m", dist_radius=2000))
  }
})

test_that( "overly wide radius is all but focal household" , {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_household <- preg$household[preg$pid==my_pid]
    my_household_residents <- preg$pid[preg$household==my_household]
    my_village <- preg$village[preg$pid==my_pid]
    all_residents <- preg$pid[preg$village==my_village]
    test <- neighbor_finder(my_pid, preg, hreg, dist_radius=100000)
    identical_set( c(my_household_residents,test), all_residents ) 
  }) 
  expect_true( all(x) )
})


test_that( "overly wide radius gives warning" , {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_household <- preg$household[preg$pid==my_pid]
    my_household_residents <- preg$pid[preg$household==my_household]
    my_village <- preg$village[preg$pid==my_pid]
    all_residents <- preg$pid[preg$village==my_village]
    expect_warning(neighbor_finder(my_pid, preg, hreg, dist_radius=100000))
  }) 
})

test_that( "n_neighbors is increasing with distance", {
  for(i in 1:n){
    test_dist <- seq(1, 5000, by=100)
    n_neighbors <- rep(NA, length(test_dist))
    my_pid <- sample(preg$pid, 1)
    for(i in 1:length(test_dist)) n_neighbors[i] <- length(neighbor_finder(my_pid, preg, hreg, dist_radius=test_dist[i]))
    expect_true( all(diff(n_neighbors) >= 0) )
  }
})

test_that( "random pid random distance", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_radius <- rexp(1 , 1/3000 )
    output <- neighbor_finder(my_pid, preg, hreg, dist_radius=my_radius)
    length(output)>=0
  })
  expect_true( all(x) )
})

test_that( "everyone in the village lives in the same household", {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    my_household <- preg$household[preg$pid==my_pid]
    all_residents <- preg$pid[preg$village==my_village & preg$pid!=my_pid]
    preg1 <- preg
    preg1$household[which(preg1$pid %in% all_residents)] <- my_household
    expect_warning(neighbor_finder(my_pid, preg1, hreg, dist_radius=10000))
  })
})
