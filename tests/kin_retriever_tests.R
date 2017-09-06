
rm(list=ls())

library(testthat)

library(devtools)
devtools::load_all()
library(kom)

data(hortVillage)

preg <- hortVillage$people

# adding these columns is necessary
preg$mm_pid <- preg$m_pid[match(preg$m_pid, preg$pid)]
preg$mf_pid <- preg$f_pid[match(preg$m_pid, preg$pid)]
preg$fm_pid <- preg$m_pid[match(preg$f_pid, preg$pid)]
preg$ff_pid <- preg$f_pid[match(preg$f_pid, preg$pid)]

test_that( "all return valid values", {
  n <- nrow(preg)
  x <- NA
  for(i in 1:n){
    x[i] <- length(kin_retriever( preg$pid[i], preg ))
  }
  expect_true( all(x >= 0) )
})


test_that( "a person with no relatives returns character zero", {
  new <- preg[1,]
  new[!is.na(new)] <- NA
  new$pid <- "ABCD"
  new$male <- 1
  preg <- rbind(preg, new)
  x <- kin_retriever( "ABCD", preg )
  expect_equal( length(x) , 0)
})

# still need to check cases one-on-one to make sure works! 