 # Automated testing of the 'util.R' file in the NPEL package
# Created by Jonathan Henkelman 27.Jan.2016

library(NPEL.Classification)
library(testthat)
context("Util Functions")
cat('\n')

# Setup testing environment
facA <- c(2,3,5,1);     lvlA <- c(3:5,1:2)
facB <- c(12,13,15,11); lvlB <- c(13:15,11:12)
tFacA <- factor (facA, levels=lvlA)
tFacB <- factor (facB, levels=lvlB)
facC <- factor(NULL)

test_that("factorValues", {
  expect_equal(factorValues(tFacA), facA)
  expect_equal(factorValues(tFacB), facB)
  expect_equal(class(factorValues(facC)), 'numeric')
  expect_equal(length(factorValues(facC)), 0)
})

test_that("sortLevels", {
  sFac <- sortLevels(tFacA)
  expect_equal(factorValues(sFac), facA)
  expect_equal(as.numeric(levels(sFac)), c(1:5))
  sFac <- sortLevels(facC)
  expect_equal(class(sFac), 'factor')
  expect_equal(length(sFac), 0)
})

test_that("trimLevels", {
  sFac <- trimLevels(tFacA)
  expect_equal(factorValues(sFac), facA)
  expect_equal(as.numeric(levels(sFac)), c(3,5,1,2))
  sFac <- trimLevels(facC)
  expect_equal(class(sFac), 'factor')
  expect_equal(length(sFac), 0)
})

test_that("mergeLevels", {
  sFac <- mergeLevels(tFacA,tFacB)
  expect_equal(factorValues(sFac), facA)
  expect_equal(as.numeric(levels(sFac)), sort(c(lvlA,lvlB)))
  sFac <- mergeLevels(tFacA,facC)
  expect_equal(sFac, sortLevels(tFacA))
})

test_that("fx2vars", {
  expect_error(fx2vars(),"fx2vars: needs some data; supply either a formula, x and y, and/or a list of names.")
  fx <- x <- y <- NULL; fx2vars(formula('a~b+c'),x,y);                                   expect_equal(x,c('b','c')); expect_equal(y,'a')
  fx <- x <- y <- NULL; fx2vars(fx,c('b','c'),'a');                                      expect_equal(fx,formula('a~b+c'))
  fx <- x <- y <- NULL; fx2vars(fx,x,y,names=c('a','b','c'));                            expect_equal(fx,formula('a~b+c')); expect_equal(x,c('b','c')); expect_equal(y,'a')

  fx <- x <- y <- NULL; expect_error(fx2vars(fx,c('b','c'),'a',names=c('a','b')),        "fx2vars: a column specified for x does not occur in the dataset.")
  fx <- x <- y <- NULL; expect_error(fx2vars(formula('a~b+c'),x,y,names=c('a','b')),     "fx2vars: a column specified for x does not occur in the dataset.")
  fx <- x <- y <- NULL; expect_error(fx2vars(formula('a+d~b+c'),x,y),                    "fx2vars: multivariate analysis not supported; specify only a single variable for y.")
  fx <- x <- y <- NULL; expect_error(fx2vars(fx,c('b','c'),c('a','d')),                  "fx2vars: multivariate analysis not supported; specify only a single variable for y.")
  fx <- x <- y <- NULL; expect_error(fx2vars(formula('d~b+c'),x,y,names=c('a','b','c')), "fx2vars: variable specified for y does not occur in the dataset; check the column names.")
  fx <- x <- y <- NULL; expect_error(fx2vars(fx,c('b','c'),'d',names=c('a','b','c')),    "fx2vars: variable specified for y does not occur in the dataset; check the column names.")
  fx <- x <- y <- NULL; expect_error(fx2vars(fx,x,'a'),                                  "fx2vars: cannot find a suitable default for x variables; provide fx and/or names.")
  fx <- x <- y <- NULL; expect_error(fx2vars(fx,c('b','d'),'a',names=c('a','b','c')),    "fx2vars: a column specified for x does not occur in the dataset.")
  fx <- x <- y <- NULL; expect_error(fx2vars(fx,c('a','b','c'),'a'),                     "fx2vars: x cannot contain the y variable.")
  fx <- x <- y <- NULL; expect_error(fx2vars(formula('a~a+b+c'),x,y),                    "fx2vars: x cannot contain the y variable.")
})
