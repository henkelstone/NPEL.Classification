# Automated testing of the 'util.R' file in the NPEL package
# Created by Jonathan Henkelman 27.Jan.2016

library(NPEL.Classification)
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
