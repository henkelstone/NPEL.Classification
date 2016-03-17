# Automated testing of 'Models.R' in the NPEL package
# Created by Jonathan Henkelman 16.Feb.2016

library(NPEL.Classification)
context("Models Functions")
verbose = T
cat('\n')

##### Setup model environment #####
rf.args  <- list(mtry=floor(sqrt(7)), importance='permute', na.action='na.omit', proximity=F)
nn.args  <- list(k=3, kmax=7, kernel=c('rectangular','optimal'), scale=F)
gbm.args <- list(n.trees=1000, keep.data=TRUE)
#svm.args <- list(scale=F, probability=T)
data('siteData')

##### Test continuous groupings fail #####
test_that("generateModels", {           # Cannot use grouping with continuous variables
  expect_error( generateModels (data = siteData,
                                modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
                                x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                                y = 'easting',
                                grouping = ecoGroup[['domSpecies','transform']],
                                rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args) )
})

##### Test categorical data #####
cat ("\n\nTest with categorical data\n")
models <- generateModels (data = siteData,
                          modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
                          x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                          y = 'ecoType',
                          grouping = ecoGroup[['domSpecies','transform']],
                          rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args)

test_that("classAcc", {
  for (i in models) {
    expect_true( !is.null(classAcc(getFitted(i), getData(i)[,'ecoType'])) )
  }
})

# test_that("npelVIMP", {
#   for (i in models) {
#     cat('\n',class(i),'\n')
#     expect_true( !is.null(runA <- npelVIMP(i,calc=F)) )
#     expect_true( !is.null(runB <- npelVIMP(i,calc=T)) )
#     if ( any(class(i) %in% c('randomForest','rfsrc','gbm')) ) { expect_false( identical(runA,runB) ) }
#     else { expect_true( identical(runA,runB) ) }
#   }
# })

test_that("npelVIF", {
  for (i in models) {
    expect_true( !is.null(npelVIF(getFormula(i),getData(i))) )
  }
})

test_that("modelAccs", {
  if (verbose) cat('\n')
  expect_true( !is.null(runA <- modelAccs(models,calc=F)) )
  expect_true( !is.null(runB <- modelAccs(models,calc=T)) )
  expect_false( identical(runA,runB) )
})

##### Test continuous data #####
cat ("\n\nTest with continuous data\n")
rf.args  <- list(mtry=floor(7/3), importance='permute', na.action='na.omit', proximity=F)
models <- generateModels (data = siteData, modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
                          x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                          y = 'easting',
                          rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args)

test_that("classAcc", {
  for (i in models) {
    expect_true( !is.null(classAcc(getFitted(i), getData(i)[,'easting'])) )
  }
})

test_that("npelVIMP", {
  for (i in models) {
    expect_true( !is.null(runA <- npelVIMP(i,calc=F)) )
    expect_true( !is.null(runB <- npelVIMP(i,calc=T)) )
    if (any(class(i) %in% c('randomForest','rfsrc','gbm'))) { expect_false( identical(runA,runB) )
    } else { expect_true( identical(runA,runB) ) }
  }
})

test_that("npelVIF", {
  for (i in models) {
    expect_true( !is.null(npelVIF(getFormula(i),getData(i))) )
  }
})

test_that("modelAccs", {
  if (verbose) cat('\n')
  expect_true( !is.null(runA <- modelAccs(models,calc=F)) )
  expect_true( !is.null(runB <- modelAccs(models,calc=T)) )
  expect_false( identical(runA,runB) )
})
