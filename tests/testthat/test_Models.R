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

##### Test continuous warnings #####
test_that("generateModels", {
  expect_warning( generateModels (data=siteData, modelTypes=suppModels, x=c('brtns','grnns','wetns','dem','slp','asp','hsd'), y='easting', grouping=ecoGroup[['domSpecies','transform']], rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args), "generateModels: cannot group a non-factor variable; ignoring grouping.")
  expect_warning( generateModels (data=siteData, modelTypes=suppModels, x=c('brtns','grnns','wetns','dem','slp','asp','hsd'), y='easting', rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args), "generateModels: fnn.FNN requires categorical data")
})

##### Test categorical data #####
cat ("\n\nTest with categorical data\n")
models <- generateModels (data = NPEL.Classification::siteData,
                          modelTypes = suppModels,
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
models <- generateModels (NPEL.Classification::siteData, contModels,
                          x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                          y = 'easting',
                          rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args)

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

test_that("classAcc", {
  for (i in models) {
    expect_true( !is.null(classAcc(getFitted(i), getData(i)[,'easting'])) )
  }
})

test_that("modelAccs", {
  if (verbose) cat('\n')
  expect_true( !is.null(runA <- modelAccs(models, calc=F, echo=verbose)) )
  expect_true( !is.null(runB <- modelAccs(models, calc=T, echo=verbose)) )
  expect_false( identical(runA,runB) )
})

test_that("validate", {
  for (i in models) {
    expect_true( !is.null(validate(i, getData(i))) )
  }
})

test_that("modelsValid", {
  if (verbose) cat('\n')
  expect_true( !is.null(runA <- modelsValid(models, getData(models[[1]]), echo=verbose)) )
})
