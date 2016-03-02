# Automated testing of 'Models.R' in the NPEL package
# Created by Jonathan Henkelman 16.Feb.2016

library(NPEL.Classification)
context("Models Functions")
verbose = T
cat('\n')

##### Setup model environment #####
rf.args  <- list(mtry=floor(sqrt(2)), importance='permute', na.action='na.omit', proximity=F)
nn.args  <- list(k=3, kmax=7, kernel=c('rectangular','optimal'), scale=F)
gbm.args <- list(distribution='multinomial', n.trees=1000, keep.data=TRUE)
#svm.args <- list(scale=F, probability=T)
args <- list('rF'        = rf.args,
             'rFSRC'     = rf.args,
             'fnn.FNN'   = nn.args,
             'fnn.class' = nn.args,
             'kknn'      = nn.args,
             'gbm'       = gbm.args)
rm (rf.args, nn.args, gbm.args)
data('siteData')

##### Test continuous groupings fail #####
test_that("generateModels", {           # Cannot use grouping with continuous variables
  expect_error( generateModels (data = siteData,
                                modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
                                x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                                y = 'easting',
                                grouping = ecoGroup[['domSpecies','transform']],
                                eval(args)) )
})


##### ??? Delete #####
siteData$ecoType <- trimLevels(siteData$ecoType)
testing <- c('ecoType','easting')[2]
test <- generateModels (
  data = siteData,
  modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm','svm')[],
  x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
  y = testing,
  eval(args))

tmp <- test[[1]]
test <- buildPredict(tmp)
prob <- test(tmp,siteData)
prob <- as.numeric(getClasses(tmp)[apply(prob,1,which.max)])
plot(prob,factorValues(siteData[,testing]))
plot(factorValues(getFitted(tmp)),factorValues(siteData[,testing]))


##### Test categorical data #####
models <- generateModels (data = siteData, modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'), eval(args),
                          x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                          y = 'ecoType',
                          grouping = ecoGroup[['domSpecies','transform']])

test_that("classAcc", {
  for (i in models) {
    expect_true( !is.null(classAcc(getFitted(i), getData(i)[,'ecoType'])) )
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

##### Test continuous data #####
models <- generateModels (data = siteData, modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'), eval(args),
                          x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                          y = 'easting')

test_that("classAcc", {
  for (i in models) {
    expect_true( !is.null(classAcc(getFitted(i), getData(i)[,'ecoType'])) )
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
