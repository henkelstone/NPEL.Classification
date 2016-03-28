# Automated testing of the 'OOP util.R' file in the NPEL package
# Created by Jonathan Henkelman 28.Jan.2016

library(NPEL.Classification)
library(testthat)
context("OOP_util Functions")
cat('\n')
verbose = T

##### Setup testing environment #####
n <- 100; low <- 1; high <- 100; sd <- 30
func <- function (x1,x2,x3) { x1 + sqrt(x2) + log(x3) }
fx <- formula('y~x1+x2+x3')
x1 <- runif (n, low, high)
x2 <- runif (n, low, high)
x3 <- runif (n, low, high)

rf.args  <- list(mtry=floor(sqrt(2)), importance='permute', na.action='na.omit', proximity=F)
nn.args  <- list(k=3, kmax=7, kernel=c('rectangular','optimal'), scale=F)
gbm.args <- list(n.trees=1000, keep.data=TRUE)
#svm.args <- list(scale=F, probability=T)
args <- list('randomForest' = rf.args,
             'rfsrc'        = rf.args,
             'fnn.FNN'      = nn.args,
             'fnn.class'    = nn.args,
             'kknn'         = nn.args,
             'gbm'          = gbm.args)

##### Test isCat/isCont #####
test_that("isCat/isCont", {
  expect_true(isCat(siteData$ecoType))
  expect_false(isCat(siteData$easting))
  expect_warning(isCat(siteData),regexp="isCat: expects only a single column; running test on the first column")
  expect_false(isCont(siteData$ecoType))
  expect_true(isCont(siteData$easting))
  expect_warning(isCont(siteData),regexp="isCont: expects only a single column; running test on the first column")
})

##### Test with categorical data #####
y <- round(func(x1,x2,x3) + rnorm (n,0,sd))
y[y<low] <- low
y[y>high] <- high
y <- factor(round (y/10+1))

fx <- formula('y~x1+x2+x3')
data <- data.frame(y,x1,x2,x3)

# Just check buildModel actually works and returns the correct type
test_that("buildModel", {
  if (verbose) print("Building models for categorical data: ")
  for (i in suppModels) {
    if (verbose) print (paste0(i,'...'))
    ( i %in% class(buildModel(i,data,fx,args[[i]])) )
  }
})

testRun <- generateModels(data=data, modelTypes=suppModels,fx=fx, grouping=ecoGroup[['identity','transform']], echo=F, rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args)
test_that("isCat/isCont", {
  for (i in testRun) {
    expect_true (isCat(i))
    expect_false(isCont(i))
  }
})
test_that("getData", {
  for (i in testRun) {
    expect_equivalent(getData(i),data)
    expect_equal(class(getData(i)),'data.frame')
  }
})
test_that("getClasses", {
  for (i in testRun) {
    expect_equal(getClasses(i),levels(y))
    expect_equal(class(getClasses(i)),'character')
  }
})
test_that("getProb", {
  # Cannot test with a random dataset... just make sure it runs without an error.
  for (i in testRun) {
    getProb(i)
  }
})
test_that("getFormula", {
  for (i in testRun) {
    expect_equal(getFormula(i),fx)
    expect_equal(class(getFormula(i)),'formula')
  }
})
test_that("getArgs", {
  for (i in length(testRun)) {
    expect_equal(getArgs(testRun[[i]]),args[[i]])
    expect_equal(class(getArgs(testRun[[i]])),'list')
  }
})
test_that("getVIMP", {
  for (i in testRun) {
    if (verbose) cat ('\nTesting VIMP for model: ',class(i)[[1]])
    vimp <- getVIMP(i)
    expect_equal(class(vimp), 'data.frame')
    expect_equal(dim(vimp), c(3,length(levels(y))+1+ifelse(class(i)[[1]]=='gbm',1,0)))
    expect_equal(colnames(vimp)[-1], c(levels(y),if(class(i)[[1]]=='gbm') 'rel.inf'))
    expect_equal(rownames(vimp), c('x1','x2','x3'))
  }
  tmp <- generateModels(data[,1:3],suppModels,nn.args=list(scale=F),echo=F)
  expect_error(getVIMP(tmp[['fnn.FNN']]),'npelVIMP: not able to compute post hoc VIMP on models with only two variables.')
})

# Most of the model types cannot be compared by dropping the same data down the model:
#   randomForest, rfsrc: both return the original y data, whereas for getFitted they return a leave-one-out cross-validation dataset
#   FNN: can be compared, but must be handled manually as it is the second nearest neighbour that will be closest
#   class: cannot be compared for a similar reason to randomForest/rfsrc, but does not return the index matrix and so can't be done manually
#   kknn: as for class
#   gbm: can be compared

test_that("getFitted and buildPredict", {
  if (verbose) cat('\nChecking getFitted and buildPredict')
  for (i in testRun) {
    gF <- getFitted(i)
    expect_equal(class(gF), 'factor')
    if (verbose) cat ('\nClass: ',class(i)[[1]],'; Accuracy:',format(classAcc(gF,y)[[4]], digits=2))
    bP <- buildPredict(i)(i,getData(i))

    if (class(i)[[1]] %in% c('randomForest','rfsrc')) {
      expect_equal(prob2class(bP), y)                           # The best value from bP should be the original data...
    } else if (class(i)[[1]] %in% 'fnn.FNN') {
      expect_equal(y[attr(bP,'nn.index')[,1]], y)               # The best value from bP should be the original data...
      expect_equal(y[attr(bP,'nn.index')[,2]], getFitted(i))    # ... but the fitted data from the original model should be the second nearest neighbour in bP
    } else if (class(i)[[1]] %in% 'fnn.class') {
    } else if (class(i)[[1]] %in% 'kknn') {
    } else if (class(i)[[1]] %in% 'gbm') {
      expect_equivalent(prob2class(bP), gF)
    }
  }
})

# Test whether predict still works with scaling
nn.args  <- list(k=3, kmax=7, kernel=c('rectangular','optimal'), scale=T)
testRun <- generateModels(data=data, modelTypes=c('fnn.FNN','fnn.class','kknn'),fx=fx, grouping=ecoGroup[['identity','transform']], nn.args=nn.args)
nn.args  <- list(k=3, kmax=7, kernel=c('rectangular','optimal'), scale=F)
test_that("buildPredict", {
  if (verbose) cat("\nCheck buildPredict with scaling: ")
  for (i in testRun) {
    gF <- getFitted(i)
    expect_equal(class(gF), 'factor')
    if (verbose) cat ('\nClass: ',class(i)[[1]],'; Accuracy:',format(classAcc(gF,y)$overallAcc, digits=2))
    bP <- buildPredict(i)(i,getData(i))

    if (class(i)[[1]] %in% 'fnn.FNN') {
      expect_equal(y[attr(bP,'nn.index')[,1]], y)               # The best value from bP should be the original data...
      expect_equal(y[attr(bP,'nn.index')[,2]], getFitted(i))    # ... but the fitted data from the original model should be the second nearest neighbour in bP
    } else if (class(i)[[1]] %in% 'fnn.class') {
    } else if (class(i)[[1]] %in% 'kknn') {
    }
  }
})

##### Test with continuous data #####
y <- func(x1,x2,x3) + rnorm (n,0,sd)
data <- data.frame(y,x1,x2,x3)

# Just check buildModel actually works and returns the correct type
test_that("buildModel", {
  if (verbose) cat("\n\nBuilding models for continuous data: ")
  for (i in contModels) {
    if (verbose) print (paste0(i,'...'))
    ( i %in% class(buildModel(i,data,fx,args[[i]])) )
  }
})

testRun <- generateModels(data=data, modelTypes=contModels,fx=fx, echo=F, rf.args=rf.args, nn.args=nn.args, gbm.args=gbm.args)
test_that("isCat/isCont", {
  for (i in testRun) {
    expect_true (isCont(i))
    expect_false(isCat(i))
  }
})
test_that("getData", {
  for (i in testRun) {
    expect_equivalent(getData(i),data)
    expect_equal(class(getData(i)),'data.frame')
  }
})
test_that("getClasses", {
  for (i in testRun) {
    expect_error(getClasses(i))
  }
})
test_that("getProb", {
  for (i in testRun) {
    expect_error(getProb(testRun[[i]]))
  }
})
test_that("getFormula", {
  for (i in testRun) {
    expect_equal(getFormula(i),fx)
    expect_equal(class(getFormula(i)),'formula')
  }
})
test_that("getArgs", {
  for (i in length(testRun)) {
    expect_equal(getArgs(testRun[[i]]),args[[class(testRun[[i]])[1]]],label=class(testRun[[i]]))
    expect_equal(class(getArgs(testRun[[i]])),'list')
  }
})
# test_that("getVIMP", {
#   for (i in testRun) {
#     if (verbose) cat ('\nTesting VIMP for model: ',class(i)[[1]])
#     vimp <- getVIMP(i)
#     expect_equal(class(vimp), 'data.frame')
#     expect_equal(dim(vimp), c(3,length(levels(y))+1+ifelse(class(i)[[1]]=='gbm',1,0)))
#     expect_equal(colnames(vimp)[-1], c(levels(y),if(class(i)[[1]]=='gbm') 'rel.inf'))
#     expect_equal(rownames(vimp), c('x1','x2','x3'))
#   }
#   tmp <- generateModels(data[,1:3],suppModels,nn.args=list(scale=F),echo=F)
#   expect_error(getVIMP(tmp[['fnn.FNN']]),'Error: not able to compute post hoc VIMP on models with only two variables.')
# })

# Cannot test getFitted with a random dataset... it would use the same code as the getFitted function! Just print the results, what type, and the error from the method. Can we do better???
test_that("getFitted", {
  if (verbose) cat('\nChecking fitted\n')
  for (i in testRun) {
    tmp <- getFitted(i)
    if (verbose) print (paste0('Class: ',class(i)[[1]],'; Accuracy:',classAcc(tmp,y)$overallAcc), digits=2 )
    expect_equal(class(tmp), 'numeric',info=class(i)[[1]])
    attributes(tmp) <- NULL
    # if (verbose) cat('Y:',factorValues(tmp),'\n')
  }
})

testthat::test_that("buildPredict", {
  if (verbose) print("Building prediction functions: ")
  for (i in testRun) {
    if (verbose) print (paste0(class(i)[1],'...'))
    buildPredict(i)(i,data)
  }
})
