# Automated testing of the 'OOP util.R' file in the NPEL package
# Created by Jonathan Henkelman 28.Jan.2016

library(NPEL.Classification)
context("OOP_util Functions")
cat('\n')

# Setup testing environment
n <- 100; low <- 1; high <- 100; sd <- 30
fx <- formula('y~x1+x2+x3')
func <- function (x1,x2,x3) { x1 + sqrt(x2) + log(x3) }
x1 <- runif (n, low, high)
x2 <- runif (n, low, high)
x3 <- runif (n, low, high)                      # Need three x variables for computing VIMP
y <- round(func(x1,x2,x3) + rnorm (n,0,sd))
y[y<low] <- low
y[y>high] <- high
y <- factor(round (y/10+1))
data <- data.frame(y,x1,x2,x3)

# Simply check the model is created, i.e. the function call doesn't fail, and that it has the correct class. Specify all args as
# generateModels would fill in the blanks anyway.
rf.args  <- list(mtry=floor(sqrt(2)), importance='permute', na.action='na.omit', proximity=F)
nn.args  <- list(k=3, kmax=7, kernel=c('rectangular','optimal'), scale=F)
gbm.args <- list(distribution='multinomial', n.trees=1000, keep.data=TRUE)
#svm.args <- list(scale=F, probability=T)
args <- list('rF'=rf.args,'rFSRC'=rf.args,'fnn.FNN'=nn.args,'fnn.class'=nn.args,'kknn'=nn.args,'gbm'=gbm.args) # A lookup list for matching arguments with classes as in generateModels
test_that("buildModel", {
  print("Building models: ")
  for (i in c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm')) {
    print (paste0(i,'...'))
    ( i %in% class(buildModel(i,data,fx,args[[i]])) )
  }
})

testRun <- generateModels ( data, modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'), # Add svm when it works ???
                            fx=formula('y~x1+x2+x3'), grouping = ecoGroup[['identity','transform']], echo=F,
                            nn.args=list(kernel=c('rectangular','optimal'), scale=F) )
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
    cat ('\nTesting VIMP for model: ',class(i)[[1]])
    vimp <- getVIMP(i)
    expect_equal(class(vimp), 'data.frame')
    expect_equal(dim(vimp), c(3,length(levels(y))+1+ifelse(class(i)[[1]]=='gbm',1,0)))
    expect_equal(colnames(vimp)[-1], c(levels(y),if(class(i)[[1]]=='gbm') 'rel.inf'))
    expect_equal(rownames(vimp), c('x1','x2','x3'))
  }
  tmp <- generateModels(data[,1:3],c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),nn.args=list(scale=F),echo=F)
  expect_error(getVIMP(tmp[['fnn.FNN']]),'Error: not able to compute post hoc VIMP on models with only two variables.')
})

# Cannot test get fitted with a random dataset... it would use the same code as the getFitted function! Just print the results, what type, and the error from the method. Can we do better ???
cat('\nChecking fitted\n')
test_that("getFitted", {
  cat('Y: ',factorValues(y),'\n')
  for (i in testRun) {
    tmp <- getFitted(i)
    print (paste0('Class: ',class(i)[[1]],'; Accuracy:',classAcc(tmp,y)[[4]]), digits=2 )
    expect_equal(class(tmp), 'factor')
    attributes(tmp) <- NULL
    cat('Y:',factorValues(tmp),'\n')
  }
})

# Again, cannot test with random dataset... just make sure the code executes and that the function returned executes. Can we do better ???
models <- generateModels (data, modelTypes = c('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
                          fx=formula('y~x1+x2+x3'), grouping = ecoGroup[['identity','transform']], echo=F)
testthat::test_that("buildPredict", {
  print("Building prediction functions: ")
  for (i in models) {
    print (paste0(class(i)[1],'...'))
    buildPredict(i)(i,data)
  }
})
