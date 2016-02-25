# Automated testing of 'rwTile.R' in the NPEL package
# Created by Jonathan Henkelman 4.Feb.2016

library(NPEL.Classification)
context("rwTile Functions")
cat('\n')

# Setup testing environment
data('egTile')
data('siteData')
models <- generateModels (
  data = siteData,
  modelTypes = c ('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
  x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
  y = 'ecoType',
  grouping = ecoGroup[['domSpecies','transform']],
  nn.args = list (scale=F),
  gbm.args = list (interaction.depth=7, shrinkage=0.005, cv.folds=0) )

# Test, can the input be either a stack or a brick???
test_that("writeTile(s)", {
  expect_error(writeTiles (egTile, models, 'Delete_', layers=c('class','prob','threshold'), echo=F), "Threshold must be specified between but not equal to 0 and 1.")
  expect_error(writeTiles (egTile, models, 'Delete_', layers=c('all'), labels.all=c('a','b'), echo=F), "Error: number of labels provided does not match the number of output classes.")
  expect_error(writeTiles (egTile, models, 'Delete_', '/Oops/', layers=c('all'), labels.all=c('a','b'),echo=F), regexpxp="Error: Not able to write .*")
#   writeTiles ('./', 'Delete_', '.tif', egTile, list(models$rFSRC), layers=c('class','prob','threshold','all'), threshold=0.5) # Should work
#   unlink ('./Delete_rfsrc.tif')
})

test_that("readTile", {
  path <- paste0(system.file("extdata", "egTile", package = "NPEL.Classification"),'/')
  expect_true(!is.null( readTile(path, c('base','grnns','wetns','brtns','dem','slp','asp','hsd')) ))
})

vData <- maptools::readShapePoints ('../../data-raw/Plots/Plots')
data (egTile)
test_that("extractPoints", {
  expect_true(!is.null( extractPoints(egTile, vData, c('EASTING','NORTHING')) ))
  expect_true(!is.null( extractPoints(egTile[[1]], vData, c('EASTING','NORTHING')) ))              # Should work on a layer
  expect_true(!is.null( extractPoints(raster::brick(egTile), vData, c('EASTING','NORTHING')) ))    # Should work on a brick
  expect_true(!is.null( extractPoints(egTile, as.data.frame(vData), c('EASTING','NORTHING')) ))    # Should work with points in a data frame
})
