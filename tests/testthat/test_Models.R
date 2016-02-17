# Automated testing of 'Models.R' in the NPEL package
# Created by Jonathan Henkelman 16.Feb.2016

library(NPEL.Classification)
context("Models Functions")
cat('\n')

data('siteData')
models <- generateModels (
  data = siteData,
  modelTypes = c ('rF','rFSRC','fnn.FNN','fnn.class','kknn','gbm'),
  x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
  y = 'ecoType',
  grouping = ecoGroup[['domSpecies','transform']],
  nn.args = list (scale=F),
  gbm.args = list (interaction.depth=7, shrinkage=0.005, cv.folds=0) )

# Simply check and see that they all run without an error
for (i in models) {
  expect_true( !is.null(classAcc(getFitted(i), getData(i)[,'ecoType'])) )
}

for (i in models) {
  expect_true( !is.null(npelVIMP(i)) )
}

for (i in models) {
  expect_true( !is.null(npelVIF(getFormula(i),getData(i))) )
}

cat('\n')
expect_true( !is.null(modelAccs(models)) )

