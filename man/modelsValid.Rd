% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{modelsValid}
\alias{modelsValid}
\title{Generate validation statistics for a list of models; a wrapper for \code{validate}.}
\usage{
modelsValid(models, valid, ...)
}
\arguments{
\item{models}{is either a list of model objects on which to find error statistics, or a single model to evaluate.}

\item{valid}{a validation dataset with which to test the accuracy of the models provided.}

\item{...}{(optional) other parameters to pass to \code{\link{classAcc}} and/or \code{\link{npelVIMP}}.}
}
\value{
As with \code{\link{modelAccs}}, this function returns different values depending on whether the model is categorical or
  continuous. For categorical data it returns a named list of accuracy statistics:
  \itemize{
    \item \code{confMatrix} = confusion matrix
    \item \code{userAcc} = user accuracy
    \item \code{prodAcc} = producer accuracy
    \item \code{overallAcc} = overall accuracy
    \item \code{kappa} = kappa
  }
For continuous data:
  \itemize{
    \item \code{overallAcc} = overall r-squared
    \item \code{mse} = mean squared error
  }
}
\description{
Given a list of models, this function runs \code{\link{validate}} on each. See details for a description of how the data is encapsulated
for return.
}
\examples{
# With categorical data
data ('siteData')
gen <- sample(1:nrow(siteData),floor(nrow(siteData)*0.5))
modelRun <- generateModels(data = siteData[sort(gen),],
                           modelTypes = suppModels,
                           x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                           y = 'ecoType',
                           grouping = ecoGroup[['identity','transform']],
                           echo = FALSE)
valid <- siteData[-gen,]
valid$ecoType <- as.factor(ecoGroup[['identity','transform']][valid$ecoType])
mV <- modelsValid(modelRun,valid)
str(mV,2)

# With continuous data
gen <- sample(1:nrow(siteData),floor(nrow(siteData)*0.5))
modelRun <- generateModels(data = siteData[sort(gen),],
                           modelTypes = contModels,
                           x = c('brtns','grnns','wetns','dem','slp','asp','hsd'),
                           y = 'easting',
                           echo = FALSE)
valid <- siteData[-gen,]
mV <- modelsValid(modelRun,valid)
str(mV,2)
}
\seealso{
\code{\link{classAcc}} for more on the computation of continuous and categorical accuracies, and \code{\link{modelAccs}} for the
  corresponding function that will compute accuracy based on the dataset used for model building.
}
