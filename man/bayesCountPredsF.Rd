\name{bayesCountPredsF}
\alias{bayesCountPredsF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian predictions of count probabilities.
}
\description{
Computes Bayesian predictions for count probabilities. Models must have a family of "poisson"" or "neg_binomial_2" and be fit using the "rstanarm" package. Predictions can be averaged over the values of the covariates in the data (average marginal predictions), or the covariates can be held at their means (marginal predictions at the means). Also, "at" values must be specified to fix at least one covariate at particular values.
}
\usage{
bayesCountPredsF(model,
                 counts,
                 at,
                 n_draws      = 2000,
                 ci           = .95,
                 hdi_interval = TRUE,
                 centrality   = 'mean',
                 digits       = 4,
                 at_means     = FALSE,
                 data_slice   = 'full')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model object of class "stanreg."
}
\item{counts}{
Vector of counts to get the predicted probabilities for.
}
\item{at}{
List of covariate values to estimate the predictions at.
}
\item{n_draws}{
The number of draws to take from the joint posterior distribution.
}
\item{ci}{
The level for the credible intervals.
}
\item{hdi_interval}{
If TRUE, the default, computes the highest density credible interval. If FALSE, computes the equal-tailed interval.
}
\item{centrality}{
Centrality measure for the posterior distribution. Options are "mean" or "median".
}
\item{digits}{
The number of digits to report in the summary table.
}
\item{at_means}{
If FALSE, the default, the predictions are averaged across the rows of the model data for each unique combination of "at" values. If TRUE, the covariate values that are not specified in the "at" argument are held at their means.
}
\item{data_slice}{
The number of rows of data to average over for the predictions. Defaults to all rows. This can be useful for very large data sets. 
}
}
\value{
A list of class "bayesmeanscale_pred" with the following components: 

\item{predTable}{
summary table of the predictions
}
\item{predDraws}{
posterior draws of the predictions
}
}
\details{
Predicted count probabilities give you useful information to complement post estimation summaries of the mean. 
}
\author{
David Dalenberg
}
\references{

Long, J. Scott and Jeremy Freese. 2001. "Predicted Probabilities for Count Models." \emph{Stata Journal} 1(1): 51-57.
 
}
\examples{

\donttest{

if(require(rstanarm)){

crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", 
                    header = TRUE)

poissonModel  <- stan_glm(sat ~ weight + width, 
                          data    = crabs, 
                          family  = poisson, 
                          refresh = 0,
                          iter    = 500)

bayesCountPredsF(poissonModel,
                 counts  = c(0,1),
                 at      = list(weight=c(2,3)),
                 n_draws = 500)
                 
}
                 
}

}


