\name{bayesOrdinalPredsF}
\alias{bayesOrdinalPredsF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian predictions for ordinal models on the mean scale.
}
\description{
Computes Bayesian outcome predictions for cumulative link models with a proportional odds structure fit using the package "rstanarm". Predictions can be averaged over the values of the covariates in the data (average marginal predictions), or the covariates can be held at their means (marginal predictions at the means). Also, "at" values must be specified to fix at least one covariate at particular values.
}
\usage{
bayesOrdinalPredsF(model,
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
A model object of class "stanreg" and "polr."
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
A list of class "bayes_mean_scale_pred" with the following components: 

\item{predTable}{
summary table of the predictions
}
\item{predDraws}{
posterior draws of the predictions
}

}
\details{
Currently, the following links for fixed-effect ordinal models fit using "rstanarm" are supported: logit, probit, and clogclog.
}
\author{
David Dalenberg
}
\references{
Agresti, Alan. 2013. \emph{Categorical Data Analysis}. Third Edition. New York: Wiley

Long, J. Scott and Sarah A. Mustillo. 2018. "Using Predictions and Marginal Effects to Compare Groups in Regression Models for Binary Outcomes." \emph{Sociological Methods & Research} 50(3): 1284-1320.

Mize, Trenton D. 2019. "Best Practices for Estimating, Interpreting, and Presenting Non-linear Interaction Effects." \emph{Sociological Science} 6: 81-117. 
}
\examples{

\donttest{

## Proportional odds logistic regression ##

if(require(rstanarm) & require(MASS)){

m1 <- stan_polr(Sat ~ Infl + Type, 
                data    = MASS::housing, 
                prior   = rstanarm::R2(0.2, 'mean'), 
                refresh = 0, 
                iter    = 500)

# marginal predictions holding covariates at means #

bayesOrdinalPredsF(m1, 
                   at       = list(Type = c("Tower", "Apartment")), 
                   at_means = TRUE,
                   n_draws  = 500)
 
}           
}

}


