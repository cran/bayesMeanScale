\name{bayesPredsF}
\alias{bayesPredsF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian predictions on the mean scale.
}
\description{
Computes Bayesian predictions on the mean scale for models fit using the package "rstanarm". Predictions can be averaged over the values of the covariates in the data (average marginal predictions), or the covariates can be held at their means (marginal predictions at the means). Also, "at" values must be specified to fix at least one covariate at particular values.
}
\usage{
bayesPredsF(model,
            at,
            n_draws      = 2000,
            ci           = .95,
            hdi_interval = TRUE,
            centrality   = 'mean',
            digits       = 4,
            at_means     = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model object of class "stanreg."
}
\item{at}{
List of covariate values to estimate the predictions at.
}
\item{n_draws}{
The number of draws to take from the posterior distribution of the expectation.
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
Currently, the following families of fixed-effect models fit using "rstanarm" are supported: 'beta', 'binomial', 'Gamma', 'gaussian', 'neg_binomial_2', and 'poisson.' Future versions of the package will contain support for additional models.
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

## Logit model ##

m1 <- rstanarm::stan_glm(switch ~ dist + educ + arsenic + assoc, 
                         data    = rstanarm::wells, 
                         family  = binomial, 
                         refresh = 0, 
                         iter    = 500)

# marginal predictions holding covariates at means #

bayesPredsF(m1, 
            at       = list(arsenic = c(.82, 1.3)), 
            at_means = TRUE,
            n_draws  = 500)
            
}

}


