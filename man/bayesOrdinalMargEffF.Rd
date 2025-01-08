\name{bayesOrdinalMargEffF}
\alias{bayesOrdinalMargEffF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian marginal effects on the mean scale.
}
\description{
Computes Bayesian marginal effects on the probability of outcomes for cumulative link models with a proportional odds structure fit using the package "rstanarm". Marginal effects can be averaged over the values of the covariates in the data (average marginal effects), or the covariates can be held at their means (marginal effects at the means). Also, "at" values can be specified to fix covariates at particular values. The marginal effects can be specified in terms of discrete changes or instantaneous rates of change.
}
\usage{
bayesOrdinalMargEffF(model,
                     n_draws         = 2000,
                     marginal_effect,
                     start_value,
                     end_value,
                     ci              = .95,
                     hdi_interval    = TRUE,
                     centrality      = 'mean',
                     digits          = 4,
                     at              = NULL,
                     at_means        = FALSE,
                     h               = .0001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
A model object of class "stanreg" and "polr."
}
\item{n_draws}{
The number of draws to take from the joint posterior distribution.
}
\item{marginal_effect}{
A character vector of terms that you want to estimate the marginal effect for.
}
\item{start_value}{
The start value(s) for the marginal effect(s). Must be a list when specifying multiple values. Specify "instantaneous" when you want to calculate an approximate instantaneous rate of change.
}

\item{end_value}{
The end value(s) for the marginal effect(s). Must be a list when specifying multiple values. Specify "instantaneous" when you want to calculate an approximate instantaneous rate of change.
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
\item{at}{
Optional list of covariate values to estimate the marginal effect at.
}
\item{at_means}{
If FALSE, the default, the marginal effects are averaged across the rows of the data. If TRUE, the covariate values that are not specified in the "at" argument are held at their means.
}
\item{h}{
Step size for calculating the approximate instantaneous rate of change.
}
}
\value{
A list of class "bayesmeanscale_marg" with the following components: 

\item{diffTable}{
summary table of the marginal effects
}
\item{diffDraws}{
posterior draws of the marginal effects
}

}
\details{
The following links for fixed-effect ordinal models fit using "rstanarm" are supported: logit, probit, and clogclog.
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

# marginal effects holding covariates at means #

bayesOrdinalMargEffF(m1,
                     marginal_effect = "Infl",
                     start_value     = "Low",
                     end_value       = "High",
                     at              = list(Type = c("Tower", "Apartment")), 
                     at_means        = TRUE,
                     n_draws         = 500)
                     
}
}

}
