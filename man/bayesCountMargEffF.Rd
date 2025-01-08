\name{bayesCountMargEffF}
\alias{bayesCountMargEffF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian marginal effects on count probabilities.
}
\description{
Computes Bayesian marginal effects on count probabilities. Models must have a family of "poisson"" or "neg_binomial_2" and be fit using the "rstanarm" package. Marginal effects can be averaged over the values of the covariates in the data (average marginal effects), or the covariates can be held at their means (marginal effects at the means). Also, "at" values can be specified to fix covariates at particular values. The marginal effects can be specified in terms of discrete changes or instantaneous rates of change.
}
\usage{
bayesCountMargEffF(model,
                   counts,
                   marginal_effect,
                   start_value,
                   end_value,
                   n_draws         = 2000,
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
A model object of class "stanreg."
}
\item{counts}{
Vector of counts to get the predicted probabilities for.
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
Marginal effects on count probabilities give you useful information to complement post estimation summaries on the mean scale. 
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

# marginal effect holding covariates at their means #

bayesCountMargEffF(poissonModel,
                   counts          = 1,
                   marginal_effect = 'width',
                   start_value     = 25,
                   end_value       = 20,
                   at_means        = TRUE,
                   n_draws         = 500)
                   
}
                   
}

}
