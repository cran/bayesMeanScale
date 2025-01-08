\name{bayesMargCompareF}
\alias{bayesMargCompareF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparison of Bayesian marginal effects 
}
\description{
Tests the differences between all marginal effects in the "bayesmeanscale_marg" object that is passed to it. This is particularly useful for testing non-linear interaction effects.
}
\usage{
bayesMargCompareF(marg_list,
                  ci           = 0.95,
                  hdi_interval = TRUE,
                  centrality   = 'mean',
                  digits       = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marg_list}{
An object of class "bayesmeanscale_marg."
}
  \item{ci}{
The level of the credible interval.
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
}
\value{
A list of class "bayesmeanscale_margcompare" with the following components: 

\item{diffTable}{
summary table of the differences of the marginal effects
}
\item{diffDraws}{
posterior draws of the differences of the marginal effects
}

}
\details{
Differences of marginal effects contained in the "bayesmeanscale_marg" object are computed.
}
\author{
David Dalenberg
}
\references{
Mize, Trenton D. 2019. "Best Practices for Estimating, Interpreting, and Presenting Non-linear Interaction Effects." \emph{Sociological Science} 6: 81-117. 
}
\examples{

\donttest{

## Logit model ##

if(require(rstanarm)){

m1 <- stan_glm(switch ~ dist + educ + arsenic + assoc, 
               data    = rstanarm::wells, 
               family  = binomial, 
               refresh = 0,
               iter    = 500)

m1Marg <- bayesMargEffF(m1, 
                        marginal_effect = 'arsenic', 
                        start_value     = 2.2, 
                        end_value       = .82, 
                        at              = list(educ=c(0, 5)),
                        n_draws         = 500)

bayesMargCompareF(m1Marg)

}

}

}
