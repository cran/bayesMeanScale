## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  echo     = T,
  eval     = T,
  include  = T
)

## ----results='hide', message=F------------------------------------------------

lapply(c('bayesMeanScale', 'dplyr', 'rstanarm', 'bayestestR', 'flextable'), function(x) base::library(x, character.only=T))


## -----------------------------------------------------------------------------

# Simulate the data #

data(wells)

modelData <- wells %>%
  mutate(assoc = if_else(assoc==1, 'Y', 'N'))

binomialModel <- stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, 
                          data    = modelData, 
                          family  = binomial, 
                          refresh = 0)


## -----------------------------------------------------------------------------

bayesPredsF(binomialModel, 
            at = list(arsenic = c(.82, 1.3, 2.2), assoc=c("Y", "N")))



## -----------------------------------------------------------------------------

bayesPredsF(binomialModel, 
            at       = list(arsenic = c(.82, 1.3, 2.2), assoc=c("Y", "N")), 
            at_means = T)


## -----------------------------------------------------------------------------

crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)

poissonModel  <- stan_glm(sat ~ weight + width, 
                          data    = crabs, 
                          family  = poisson, 
                          refresh = 0)

bayesCountPredsF(poissonModel,
                 counts = c(0,1,2),
                 at     = list(weight=c(2,3,4)))


## -----------------------------------------------------------------------------

binomialAME <- bayesMargEffF(binomialModel,
                             marginal_effect = 'arsenic',
                             start_value     = 2.2,
                             end_value       = .82)

binomialAME
head(binomialAME$diffDraws)


## -----------------------------------------------------------------------------

bayesMargEffF(binomialModel,
              marginal_effect = c('arsenic', 'dist'),
              start_value     = list(2.2, 64.041),
              end_value       = list(.82, 21.117))


## -----------------------------------------------------------------------------

binomialAMEInteraction <- bayesMargEffF(binomialModel,
                                        marginal_effect = 'dist',
                                        start_value     = 64.041,
                                        end_value       = 21.117,
                                        at              = list(educ=c(0, 5, 8)))

binomialAMEInteraction


## -----------------------------------------------------------------------------

countMarg <- bayesCountMargEffF(poissonModel,
                                counts          = c(0,1,2),
                                marginal_effect = 'width',
                                start_value     = 25,
                                end_value       = 20,
                                at              = list(weight=c(2,3,4)))

countMarg


## -----------------------------------------------------------------------------

binomialMEMInteraction <- bayesMargEffF(binomialModel,
                                        marginal_effect = 'dist',
                                        start_value     = 64.041,
                                        end_value       = 21.117,
                                        at              = list(educ=c(0, 5, 8)),
                                        at_means        = T)

binomialMEMInteraction


## -----------------------------------------------------------------------------

bayesMargCompareF(binomialAMEInteraction)


## -----------------------------------------------------------------------------

bayesMargCompareF(countMarg)


## ----echo=F-------------------------------------------------------------------

data.frame(Class  = rep("stanreg", 6),
           Family = c('beta', 'binomial', 'Gamma', 'gaussian', 'neg_binomial_2', 'poisson'),
           Links  = c("logit; probit; cloglog", "logit; probit; cloglog", "inverse; log; identity", "identity", "identity; log; sqrt", "identity; log; sqrt")) %>%
  qflextable()


