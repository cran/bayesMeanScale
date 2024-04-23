
predsErrorCheckF <- function(model, at, centrality){

  # make sure model is supported #
  
  if(any(c('aov', 'lmerMod', 'gamm4', 'stanmvreg', 'stanjm', 'nlmerMod', 'polr', 'clogit') %in% class(model))){
    stop("This model is not currently supported!")
  }
  
  # make sure model is a stanreg object and of a supported exponential family #

  if(!('stanreg' %in% class(model))){
    stop("The model must be a 'stanreg' object!")
  }

  if(!(model$family$family %in% c('beta', 'binomial', 'gaussian', 'Gamma', 'poisson', 'neg_binomial_2'))){
    stop("The model must be in a supported exponential family!")
  }
  
  # make sure the link function is supported #
  
  if(!model$family$link %in% c('logit', 'identity', 'inverse', 'log', 'cloglog', 'probit', 'sqrt')){
    stop('The link function of your model is not supported!')
  }

  # check that the names for the at values are correct #

  if(!all(names(at) %in% names(model$model))){
    stop("The names for the at values don't match up with the names in the model data!")
  }
  
  # check that the centrality measure is supported #
  
  if(!(centrality %in% c('mean', 'median'))){
    stop("Centrality options are 'mean' or 'median'!")
  }

}
