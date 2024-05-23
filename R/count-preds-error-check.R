
countPredsErrorCheckF <- function(model, counts, at, centrality){
  
  # check that the counts values are integers #
  
  if(any(!(is.numeric(counts)))){
    stop("Counts values must be integers!")
  }
  
  if(any(floor(counts)!=counts)){
    stop("Counts values must be integers!")
  }
  
  # check that the counts values are strictly non-negative #
  
  if(any(counts<0)){
    stop("Counts values must be non-negative!")
  }

  # make sure model is supported #
  
  if(any(c('aov', 'lmerMod', 'gamm4', 'stanmvreg', 'stanjm', 'nlmerMod', 'polr', 'clogit') %in% class(model))){
    stop("This model is not currently supported!")
  }
  
  # make sure model is a stanreg object and of a supported exponential family #

  if(!('stanreg' %in% class(model))){
    stop("The model must be a 'stanreg' object!")
  }

  if(!(model$family$family %in% c('poisson', 'neg_binomial_2'))){
    stop("The model must be poisson or negative binomial!")
  }
  
  # make sure the link function is supported #
  
  if(!model$family$link %in% c('log', 'identity', 'sqrt')){
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
