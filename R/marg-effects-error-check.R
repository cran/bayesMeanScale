
margErrorCheckF <- function(model, marginal_effect, at, start_value, end_value, centrality){

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

  # check for correct length of arguments #

  if(!(length(marginal_effect)==length(start_value) & length(start_value)==length(end_value))){
    stop("The arguments for 'marginal_effect,' 'start_value,' and 'end_value' must all have the same length!")
  }

  # check that marginal effects have the right names #

  if(!all(marginal_effect %in% names(model$model))){
    stop("The names of the marginal effects don't match up with the names in the model data!")
  }

  # check that the names for the at values are correct #

  if(!is.null(at) & !all(names(at) %in% names(model$model))){
    stop("The names for the at values don't match up with the names in the model data!")
  }

  # check that the start and end values are either vectors of length 1 or lists #

  if(length(start_value) > 1 & !is.list(start_value)){
    stop("When specifying multiple start values, they must be contained in a list to preserve data type!")
  }

  if(length(end_value) > 1 & !is.list(end_value)){
    stop("When specifying multiple end values, they must be contained in a list to preserve data type!")
  }
  
  # check that the centrality measure is supported #
  
  if(!(centrality %in% c('mean', 'median'))){
    stop("Centrality options are 'mean' or 'median'!")
  }

}
