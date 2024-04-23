
meanPredF <- function(model, new_data, at, draws, new_formula, at_means){

  # make the new model matrix #

  modelMatrix   <- model.matrix(new_formula, data=new_data)

  # set aside the offset if there is one #

  if(!is.null(model$offset)){
    modelOffset <- new_data$offset
  }

  # get the draws from the joint posterior #

  modelDrawsOrg <- data.table::as.data.table(model)

  # check that new model matrix doesn't have any columns that aren't in joint posterior #

  if(!all(dimnames(modelMatrix)[[2]] %in% names(modelDrawsOrg))){
    stop("Something is wrong with the model matrix!")
  }

  # make sure to only get joint posterior columns that match up with model matrix #

  betaDraws <- modelDrawsOrg %>%
    .[, .SD, .SDcols = dimnames(modelMatrix)[[2]]] %>%
    as.matrix()

  # make sure model matrix lines up with draws matrix #

  if(at_means==F){

    modelMatrixNew <- modelMatrix %>%
      data.table::as.data.table() %>%
      .[, .SD, .SDcols = dimnames(betaDraws)[[2]]] %>%
      as.matrix()

  }
  
  if(at_means==T & !is.null(at)){

    atVars <- names(expand.grid(at))

    atVarsNew <- paste0(atVars, "_new")
    data.table::setnames(new_data, old=names(new_data[, ..atVars]), new=atVarsNew)

    modelMatrixNew <- modelMatrix %>%
      data.table::as.data.table() %>%
      .[, .SD, .SDcols = dimnames(betaDraws)[[2]]] %>%
      cbind(new_data[, ..atVarsNew]) %>%
      .[, lapply(.SD, mean), by=atVarsNew] %>%
      .[, !..atVarsNew] %>%
      as.matrix()

    data.table::setnames(new_data, old=names(new_data[, ..atVarsNew]), new=atVars)

  }

  if(at_means==T & is.null(at)){

    modelMatrixNew <- modelMatrix %>%
      data.table::as.data.table() %>%
      .[, .SD, .SDcols = dimnames(betaDraws)[[2]]] %>%
      .[, lapply(.SD, mean)] %>%
      as.matrix()

  }
  
  # get a sample from the joint posterior #

  betaSamples <- sample(1:nrow(betaDraws), size=draws, replace=T)

  # compute the linear predictor #

  if(!is.null(model$offset)){

    Z <- (modelMatrixNew %*% t(betaDraws[betaSamples,])) + modelOffset

  } else{

    Z <- modelMatrixNew %*% t(betaDraws[betaSamples,])

  }

  # apply the inverse link function #

  if(model$family$link=="logit"){
    meanPreds <- exp(Z) / (1 + exp(Z))
  }

  if(model$family$link=='probit'){
    meanPreds <- pnorm(Z, mean=0, sd=1)
  }

  if(model$family$link=='cloglog'){
    meanPreds <- 1 - exp(-exp(Z))
  }

  if(model$family$link=="log"){
    meanPreds <- exp(Z)
  }

  if(model$family$link=='identity'){
    meanPreds <- Z
  }

  if(model$family$link=="inverse"){
    meanPreds <- 1/Z
  }
  
  if(model$family$link=='sqrt'){
    meanPreds <- Z^2
  }

  # output #

  return(meanPreds)

}
