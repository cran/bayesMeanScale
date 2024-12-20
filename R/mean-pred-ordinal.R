
ordinalMeanPredF <- function(model, new_data, at, draws, y_outcomes, new_formula, at_means){

  # make the new model matrix #

  modelMatrix <- model.matrix(formula(model), data=new_data) %>%
    .[, colnames(.) != "(Intercept)"]

  # get the draws from the joint posterior #

  modelDrawsOrg <- posterior::as_draws_df(model) %>% 
    data.table::setDT()
  
  thresholds <- names(modelDrawsOrg[, .SD, .SDcols=patterns("\\|")])

  # initialize the array to store the linear predictors #

  linearPredictors <- data.frame()

  for(i in thresholds){

    # initialize model matrix and draws for one threshold at a time #

    modelDrawsNew  <- modelDrawsOrg %>%
      .[, .SD, .SDcols=!patterns("\\|")]

    # check that new model matrix doesn't have any columns that aren't in joint posterior #

    if(!all(dimnames(modelMatrix)[[2]] %in% names(modelDrawsNew))){
      stop("Something is wrong with the model matrix!")
    }

    # make sure to only get joint posterior columns that match up with model matrix #

    modelMatrixNames <- dimnames(modelMatrix)[[2]]

    betaDraws <- modelDrawsNew %>%
      .[, .SD, .SDcols=modelMatrixNames] %>%
      as.matrix()

    # make sure model matrix lines up with draws matrix #

    betaDrawsNames <- dimnames(betaDraws)[[2]]
    
    if(at_means==F){
      
      modelMatrixNew <- modelMatrix %>%
        data.table::as.data.table() %>%
        .[, .SD, .SDcols=betaDrawsNames] %>%
        as.matrix()
      
    }
    
    if(at_means==T & !is.null(at)){
      
      atVars <- names(at)
      
      atVarsNew <- paste0(atVars, "_new")
      data.table::setnames(new_data, old=names(new_data[, ..atVars]), new=atVarsNew)
      
      modelMatrixNew <- modelMatrix %>%
        data.table::as.data.table() %>%
        .[, .SD, .SDcols=betaDrawsNames] %>%
        cbind(new_data[, ..atVarsNew]) %>%
        .[, lapply(.SD, mean), by=atVarsNew] %>%
        .[, !..atVarsNew] %>%
        as.matrix()
      
      data.table::setnames(new_data, old=names(new_data[, ..atVarsNew]), new=atVars)
      
    }
    
    if(at_means==T & is.null(at)){
      
      modelMatrixNew <- modelMatrix %>%
        data.table::as.data.table() %>%
        .[, .SD, .SDcols=betaDrawsNames] %>%
        .[, lapply(.SD, mean)] %>%
        as.matrix()
      
    }

    # get a sample from the joint posterior #

    intercept <- modelDrawsOrg %>%
      .[, .SD, .SDcols=i] %>%
      as.matrix() %>%
      .[draws,]

    # compute the linear predictor #

    Z <- (modelMatrixNew %*% t(betaDraws[draws,]))*-1 + intercept

    # store the linear predictor in a data frame #

    linearPredictors <- rbind(linearPredictors, data.frame(Z, threshold=i, observation=1:dim(Z)[[1]]))

  }

  # get the threshold data #
  
  thresholdData <- data.table::data.table(
    threshold = unique(linearPredictors$threshold),
    which_cat = sub("\\|.*", "", unique(linearPredictors$threshold))
  )

  measureVars <- names(subset(linearPredictors, select=-c(threshold, observation)))

  # apply the inverse link function and join the threshold data #
  
  if(at_means==F & !is.null(at)){
    bindData <- new_data[, .SD, .SDcols=names(at)]
  }

  if(at_means==T & !is.null(at)){
    bindData <- expand.grid(at)
  }
  
  if(is.null(at)){
    bindData <- rep(NA, nrow(linearPredictors))
  }
  
  if(model$method=="logistic"){
  
    tempData <- linearPredictors %>%
      cbind(bindData) %>%
      data.table::setDT() %>%
      data.table::melt(measure.vars  = measureVars,
                       variable.name = 'draw',
                       value.name    = 'z') %>%
      .[, inv_z := exp(z) / (1 + exp(z)), key='threshold'] %>%
      .[thresholdData] %>%
      .[order(observation, draw)]
  
  }
  
  if(model$method=="probit"){
    
    tempData <- linearPredictors %>%
      cbind(bindData) %>%
      data.table::setDT() %>%
      data.table::melt(measure.vars  = measureVars,
                       variable.name = 'draw',
                       value.name    = 'z') %>%
      .[, inv_z := pnorm(z, mean=0, sd=1), key='threshold'] %>%
      .[thresholdData] %>%
      .[order(observation, draw)]
    
  }
  
  if(model$method=="cloglog"){
    
    tempData <- linearPredictors %>%
      cbind(bindData) %>%
      data.table::setDT() %>%
      data.table::melt(measure.vars  = measureVars,
                       variable.name = 'draw',
                       value.name    = 'z') %>%
      .[, inv_z := 1 - exp(-exp(z)), key='threshold'] %>%
      .[thresholdData] %>%
      .[order(observation, draw)]
    
  }

  # remove some data that's no longer necessary, to free up some memory #

  rm(linearPredictors, modelMatrix, modelMatrixNew, modelDrawsOrg, modelDrawsNew, betaDraws)

  # initialize the data frame for the probabilities #

  if(!is.null(at)){
    
    probColumns <- c("draw", names(at))
    
  } else{
    
    probColumns <- c("draw")
    
  }
  
  probData <- tempData %>%
    unique(., by=c('observation', 'draw')) %>%
    .[, .SD, .SDcols=probColumns]

  for(i in 1:length(y_outcomes)){

    if(i==1){

      probData[, y_outcomes[[i]]] <- tempData %>%
        .[which_cat==y_outcomes[[i]], inv_z]

    } else{
      if(i==length(y_outcomes)){

        probData[, y_outcomes[[i]]] <- tempData %>%
          .[, 1 - inv_z[which_cat==y_outcomes[[i-1]]]]

      } else{

        probData[, y_outcomes[[i]]] <- tempData %>%
          .[, inv_z[which_cat==y_outcomes[[i]]] - inv_z[which_cat==y_outcomes[[i-1]]]]

      }
    }


  }

  # output #

  return(probData)

  }

