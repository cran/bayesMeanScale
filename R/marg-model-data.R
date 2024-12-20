

margModelDataF <- function(model, new_formula, at, marg_list, i){

  if(!is.null(at)){

    atValues <- expand.grid(at)
    atVars   <- names(at)

    modelDataOrg <- model$data %>%
      .[, colnames(.) %in% all.vars(new_formula), drop=F] %>%
      .[row.names(model$model),] %>%
      {if(!is.null(model$offset)) cbind(., offset=model$offset) else .}

    modelData <- modelDataOrg %>%
      .[, !(colnames(.) %in% atVars), drop=F] %>%
      merge(atValues, all=T) %>%
      data.table::setDT()
    
    # drop any unused factor levels #
    
    modelData[]    <- lapply(modelData, function(x) if(is.factor(x)) droplevels(x) else x)
    modelDataOrg[] <- lapply(modelDataOrg, function(x) if(is.factor(x)) droplevels(x) else x)

    # get the start and end data #
    
    startData  <- newValueF(data          = modelData,
                            marg_effect   = marg_list$marg[[i]],
                            new_value     = marg_list$start[[i]],
                            at            = atValues,
                            original_data = modelDataOrg)

    endData    <- newValueF(data          = modelData,
                            marg_effect   = marg_list$marg[[i]],
                            new_value     = marg_list$end[[i]],
                            at            = atValues,
                            original_data = modelDataOrg)

    # check the data integrity #
    
    dataCheckF(startData, modelDataOrg)
    dataCheckF(endData, modelDataOrg)

  } else{


    if(!is.null(model$offset)){

      modelData <- model.frame(formula=new_formula, data=model$data) %>%
        cbind(offset=model$offset)

    } else{

      modelData <- model.frame(formula=new_formula, data=model$data)

    }
    
    # drop any unused factor levels #
    
    modelData[] <- lapply(modelData, function(x) if(is.factor(x)) droplevels(x) else x)
    
    # get the start and end data #

    startData  <- newValueF(data        = modelData, 
                            marg_effect = marg_list$marg[[i]], 
                            new_value   = marg_list$start[[i]])
    
    endData    <- newValueF(data        = modelData, 
                            marg_effect = marg_list$marg[[i]], 
                            new_value   = marg_list$end[[i]])

    # check the data integrity #
    
    dataCheckF(startData, modelData)
    dataCheckF(endData, modelData)

  }

    return(list(modelData = modelData,
                startData = startData,
                endData   = endData))

}

