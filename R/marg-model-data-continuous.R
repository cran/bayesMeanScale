

margModelDataContinuousF <- function(model, new_formula, at, marg_list, i, h){

  if(!is.null(at)){

    atValues <- expand.grid(at)
    atVars   <- names(atValues)

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

    startData  <- newValueContinuousF(data          = modelData,
                                      marg_effect   = marg_list$marg[[i]],
                                      h             = 0,
                                      at            = atValues,
                                      original_data = modelDataOrg)

    endData    <- newValueContinuousF(data          = modelData,
                                      marg_effect   = marg_list$marg[[i]],
                                      h             = h,
                                      at            = atValues,
                                      original_data = modelDataOrg)

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
    
    startData  <- newValueContinuousF(data        = modelData, 
                                      marg_effect = marg_list$marg[[i]], 
                                      h           = 0)
    
    endData    <- newValueContinuousF(data        = modelData, 
                                      marg_effect = marg_list$marg[[i]], 
                                      h           = h)

    dataCheckF(startData, modelData)
    dataCheckF(endData, modelData)

  }

    return(list(modelData = modelData,
                startData = startData,
                endData   = endData))

}

