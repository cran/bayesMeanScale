

margModelDataF <- function(model, new_formula, at, marg_list, i){

  if(!is.null(at)){

    atValues <- expand.grid(at)
    atVars   <- names(atValues)

    if(!is.null(model$offset)){
      
      modelDataOrg <- model$data %>%
        cbind(offset=model$offset)
      
    } else{
      
      modelDataOrg <- model$data
      
    }

    modelData <- modelDataOrg %>%
      na.omit() %>%
      .[, !(colnames(.) %in% atVars), drop=F] %>%
      merge(atValues, all=T) %>%
      data.table::setDT()

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

    dataCheckF(startData, modelDataOrg)
    dataCheckF(endData, modelDataOrg)

  } else{


    if(!is.null(model$offset)){

      modelData <- model.frame(formula=new_formula, data=model$data) %>%
        cbind(offset=model$offset)

    } else{

      modelData <- model.frame(formula=new_formula, data=model$data)

    }

    startData  <- newValueF(data=modelData, marg_effect=marg_list$marg[[i]], new_value=marg_list$start[[i]])
    endData    <- newValueF(data=modelData, marg_effect=marg_list$marg[[i]], new_value=marg_list$end[[i]])

    dataCheckF(startData, modelData)
    dataCheckF(endData, modelData)

  }

    return(list(modelData = modelData,
                startData = startData,
                endData   = endData))

}

