
newValueContinuousF <- function(data, marg_effect, h, at=NULL, original_data=NULL){

  data[[marg_effect]] <- data[[marg_effect]] + h

  if(!is.null(at)){

    for(j in names(at)){

      if(is.character(original_data[[j]])){

        data[[j]] <- factor(data[[j]], levels=sort(unique(original_data[[j]])))

      }

      if(is.factor(original_data[[j]])){

        data[[j]] <- factor(data[[j]], levels=levels(original_data[[j]]))

      }

    }

  }

  return(data)

}
