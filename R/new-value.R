
newValueF <- function(data, marg_effect, new_value, at=NULL, original_data=NULL){

  if(inherits(data[[marg_effect]], 'factor')){

    data[[marg_effect]] <- factor(new_value, levels=levels(data[[marg_effect]]))

  } else{

  if(inherits(data[[marg_effect]], 'character')){

    data[[marg_effect]] <- factor(new_value, levels=sort(unique(data[[marg_effect]])))

  } else{

    data[[marg_effect]] <- new_value

  }
  }

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
