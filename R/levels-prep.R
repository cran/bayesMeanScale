
levelsPrepF <- function(data, at, original_data){

    for(j in names(at)){

      if(is.numeric(data[[j]]) & (is.character(original_data[[j]]) | is.factor(original_data[[j]]))){

        stop("You supplied numeric values for a character/factor variable!")

      }

      if(is.character(original_data[[j]])){

        data[[j]] <- factor(data[[j]], levels=sort(unique(original_data[[j]])))

      }

      if(is.factor(original_data[[j]])){

        data[[j]] <- factor(data[[j]], levels=levels(original_data[[j]]))

      }

    }


  return(data)

}

