
dataCheckF <- function(new_data, model_frame){

  for(k in names(new_data)){

      if((is.factor(new_data[[k]]) & is.factor(model_frame[[k]])) & !all(levels(new_data[[k]])==levels(model_frame[[k]]))){

        stop("There's a factor level mismatch in or or more of the model frame columns!")

      }

      if((is.character(new_data[[k]]) & is.character(model_frame[[k]])) & !all(unique(new_data[[k]]) %in% unique(model_frame[[k]]))){

        stop("You specified the values wrong for some character/factor variable!")

      }

      if((is.character(new_data[[k]]) & is.factor(model_frame[[k]])) & !all(unique(new_data[[k]]) %in% levels(model_frame[[k]]))){

        stop("You specified the values wrong for some character/factor variable!")

      }

      if((is.factor(new_data[[k]]) & is.character(model_frame[[k]])) & !all(levels(new_data[[k]]) %in% unique(model_frame[[k]]))){

        stop("You specified the values wrong for some character/factor variable!")

      }

      if((is.character(new_data[[k]]) | is.factor(new_data[[k]])) & is.numeric(model_frame[[k]])){

        stop('You supplied character values for some numeric variable!')

      }

  }

}

