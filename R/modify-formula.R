
modifyFormulaF <- function(model){

  originalFormula  <- formula(model)
  modifiedFormula  <- as.formula(paste0(gsub("offset\\(", "offset2(", deparse(originalFormula)), collapse=" "))
  formulaNoOffsets <- as.formula(paste0(gsub(" [\\+-] offset2\\(.*\\)", "", deparse(modifiedFormula)), collapse=" "))

  return(formulaNoOffsets)

}
