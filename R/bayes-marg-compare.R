
bayesMargCompareF <- function(marg_list, ci=.95, hdi_interval=TRUE, centrality='mean', digits=4){

  margCompareErrorCheckF(marg_list    = marg_list, 
                         ci           = ci,
                         hdi_interval = hdi_interval,
                         centrality   = centrality)

  # get the MCMC draws #

  drawData <- marg_list$diffDraws

  # get the unique combos of marg effects and comparisons #

  comboData <- unique(subset(drawData, select=-diff))

  # initialize the diff table

  drawDiffTableBig <- data.frame()
  drawDistribution <- data.frame()

  for(i in 1:nrow(comboData)){
    for(j in 1:nrow(comboData)){

      if(i==j){
        break
      }

      # specify the first and second comparisons #

        comboTemp1 <- comboData[i,]
        comboTemp2 <- comboData[j,]

        draw1 <- merge(drawData, comboTemp1, by=names(comboTemp1), all=F)
        draw2 <- merge(drawData, comboTemp2, by=names(comboTemp2), all=F)

        # get the differences of the draws #

        drawDiffs <- draw1$diff - draw2$diff

        # modify the names of the comboTemp data frames #

        names(comboTemp1) <- paste0(names(comboTemp1), "1")
        names(comboTemp2) <- paste0(names(comboTemp2), "2")

        # make the table

        if(centrality=='map'){
          centrality <- "map_estimate"
        }
        
        centralityF <- eval(parse(text=centrality))
        
        if(hdi_interval==T){
        
          tempTable <- data.frame(
            centrality = round(centralityF(drawDiffs), digits=digits),
            lower      = round(bayestestR::hdi(drawDiffs, ci=ci)$CI_low, digits=digits),
            upper      = round(bayestestR::hdi(drawDiffs, ci=ci)$CI_high, digits=digits)
          )
        
        } else{
          
          tempTable <- data.frame(
            centrality = round(centralityF(drawDiffs), digits=digits),
            lower      = round(quantile(drawDiffs, probs=(1-ci)/2), digits=digits),
            upper      = round(quantile(drawDiffs, probs=1-(1-ci)/2), digits=digits)
          )
          
        }
        
        names(tempTable) <- c(centrality, 'lower', 'upper')

        drawDistribution <- rbind(drawDistribution, drawDiffs)
        drawDiffTable    <- cbind(comboTemp1, comboTemp2, tempTable)
        drawDiffTableBig <- rbind(drawDiffTableBig, drawDiffTable)

  }}

  # output #

  drawDiff <- structure(list(diffDraws = drawDistribution,
                             diffTable = as.data.frame(drawDiffTableBig)),
                        class = c("bayes_mean_scale_marg_compare", "list"))
  
  return(drawDiff)

}
