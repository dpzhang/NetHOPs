repeat_allRows_ntimes = function(df, n){
  outputDF = do.call("rbind", replicate(n, df, simplify = FALSE))
  return(outputDF)
}
repeat_allCols_ntimes = function(df, n){
  outputDF = do.call("cbind", replicate(n, df, simplify = FALSE))
  return(outputDF)
}

present_summaryStats = function(vec){
  summaryTable = summary(vec)
  outputDF = summaryTable %>%
    unclass %>% data.frame %>% t %>% 
    data.frame(row.names = NULL) %>%
    mutate(sd = sd(vec)) %>%
    select(Min., Median, Mean, sd, Max.)
  return(outputDF)
}
