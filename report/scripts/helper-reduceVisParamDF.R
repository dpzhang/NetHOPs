reduce_visParamDF = function(visParamDF){
  
  # function to sample 1 task from AUN, FUN, AEO, and FEO
  # returns a string that combines all tasks across AUN, FUN, AEO, and FEO but the sampled one
  sample_visParam_guessTask = function(){
    AUN_subtask_names = c("node12Blue", "node17Blue", "node10Green", "node16Green") 
    FUN_subtask_names = c("node13Blue", "node15Blue", "node7Green", "node21Green") 
    AEO_subtask_names = c("edgeWin", "edgeOrange", "edgeBlue") 
    FEO_subtask_names = c("edgeWin", "edgeOrange", "edgeBlue") 
    
    set.seed(666)
    # Take node17Blue
    AUN_remove = AUN_subtask_names[!AUN_subtask_names %in% sample(AUN_subtask_names, 1)]
    # Take node15Blue
    FUN_remove = FUN_subtask_names[!FUN_subtask_names %in% sample(FUN_subtask_names, 1)]
    # Take edgeBlue
    AEO_remove = AEO_subtask_names[!AEO_subtask_names %in% sample(AEO_subtask_names, 1)]
    # Take edgeBlue
    FEO_remove = FEO_subtask_names[!FEO_subtask_names %in% sample(FEO_subtask_names, 1)]
    
    
    matchPattern = paste(paste(AUN_remove, collapse = "|"), 
                         paste(FUN_remove, collapse = "|"),
                         paste(AEO_remove, collapse = "|"),
                         paste(FEO_remove, collapse = "|"), 
                         sep = "|")
    return(matchPattern)
  }  
  
  # Reduced visParamDF
  sVisParamDF = visParamDF %>% 
    select(-matches("upperBound|lowerBound")) %>% 
    select(-matches(sample_visParam_guessTask()))
  
  # rename the columns
  new_tune_colnames = sVisParamDF %>% 
    select(contains("_tune")) %>%
    colnames() %>%
    data.frame %>%
    setNames("old_colnames") %>%
    mutate(task = str_extract(old_colnames, "^[^_]+(?=_)"), 
           tuneFlag = "tune",
           param = str_extract(old_colnames, "[^_]([^_]+)$")) %>%
    mutate(new_colnames = paste(task, tuneFlag, param, sep = "_")) %>%
    pull(new_colnames)
  
  noTune_colnames_same = sVisParamDF %>%
    select(contains("_noTune")) %>%
    colnames
  
  svisParam_new_colnames = c("userID", "complete", new_tune_colnames, noTune_colnames_same)
  
  colnames(sVisParamDF) = svisParam_new_colnames
  return(sVisParamDF)
}