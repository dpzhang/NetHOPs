get_userDist = function(userResponseDF, taskName, tuneFlag){
  # Create a task key that combines taskName and tuneFlag
  taskKey = paste(taskName, tuneFlag, sep = "_")
  # Subset the task dataframe from the master dataframe
  # this DF contains 2 columns process labels and columns separately
  taskDF = userResponseDF %>%
    select(contains(taskKey) & matches("_distLabel|_distResponse"))
  
  # DistLabels and distResponses are in list format
  distLabel_lst = process_distLabel_distResponse(taskDF, "distLabel")
  distResponse_lst = process_distLabel_distResponse(taskDF, "distResponse")
  
  # repeat distLabel by distResponse of time
  userDist_lst = Map(rep, distLabel_lst, distResponse_lst)
  # Create a dummy which is an indicator of user
  userID_lst = Map(rep, as.list(1:nrow(userResponseDF)), 20)
  # Combine userDIst and userID forming a dataframe
  userDistDF = Map(data.frame, dist = userDist_lst, user = userID_lst) %>%
    lapply(function(x) x %>% pull(dist) %>% paste(collapse = ",")) %>%
    do.call("rbind", .) %>%
    data.frame %>% 
    setNames(taskName)
  
  return(userDistDF)
}

noTuneProbDF = userResponseDF %>% 
  select(contains("_noTune") & 
           !contains("_timeSpent") & !contains("lowerBound") & 
           !contains("upperBound") & !contains("edgeWin")) %>%
  select(starts_with("AUN") | starts_with("FUN") | starts_with("AEO") | starts_with("FEO"))

tuneProbDF = userResponseDF %>% 
  select(contains("_tune") & 
           !contains("_timeSpent") & !contains("lowerBound") & 
           !contains("upperBound") & !contains("edgeWin")) %>%
  select(starts_with("AUN") | starts_with("FUN") | starts_with("AEO") | starts_with("FEO"))

noTuneResponseDF = cbind(get_userDist(userResponseDF, "AC", "noTune"),
                         get_userDist(userResponseDF, "FC", "noTune"),
                         get_userDist(userResponseDF, "ASP", "noTune"),
                         get_userDist(userResponseDF, "FSP", "noTune"),
                         get_userDist(userResponseDF, "AD", "noTune"),
                         get_userDist(userResponseDF, "FD", "noTune"),
                         get_userDist(userResponseDF, "I", "noTune"),
                         noTuneProbDF
)

tuneResponseDF = cbind(get_userDist(userResponseDF, "AC", "tune"),
                       get_userDist(userResponseDF, "FC", "tune"),
                       get_userDist(userResponseDF, "ASP", "tune"),
                       get_userDist(userResponseDF, "FSP", "tune"),
                       get_userDist(userResponseDF, "AD", "tune"),
                       get_userDist(userResponseDF, "FD", "tune"),
                       get_userDist(userResponseDF, "I", "tune"),
                       tuneProbDF
)

(noTuneResponseDF == tuneResponseDF) %>% apply(1, sum) %>% `==`(ncol(noTuneResponseDF)) %>% sum
