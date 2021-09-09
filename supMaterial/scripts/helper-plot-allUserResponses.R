# Continuous Distribution
process_distLabel_distResponse = function(df, var = c("distLabel", "distResponse")){
  tempLst = df %>% 
    select(contains(var)) %>% pull %>% as.list %>%
    lapply(function(x) x %>% as.character %>% strsplit(",") %>% unlist %>% as.numeric)
  return(tempLst)
}
compute_groundTruth_inQDP = function(truthDF, taskName, tuneFlag){
  truthDat = truthDF %>% select(contains(taskName)) %>% pull 
  
  if(sum(is.na(truthDat)) > 0){
    truthDat = truthDat[!is.na(truthDat)]
  }
  
  tempDF = tibble(
    # equally spaced intervals of length 20 to ensure 20 circles
    p_less_than_x = seq(from = 1/20/ 2, 
                        to = 1 - (1/20/2), 
                        length=20),
    # quantile values based on p_less_than_x
    dist = quantile(truthDat, p_less_than_x) %>% round(0)
  ) %>% 
    select(dist)
  
  outputDF = tempDF %>%
    mutate(user = 52, 
           task = taskName, 
           tuneFlag = tuneFlag,
           Type = "Ground Truth")
  
  return(outputDF)
}
create_densityPlotDF_byTask = function(userResponseDF, truthDF, taskName, tuneFlag){
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
  userDist_dfLst = Map(data.frame, dist = userDist_lst, user = userID_lst)
  
  
  # Compute the ground truth
  groundTruthDF = compute_groundTruth_inQDP(truthDF, taskName, tuneFlag)
  # Generate output by row binding all elements in the list
  # Do some simple processing of variables
  outputDF = do.call("rbind", userDist_dfLst) %>% 
    mutate(task = taskName,
           tuneFlag = tuneFlag,
           Type = "User Response") %>% 
    rbind(groundTruthDF)
  

  # Compute the ground truth
  
  return(outputDF)
}

plotDF = rbind(
  create_densityPlotDF_byTask(userResponseDF, truthDF, "AD", "noTune"),
  create_densityPlotDF_byTask(userResponseDF, truthDF, "AD", "tune"),
  create_densityPlotDF_byTask(userResponseDF, truthDF, "FD", "noTune"),
  create_densityPlotDF_byTask(userResponseDF, truthDF, "FD", "tune")
  ) %>%
  mutate(tuneFlag = case_when(tuneFlag == "noTune" ~ "No Tuning",
                              tuneFlag == "tune" ~ "Tuning")) %>%
  mutate(task = case_when(task == "AD" ~ "Density Elicitation on Advice-seeking",
                          task == "FD" ~ "Density Elicitation on Friendship"))




#############################################################################
discretePlotDF = rbind(create_densityPlotDF_byTask(userResponseDF, truthDF, "AC", "noTune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "AC", "tune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "FC", "noTune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "FC", "tune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "ASP", "noTune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "ASP", "tune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "FSP", "noTune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "FSP", "tune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "I", "noTune"),
                       create_densityPlotDF_byTask(userResponseDF, truthDF, "I", "tune")) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning")) %>%
  mutate(task = factor(task, levels = c("AC", "FC", "ASP", "FSP", "I"))) %>%
  mutate(task  = case_when(task == "AC" ~ "Distinct Community \non Advice-seeking",
                           task == "FC" ~ "Distinct Community \non Friendship",
                           task == "ASP" ~ "Shortest Path Length \non Advice-seeking",
                           task == "FSP" ~ "Shortest Path Length \non Friendship",
                           task == "I" ~ "Isolates Overview \non Friendship")) %>%
  mutate(dist = as.integer(dist)) %>%
  mutate(task = factor(task, levels = c("Shortest Path Length \non Advice-seeking",
                                        "Shortest Path Length \non Friendship",
                                        "Isolates Overview \non Friendship",
                                        "Distinct Community \non Advice-seeking",
                                        "Distinct Community \non Friendship")))
#############################################################################
# # Discrete cases
compute_groundTruth_inQDP_discrete = function(truthDF, taskName, tuneFlag){
  truthDat = truthDF %>% select(contains(taskName)) %>% pull

  if(sum(is.na(truthDat)) > 0){
    truthDat = truthDat[!is.na(truthDat)]
  }

  truthTable = tibble(
    # equally spaced intervals of length 20 to ensure 20 circles
    p_less_than_x = seq(from = 1/20/ 2,
                        to = 1 - (1/20/2),
                        length=20),
    # quantile values based on p_less_than_x
    dist = quantile(truthDat, p_less_than_x) %>% round(2)
  ) %>%
    select(dist) %>%
    mutate(dist = round(dist, 0)) %>%
    pull %>%
    table



  outputDF = as.data.frame(truthTable) %>%
    setNames(c("labels", "counts")) %>%
    mutate(user = 52,
           task = taskName,
           tuneFlag = tuneFlag,
           Type = "Ground Truth")

  if (taskName %in% c("AD", "FD")){
    outputDF %<>% mutate(dist = (dist * 100) %>% round(0))
  }
  return(outputDF)
}
create_discretePlotDF_byTask = function(userResponseDF, truthDF, taskName, tuneFlag){
  # Create a task key that combines taskName and tuneFlag
  taskKey = paste(taskName, tuneFlag, sep = "_")
  # Subset the task dataframe from the master dataframe
  # this DF contains 2 columns process labels and columns separately
  taskDF = userResponseDF %>%
    select(contains(taskKey) & matches("_distLabel|_distResponse"))

  # DistLabels and distResponses are in list format
  distLabel_lst = process_distLabel_distResponse(taskDF, "distLabel")
  distResponse_lst = process_distLabel_distResponse(taskDF, "distResponse")

  outputDF = do.call(rbind, Map(data.frame,
                     labels=distLabel_lst,
                     counts=distResponse_lst,
                     user = as.list(1:nrow(userResponseDF)))) %>%
    mutate(task = taskName,
           tuneFlag = tuneFlag,
           Type = "User Response")

  # Compute the ground truth
  groundTruthDF = compute_groundTruth_inQDP_discrete(truthDF, taskName, tuneFlag)
  # Generate output by row binding all elements in the list
  # Do some simple processing of variables
  outputDF = outputDF %>%
    rbind(groundTruthDF)


  # Compute the ground truth

  return(outputDF)
}

discreteBarPlotDF = rbind(create_discretePlotDF_byTask(userResponseDF, truthDF, "AC", "noTune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "AC", "tune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "FC", "noTune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "FC", "tune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "ASP", "noTune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "ASP", "tune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "FSP", "noTune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "FSP", "tune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "I", "noTune"),
                          create_discretePlotDF_byTask(userResponseDF, truthDF, "I", "tune")) %>%
  mutate(labels = as.numeric(labels),
         counts = as.numeric(counts),
         user = factor(user),
         task = factor(task, levels = c("AC", "FC", "ASP", "FSP", "I")),
         Type = factor(Type, levels = c("Ground Truth", "User Response"))) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning")) %>%
  mutate(task  = case_when(task == "AC" ~ "Distinct Community \non Advice-seeking",
                           task == "FC" ~ "Distinct Community \non Friendship",
                           task == "ASP" ~ "Shortest Path Length \non Advice-seeking",
                           task == "FSP" ~ "Shortest Path Length \non Friendship",
                           task == "I" ~ "Isolates Overview \non Friendship")) %>%
  mutate(task = factor(task, levels = c("Shortest Path Length \non Advice-seeking",
                                        "Shortest Path Length \non Friendship",
                                        "Isolates Overview \non Friendship",
                                        "Distinct Community \non Advice-seeking",
                                        "Distinct Community \non Friendship")))
#############################################################################


#############################################################################
# Node staibility plot
compute_truth_unstableNodes = function(truthDF, varName = c("AUN", "FUN")){
  tempDF = truthDF %>% select(starts_with(varName)) %>%
    summarize_all(sum) %>%
    `/`(150) %>%
    `*`(100) %>%
    round(0) %>%
    melt %>%
    mutate(task = str_extract(variable, "^[^_]+(?=_)"),
           question = str_extract(variable, "[^_]([^_]+)$")) %>% 
    select(-variable) %>%
    mutate(question = str_remove_all(question, "node"),
           nodeID = str_extract(question, "([0-9]+)"),
           nodeColor = str_remove(question, nodeID)) %>%
    select(-question) %>%
    mutate(question = paste("Node", nodeID)) %>%
    select(-nodeID, -nodeColor)
  
  tempDF = tempDF %>%
    repeat_allRows_ntimes(2) %>%
    mutate(tuneFlag = rep(c("noTune", "tune"), each = nrow(tempDF))) %>%
    select(value, task, tuneFlag, question) %>%
    mutate(Type = "Ground Truth")
  
  return(tempDF)
}
process_unstableNodes = function(userResponseDF, varName = c("AUN", "FUN")){
  tempDF = userResponseDF %>% select(contains(varName)) %>%
    select(-contains("timeSpent")) %>%
    melt %>%
    mutate(task = str_extract(variable, "^[^_]+(?=_)"),
           tuneFlag = str_extract(variable, "(?<=_)([^_]+)(?=_)"),
           question = str_extract(variable, "[^_]([^_]+)$")) %>% 
    select(-variable) %>%
    mutate(question = str_remove_all(question, "node"),
           nodeID = str_extract(question, "([0-9]+)"),
           nodeColor = str_remove(question, nodeID)) %>%
    select(-question) %>%
    mutate(question = paste("Node", nodeID)) %>%
    select(-nodeID, -nodeColor) %>%
    mutate(Type = "User Response")
  
  return(tempDF)
}
bootstrap_unstableNodes_byName = function(dataDF, questionName){
  set.seed(666)
  bootstrapDF = dataDF %>%
    select(all_of(questionName)) %>% # select the variable
    setNames("stat") %>% # rename the variable to show 
    rep_sample_n(size = 51, replace = T, reps = 10000) %>% # sample with replacement for reps number of times, each sample contains size obs.
    dplyr::summarize(stat = mean(stat), .groups = 'drop') %>% # compute the mean for each group
    mutate(question = questionName) # add a flag to indicate type 
  
  mu_hat = dataDF %>% select(all_of(questionName)) %>% pull %>% mean
  
  CI = bootstrapDF %>% 
    get_confidence_interval(level = 0.95, type = "percentile") %>%
    #get_confidence_interval(type = "se", point_estimate = mu_hat) %>%
    mutate(mu_hat = mu_hat,
           question = questionName) %>%
    select(lower_ci, mu_hat, upper_ci, question)
  return(CI)
}
compute_unstableNodes_confidenceInterval = function(userResponseDF, varName = c("AUN", "FUN")){
  dataDF = userResponseDF %>%
    select(starts_with(varName)) %>%
    select(-contains("timeSpent"))
  
  questionNames = colnames(dataDF)
  containerLst = list()
  for(question in questionNames){
    containerLst = list.append(containerLst, 
                               bootstrap_unstableNodes_byName(dataDF, question))
  }
  ciDF = do.call(rbind, containerLst) %>%
    mutate(task = str_extract(question, "^[^_]+(?=_)"),
           tuneFlag = str_extract(question, "(?<=_)([^_]+)(?=_)"),
           question = str_extract(question, "[^_]([^_]+)$")) %>% 
    mutate(question = str_remove_all(question, "node"),
           nodeID = str_extract(question, "([0-9]+)"),
           nodeColor = str_remove(question, nodeID)) %>%
    select(-question) %>%
    mutate(question = paste("Node", nodeID)) %>%
    select(-nodeID, -nodeColor) %>%
    mutate(Type = "Confidence Interval")
  return(ciDF)
}

unstableNodesPlottingDF = rbind(process_unstableNodes(userResponseDF, "AUN"), 
                                process_unstableNodes(userResponseDF, "FUN"))  %>%
  mutate(tuneFlag = case_when(tuneFlag == "noTune" ~ "No Tuning",
                              tuneFlag == "tune" ~ "Tuning")) %>%
  mutate(task = case_when(
    task == "AUN" ~ "Node Community Stability on Advice-seeking",
    task == "FUN" ~ "Node Community Stability on Friendship"
  )) %>%
  mutate(question = factor(question, levels = c("Node 12", "Node 17", "Node 10", "Node 16",
                                                "Node 13", "Node 15", "Node 7", "Node 21")))

unstableNodesTruthDF = rbind(compute_truth_unstableNodes(truthDF, "AUN"),
                             compute_truth_unstableNodes(truthDF, "FUN")) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning")) %>%
  mutate(task = case_when(
    task == "AUN" ~ "Node Community Stability on Advice-seeking",
    task == "FUN" ~ "Node Community Stability on Friendship"
  )) %>%
  mutate(question = factor(question, levels = c("Node 12", "Node 17", "Node 10", "Node 16",
                                                "Node 13", "Node 15", "Node 7", "Node 21")))

unstableNodesConfidenceIntervalDF = rbind(compute_unstableNodes_confidenceInterval(userResponseDF, "AUN"),
                                          compute_unstableNodes_confidenceInterval(userResponseDF, "FUN")) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning")) %>%
  mutate(task = case_when(
    task == "AUN" ~ "Node Community Stability on Advice-seeking",
    task == "FUN" ~ "Node Community Stability on Friendship"
  )) %>%
  mutate(question = factor(question, levels = c("Node 12", "Node 17", "Node 10", "Node 16",
                                                "Node 13", "Node 15", "Node 7", "Node 21")))
#############################################################################
# Edge Occurrence
compute_truth_edgeOccurrence = function(truthDF, varName = c("AEO", "FEO")){
  tempDF = truthDF %>% select(starts_with(varName)) %>%
    summarize_all(sum) %>%
    `/`(150) %>%
    `*`(100) %>%
    round(0) %>%
    melt %>%
    mutate(task = str_extract(variable, "^[^_]+(?=_)"),
           question = str_extract(variable, "[^_]([^_]+)$")) %>% 
    select(-variable) %>%
    mutate(question = str_remove_all(question, "edge"))
  
  tempDF = tempDF %>%
    repeat_allRows_ntimes(2) %>%
    mutate(tuneFlag = rep(c("noTune", "tune"), each = nrow(tempDF))) %>%
    select(value, task, tuneFlag, question) %>%
    mutate(Type = "Ground Truth")
  
  return(tempDF)
}
process_edgeOccurrence = function(userResponseDF, truthDF, varName = c("AEO", "FEO")){
  tempDF = userResponseDF %>% select(contains(varName)) %>%
    select(-contains("timeSpent") & -contains("edgeWin")) %>%
    melt %>%
    mutate(task = str_extract(variable, "^[^_]+(?=_)"),
           tuneFlag = str_extract(variable, "(?<=_)([^_]+)(?=_)"),
           question = str_extract(variable, "[^_]([^_]+)$")) %>% 
    select(-variable) %>%
    mutate(question = str_remove_all(question, "edge")) %>%
    mutate(Type = "User Response")
  
  return(tempDF)
}
bootstrap_edgeOccurrence_byName = function(dataDF, questionName){
  set.seed(666)
  bootstrapDF = dataDF %>%
    select(all_of(questionName)) %>% # select the variable
    setNames("stat") %>% # rename the variable to show 
    rep_sample_n(size = 51, replace = T, reps = 10000) %>% # sample with replacement for reps number of times, each sample contains size obs.
    dplyr::summarize(stat = mean(stat), .groups = 'drop') %>% # compute the mean for each group
    mutate(question = questionName) # add a flag to indicate type 
  
  mu_hat = dataDF %>% select(all_of(questionName)) %>% pull %>% mean
  
  CI = bootstrapDF %>% 
    get_confidence_interval(level = 0.95, type = "percentile") %>%
    #get_confidence_interval(type = "se", point_estimate = mu_hat) %>%
    mutate(mu_hat = mu_hat,
           question = questionName) %>%
    select(lower_ci, mu_hat, upper_ci, question)
  return(CI)
}
compute_edgeOccurrence_confidenceInterval = function(userResponseDF, varName = c("AEO", "FEO")){
  dataDF = userResponseDF %>%
    select(starts_with(varName)) %>%
    select(-contains("timeSpent")  & -contains("edgeWin"))
  
  questionNames = colnames(dataDF)
  containerLst = list()
  for(question in questionNames){
    containerLst = list.append(containerLst, 
                               bootstrap_unstableNodes_byName(dataDF, question))
  }
  ciDF = do.call(rbind, containerLst) %>%
    mutate(task = str_extract(question, "^[^_]+(?=_)"),
           tuneFlag = str_extract(question, "(?<=_)([^_]+)(?=_)"),
           question = str_extract(question, "[^_]([^_]+)$")) %>% 
    mutate(question = str_remove_all(question, "edge")) %>%
    mutate(Type = "Confidence Interval")
  return(ciDF)
}
process_edgeOccurrence = function(userResponseDF, varName = c("AEO", "FEO")){
  tempDF = userResponseDF %>% select(contains(varName)) %>%
    select(-contains("timeSpent") & -contains("edgeWin")) %>%
    melt %>%
    mutate(task = str_extract(variable, "^[^_]+(?=_)"),
           tuneFlag = str_extract(variable, "(?<=_)([^_]+)(?=_)"),
           question = str_extract(variable, "[^_]([^_]+)$")) %>%
    select(-variable) %>%
    mutate(question = str_remove(question, "edge")) 
  return(tempDF)
}


edgeOccurrencePlottingDF = rbind(process_edgeOccurrence(userResponseDF, "AEO"), 
                                process_edgeOccurrence(userResponseDF, "FEO"))  %>%
  mutate(tuneFlag = case_when(tuneFlag == "noTune" ~ "No Tuning",
                              tuneFlag == "tune" ~ "Tuning"),
         edge = case_when(
           task == "AEO" & question == "Orange" ~ "Edge 5 => 20",
           task == "AEO" & question == "Blue" ~ "Edge 10 => 16",
           task == "FEO" & question == "Orange" ~ "Edge 2 => 18",
           task == "FEO" & question == "Blue" ~ "Edge 7 => 14"
         ))%>%
  mutate(task = case_when(
    task == "AEO" ~ "Edge Occurrence on Advice-seeking",
    task == "FEO" ~ "Edge Occurrence on Friendship"
  )) %>%
  mutate(edge = factor(edge, levels = c("Edge 5 => 20", "Edge 10 => 16", "Edge 2 => 18", "Edge 7 => 14")))

edgeOccurrenceTruthDF = rbind(compute_truth_edgeOccurrence(truthDF, "AEO"),
                             compute_truth_edgeOccurrence(truthDF, "FEO")) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning"),
    edge = case_when(
      task == "AEO" & question == "Orange" ~ "Edge 5 => 20",
      task == "AEO" & question == "Blue" ~ "Edge 10 => 16",
      task == "FEO" & question == "Orange" ~ "Edge 2 => 18",
      task == "FEO" & question == "Blue" ~ "Edge 7 => 14"
    ))%>%
  mutate(task = case_when(
    task == "AEO" ~ "Edge Occurrence on Advice-seeking",
    task == "FEO" ~ "Edge Occurrence on Friendship"
  )) %>%
  mutate(edge = factor(edge, levels = c("Edge 5 => 20", "Edge 10 => 16", "Edge 2 => 18", "Edge 7 => 14")))

edgeOccurrenceConfidenceIntervalDF = rbind(compute_edgeOccurrence_confidenceInterval(userResponseDF, "AEO"),
                                          compute_edgeOccurrence_confidenceInterval(userResponseDF, "FEO")) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning"),
    edge = case_when(
      task == "AEO" & question == "Orange" ~ "Edge 5 => 20",
      task == "AEO" & question == "Blue" ~ "Edge 10 => 16",
      task == "FEO" & question == "Orange" ~ "Edge 2 => 18",
      task == "FEO" & question == "Blue" ~ "Edge 7 => 14"
    )) %>%
  mutate(task = case_when(
    task == "AEO" ~ "Edge Occurrence on Advice-seeking",
    task == "FEO" ~ "Edge Occurrence on Friendship"
  )) %>%
  mutate(edge = factor(edge, levels = c("Edge 5 => 20", "Edge 10 => 16", "Edge 2 => 18", "Edge 7 => 14")))



#############################################################################
# discretePlot = ggplot(discretePlotDF) + 
#   stat_ecdf(aes(x = dist, group = user, color = Type, size = Type), 
#             alpha = 0.5, pad = TRUE) + 
#   scale_size_manual(values = c(2, 0.5)) + 
#   scale_color_manual(values = c("#e15759", "#bab0ac")) +
#   scale_x_continuous(breaks= pretty_breaks(n = 5)) + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 5L),
#                      breaks= pretty_breaks(n = 3)) +
#   facet_grid(rows = vars(tuneFlag), cols = vars(task)) + 
#   theme_minimal() +
#   labs(x = "Possible Value", y = "F(x)") + 
#   theme(legend.position = "bottom",
#         legend.title=element_blank(),
#         axis.text.x = element_text(size=11),
#         axis.text.y = element_text(size=11),
#         axis.title = element_text(size=16),
#         strip.text.x = element_text(size = 14),
#         strip.text.y = element_text(size = 12),
#         legend.text = element_text(size = 14))
# discretePlot
# 
# continuousPlot_density = ggplot(plotDF) +
#   geom_density(aes(x = dist, group = user, color = Type, size = Type, alpha = Type), adjust = 3) + 
#   scale_size_manual(values = c(1, 0.5)) + 
#   scale_alpha_manual(values = c(1, 0.5)) + 
#   scale_color_manual(values = c("#e15759", "#bab0ac")) + 
#   labs(x = "Density Values",
#        y = "% of obs.") + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 5L),
#                      breaks= pretty_breaks(n = 2)) +
#   scale_x_continuous(breaks= pretty_breaks()) + 
#   facet_grid(rows = vars(tuneFlag), cols = vars(task)) + 
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.title=element_blank(),
#         axis.text.x = element_text(size=11),
#         axis.text.y = element_text(size=11),
#         axis.title = element_text(size=16),
#         strip.text.x = element_text(size = 14),
#         strip.text.y = element_text(size = 12),
#         legend.text = element_text(size = 14))
# continuousPlot_density

#########################################################################################################
continuousPlot_density = ggplot(plotDF) +
  geom_density(aes(x = dist, group = user, color = Type, size = Type, alpha = Type), adjust = 3) + 
  scale_size_manual(values = c(1, 0.5)) + 
  scale_alpha_manual(values = c(1, 0.5)) + 
  scale_color_manual(values = c("#e15759", "#bab0ac")) + 
  labs(x = "Density Values",
       y = "Num. Circles") + 
  scale_y_continuous(breaks= pretty_breaks(n =2),
                     labels = scales::percent_format(accuracy = 5L)) +
  scale_x_continuous(breaks= pretty_breaks(n =5)) + 
  facet_grid(rows = vars(tuneFlag), cols = vars(task)) + 
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.margin=margin(t = -10, r = 0, b = 0, l = 0),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13),
        legend.text = element_text(size = 13))

continuousPlot_cdf = ggplot(plotDF) +
  stat_ecdf(aes(x = dist, group = user, color = Type, alpha = Type, size = Type),
            alpha = 0.5, pad = T) + 
  scale_size_manual(values = c(1, 0.5)) + 
  scale_alpha_manual(values = c(1, 0.5)) + 
  scale_color_manual(values = c("#e15759", "#bab0ac")) +
  scale_y_continuous(breaks= pretty_breaks(n=2),
                     labels = scales::percent_format(accuracy = 5L)) +
  scale_x_continuous(breaks= pretty_breaks(n=5)) + 
  facet_grid(rows = vars(tuneFlag), cols = vars(task)) +
  theme_minimal() +
  labs(x = "Density Value",
       y = "F(X)") + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.margin=margin(t = -10, r = 0, b = 0, l = 0),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13),
        legend.text = element_text(size = 13))
continuousPlots = ggarrange(continuousPlot_density, continuousPlot_cdf, ncol = 2)
#########################################################################################################
discretePlot_bar = ggplot(discreteBarPlotDF) +
  geom_bar(aes(x = labels, y = counts,
               group = user, # plot each user
               color = Type,
               fill = Type,
               size = Type,
               alpha = Type),
           stat="identity",position = "identity") +
  scale_fill_manual(values = c("#e15759", "#bab0ac")) +
  scale_alpha_manual(values = c(0, 0.2)) +
  scale_color_manual(values = c("#e15759", "#bab0ac")) +
  scale_size_manual(values = c(1, 0)) +
  labs(x = "Possible Value",
       y = "Num. Circles") +
  facet_grid(rows = vars(tuneFlag), cols = vars(task)) +
  scale_y_continuous(breaks = seq(0, 20, by = 10)) +
  scale_x_continuous(limits = c(0, 10), 
                     breaks= pretty_breaks(4)) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title=element_blank(),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13),
        legend.text = element_text(size = 13))

discretePlot_cdf = ggplot(discretePlotDF) + 
  stat_ecdf(aes(x = dist, group = user, color = Type, size = Type), 
            alpha = 0.5, pad = TRUE) + 
  scale_size_manual(values = c(2, 0.5)) + 
  scale_color_manual(values = c("#e15759", "#bab0ac")) +
  scale_x_continuous(breaks= pretty_breaks(n = 5)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L),
                     breaks= pretty_breaks(n = 2)) +
  facet_grid(rows = vars(tuneFlag), cols = vars(task)) + 
  theme_minimal() +
  labs(x = "Possible Value", y = "F(x)") + 
  theme(legend.position = "none",
        legend.title=element_blank(),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 13),
        legend.text = element_text(size = 13))
discretePlots = ggarrange(discretePlot_bar, discretePlot_cdf, ncol = 2)
#########################################################################################################
nodeStabilityPlot = ggplot(unstableNodesPlottingDF) +
  geom_jitter(aes(x = question, y = value, color = tuneFlag), 
              alpha = 0.1, 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = .5), 
              size = 2, show.legend = T) +
  geom_errorbar(data = unstableNodesConfidenceIntervalDF, 
                aes(x = question, ymin= lower_ci, ymax=upper_ci, color = tuneFlag), 
                width=.2,
                size = 1.5,
                position=position_dodge(width = .5), show.legend = T) + 
  geom_point(data = unstableNodesConfidenceIntervalDF, 
             aes(x = question, y = mu_hat, color = tuneFlag),
             position=position_dodge(width = .5),
             size = 3) + 
  geom_point(dat = unstableNodesTruthDF, 
             aes(x = question, y = value, color = Type),  
             size = 3, show.legend = T) + 
  scale_color_manual(values = c("#e15759", "#4e79a7", "#f28e2b")) + 
  scale_y_continuous(breaks= pretty_breaks(n =3),
                     labels = function(x) paste0(x, "%")) + 
  facet_grid(cols = vars(task), scales = "free") +
  labs(x = "Target Node", y = "Elicited Probability", color = NULL) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.margin=margin(t = -10, r = 0, b = 0, l = 0),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 13),
        legend.text = element_text(size = 13))


edgeOccurrencePlot = ggplot(edgeOccurrencePlottingDF) +
  geom_jitter(aes(x = edge, y = value, color = tuneFlag), 
              alpha = 0.2, 
              position = position_jitterdodge(jitter.width = .3, dodge.width = 1), 
              size = 2, show.legend = T) +
  geom_errorbar(data = edgeOccurrenceConfidenceIntervalDF, 
                aes(x = edge, ymin= lower_ci, ymax=upper_ci, color = tuneFlag), 
                width=.2,
                size = 1.5,
                position=position_dodge(width = 1), show.legend = T) + 
  geom_point(data = edgeOccurrenceConfidenceIntervalDF, 
             aes(x = edge, y = mu_hat, color = tuneFlag),
             position=position_dodge(width = 1),
             size = 3) + 
  geom_point(dat = edgeOccurrenceTruthDF, 
             aes(x = edge, y = value, color = Type),  
             size = 3, show.legend = T) + 
  scale_color_manual(values = c("#e15759", "#4e79a7", "#f28e2b")) + 
  scale_y_continuous(limits = c(0, 100),
                     breaks= seq(0, 100, by = 50),
                     labels = function(x) paste0(x, "%")) + 
  facet_grid(cols = vars(task), scales = "free") +
  labs(x = "Target Edge", y = "Elicited Probability", color = NULL) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.margin=margin(t = -10, r = 0, b = 0, l = 0),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13),
        legend.text = element_text(size = 13))
