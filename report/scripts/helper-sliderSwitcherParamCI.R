# Compare those who did better vs who did worse
generate_bootstrap_distribution = function(df){
  set.seed(666)
  taskName = df$task %>% unique
  paramName = df$param %>% unique
  bootDist = df %>%
    rep_sample_n(size = 13, replace = TRUE,reps = 1000) %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarize(stat = mean(value)) %>%
    mutate(task = taskName,
           param = paramName)
  return(bootDist)
}
compute_param_CI = function(bootDist, df){
  taskName = bootDist$task %>% unique
  paramName = bootDist$param %>% unique
  mu_hat = df$value %>% mean
  
  boot_CI = bootDist %>%
    get_confidence_interval(type = "se",
                            point_estimate = mu_hat) %>%
    mutate(mu_hat = mu_hat,
           task = taskName,
           param = paramName) %>%
    select(lower_ci, mu_hat, upper_ci, task, param)
  return(boot_CI)
}
compute_CI = function(topVisParamDF, bottomVisParamDF){
  # Combine top user and bottom user
  masterDF = rbind(topVisParamDF, bottomVisParamDF)
  topParamDF = topVisParamDF %>% 
    select(-userID, -complete) %>%
    melt(id = NULL) %>%
    mutate(task = str_extract(variable, "^[^_]+(?=_)"),
           param = str_extract(variable, "[^_]([^_]+)$"),
           userType = "Top") %>%
    select(-variable)
  bottomParamDF = bottomVisParamDF %>% 
    select(-userID, -complete) %>%
    melt(id = NULL) %>%
    mutate(task = str_extract(variable, "^[^_]+(?=_)"),
           param = str_extract(variable, "[^_]([^_]+)$"),
           userType = "Bottom") %>%
    select(-variable)
  
  masterDF = rbind(topParamDF, bottomParamDF)
  
  # all tasknames
  taskNames = masterDF %>% select(task) %>% pull %>% unique
  paramNames = masterDF %>% select(param) %>% pull %>% unique
  userTypes = masterDF %>% select(userType) %>% pull %>% unique
  
  bootDist_lst = list()
  bootCI_lst = list()
  # Loop through task names to create confidence intervals based on userType, taskName and parameter
  taskName = "AC"; user = "Top"; parameter = "alpha"
  for(parameter in paramNames){
    for(taskName in taskNames){
      for(user in userTypes){
        # Subset the master dataset
        subsetDF = masterDF %>% filter(userType == user & task == taskName & param == parameter)
        
        # Create bootstrap distribution
        bootstrap_dist = generate_bootstrap_distribution(subsetDF) %>%
          mutate(userType = user)
        bootDist_lst = list.append(bootDist_lst, bootstrap_dist)
        
        # Create the confidence intervals
        bootstrap_CI = compute_param_CI(bootstrap_dist, subsetDF) %>%
          mutate(userType = user)
        bootCI_lst = list.append(bootCI_lst, bootstrap_CI)
      }
    }
  }
  
  bootDistDF = do.call("rbind", bootDist_lst)
  bootCIDF = do.call("rbind", bootCI_lst)
  return(list(bootDistDF, bootCIDF))
}

CI_object = compute_CI(topVisParamDF, bottomVisParamDF)
ciDF = CI_object[[2]] %>%
  mutate(data = case_when(task == "AC" ~ "Advice-seeking",
                          task == "FC" ~ "Friendship",
                          task == "I" ~ "Friendship",
                          task == "AD" ~ "Advice-seeking",
                          task == "FD" ~ "Friendship",
                          task == "ASP" ~ "Advice-seeking",
                          task == "FSP" ~ "Friendship",
                          task == "AUN" ~ "Advice-seeking",
                          task == "FUN" ~ "Friendship",
                          task == "AEO" ~ "Advice-seeking",
                          task == "FEO" ~ "Friendship")) %>%
  mutate(param = case_when(param == "alpha" ~ "Anchoring",
                           param == "frameRate"~"Animation Speed",
                           param == "darkEdges"~"Dark Edges",
                           param == "convexHull"~"Convex Hull",
                           param == "nodeColor"~"Node Color",
                           param == "nodeLabel"~"Node Label")) %>%
  mutate(task = case_when(task == "AC" ~ "Distinct\nCommunity",
                          task == "FC" ~ "Distinct\nCommunity",
                          task == "I" ~ "Isolate",
                          task == "AD" ~ "Density",
                          task == "FD" ~ "Density",
                          task == "ASP" ~ "Shortest Path\nLength",
                          task == "FSP" ~ "Shortest Path\nLength",
                          task == "AUN" ~ "Node\nStability",
                          task == "FUN" ~ "Node\nStability",
                          task == "AEO" ~ "Edge\nOccurrence",
                          task == "FEO" ~ "Edge\nOccurrence"))  %>%
  mutate(userType = factor(userType, levels = c("Top", "Bottom"))) %>%
  mutate(task = factor(task, levels = c("Node\nStability", "Edge\nOccurrence", 
                                        "Distinct\nCommunity", "Shortest Path\nLength", 
                                        "Density", "Isolate")))

# create another dataset that contains default parameter
defaultDF = visParamDF %>% 
  select(contains("_noTune") & (contains("alpha") | contains("frameRate"))) %>%
  slice(1) %>%
  melt(id = NULL) %>%
  mutate(task = str_extract(variable, "^[^_]+(?=_)"),
         param = str_extract(variable, "[^_]([^_]+)$")) %>%
  select(-variable) %>%
  mutate(data = case_when(task == "AC" ~ "Advice-seeking",
                          task == "FC" ~ "Friendship",
                          task == "I" ~ "Friendship",
                          task == "AD" ~ "Advice-seeking",
                          task == "FD" ~ "Friendship",
                          task == "ASP" ~ "Advice-seeking",
                          task == "FSP" ~ "Friendship",
                          task == "AUN" ~ "Advice-seeking",
                          task == "FUN" ~ "Friendship",
                          task == "AEO" ~ "Advice-seeking",
                          task == "FEO" ~ "Friendship")) %>%
  mutate(param = case_when(param == "alpha" ~ "Anchoring",
                           param == "frameRate"~"Animation Speed",
                           param == "darkEdges"~"Dark Edges",
                           param == "convexHull"~"Convex Hull",
                           param == "nodeColor"~"Node Color",
                           param == "nodeLabel"~"Node Label")) %>%
  mutate(task = case_when(task == "AC" ~ "Distinct\nCommunity",
                          task == "FC" ~ "Distinct\nCommunity",
                          task == "I" ~ "Isolate",
                          task == "AD" ~ "Density",
                          task == "FD" ~ "Density",
                          task == "ASP" ~ "Shortest Path\nLength",
                          task == "FSP" ~ "Shortest Path\nLength",
                          task == "AUN" ~ "Node\nStability",
                          task == "FUN" ~ "Node\nStability",
                          task == "AEO" ~ "Edge\nOccurrence",
                          task == "FEO" ~ "Edge\nOccurrence")) %>%
  mutate(task = factor(task, levels = c("Node\nStability", "Edge\nOccurrence", 
                                        "Distinct\nCommunity", "Shortest Path\nLength", 
                                        "Density", "Isolate"))) %>%
  mutate(lower_ci = value,
         upper_ci = value,
         mu_hat = value,
         userType = "Default") %>%
  select(-value) %>%
  select(lower_ci, mu_hat, upper_ci, task, param, userType, data) 

sliderParamPlot = ciDF %>%
  filter(param %in% c("Anchoring", "Animation Speed")) %>%
  rbind(defaultDF) %>% 
  mutate(userType = factor(userType, levels = c("Top", "Default", "Bottom"))) %>%
  ggplot(.) +
  geom_errorbar(aes(x = task, ymin=lower_ci, ymax=upper_ci, color = userType), 
                width=.6, size = 0.6,position=position_dodge(0.7)) + 
  geom_point(aes(x = task, y = mu_hat, color = userType, shape = userType), 
             size = 1,
             position = position_dodge(0.7)) +
  scale_color_manual(labels = c("Top", "Default", "Bottom"),
                     values = c("#4e79a7", "#e15759", "#f28e2b")) + 
  scale_shape_manual(values = c(16,15,16)) + 
  facet_grid(cols = vars(data), rows = vars(param), scales = "free") + 
  labs(x = "Task Taxonomy", y = "Parameter Value", color = NULL) + 
  guides(shape = FALSE ) + 
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 5, angle=45),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 6),
        axis.title.x = element_blank(),
        legend.margin=margin(t = -30, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 7),
        strip.text.y = element_text(size = 7))

################################################################################
topSwitcherUseDF = topVisParamDF %>%
  select(-userID, -complete) %>% 
  select(contains("_tune")) %>%
  select(matches("darkEdges|convexHull|nodeColor|nodeLabel")) %>%
  melt(id = NULL) %>%
  mutate(task = str_extract(variable, "^[^_]+(?=_)"),
         param = str_extract(variable, "[^_]([^_]+)$"),
         userType = "Top") %>%
  select(-variable) %>%
  dplyr::group_by(param, task, userType) %>%
  dplyr::summarize(percent = sum(value)/13)


bottomSwitcherUseDF = bottomVisParamDF %>%
  select(-userID, -complete) %>% 
  select(contains("_tune")) %>%
  select(matches("darkEdges|convexHull|nodeColor|nodeLabel")) %>%
  melt(id = NULL) %>%
  mutate(task = str_extract(variable, "^[^_]+(?=_)"),
         param = str_extract(variable, "[^_]([^_]+)$"),
         userType = "Bottom") %>%
  select(-variable) %>%
  dplyr::group_by(param, task, userType) %>%
  dplyr::summarize(percent = sum(value)/13)


switcherPlotDF = rbind(topSwitcherUseDF, bottomSwitcherUseDF) %>%
  mutate(data = case_when(task == "AC" ~ "Advice-seeking",
                          task == "FC" ~ "Friendship",
                          task == "I" ~ "Friendship",
                          task == "AD" ~ "Advice-seeking",
                          task == "FD" ~ "Friendship",
                          task == "ASP" ~ "Advice-seeking",
                          task == "FSP" ~ "Friendship",
                          task == "AUN" ~ "Advice-seeking",
                          task == "FUN" ~ "Friendship",
                          task == "AEO" ~ "Advice-seeking",
                          task == "FEO" ~ "Friendship")) %>%
  mutate(param = case_when(param == "darkEdges"~"Dark Edges",
                           param == "convexHull"~"Convex Hull",
                           param == "nodeColor"~"Node Color",
                           param == "nodeLabel"~"Node Label")) %>%
  mutate(task = case_when(task == "AC" ~ "Distinct\nCommunity",
                          task == "FC" ~ "Distinct\nCommunity",
                          task == "I" ~ "Isolate",
                          task == "AD" ~ "Density",
                          task == "FD" ~ "Density",
                          task == "ASP" ~ "Shortest Path\nLength",
                          task == "FSP" ~ "Shortest Path\nLength",
                          task == "AUN" ~ "Node Community\nStability",
                          task == "FUN" ~ "Node Community\nStability",
                          task == "AEO" ~ "Edge\nOccurrence",
                          task == "FEO" ~ "Edge\nOccurrence"))  %>%
  mutate(task = factor(task, levels = c("Node Community\nStability", "Edge\nOccurrence", 
                                        "Distinct\nCommunity", "Shortest Path\nLength", "Density", "Isolate"))) %>%
  mutate(userType = factor(userType, levels = c("Top", "Bottom"))) %>%
  mutate(param = factor(param, levels = c("Dark Edges", "Convex Hull", "Node Color", "Node Label")))


switcherPlot = switcherPlotDF %>%
  ggplot(aes(x=userType, y=percent, group = data)) +
  geom_bar(aes(fill = data), stat="identity", position=position_dodge(width= 0.6), width = .5) + 
  geom_text(aes(label = scales::percent(percent, accuracy=1), color = data), 
            position=position_dodge(width=1), vjust=-0.25, show.legend = F, size = 1.2) + 
  facet_grid(param ~ task, scale = "free", drop = T) + 
  scale_color_manual(values = c("#003262", "#C4820E")) + 
  scale_fill_manual(values = c("#003262", "#C4820E")) + 
  scale_y_continuous(limits = c(0, 1.2),
                     breaks = pretty_breaks(n = 2),
                     labels = scales::percent_format(accuracy = 5L)) + 
  labs(x = "User Type", y = "Percent", fill = NULL) + 
  theme_bw() +
  guides(color = guide_legend(show = F)) + 
  theme(legend.position = "bottom",
        legend.margin=margin(t = -10, r = 0, b = 0, l = 0),
        legend.key.size = unit(0.5,"line"),
        legend.text = element_text(size = 5),
        strip.text = element_text(size = 4, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_text(size = 5))
