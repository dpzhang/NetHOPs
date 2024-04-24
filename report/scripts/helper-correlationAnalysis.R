# Time analysis
outlier_i = userResponseDF %>% 
  select(ends_with("timeSpent")) %>% 
  select(-test_timeSpent, -demographics_timeSpent) %>% 
  `/`(60) %>%
  apply(1, sum) %>%
  which.max()
taskCompletionTimeVec = userResponseDF %>% 
  select(ends_with("timeSpent")) %>% 
  select(-test_timeSpent, -demographics_timeSpent) %>% 
  `/`(60) %>%
  apply(1, sum)
taskCompletionTimeVec_noOutlier = taskCompletionTimeVec[-outlier_i]
c(summary(taskCompletionTimeVec_noOutlier), "sd" = sd(taskCompletionTimeVec_noOutlier))

# Summary statistics of time spent
# FUN_noTune: 30777 ~ (8.5 hours) 
timeDistPlot = userResponseDF %>%
  select(ends_with("timeSpent")) %>% 
  select(-test_timeSpent, -demographics_timeSpent) %>% # removing the amount of time spent on test and demographics
  `/`(60) %>%
  apply(1, sum) %>%
  data.frame %>%
  setNames("timeSpent") %>%
  ggplot(aes(x = timeSpent)) + 
  geom_histogram(bins = 15, color = "#4E2A84", fill = "#4E2A84", alpha = 0.2) + 
  scale_x_continuous(breaks = seq(0, 500, by = 100)) + 
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  labs(x = "Time Spent (Mins.)", y = "Freq.", 
       title = "Distribution of Study Completion Time",
       subtitle = "Num of Obs. = 51") + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.margin=margin(t = -10, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 15))

################################################################################
# Task completion time by task
timeSpentDF = userResponseDF %>%
  select(ends_with("timeSpent")) %>% 
  select(-test_timeSpent, -demographics_timeSpent) %>%
  `/`(60) %>%
  melt %>%
  filter(value != max(value)) %>% #remove the outlier
  mutate(taskName = str_extract(variable, "^[^_]+(?=_)"),
         tuneFlag = str_extract(variable, "(?<=_)([^_]+)(?=_)"),
         network = substr(taskName, 1, 1)) %>%
  mutate(network = case_when(
    network == "A" ~ "Advice-seeking",
    network == "F" ~ "Friendship",
    network == "I" ~ "Friendship",
  )) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning"
  )) %>%
  mutate(task = case_when(taskName == "AC" ~ "Distinct\nCommunity",
                          taskName == "FC" ~ "Distinct\nCommunity",
                          taskName == "I" ~ "Isolate",
                          taskName == "AD" ~ "Density",
                          taskName == "FD" ~ "Density",
                          taskName == "ASP" ~ "Shortest Path\nLength",
                          taskName == "FSP" ~ "Shortest Path\nLength",
                          taskName == "AUN" ~ "Node\nStability",
                          taskName == "FUN" ~ "Node\nStability",
                          taskName == "AEO" ~ "Edge\nOccurrence",
                          taskName == "FEO" ~ "Edge\nOccurrence")) %>%
  mutate(task = factor(task, levels = c("Node\nStability", "Edge\nOccurrence", 
                                        "Distinct\nCommunity", 
                                        "Shortest Path\nLength", 
                                        "Density", "Isolate")))

timeDist_byTask_plot = ggplot(timeSpentDF, 
                              aes(x = task, y = value, color = tuneFlag)) +
  geom_boxplot(width = 0.2, position = position_dodge(.5)) + 
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) + 
  facet_grid(cols = vars(network), scales = "free") + 
  theme_minimal() + 
  labs(x = "Task", y = "Time (Mins)", color = NULL) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.margin=margin(t = -10, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 15))
################################################################################
# Time analysis
timeDF = userResponseDF %>% 
  select(ends_with("timeSpent")) %>% 
  select(-test_timeSpent, -demographics_timeSpent) %>% 
  `/`(60) %>%
  cbind(scoreDF) %>%
  filter(!row_number() == outlier_i)





# Analysis plan:
# Bootstrapped mean for multiple response
# 1. Correlation by task
# 2. Correlation by part
# 3. Correlation overall
distCode = c("AC_", "FC_", "I_", "ASP_", "FSP_", "AD_", "FD_")
guessCode = c("AUN_", "FUN_", "AEO_", "FEO_")
allTasks = c(distCode, guessCode)
tuneFlagVec = c("noTune", "tune")
corDF_lst = list()
for (task in allTasks){
  for (tuneFlag in tuneFlagVec){
    # Combine for task name
    taskName = paste0(task, tuneFlag)
    # Subset for relevant columns
    taskDF = timeDF %>% 
      select(starts_with(taskName))
    
    # For distribution elicitation tasks
    if (task %in% distCode){
      corDF = taskDF %>% 
        setNames(c("timeSpent", "performance")) %>%
        mutate(task = gsub("_", "", task),
               tuneFlag = tuneFlag)
    }
    # In case task is for deterministic guess that has multiple sub-questions
    # Compute the average performance metrics
    if (task %in% guessCode){
      absMeanPerformance = taskDF[,-1] %>% apply(1, mean) %>% abs
      corDF = data.frame("timeSpent" = taskDF[,1], 
                         "performance" = absMeanPerformance) %>%
        mutate(task = gsub("_", "", task),
               tuneFlag = tuneFlag)
    }
    corDF_lst %<>% list.append(corDF)
  }
}

allCorDF = corDF_lst %>% Reduce("rbind", .)


################################################################################
# Time vs. Performance Correlation by Task
################################################################################
cor_byTaskDF = corDF_lst %>%
  lapply(function(x) {
    pearsonCorStat = cor(x$timeSpent, x$performance, method = "pearson")
    corCI = DescTools::CorCI(pearsonCorStat, n = nrow(x), 
                             conf.level = 0.95, alternative = "two.sided")
    taskOutputDF = data.frame(taskName = x$task[1], 
                              tuneFlag = x$tuneFlag[1],
                              lwr.ci = corCI['lwr.ci'], 
                              cor = corCI['cor'], 
                              upr.ci = corCI['upr.ci'],
                              row.names = NULL)
    }) %>% 
  Reduce("rbind", .) %>%
  mutate(network = substr(taskName, 1, 1)) %>%
  mutate(network = case_when(
    network == "A" ~ "Advice-seeking",
    network == "F" ~ "Friendship",
    network == "I" ~ "Friendship",
  )) %>%
  mutate(tuneFlag = case_when(
    tuneFlag == "noTune" ~ "No Tuning",
    tuneFlag == "tune" ~ "Tuning"
  )) %>%
  mutate(task = case_when(taskName == "AC" ~ "Distinct\nCommunity",
                          taskName == "FC" ~ "Distinct\nCommunity",
                          taskName == "I" ~ "Isolate",
                          taskName == "AD" ~ "Density",
                          taskName == "FD" ~ "Density",
                          taskName == "ASP" ~ "Shortest Path\nLength",
                          taskName == "FSP" ~ "Shortest Path\nLength",
                          taskName == "AUN" ~ "Node\nStability",
                          taskName == "FUN" ~ "Node\nStability",
                          taskName == "AEO" ~ "Edge\nOccurrence",
                          taskName == "FEO" ~ "Edge\nOccurrence")) %>%
  mutate(task = factor(task, levels = c("Node\nStability", "Edge\nOccurrence", 
                                        "Distinct\nCommunity", "Shortest Path\nLength", 
                                        "Density", "Isolate")))

corOverViewPlot = 
  ggplot(cor_byTaskDF) +
  geom_errorbar(aes(x = task, ymin=lwr.ci, ymax=upr.ci, color = tuneFlag), 
                width=.3, size = .5,  position=position_dodge(0.5)) + 
  geom_point(aes(x = task, y = cor, color = tuneFlag), 
             size = 1, position = position_dodge(0.5)) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") + 
  scale_y_continuous(limits = c(-0.6, 0.45)) + 
  facet_grid(cols = vars(network), scales = "free") + 
  theme_minimal() + 
  labs(x = "Task Taxonomy", y = "Correlation Coefficient", color = NULL) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 5, angle=45),
        axis.text.y = element_text(size = 5),
        axis.title = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.margin=margin(t = -30, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 5),
        strip.text = element_text(size = 8))
