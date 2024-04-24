scoreMeltedDF = scoreDF %>%
  melt(id = NULL) %>%
  mutate(task = str_extract(variable, regEx_taskName),
         tuneFlag = str_extract(variable, regEx_tungFlag),
         question = str_extract(variable, regEx_taskquestion)) %>% 
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
                          task == "FEO" ~ "Friendship"),
         question = case_when(
           task == "AEO" & question == "edgeOrange" ~ "Edge 5 => 20",
           task == "AEO" & question == "edgeBlue" ~ "Edge 10 => 16",
           task == "FEO" & question == "edgeOrange" ~ "Edge 2 => 18",
           task == "FEO" & question == "edgeBlue" ~ "Edge 7 => 14",
           question == "node10Green" ~ "Node 10",
           question == "node12Blue" ~ "Node 12",
           question == "node13Blue" ~ "Node 13",
           question == "node15Blue" ~ "Node 15",
           question == "node16Green" ~ "Node 16",
           question == "node17Blue" ~ "Node 17",
           question == "node21Green" ~ "Node 21",
           question == "node7Green" ~ "Node 7",
           question == "EMD" ~ "EMD"
         ),
         tuneFlag = case_when(
           tuneFlag == "noTune" ~ "No Tuning",
           tuneFlag == "tune" ~ "Tuning"
         ),
         task = case_when(
           task == "AD" ~ "Density Estimation on \nAdvice-seeking",
           task == "FD" ~ "Density Estimation on \nFriendship",
           task == "AC" ~ "Distinct Community on \nAdvice-seeking",
           task == "FC" ~ "Distinct Community on \nFriendship",
           task == "ASP" ~ "Shortest Path Length on \nAdvice-seeking",
           task == "FSP" ~ "Shortest Path Length on \nFriendship",
           task == "I" ~ "Isolate Overview \non Friendship",
           task == "AUN" ~ "Node Community Stability on \nAdvice-seeking",
           task == "FUN" ~ "Node Community Stability on \nFriendship",
           task == "AEO" ~ "Edge Occurrence on \nAdvice-seeking",
           task == "FEO" ~ "Edge Occurrence on \nFriendship"
         )) %>%
  mutate(task = factor(task, levels = c("Shortest Path Length on \nAdvice-seeking", "Shortest Path Length on \nFriendship",
                                        "Isolate Overview \non Friendship",
                                        "Distinct Community on \nAdvice-seeking", "Distinct Community on \nFriendship",
                                        "Density Estimation on \nAdvice-seeking", "Density Estimation on \nFriendship",
                                        "Node Community Stability on \nAdvice-seeking", "Node Community Stability on \nFriendship",
                                        "Edge Occurrence on \nAdvice-seeking","Edge Occurrence on \nFriendship")))


# Compute the confidence intervals

compute_CI_all = function(scoreMeltedDF){
  dataNames = scoreMeltedDF %>% pull(data) %>% unique
  questionNames = scoreMeltedDF %>% pull(question) %>% unique
  tuneFlagNames = scoreMeltedDF %>% pull(tuneFlag) %>% unique
  taskNames = scoreMeltedDF %>% pull(task) %>% unique
  
  container_lst = list()
  for (taskName in taskNames){
    for (dataName in dataNames){
      for (questionName in questionNames){
        for (tuneFlagName in tuneFlagNames){
          tempDat = scoreMeltedDF %>%
            filter(data == dataName & question == questionName & tuneFlag == tuneFlagName & task == taskName)
          if (nrow(tempDat) != 0){
            # Get the sample mean
            mu_hat = tempDat %>% pull(value) %>% mean
            # Boostrap to compute confidence intervals
            set.seed(2333)
            CI_df = tempDat %>%
              specify(response = value) %>%
              generate(reps = 10000, type = "bootstrap") %>% 
              calculate(stat = "mean") %>%
              get_confidence_interval(type = "se", point_estimate = mu_hat) %>%
              mutate(mu_hat = mu_hat) %>%
              select(lower_ci, mu_hat, upper_ci) %>%
              mutate(task = taskName, data = dataName, question = questionName, tuneFlag = tuneFlagName)
            container_lst = list.append(container_lst, CI_df) 
          }
        }
      }
    }
  }
  
  outputDF = do.call(rbind, container_lst)
  return(outputDF)
}

compute_CI_deterministic_pooled = function(scoreMeltedDF){
  subsetDF = scoreMeltedDF %>% filter(question!="EMD")
  taskNames = subsetDF %>% pull(task) %>% unique
  tuneFlagNames = scoreMeltedDF %>% pull(tuneFlag) %>% unique
  
  container_lst = list()
  for(taskName in taskNames){
    for(tuneFlagName in tuneFlagNames){
      tempDat = scoreMeltedDF %>%
        filter(task == taskName & tuneFlag == tuneFlagName)
      
      # Get the sample mean
      mu_hat = tempDat %>% pull(value) %>% mean
      # Boostrap to compute confidence intervals
      set.seed(2333)
      CI_df = tempDat %>%
        specify(response = value) %>%
        generate(reps = 10000, type = "bootstrap") %>% 
        calculate(stat = "mean") %>%
        get_confidence_interval(type = "se", point_estimate = mu_hat) %>%
        mutate(mu_hat = mu_hat) %>%
        select(lower_ci, mu_hat, upper_ci) %>%
        mutate(task = taskName, tuneFlag = tuneFlagName)
      container_lst = list.append(container_lst, CI_df)  
    }
  }
  outputDF = do.call(rbind, container_lst)
  return(outputDF)
}

CIDF = compute_CI_all(scoreMeltedDF)

deterministicPerformance = scoreMeltedDF %>%
  filter(question != "EMD") %>%
  mutate(question = factor(question, levels = c("Node 12", "Node 17", "Node 10", "Node 16",
                                                "Node 13", "Node 15", "Node 7", "Node 21",
                                                "Edge 5 => 20", "Edge 10 => 16",
                                                "Edge 2 => 18", "Edge 7 => 14")))
deterministic_CI = CIDF %>% filter(question != "EMD")

deterministic_CI_pooled = compute_CI_deterministic_pooled(scoreMeltedDF)
deterministicPlot_pooled = deterministicPerformance %>%
  ggplot +
  geom_jitter(aes(x = task, y = value, color = tuneFlag), 
              alpha = 0.1, 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = .5), 
              size = 2, show.legend = T) +
  geom_errorbar(data = deterministic_CI_pooled,
                aes(x = task, ymin= lower_ci, ymax= upper_ci, color = tuneFlag), 
                width=.2,
                size = 1.5,
                position=position_dodge(width = .3), show.legend = T) + 
  geom_point(dat = deterministic_CI_pooled,
             aes(x = task, y = mu_hat, color = tuneFlag),  
             position=position_dodge(width = .3), size = 3, show.legend = T) +
  scale_x_discrete(labels = function(x) str_extract(x, "Advice-seeking|Friendship")) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) + 
  labs(x = NULL, y = "Diff. from Truth", color = NULL) + 
  facet_grid(cols = vars(task), scales = "free") +
  theme_minimal() + 
  theme(plot.margin = margin(t = 1,r = 0, b = 0, l = 0, "cm"),
        legend.position = "bottom",
        legend.title=element_blank(),
        #axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=10),
        #axis.title = element_text(size=17),
        strip.text.x = element_text(size = 16))
#strip.text.x = element_blank()
#strip.text.y = element_text(size = 14),
#legend.text = element_text(size = 14))


#########################################################################################################################
discretePerformance = scoreMeltedDF %>%
  filter(question == "EMD" & !task %in% c("Density Estimation on \nAdvice-seeking", "Density Estimation on \nFriendship"))
discrete_CI = CIDF %>% filter(question == "EMD" & !task %in% c("Density Estimation on \nAdvice-seeking", "Density Estimation on \nFriendship"))
# Start with discrete tasks
discretePlotDF = discretePerformance %>%
  filter(question == "EMD") %>% 
  mutate(task = factor(task, levels = c("Shortest Path Length on \nAdvice-seeking", "Shortest Path Length on \nFriendship",
                                        "Isolate Overview \non Friendship",
                                        "Distinct Community on \nAdvice-seeking", "Distinct Community on \nFriendship"))) %>%
  mutate(facet_order = case_when(
    task == "Shortest Path Length on \nAdvice-seeking" ~ 1,
    task == "Shortest Path Length on \nFriendship" ~ 2,
    task == "Isolate Overview \non Friendship" ~ 3,
    task == "Distinct Community on \nAdvice-seeking" ~ 4, 
    task == "Distinct Community on \nFriendship" ~ 5
  )) %>%
  mutate(facet_order = factor(facet_order, levels = c(1, 2, 3, 4, 5)))
discretePlot =  ggplot(discretePlotDF) + 
  geom_jitter(aes(x = task, y = value, color = tuneFlag), 
              alpha = 0.1, 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = .8), 
              size = 1, show.legend = T) +
  geom_errorbar(data = discrete_CI,
                aes(x = task, ymin= lower_ci, ymax= upper_ci, color = tuneFlag), 
                width=.2,
                size = 0.5,
                position=position_dodge(width = .8), show.legend = T) + 
  geom_point(dat = discrete_CI,
             aes(x = task, y = mu_hat, color = tuneFlag),  
             position=position_dodge(width = .8), size = 1, show.legend = T) +
  scale_x_discrete(labels = function(x) str_extract(x, "Advice-seeking|Friendship")) + 
  scale_y_continuous(limits = c(0, 5)) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) + 
  labs(x = NULL, y = "EMD", color = NULL) + 
  #facet_grid(cols = vars(facet_order), scales = "free") +
  theme_minimal() + 
  theme(plot.margin = margin(t = 0.5,r = 0, b = 0, l = 0, "cm"),
        legend.position = "none",
        #legend.title=element_blank(),
        #legend.text = element_text(size = 14)
        #axis.text.x = element_text(size=14),
        #axis.text.x = element_blank(),
        #axis.title = element_text(size=17),
        #axis.title = element_blank(),
        #strip.text.x = element_text(size = 16),
        #strip.text.x = element_blank(),
        #strip.text.y = element_text(size = 14)
        axis.title.y = element_text(size = 6),
        axis.text.y = element_text(size=7),
        #axis.text.x = element_text(size = 4, vjust = 10, face = "bold"))
        axis.text.x = element_blank())
discretePlot


continuousPerformance = scoreMeltedDF %>%
  filter(task %in% c("Density Estimation on \nAdvice-seeking", "Density Estimation on \nFriendship"))
continuous_CI = CIDF %>% filter(task %in% c("Density Estimation on \nAdvice-seeking", "Density Estimation on \nFriendship"))
# Start with discrete tasks
continuousPlot = continuousPerformance %>%
  ggplot +
  geom_jitter(aes(x = task, y = value, color = tuneFlag), 
              alpha = 0.1, 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = .8), 
              size = 1, show.legend = T) +
  geom_errorbar(data = continuous_CI,
                aes(x = task, ymin= lower_ci, ymax= upper_ci, color = tuneFlag), 
                width=.2,
                size = .5,
                position=position_dodge(width = .8), show.legend = T) + 
  geom_point(dat = continuous_CI,
             aes(x = task, y = mu_hat, color = tuneFlag),  
             position=position_dodge(width = .8), size = 1, show.legend = T) + 
  scale_x_discrete(labels = function(x) str_extract(x, "Advice-seeking|Friendship")) + 
  scale_y_continuous(breaks = pretty_breaks(n = 5)) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) + 
  labs(x = NULL, y = "EMD", color = NULL) + 
  #facet_grid(cols = vars(task), scales = "free") +
  theme_minimal() + 
  theme(plot.margin = margin(t = 0.5,r = 0, b = 0, l = 0, "cm"),
        legend.position = "none",
        #legend.title=element_blank(),
        #legend.text = element_text(size = 14)
        #axis.text.x = element_text(size=14),
        #axis.text.x = element_blank(),
        #axis.title = element_text(size=17),
        #axis.title = element_blank(),
        #strip.text.x = element_text(size = 16),
        #strip.text.x = element_blank(),
        #strip.text.y = element_text(size = 14)
        axis.title.y= element_blank(),
        #axis.title.x= element_text(size = 7), 
        axis.text.y = element_text(size=7),
        #axis.text.x = element_text(size = 4, vjust = 10, face = "bold")
        axis.text.x = element_blank())
#continuousPlot
allEMD = ggarrange(discretePlot, continuousPlot, ncol = 2, widths = c(2,1))
allEMD
#ggsave("./img/allEMD-performance.png", allEMD, width = 10, height = 5, units = "cm")

######################################################################




# Node stability
nodeStabilityDF = deterministicPerformance %>%
  filter(task %in% c("Node Community Stability on \nAdvice-seeking",
                     "Node Community Stability on \nFriendship"))
nodeStability_CI = deterministic_CI %>%
  filter(task %in% c("Node Community Stability on \nAdvice-seeking",
                     "Node Community Stability on \nFriendship"))
nodeStabilityPlot = nodeStabilityDF %>%
  ggplot +
  geom_jitter(aes(x = question, y = value, color = tuneFlag), 
              alpha = 0.1, 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = .8), 
              size = 0.5, show.legend = T) +
  geom_errorbar(data = nodeStability_CI,
                aes(x = question, ymin= lower_ci, ymax= upper_ci, color = tuneFlag), 
                width=.2,
                size = 0.5,
                position=position_dodge(width = .8), show.legend = T) + 
  geom_point(dat = nodeStability_CI,
             aes(x = question, y = mu_hat, color = tuneFlag),  
             position=position_dodge(width = .8), size = .8, show.legend = T) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) + 
  scale_y_continuous(limits = c(-45, 30)) + 
  labs(x = NULL, y = "Probability Error", color = NULL) + 
  #facet_grid(cols = vars(task), scales = "free") +
  theme_minimal() + 
  theme(plot.margin = margin(t = .3,r = 0, b = 0, l = 0, "cm"),
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 5),
        legend.margin=margin(t = -5, r = 0, b = 0, l = 0),
        #axis.text.x = element_text(size=14),
        #axis.text.x = element_blank(),
        #axis.title = element_text(size=17),
        #axis.title = element_blank(),
        #strip.text.x = element_text(size = 16),
        #strip.text.x = element_blank(),
        #strip.text.y = element_text(size = 14)
        axis.title.y = element_text(size = 7),
        axis.text.y = element_text(size=7),
        axis.text.x = element_blank())
nodeStabilityPlot


# Node stability
edgeOccurrenceDF = deterministicPerformance %>%
  filter(task %in% c("Edge Occurrence on \nAdvice-seeking",
                     "Edge Occurrence on \nFriendship"))
edgeOccurrence_CI = deterministic_CI %>%
  filter(task %in% c("Edge Occurrence on \nAdvice-seeking",
                     "Edge Occurrence on \nFriendship"))
edgeOccurrencePlot = edgeOccurrenceDF %>%
  ggplot +
  geom_jitter(aes(x = question, y = value, color = tuneFlag), 
              alpha = 0.1, 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = .8), 
              size = 0.5, show.legend = T) +
  geom_errorbar(data = edgeOccurrence_CI,
                aes(x = question, ymin= lower_ci, ymax= upper_ci, color = tuneFlag), 
                width=.2,
                size = 0.5,
                position=position_dodge(width = .8), show.legend = T) + 
  geom_point(dat = edgeOccurrence_CI,
             aes(x = question, y = mu_hat, color = tuneFlag),  
             position=position_dodge(width = .8), size = .8, show.legend = T) + 
  scale_color_manual(values = c("#4e79a7", "#f28e2b")) + 
  scale_y_continuous(limits = c(-45, 30)) +  
  labs(x = NULL, y = NULL, color = NULL) + 
  #facet_grid(cols = vars(task), scales = "free") +
  theme_minimal() + 
  theme(plot.margin = margin(t = 0.3,r = 0, b = 0, l = 0, "cm"),
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 5),
        legend.margin=margin(t = -5, r = 0, b = 0, l = 0),
        axis.text.y = element_text(size = 5, face = "bold"),
        axis.text.x = element_blank())
edgeOccurrencePlot

allDeterministic = ggarrange(nodeStabilityPlot, edgeOccurrencePlot, ncol = 2, widths = c(2, 1), common.legend = T, legend = "bottom")
allDeterministic
#ggsave("./img/allDeterministic-performance.png", allDeterministic, width = 12, height = 8, units = "cm")


