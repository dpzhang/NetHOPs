topDF = topVisParamDF %>%
  select(-userID, -complete) %>% 
  select(contains("_tune")) %>%
  melt(id = NULL) %>%
  mutate(task = str_extract(variable, "^[^_]+(?=_)"),
         param = str_extract(variable, "[^_]([^_]+)$")) %>%
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
  select(-variable)

bottomDF = bottomVisParamDF %>%
  select(-userID, -complete) %>% 
  select(contains("_tune")) %>%
  melt(id = NULL) %>%
  mutate(task = str_extract(variable, "^[^_]+(?=_)"),
         param = str_extract(variable, "[^_]([^_]+)$")) %>%
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
  select(-variable)

masterDF = rbind(topDF %>% mutate(userType = "Top"),
                 bottomDF %>% mutate(userType = "Bottom"))

compute_CI_subset = function(masterDF, 
                             parameter = c("alpha", "frameRate"), 
                             dataType = c("Advice-seeking", "Friendship"), 
                             user = c("Top", "Bottom")){
  set.seed(666)
  subsetDF = masterDF %>% 
    filter(param == parameter & data == dataType & userType == user)
  
  mu_hat = subsetDF$value %>% mean
  CI = subsetDF %>%
    specify(response = value) %>%
    generate(reps = 10000, type = "bootstrap") %>%
    calculate(stat = "mean") %>%
    get_confidence_interval(type = "se", point_estimate = mu_hat) %>%
    mutate(mu_hat = mu_hat) %>%
    select(lower_ci, mu_hat, upper_ci) %>%
    round(3)
  return(CI)
}