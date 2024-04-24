pvalue_sliderParam_variance = function(visParamDF, taskName, paramName){
  taskKey = paste0(taskName, "_", "tune", "_")
  
  # Prepare parameter for anova analysis
  param_melted = visParamDF %>%
    select(starts_with(taskKey) & contains(paramName)) %>%
    melt(id = NULL) %>%
    mutate(subTask = str_remove(variable, taskKey) %>% str_remove(paste0("_", paramName))) %>%
    select(-variable)
  
  # ggboxplot(param_melted, x = "subTask", y = "value", 
  #           color = "subTask", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  #           ylab = "Alpha", xlab = "Sub-task")
  # ggline(param_melted, x = "subTask", y = "value", 
  #        add = c("mean_se", "jitter"), 
  #        ylab = "Alpha", xlab = "Sub-task")
  
  # NULL: the mean of different groups are the same
  kw_rankSumTest = kruskal.test(value ~ subTask, data = param_melted)
  p_value = kw_rankSumTest$p.value
  #res.aov = aov(value ~ subTask, data = param_melted)
  #p_value = summary(res.aov)[[1]]$'Pr(>F)'[[1]]
  return(p_value)
}


pvalue_switcherParam_variance = function(visParamDF, taskName, paramName, returnPvalue = T){
  taskKey = paste0(taskName, "_", "tune", "_")
  
  # Prepare parameter for anova analysis
  switcher_paramDF = visParamDF %>%
    select(starts_with(taskKey) & contains(paramName)) 
  
  # Proportion test
  propTest_result = prop.test(x = switcher_paramDF %>% apply(2, sum), 
                              n = switcher_paramDF %>% apply(2, length))
  p_value = propTest_result$p.value
  if(returnPvalue){
    return(p_value)  
  }else{
    return(propTest_result)
  }
}

compute_propTest_pvalue_switcherParams = function(visParamDF){
  allTaskNames = c("AC", "FC", "I", "ASP", "FSP", "AD", "FD", "AUN", "FUN", "AEO", "FEO")
  switcher_paramNames = c("darkEdges", "convexHull", "nodeColor", "nodeLabel")
  
  containerLst = list()
  for(param in switcher_paramNames){
    i = 1
    param_pvalues = c()
    for (task in allTaskNames){
      param_pvalues[i] = pvalue_switcherParam_variance(visParamDF, task, param)
      i = i + 1
    }
    containerLst[[param]] = param_pvalues
  }
  
  outputDF = do.call("rbind", containerLst) %>%
    `colnames<-`(allTaskNames)
  
  return(outputDF)
}

compute_kruskalWallis_pvalue_sliderParams = function(visParamDF){
  allTaskNames = c("AC", "FC", "I", "ASP", "FSP", "AD", "FD", "AUN", "FUN", "AEO", "FEO")
  slider_paramNames = c("alpha", "frameRate")
  
  containerLst = list()
  for(param in slider_paramNames){
    i = 1
    param_pvalues = c()
    for(task in allTaskNames){
      param_pvalues[i] = pvalue_sliderParam_variance(visParamDF, task, param)
      i = i + 1
    }
    containerLst[[param]] = param_pvalues
  }
  
  outputDF = do.call("rbind", containerLst) %>%
    `colnames<-`(allTaskNames)
  
  return(outputDF)
}

compute_allParam_variance_bySubtasks = function(visParamDF){
  outputDF = rbind(compute_kruskalWallis_pvalue_sliderParams(visParamDF),
                   compute_propTest_pvalue_switcherParams(visParamDF))
  return(outputDF)
}