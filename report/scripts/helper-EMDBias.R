source("./scripts/helper-emd.R")

load("./data/groundTruth/interfaceData/network150.RData")
load("./data/groundTruth/netseq500.RData") # 500 resempled sets of 150 networks
data(kracknets, package = "NetData") # necessary css data objects: advice_nets, friendship_nets



create_QDP_plotDF = function(vec){
  outputDF = tibble(
    # equally spaced intervals of length 20 to ensure 20 circles
    p_less_than_x = seq(from = 1/20/ 2, 
                        to = 1 - (1/20/2), 
                        length=20),
    # quantile values based on p_less_than_x
    x = quantile(vec[!is.na(vec)], 
                 p_less_than_x) %>% round(0)
  )  
  return(outputDF)
}

compute_distinctCommunity = function(sampleNetLst150, baseNetLst){
  ##############################################################################
  # Prepwork: convert a list of networks into label and response
  ##############################################################################
  netLst_into_labelResponse = function(any_networkLst){
    countTable = any_networkLst %>% 
      lapply(function (net) igraph::V(net)$com %>% n_distinct) %>% 
      unlist %>%
      data.frame %>% 
      setNames("x") %>%
      create_QDP_plotDF %>%
      pull(x) %>%
      table
    userLabel = names(countTable) %>% as.numeric
    userInput = unname(countTable)
    return(list(userInput, userLabel))
  }
  # Compute the label and response for the baseline
  baseline_QDP_result = netLst_into_labelResponse(baseNetLst)
  baseline_input = baseline_QDP_result[[1]]
  baseline_label = baseline_QDP_result[[2]]
  # Compute the label and response for each netlist
  sample_QDP_result = netLst_into_labelResponse(sampleNetLst150)
  sample_input = sample_QDP_result[[1]]
  sample_label = sample_QDP_result[[2]]
  
  
  emdScore_sample = computeEMD(userInput = sample_input, userLabel = sample_label, 
                               groundTruth = baseline_input, groundTruthLabel = baseline_label, 
                               distanceType = "manhattan")
  # Compute the EMD
  return(emdScore_sample)
}

AC_EMDBias = advice_lst_lst %>%
  lapply(function(x) {compute_distinctCommunity(x, adviceNets150)}) %>%
  unlist
FC_EMDBias = friend_lst_lst %>% 
  lapply(function(x) {compute_distinctCommunity(x, friendNets150)}) %>%
  unlist



compute_shortestPath = function(sampleNetLst150, baseNetLst, networkType = c("advice", "friend")){
  if (networkType == "advice"){
    from = 10
    to = 21
  }else{
    from = 3
    to = 6
  }
  ##############################################################################
  # Prepwork: convert a list of networks into label and response
  ##############################################################################
  if (networkType == "advice"){
    from = 10
    to = 21
  }else{
    from = 3
    to = 6
  }
  
  find_shortestPath = function(ig, from, to) {
    # compute the shortest path
    spObject = igraph::all_shortest_paths(ig, from = from, to = to) %>% extract2("res")
    # compute the number of the shortest path
    numSP = length(spObject)
    if(numSP == 0){
      return(NA)
    }else{
      avgLength = spObject %>% lapply(function(sp) sp %>% length %>% `-`(1)) %>% unlist %>% mean
      return(avgLength)
    }
  }
  
  netLst_into_labelResponse = function(any_networkLst){
    countTable = any_networkLst %>% 
      lapply(function(net) net %>% find_shortestPath(from = from, to = to)) %>% 
      unlist %>%
      data.frame %>% 
      setNames("x") %>%
      create_QDP_plotDF %>%
      pull(x) %>%
      table
    userLabel = names(countTable) %>% as.numeric
    userInput = unname(countTable)
    return(list(userInput, userLabel))
  }
  # Compute the label and response for the baseline
  baseline_QDP_result = netLst_into_labelResponse(baseNetLst)
  baseline_input = baseline_QDP_result[[1]]
  baseline_label = baseline_QDP_result[[2]]
  # Compute the label and response for each netlist
  sample_QDP_result = netLst_into_labelResponse(sampleNetLst150)
  sample_input = sample_QDP_result[[1]]
  sample_label = sample_QDP_result[[2]]
  
  
  emdScore_sample = computeEMD(userInput = sample_input, userLabel = sample_label, 
                               groundTruth = baseline_input, groundTruthLabel = baseline_label, 
                               distanceType = "manhattan")
  # Compute the EMD
  return(emdScore_sample)
}

ASP_EMDBias = advice_lst_lst %>%
  lapply(function(x) {compute_shortestPath(x, adviceNets150, "advice")}) %>%
  unlist
FSP_EMDBias = friend_lst_lst %>% 
  lapply(function(x) {compute_shortestPath(x, friendNets150, "friend")}) %>%
  unlist


compute_isolate = function(sampleNetLst150, baseNetLst){
  ##############################################################################
  # Prepwork: convert a list of networks into label and response
  ##############################################################################
  netLst_into_labelResponse = function(any_networkLst){
    countTable = any_networkLst %>% 
      lapply(function (net) net %>% igraph::degree(mode="all") %>% `==`(0) %>% sum) %>% 
      unlist %>%
      data.frame %>% 
      setNames("x") %>%
      create_QDP_plotDF %>%
      pull(x) %>%
      table
    userLabel = names(countTable) %>% as.numeric
    userInput = unname(countTable)
    return(list(userInput, userLabel))
  }
  # Compute the label and response for the baseline
  baseline_QDP_result = netLst_into_labelResponse(baseNetLst)
  baseline_input = baseline_QDP_result[[1]]
  baseline_label = baseline_QDP_result[[2]]
  # Compute the label and response for each netlist
  sample_QDP_result = netLst_into_labelResponse(sampleNetLst150)
  sample_input = sample_QDP_result[[1]]
  sample_label = sample_QDP_result[[2]]
  
  
  emdScore_sample = computeEMD(userInput = sample_input, userLabel = sample_label, 
                               groundTruth = baseline_input, groundTruthLabel = baseline_label, 
                               distanceType = "manhattan")
  # Compute the EMD
  return(emdScore_sample)
}
AI_EMDBias = advice_lst_lst %>%
  lapply(function(x) {compute_isolate(x, adviceNets150)}) %>%
  unlist
FI_EMDBias = friend_lst_lst %>%
  lapply(function(x) {compute_isolate(x, friendNets150)}) %>%
  unlist


compute_density = function(sampleNetLst150, baseNetLst){
  ##############################################################################
  # Prepwork: convert a list of networks into label and response
  ##############################################################################
  netLst_into_labelResponse = function(any_networkLst){
    countTable = any_networkLst %>% 
      lapply( function (net) igraph::edge_density(net, loops = F)*100) %>% 
      unlist %>%
      round(0) %>%
      data.frame %>% 
      setNames("x") %>%
      create_QDP_plotDF %>%
      pull(x) %>%
      table
    userLabel = names(countTable) %>% as.numeric
    userInput = unname(countTable)
    return(list(userInput, userLabel))
  }
  # Compute the label and response for the baseline
  baseline_QDP_result = netLst_into_labelResponse(baseNetLst)
  baseline_input = baseline_QDP_result[[1]]
  baseline_label = baseline_QDP_result[[2]]
  # Compute the label and response for each netlist
  sample_QDP_result = netLst_into_labelResponse(sampleNetLst150)
  sample_input = sample_QDP_result[[1]]
  sample_label = sample_QDP_result[[2]]
  
  
  emdScore_sample = computeEMD(userInput = sample_input, userLabel = sample_label, 
                               groundTruth = baseline_input, groundTruthLabel = baseline_label, 
                               distanceType = "manhattan")
  # Compute the EMD
  return(emdScore_sample)
}

AD_EMDBias = advice_lst_lst %>%
  lapply(function(x) {compute_density(x, adviceNets150)}) %>%
  unlist
FD_EMDBias = friend_lst_lst %>%
  lapply(function(x) {compute_density(x, friendNets150)}) %>%
  unlist


EMDBiasDF = data.frame("AC" = AC_EMDBias, "FC" = FC_EMDBias,
                       "FI" = FI_EMDBias,
                       "ASP" = ASP_EMDBias, "FSP" = FSP_EMDBias,
                       "AD" = AD_EMDBias, "FD" = FD_EMDBias)
write.csv(EMDBiasDF, "./data/EMDbias.csv", row.names = F)

EMDBiasDF = read.csv("./data/EMDbias.csv") %>% View


biasCIDF = data.frame(mean = EMDBiasDF %>% apply(2, mean) , 
                      se = EMDBiasDF %>% 
                        apply(2, function(x) sd(x) / sqrt(length(x)))) %>%
  mutate(var = rownames(.)) %>%
  mutate(network = ifelse(substring(var, 1, 1) == "A", 
                          "Advice-seeking", "Friendship")) %>%
  mutate(task = case_when(var == "AC" ~ "Distinct\nCommunity",
                          var == "FC" ~ "Distinct\nCommunity",
                          var == "FI" ~ "Isolate",
                          var == "AD" ~ "Density",
                          var == "FD" ~ "Density",
                          var == "ASP" ~ "Shortest Path\nLength",
                          var == "FSP" ~ "Shortest Path\nLength")) %>%
  mutate(task = factor(task, levels = c("Distinct\nCommunity", "Density", 
                                        "Shortest Path\nLength", "Isolate")))

emdBiasPlot = ggplot(biasCIDF, aes(x=task, y=mean, group = task)) + 
  geom_point(color = "#4E2A84", size = 0.8)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05), color = "#4E2A84") + 
  labs(x = NULL, y = "Sampling Error (EMD)") + 
  facet_grid(cols = vars(network), scale = "free", drop = T) + 
  theme_minimal() +
  theme(axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        strip.text.x = element_text(size = 12))