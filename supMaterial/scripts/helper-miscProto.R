subset_netLstByColors = function(netLst, colorInterestVec, complement = F){
  # define function based on a single net
  check_colorMatchSingleNet = function(net, colorInterestVec) {
    colors = igraph::V(net)$color # vector of all node colors
    uniqueColors = colors %>% unique # vector of all unique colors
    
    criteria = sum(colorInterestVec %in% uniqueColors) == length(colorInterestVec) & length(uniqueColors) == length(colorInterestVec)
    identification = ifelse(criteria, T, F)
    return(identification)
  }
  
  matchedNetwork = netLst %>% lapply(check_colorMatchSingleNet, colorInterestVec) %>% unlist
  subset_netLst = netLst[matchedNetwork] #%>% lapply(asNetwork)  
  
  complement_subset_netLst = netLst[!matchedNetwork]
  
  if (complement){
    return(complement_subset_netLst)
  } else {
    return(subset_netLst)  
  }
}

sample_lst = function(netLst, numSample, seed){
  set.seed(seed)
  numNets = length(netLst)
  sampleIndex = sample(1:numNets, numSample)
  return(netLst[sampleIndex])
}

find_uniqueColorSets = function(netLst){
  numNets = length(netLst)
  
  # get the color vault 
  colorVault = get_tableauColor()
  
  uniqueColorLst = netLst %>% lapply(function(net) igraph::V(net)$color %>% unique)
  
  newColorOrders = uniqueColorLst %>% lapply(function(colorSet) colorSet %>% match(colorVault) %>% order)
  
  for (i in 1:numNets){
    uniqueColorLst[[i]] = uniqueColorLst[[i]][newColorOrders[[i]]]
  }
  
  # convert unique color sets to a long string
  df = uniqueColorLst %>% lapply(toString) %>% unlist %>% table %>%
    as.data.frame %>%
    setNames(c("colorSet", "freq")) %>% 
    arrange(desc(freq))
  
  return(df)
}

extract_colorSet = function(colorSetCountsDF, n) {
  colorVec = colorSetCountsDF %>%
    pull(colorSet) %>% 
    extract(n) %>% 
    as.character %>% 
    strsplit(split = ", ") %>% 
    extract2(1)
  return(colorVec)
}

colorVault_backup = c("#4E79A7","#F28E2B","#59A14F","#B6992D","#499894","#E15759","#79706E","#D37295","#B07AA1","#9D7660","#A0CBE8","#FFBE7D","#8CD17D","#F1CE63","#86BCB6","#FF9D9A","#BAB0AC","#FABFD2","#D4A6C8","#D7B5A6")