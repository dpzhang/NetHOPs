get_cssNodesDF = function(){
  data(kracknets, package = "NetData")
  nodesDF = cbind("name" = 1:21, attributes) %>%
    dplyr::rename(age = AGE, tenure = TENURE, level = LEVEL, dept = DEPT)
  return(nodesDF)
}

get_adjmProb = function(css_net_lst) {
  adjmCount = lapply(css_net_lst, function(graph) {asIgraph(graph)} %>% get.adjacency(sparse = F)) %>% 
    Reduce("+", .)
  adjmProb = adjmCount / nrow(adjmCount)
  return(adjmProb)
}

draw_nNets = function(css_net_lst, n, seed = 1) {
  verticesDF = get_cssNodesDF()
  adjm_prob = get_adjmProb(css_net_lst)
  # empty list to contain draws
  lst_igs = list()
  set.seed(seed)
  for (i in 1:n) {
    print(paste("Sampling Network", i))
    random_draw = apply(adjm_prob, 1:2, function(x) rbern(n = 1, prob = x))
    lst_igs[[i]] = igraph::graph_from_adjacency_matrix(random_draw) %>%
      get.edgelist() %>%
      graph_from_data_frame(vertices = verticesDF)
  }
  return (lst_igs)
}

#remove duplicated columns
remove_duplicatedCols = function(df) {
  df = df[!duplicated(as.list(df))]
  return(df)
}

# generate network, community detection on networks, find the max number of communities
detect_communities = function(netLst, FUN, seed = 1){
  set.seed(seed)  
  comLst = netLst %>% lapply(FUN)
  comAssignLst = comLst %>% lapply(membership)
  # assign community as an edge attribute
  for (i in 1:length(netLst)){
    print(paste("Detecting Community", i))
    igraph::V(netLst[[i]])$com = comAssignLst[[i]]
  }
  return(netLst)
}

# create a binary edge attribute for each network indicating: if the source node and the target node is in the same community
label_edgeSameCommunity = function(net){
  netRelations = igraph::as_data_frame(net)
  nodeAssigned = igraph::V(net)$com
  
  fromNodesID = as.numeric(netRelations$from)
  toNodesID  = as.numeric(netRelations$to)
  E(net)$same = as.numeric(nodeAssigned[fromNodesID] == nodeAssigned[toNodesID])
  return(net)
}


create_coCommunityEdgelist = function(netLst){
  numNodes = netLst %>% extract2(1) %>% vcount
  dyadsDF = make_full_graph(numNodes, directed = T, loops = F) %>% 
    get.edgelist %>% 
    as.data.frame %>% 
    setNames(c('from', 'to'))
  
  count_sameComNodes = netLst %>% 
    lapply(label_edgeSameCommunity) %>%
    lapply(igraph::as_data_frame) %>%
    do.call(rbind, .) %>% # rowbind the list 
    mutate(from = as.numeric(from), to = as.numeric(to)) %>%
    group_by(from, to) %>% # group by edge
    summarise(freq = sum(same), .groups = 'drop') %>%# count the number of occurrences
    as.data.frame %>%
    right_join(dyadsDF, by = c("from", "to")) %>%
    arrange(desc(freq))
  
  return(count_sameComNodes)
}


compute_refDF = function(netLst){
  # network prep
  netLst_length = length(netLst)
  nodesDF = as.data.frame( vertex_attr( netLst[[1]]) )
  numNodes = igraph::V(netLst[[1]]) %>% length()
  
  freqEdgeComHomo = netLst %>% 
    lapply(label_edgeSameCommunity) %>%
    lapply(igraph::as_data_frame) %>%
    do.call(rbind, .) %>% # rowbind the list 
    group_by(from, to) %>% # group by edge
    summarise(freq = sum(same), .groups = 'drop') %>%# count the number of occurrences
    arrange(desc(freq))  
  
  # create a color reference dataframe
  colorReferenceDF = data.frame(matrix(0, ncol = netLst_length, nrow = numNodes)) %>% 
    setNames(paste0('t', 1:netLst_length))
  for (i in 100:1){
    colorReferenceDF[,i] = freqEdgeComHomo %>%
      filter(freq >= i) %>%
      graph_from_data_frame(vertices = nodesDF) %>%
      components() %>%
      magrittr::extract2('membership')
  }
  # remove all duplicated columns
  refDF = remove_duplicatedCols(colorReferenceDF)
  # remove the ones that have the same component
  #refDF = colorReferenceDF[,apply(colorReferenceDF, 2, n_distinct) != 1]
  
  return(refDF)
}


# Threshold indicates how many times does two nodes connect to each other in the same community
# the higher the threshold, the more likely the two nodes are in the same community 
# If starting the threshold from 1, then as the threshold increases, node will become detached from its component and become isolates
# So, this function figures out when does this node become an isolate at what thresholdcompute the threshold where a node stop becoming an isolate
compute_isolateThreshold = function(netLst){
  nodesDF = netLst %>% extract2(1) %>% vertex_attr %>% as.data.frame %>% dplyr::select(name, dplyr::everything())
  numNodes = nrow(nodesDF)
  countDF = matrix(0, ncol = length(netLst), nrow = numNodes) %>%
    data.frame %>%
    setNames(paste0('t', 1:length(netLst)))
  
  for (i in 1:length(netLst)){
    freq_homoComNodes = create_coCommunityEdgelist(netLst) %>%
      filter(freq >= i) %>%
      graph_from_data_frame(vertices = nodesDF) %>% 
      asNetwork %>% 
      isolates  
    countDF[freq_homoComNodes,i] = 1
  }
  
  nodeID_vec = 1:numNodes
  isolate_vec = countDF %>% apply(1, function(nodeIsolateVec) {which(nodeIsolateVec == 1) %>% head(1)})
  isolateThresholdDF = data.frame(nodeID = nodeID_vec, tIsolate = isolate_vec) %>%
    arrange(desc(tIsolate))
  return(isolateThresholdDF)
}

# put component into a list by component
create_componentLst = function(componentVec){
  containerLst = list()
  for ( i in 1:n_distinct(componentVec) ){
    containerLst[[i]] = which(componentVec == i)
  }
  return(containerLst)
}

get_tableauColor = function(palette_name = "Classic 10"){
  # prepare colors
  colorVault = ggthemes_data[["tableau"]][["color-palettes"]][["regular"]] %>% 
    extract2(palette_name) %>% 
    pull(value)
  
  if (palette_name == "Tableau 20"){
    numTotalColors = length(colorVault)
    order_index = seq(1, numTotalColors, by = 2) %>% append(seq(1, numTotalColors, by = 2) + 1)
    colorVault = colorVault[order_index]  
  }
  
  return(colorVault)
}

##################################################################################
# Assign color 
# "T" isolate threshold vector
# "C" color vector for nodes
assign_nodeColors = function(netLst, flag_isolateThreshold = F){
  nodesDF = netLst %>% extract2(1) %>% vertex_attr %>% as.data.frame %>% dplyr::select(name, dplyr::everything())
  numNodes = nrow(nodesDF)
  
  isolateThreshold = compute_isolateThreshold(netLst)
  
  # prepare colors
  colorVault = get_tableauColor()
  C = rep(NA, times = numNodes) %>%
    `names<-`(1:numNodes)# color assignment to node
  
  # loop through each threshold t
  for (t in 1:max(isolateThreshold$tIsolate)){
    # Create a subgraph by removing all edges with weight < t in CG
    subg = create_coCommunityEdgelist(netLst) %>%
      filter(freq >= t) %>%
      graph_from_data_frame(vertices = nodesDF)
    
    # Find all connected components at this threshold
    CC = subg %>% components %>% membership %>% unname %>% create_componentLst
    
    # For each component that consists of > 1 node, find the max isolate score of all nodes in that component. 
    # In case when the number of connected component = node number, break the loop
    # because it means it has reached a point where all nodes become isolates
    if (length(CC) == numNodes){break} # -- special case
    filteredDFLst = list()
    max_tscore = list()
    max_vertex = list()
    # Loop through each unique component in the subgraph
    for (i in 1:length(CC)){
      comp = CC[[i]]
      # In the component, find the node with the greatest isolate score
      filteredDF = isolateThreshold %>% 
        filter(nodeID %in% comp) %>% 
        arrange(nodeID) %>% 
        filter(tIsolate == max(tIsolate))
      # Keep trach of the node(s) with the greatest isolate score
      filteredDFLst[[i]] = filteredDF
      # if the component contains greater than 1 node
      if (length(comp) > 1){
        max_vertex[[i]] = filteredDF %>%
          select(nodeID) %>%
          slice(1) %>%
          pull
        max_tscore[[i]] = filteredDF %>%
          select(tIsolate) %>%
          slice(1) %>% 
          pull
      }
    }
    # new ordering (starting with the most stable nodes)
    re_ordering_index = unlist(max_tscore) %>% order(decreasing = T)
    nodes_needAssignment = unlist(max_vertex)[re_ordering_index]
    print( paste("@Threshold:", t) )
    print( paste("Nodes with highest isolate score", toString(nodes_needAssignment)) )
    print( paste("The highest isolate scores are:", toString(unlist(max_tscore)[re_ordering_index]) ) )
    print( "")
    
    # assign colors
    for (i in nodes_needAssignment){
      if (C[i] %>% is.na){
        C[i] = colorVault[1]
        colorVault = colorVault[-1]
      }
    }
  }
  # remove all NAs f C 
  C = C[!is.na(C)]
  # Sorting these nodes in a descending order according to their isolate threshold score
  C_df = data.frame(nodeID = as.numeric(names(C)), color = C)
  reOrderedDF = isolateThreshold %>% 
    filter(nodeID %in% (C %>% names %>% as.numeric)) %>%
    left_join(C_df, by = "nodeID") %>%
    select(nodeID, color)
  C = as.character(reOrderedDF$color) %>%
    `names<-` (reOrderedDF$nodeID)
  
  if (flag_isolateThreshold){
    print(C)
    return(list(C, isolateThreshold))
  }else{
    print(C)
    return(C)  
  }
}

create_nodeColorAttribute = function(netLst){
  colorVault = get_tableauColor()
  # with the flag to be turned on, it returns a list
    # the first element of the list returns all reference nodes
    # the second element of the list returns a dataframe of isolate scores
  colorObject = assign_nodeColors(netLst, flag_isolateThreshold = T)
  colorRefs = colorObject[[1]]
  isolateThreshold = colorObject[[2]]
  
  new_netLst = list()
  for (i in 1:length(netLst)){
    print(paste("Assigning node color to Network", i))
    new_netLst[[i]] = assign_colorToSingleNetwork(netLst[[i]], colorVault, colorRefs, isolateThreshold)
  }
  return(new_netLst)
}

assign_colorToSingleNetwork = function(net, colorVault, colorRefs, isolateThreshold){
  # get the number of nodes
  numNodes = net %>% vcount
  # create an empty array to contain colors corresponding to each node
  actualNodeColors = rep(NA, times = numNodes)
  # create a list and put nodes in community by list element
  communityLst = igraph::V(net)$com %>% create_componentLst
  refNodes = names(colorRefs) %>% as.numeric
  
  # assign node colors based on the reference node
  for (refNode in refNodes){
    for (community in communityLst){
      if (refNode %in% community & is.na(actualNodeColors[refNode])){
        actualNodeColors[community] = colorRefs[refNode %>% as.character]
      }
    }    
  }
  
  nodeColor_inLst_byCommunity = list()
  for (i in 1:length(communityLst)){
    nodeColor_inLst_byCommunity[[i]] = actualNodeColors[communityLst[[i]]] %>% unique
  }
  
  # after assiging colors based on the color reference node, and if there is still nodes
  # that have not been assigned a color
  communityIndex_noColor = nodeColor_inLst_byCommunity %>% is.na %>% which
  checker = length(communityIndex_noColor)
  if (checker == 0){
    # end the process
    igraph::V(net)$color = actualNodeColors
    return(net)
  }
  # in the communities that still got no color, need to figure out which community has a node that has the highest isolate threshold
  if (checker > 1){
    reOrder_index = communityLst[communityIndex_noColor] %>% 
      lapply(function(community) isolateThreshold %>% 
               filter(nodeID %in% community) %>% 
               filter(tIsolate == max(tIsolate)) %>%
               select(tIsolate) %>%
               unique %>% 
               pull(tIsolate)) %>%
      unlist %>%
      order(decreasing = T)
    # reOrder the sequence
    communityIndex_noColor = communityIndex_noColor[reOrder_index]
  }
  
  
  # find the color in the vault that are still unused
  colorVault_unused = colorVault[!colorVault %in% colorRefs]
  
  
  for (i in communityIndex_noColor) {
    # find all node index in this community
    community = communityLst[[i]]
    # from these nodes in this community, which node has the highest isolate score
    actualNodeColors[community] = colorVault_unused[1]
    colorVault_unused = colorVault_unused[-1]
  }
  
  igraph::V(net)$color = actualNodeColors
  return(net)
}

plot_subgraph = function(netLst, i){
  nodesDF = get_nodesDF()
  create_coCommunityEdgelist(netLst) %>%
    filter(freq >= i) %>%
    graph_from_data_frame(vertices = nodesDF) %>%
    plot()#vertex.label.cex =5)
}


create_edgelistProb = function(css_net_lst){
  adjm_prob = get_adjmProb(css_net_lst)
  numNodes = nrow(adjm_prob)
  df = make_full_graph(numNodes, directed = T) %>% 
    get.edgelist %>% 
    as.data.frame %>% 
    setNames(c("from", "to")) %>%
    filter(from != to)
  weight = c()
  for (i in 1:nrow(adjm_prob)){
    for(j in 1:ncol(adjm_prob)){
      if (i != j){
        weight = c(weight, adjm_prob[i, j])  
      }
    }
  }
  df$weight = weight
  return(df)
}

include_edgeWeight = function(netLst, css_net_lst){
  full_edgelist_prob = create_edgelistProb(css_net_lst)
  new_netLst = list()
  for (i in 1:length(netLst)){
    net = netLst[[i]]
    edge_prob = net %>% 
      get.edgelist %>%
      apply(2, as.numeric) %>%
      as.data.frame %>%
      setNames(c("from", "to")) %>%
      left_join(full_edgelist_prob, by = c("from", "to")) %>%
      pull(weight)
    E(net)$weight = edge_prob 
    new_netLst[[i]] = net
  }
  return(new_netLst)
}