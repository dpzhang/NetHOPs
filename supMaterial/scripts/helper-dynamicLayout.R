# make the coordinate as a node attribute
add_anchoringCoordinate = function(netLst, weights = NULL, alpha, iter, tol, seed = 1){
  set.seed(seed)
  layoutLst = netLst %>%
    graphlayouts::layout_as_dynamic(weights = weights, alpha, iter, tol)
  
  outputNetLst = list()
  for (i in 1:length(netLst)){
    net = netLst[[i]]
    layout = layoutLst[[i]]
    igraph::V(net)$x = layout[,1]
    igraph::V(net)$y = layout[,2]
    outputNetLst %<>% list.append(net)
  }
  return(outputNetLst)
}

add_edgeColorAttribute = function(netLst, edgeColor){
  new_netLst = list()
  for (net in netLst){
    E(net)$color = edgeColor
    new_netLst %<>% list.append(net)
  }
  return(new_netLst)
}

normalize_layoutCoordinate = function(netLst, min = -0.5, max = 0.5){
  outputNetLst = list()
  for(net in netLst){
    normalizedLayout = net %>% 
      extract_netLayout %>%
      igraph::norm_coords(xmin = -0.5, xmax = 0.5, 
                          ymin = -0.5, ymax = 0.5, 
                          zmin = NULL, zmax = NULL)
    igraph::V(net)$x = normalizedLayout[,1]
    igraph::V(net)$y = normalizedLayout[,2]
    outputNetLst %<>% list.append(net)
  }
  return(outputNetLst)
}