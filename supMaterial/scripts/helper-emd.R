# In statistics, the earth mover's distance (EMD) is a measure of the distance between two probability distributions over a region D. 
# In mathematics, this is known as the Wasserstein metric. Informally, if the distributions are interpreted as two different ways of piling up a certain amount of dirt over the region D, 
# the EMD is the minimum cost of turning one pile into the other; where the cost is assumed to be amount of dirt moved times the distance by which it is moved
library(emdist)
library(dplyr)

createPointMat = function(input, label){
  # get the total number of balls
  numBalls = sum(input)
  
  # get the number of labels
  numLabels = length(label)
  
  # repeat each element in userLabel by userInput number of times
  # matched by index
  x = rep(label, times = input)
  
  # create x: for each element in userInput of non-0, generate a vector from 1
  y = input[input !=0] %>% lapply(function(x) 1:x) %>% unlist
  
  # create a dummy dataframe of 2 columns and x rows (rows is the total number of balls)
  outputDF = data.frame(x, y) 
  
  return (outputDF)
}



computeEMD = function(userInput, userLabel, groundTruth, groundTruthLabel, distanceType = "manhattan"){
  # prep for 
  userDist = createPointMat(userInput, userLabel) %>% as.matrix
  groundTruthDist = createPointMat(groundTruth, groundTruthLabel) %>% as.matrix
  #print(userDist)
  #print(groundTruthDist)
  # compute the emd
  emdResult = emdw(A = userDist, wA = 1, 
       B = groundTruthDist, wB = 1, 
       dist = distanceType)
  
  return(emdResult)
}

computeEMD_all = function(userInputDF, userLabelDF, 
                          gtInputDF, gtLabelDF, 
                          distanceType = "manhattan"){
  numUsers = nrow(userInputDF)
  userKeys = colnames(userResponse_distResponse) %>% str_extract("^[^_]*_[^_]*[^_]")
  gtKeys = userKeys %>% str_extract("^[^_]+(?=_)")
  
  # get user data by task and index
  get_userData = function(df, taskKey, i){
    output = df %>% 
      select(starts_with(taskKey)) %>% 
      slice(i) %>% 
      pull %>% 
      as.character %>% 
      strsplit(",") %>% 
      unlist %>% 
      as.numeric
    return(output)
  }
  
  outputLst = list()
  
  for(i in seq_along(userKeys)){
    # EMD score for the specific task
    EMD_score = c()
    for(j in 1:numUsers){
      userInput = userInputDF %>% get_userData(userKeys[i], j)
      userLabel = userLabelDF %>% get_userData(userKeys[i], j)
      gtInput = gtInputDF %>% get_userData(gtKeys[i], j)
      gtLabel = gtLabelDF %>% get_userData(gtKeys[i], j)
      EMD_for_taskI_userJ = computeEMD(userInput, userLabel, 
                                       gtInput, gtLabel,
                                       distanceType = distanceType)
      EMD_score = c(EMD_score, EMD_for_taskI_userJ)
    }
    outputLst[[userKeys[i]]] = EMD_score
  }
  outputDF = do.call("cbind", outputLst) %>%
    `colnames<-`(colnames(.) %>% paste0("_EMD")) %>%
    data.frame
  return(outputDF)
}


computeEMD_byTask = function(taskNum, userResponseDF, costMetrics = "manhattan"){
  # Build a task key
  taskKey = paste0("task", taskNum)
  taskKey_forDF = paste0("task", taskNum, "_")
  
  # find ground truth based on task number
  load("../data/groundTruth/truthDistBuilder.Rdata")
  gtLabel = distBuilder_gt[[taskKey]]$distLabels
  gtResponse = distBuilder_gt[[taskKey]]$distResponse
  
  # get the user response
  labelLst = userResponseDF %>% 
    select(starts_with(taskKey_forDF) & ends_with("distLabels")) %>% # select a distlabel for a task
    pull %>% # pull it out as a vector
    as.list %>% # convert it to list
    lapply(function(x) x %>% 
             str_remove_all("%") %>%# remove the percentage sign
             strsplit(",") %>%  # split the string by comma
             unlist %>%  # unlist into a vector
             as.numeric # convert it numerically
           )
  
  responseLst = userResponseDF %>% 
    select(starts_with(taskKey_forDF) & ends_with("distResponse")) %>% # select a distlabel for a task
    pull %>% # pull it out as a vector
    as.list %>% # convert it to list
    lapply(function(x) x %>% 
             str_remove_all("%") %>%# remove the percentage sign
             strsplit(",") %>%  # split the string by comma
             unlist %>%  # unlist into a vector
             as.numeric # convert it numerically
    )
  
  # compute EMD
  taskEMD = c()
  for (i in 1:nrow(userResponseDF)){
    EMDscore = computeEMD(responseLst[[i]], labelLst[[i]], gtResponse, gtLabel, distanceType = costMetrics)
    taskEMD = c(taskEMD, EMDscore)
  }
  
  return(taskEMD)
}