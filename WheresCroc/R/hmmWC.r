debugWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$mem$status=1
  
  options=getOptions(positions[3],edges)
  
  print("MoveInfo:")
  print(moveInfo$moves)
  print(moveInfo$mem)
  
  print("Position")
  print(positions)
  
  s0 = getInitialState(positions[1], positions[2])
  e = getEmissionMatrix(probs, readings)
  st = hiddenMarkovModel(s0, positions[3], edges, e)
  
  # TODO: Search for the index with greatest common percentiles
  
  print("Move 1/2 options (plus 0=search, q=quit):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) {
    options=getOptions(mv1,edges)
  }
  print("Move 2/2 options (plus 0=search, q=quit):")
  print(options)
  mv2=readline("Move 2: ")
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }
  moveInfo$moves=c(mv1,mv2)
  
  return(moveInfo)
}

#' myFunction
#' 
#' The function to be passed
myFunction = function(moveInfo, readings, positions, edges, probs) {
  currentNode = positions[3]
  bp1Pos = positions[1]
  bp2Pos = positions[2]

  ######## GET CROC'S LOCATION #######
  # Is this the first move?
  if(moveInfo$mem$status == 0){
    s0 = getInitialState(bp1Pos, bp2Pos)
    moveInfo$mem$status = 1
  }else{
    s0 = getInitialState(bp1Pos, bp2Pos, moveInfo$mem)
  }
  s0 = getInitialState(bp1Pos, bp2Pos)
  e = getEmissionMatrix(probs, readings)
  st = hiddenMarkovModel(s0, currentNode, edges, e)
  crocLocation = which(st == max(st))

  print("crocLocation")
  print(crocLocation)

  ######## GET PATH AND RETURN #######
  # path = getShortestPath(currentNode, crocLocation, edges)
  # if(length(path) >= 2) { # croc is two or more nodes away
  #   moveInfo$moves = c(path[1], path[2])
  # } else if(length(path) == 1) { # croc is one node away
  #   moveInfo$moves = c(path[1], 0)
  # } else if(length(path) == 0) { # croc is at same position
  #   moveInfo$moves=c(0,0)  
  # }
  # return (moveInfo)
  
  # Use manual movements in the meantime
  print("Move 1/2 options (plus 0=search, q=quit):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) {
    options=getOptions(mv1,edges)
  }
  print("Move 2/2 options (plus 0=search, q=quit):")
  print(options)
  mv2=readline("Move 2: ")
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }
  moveInfo$moves=c(mv1,mv2)
  
  return(moveInfo)
}

#' hiddenMarkovModel
#' 
#' Calculates the next state (St) given the current state,
#' the transition matrix and the emission matrix
hiddenMarkovModel = function(prevStateProbs, currentNode, edges, observations) {
  st = c();
  for (i in 1:40) {
    st[i] = (prevStateProbs[i] * getTransitionValue(i, currentNode, edges))
    print(i)
    print(getTransitionValue(i, currentNode, edges))
  }
  st = st / sum(st) # normalize

  return (st)
}

#' getInitialState
#' Gets the initial/first state (S0)
#' @param bp1Pos Integer. Position of backpacker 1
#' @param bp2Pos Integer. Position of backpacker 2
#' @param bp1Dead Boolean. Did backpacker 1 die on a previous turn?
#' @param bp1Pos Boolean. Did backpacker 2 die on a previous turn?
#' @param numNodes Integer. Number of nodes in the network
getInitialState = function(bp1Pos, bp2Pos, bp1Dead=0, bp2Dead=0, numNodes = 40) {
  s0 = rep(1/numNodes,numNodes)
  remNodes = numnodes
  
  # if both backpackers are alive, croc is not at those positions
  # not checking for NA because initially, they won't be NA
  if (bp1Pos > 0 && bp2Pos > 0) {
    if(bp1Pos == bp2Pos){
      remNodes = remNodes -1
    }
    else{
      remNodes = remNodes -2
    }
    
    s0 = rep(1/remNodes, numNodes)
    s0[bp1Pos] = 0
    s0[bp2Pos] = 0
  } else {
    # Someone is dead...
    # Is backpacker 1 dead?
    if(bp1Pos < 0){
      # Did he just die (was NOT dead previously)?
      if(!(bp1Dead))
        # Croc must be at backpacker's position
        croc_at = abs(bp1Pos)
    }else{
      # Carry on then...
      remNodes = remNodes - 1
    }
    # Is backpacker 2 dead?
    if(bp1Pos < 0){
      # Did he just die (was NOT dead previously)?
      if(!(bp2Dead))
        # Croc must be at backpacker's position
        croc_at = abs(bp2Pos)
    }else{
      # Carry on then...
      remNodes = remNodes - 1
    }
    
    # Do we know where croc is?
    if(croc_at > 0){
      s0 = rep(0, numNodes)
      s0[croc_at] = 1
    }
    else{
      s0 = rep(1/remNodes, numNodes)
      # Backpacker 1 alive?
      if(bp1Pos>0)
        s0[bp1Pos] = 0
      else if(bp2Pos>0)
        s0[bp2Pos] = 0
    }
  }

  # print("s0")
  # print(s0)
  return (s0)
}

#' getTransitionMatrix
#'
#' Gets a 40-40 matrix with all the probabilities of going from node A to node B
#' Matrix is structured as:
#' matrix[A,B]
#' For example:
#'    Going from node 5 to node 7
#'    matrix[5,7] = 0.2
getTransitionMatrix = function(edges) {
  tm = matrix(0,40,40)
  neighbours = 0
  prob = 0
  
  for(i in 1:40){
    neighbours = getOptions(i, edges)
    prob = 1/length(neighbours)
    for(n in neighbours){
      tm[i,n] = prob
    }
  }
  
  return(tm)
}

#' getTransitionValue
#' 
#' Computes a single transition value from the transition matrix "T"
#' P(S1 = nextNode | S0 = currentNode)
getTransitionValue = function(currentNode, nextNode, edges) {
  
  neighbours = getOptions(currentNode, edges)

  # if the nextNode is a neighbour of the currentNode, then probability is equal among all neighbours
  if (is.element(nextNode, neighbours)) {
    return(1/length(neighbours))
  }
  # otherwise the probability is 0 (can only travel to an immediate neighbour in a turn)
  return (0)
}

#' getEmissionMatrix
#''
#' Compares the chemical levels in probs with provided readings to generate
#' percentual likelyhoods of each pool of water for croc's observations/emissions,
#' given the current state
#' Computes the emission matrix "E"
getEmissionMatrix = function(probs, readings) {
  salinity = dnorm(readings[1],probs$salinity[,1],probs$salinity[,2])
  phosphate = dnorm(readings[2],probs$phosphate[,1],probs$phosphate[,2])
  nitrogen = dnorm(readings[3],probs$nitrogen[,1],probs$nitrogen[,2])
  emissionMatrix = salinity * phosphate * nitrogen
  emissionMatrix = emissionMatrix / sum(emissionMatrix) # normalize
  return (emissionMatrix)
}

#' getShortestPath
#' 
#' Gets shortest path from source node to destination using graph search
#' algorithm (BFS)
getShortestPath = function(source, destination, edges) {
  visited = c()
  frontier = c()
  parents = replicate(40, 0)

  expandedNode = source
  parents[source] = -1
  while(expandedNode != destination) {
    # set the expanded node as visited
    visited = append(visited, expandedNode)

    # get its neighbours and add to the frontier and search tree
    neighbours = getOptions(expandedNode, edges)
    for (node in neighbours) {
      if (!is.element(node, frontier) && !is.element(node, visited)) {
        frontier = append(frontier, node)
        parents[node] = expandedNode
      }
    }

    # get the next node from the frontier
    expandedNode = frontier[1]
    frontier = setdiff(frontier, expandedNode)
  }
  
  # print("parents")
  # print(parents)
  nextNode = parents[destination]
  path = c()
  while(nextNode != source) {
    nextNode = parents[nextNode]
    path = c(c(nextNode), path)
  }

  return (path)
}