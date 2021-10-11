myStepFunction = function(moveInfo, readings, positions, edges, probs) {
  moveInfo = myFunction(moveInfo, readings, positions, edges, probs)
  
  print("Chosen moves are: ")
  print(moveInfo$moves)
  mv1=readline("Enter to continue, 0 to stay put, q to quit: ")
  if (mv1=="q") {stop()}
  else if (mv1=="0") {moveInfo$moves = c(0,0)}
  
  return(moveInfo)
}

#' myFunction
#' 
#' The function to be passed
myFunction = function(moveInfo, readings, positions, edges, probs) {
  currentNode = positions[3]
  
  bp1Pos = positions[1]
  if(is.na(bp1Pos))
    bp1Pos = 0
  
  bp2Pos = positions[2]
  if(is.na(bp2Pos))
    bp2Pos = 0
  
  st = rep(0, 40)
  e = getEmissionMatrix(probs, readings)
  
  ######## GET CROC'S LOCATION #######
  # If this is the first move
  if (moveInfo$mem$status < 2) {
    #print("Start of round")
    
    s0 = getInitialState(bp1Pos, bp2Pos, currentNode)
    moveInfo$mem$s0 = s0
    moveInfo$mem$status = 2
    
    # Handle first prediction uniquely
    for (i in 1:40) {
      #maybe add weight?
      st[i] = s0[i] * e[i]
    }
    st = st / sum(st) # normalize
    #print(st)
    
    crocLocation = which(st == max(st))
  }
  # One or both backpackers died
  else if(bp1Pos < 0 || bp2Pos < 0){
    if (bp1Pos < 0) {
      #print("Backpacker 1 died")
      crocLocation = -1 * bp1Pos
      st[crocLocation] = 1
    }
    if (bp2Pos < 0) {
      #print("Backpacker 2 died")
      crocLocation = -1 * bp2Pos
      st[crocLocation] = 1
    }
  } else {
    s0 = moveInfo$mem$s0
    tMat = getTransitionMatrix(edges)
    st = hiddenMarkovModel(s0, edges, e, tMat,bp1Pos, bp2Pos)
    crocLocation = which(st == max(st))
  }
  
  #print("crocLocation")
  #print(crocLocation)
  
  ######## GET PATH AND RETURN #######
  path = getShortestPath(currentNode, crocLocation, edges)
  if(length(path) >= 2) { # croc is two or more nodes away
    moveInfo$moves = c(path[1], path[2])
  } else if(length(path) == 1) { # croc is one node away
    moveInfo$moves = c(path[1], 0)
    st[crocLocation] = 0 # In case search misses, mark pool as not having croc
  } else if(length(path) == 0) { # croc is at same position
    moveInfo$moves=c(0,0)  
    st[crocLocation] = 0 # In case search misses, mark pool as not having croc
  }
  
  moveInfo$mem$s0 = st
  
  return (moveInfo)
}

#' hiddenMarkovModel
#' 
#' Calculates the next state (St) given the current state,
#' the transition matrix and the emission matrix
hiddenMarkovModel = function(prevStateProbs, edges, observations, tMat, bp1Pos, bp2Pos) {
  #print("Backpacker 1 & 2")
  #print(bp1Pos)
  #print(bp2Pos)
  
  st = matrix(0,40,40)
  for(i in 1:40){
    # Skip any nodes with backpackers on them
    if(i != bp1Pos && i != bp2Pos){
      options = getOptions(i, edges)
      for(o in options){
        st[i,o] = prevStateProbs[i] * tMat[i, o]
      }
    }
  }
  
  tProb = rep(0,40)
  for (i in 1:40) {
    # Skip any nodes with backpackers on them
    if(i != bp1Pos && i != bp2Pos){
      #maybe add weight?
      tProb[i] = sum(st[,i]) * observations[i]
    }
  }
  
  tProb = tProb / sum(tProb) # normalize
  
  return (tProb)
}

#' getInitialState
#' Gets the initial/first state (S0)
#' @param bp1Pos Integer. Position of backpacker 1
#' @param bp2Pos Integer. Position of backpacker 2
#' @param rangerPos Integer. Position of ranger
#' @param numNodes Integer. Number of nodes in the network
getInitialState = function(bp1Pos, bp2Pos, rangerPos, numNodes = 40) {
  # We assume backpackers and the ranger won't start on the same spot
  remNodes = numNodes-3
  s0 = rep(1/remNodes, numNodes)
  
  # Croc doesn't start on any of the backpackers or ranger's positions
  s0[rangerPos] = 0
  s0[bp1Pos] = 0
  s0[bp2Pos] = 0
  
  return (s0)
}

#' getTransitionMatrix
#'
#' Gets a 40 by 40 matrix with all the probabilities of going from node A to node B
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
  # print("expandedNode")
  # print(expandedNode)
  # print("destination")
  # print(destination)
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
  
  prevNode = destination
  nextNode = parents[destination]
  path = c()
  while(nextNode != -1) {
    path = c(c(prevNode), path)
    prevNode = nextNode
    nextNode = parents[nextNode]
  }
  
  return (path)
}