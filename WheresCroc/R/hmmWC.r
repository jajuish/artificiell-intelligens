#' myFunction
#' 
#' The function to be passed
myFunction = function(moveInfo, readings, positions, edges, probs) {
  currentNode = positions[3]
  bp1Pos = positions[1]
  bp2Pos = positions[2]

  status = moveInfo$mem$status

  if (status == 0 || status == 1) {
    # moveInfo$mem$s0 = getInitialState(bp1Pos, bp2Pos)
    moveInfo$mem$s0 = fInit(bp1Pos, bp2Pos)
  }

  ######## GET CROC'S LOCATION #######
  s0 = moveInfo$mem$s0
  e = getEmissionMatrix(probs, readings)
  st = hiddenMarkovModel(s0, currentNode, edges, e)
  crocLocation = which(st == max(st))

  # print("crocLocation")
  # print(crocLocation)

  neighbors = getOptions(positions[3], edges)
  if(crocLocation %in% neighbors){
    moveInfo$moves = c(crocLocation,0)
    return (moveInfo)
  }

  ######## GET PATH AND RETURN #######
  # path = getShortestPath(currentNode, crocLocation, edges)
  path = bfsSearch(currentNode, crocLocation, edges)
  if(length(path) >= 2) { # croc is two or more nodes away
    moveInfo$moves = c(path[1], path[2])
  } else if(length(path) == 1) { # croc is one node away
    moveInfo$moves = c(path[1], 0)
  } else if(length(path) == 0) { # croc is at same position
    moveInfo$moves=c(0,0)  
  }

  moveInfo$mem$s0 = st
  moveInfo$mem$status = 2

  return (moveInfo)
}

#' hiddenMarkovModel
#' 
#' Calculates the next state (St) given the current state,
#' the transition matrix and the emission matrix
hiddenMarkovModel = function(prevStateProbs, currentNode, edges, observations) {
  st = rep(0,40)
  for (i in 1:40) {
    p = 0
    for (j in 1:40) {
      p = p + (prevStateProbs[j] * getTransitionValue(j, currentNode, edges))
    }
    st[i] = p * observations[i]
  }
  st = st / sum(st) # normalize

  return (st)

}

#' getInitialState
#' 
#' Gets the initial/first state (S0)
getInitialState = function(bp1Pos, bp2Pos, numNodes = 40) {
  s0 = rep(1/numNodes,numNodes)

  # if both backpackers are alive, croc is not at those positions
  # not checking for NA because initially, they won't be NA
  if (bp1Pos > 0 && bp2Pos > 0) {
    s0 = rep(1/(numNodes-2), numNodes)
    s0[bp1Pos] = 0
    s0[bp2Pos] = 0
  } else if (bp1Pos < 0 || bp2Pos < 0) { # if one just died, croc is at that position
    croc_at = 0
    if (bp1Pos < 0) {
      croc_at = -1 * bp1Pos
    } else {
      croc_at = -1 * bp2Pos
    }
    s0 = rep(0, numNodes)
    s0[croc_at] = 1
  }

  # print("s0")
  # print(s0)
  return (s0)
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
#'
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