#' myFunction
#' 
#' The function to be passed
myFunction = function(moveInfo, readings, positions, edges, probs) {
  s0 = getInitialState(positions[1], positions[2])
  e = getEmissionMatrix(probs, readings)
  st = hiddenMarkovModel(s0, positions[3], edges, e)
  crocLocation = which(st == max(st))
}

#' hiddenMarkovModel
#' 
#' Calculates the next state (St) given the current state,
#' the transition matrix and the emission matrix
hiddenMarkovModel = function(prevStateProbs, currentNode, edges, observations) {
  st = 0
  for (i in 1:40) {
    st = st + (prevStateProbs[i] * getTransitionValue(i, currentNode, edges))
  }
  st = st * observations
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