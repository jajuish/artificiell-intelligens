#' astarDM
#'
#' The function to be passed in place of "carReady"
#' @param roads
#' @param car
#' @param packages
#' @return car with the updated $nextMove
astarDM <- function (roads, car, packages) {
  if (car$load == 0) {
    # if car load is 0 and there is no point to reach yet, set a package to pick up
    nextPackageTopick = selectPackage(c(car$x, car$y), packages)
    destination = c(packages[nextPackageTopick, 1], packages[nextPackageTopick, 2])
  } else {
    # astar search for current point till destination
    destination = c(packages[car$load, 3], packages[car$load, 4])
  }
  if (car$x == destination[1] & car$y == destination[2]) {
    car$nextMove = 5
    return (car)
  }
  nm = astarSearch(roads, car, destination)
  car$nextMove = nm
  return(car)
}

selectPackage <- function (currentPos, packages) {
  availablePackages = which(packages[,5] == 0)
  if (length(availablePackages) == 1) {
    return (availablePackages[1])
  }
  leastDistance = NULL
  nextPackageIndex = NULL
  for (packageIndex in availablePackages) {
    packagePos = c(packages[packageIndex, 1], packages[packageIndex, 2])
    dist = distanceBetweenCoordinates(currentPos, packagePos)
    if (dist < leastDistance || is.null(leastDistance)) {
      nextPackageIndex = packageIndex
      leastDistance = dist
    }
  }
  return (nextPackageIndex)
}

distanceBetweenCoordinates <- function (src, dest) {
  return (abs(src[1] - dest[1]) + abs(src[2] - dest[2]))
}

astarSearch <- function (roads, car, destination) {
  h = distanceBetweenCoordinates(c(car$x, car$y), destination)
  frontier = list(list(
    x = car$x,
    y = car$y,
    g = 0,
    h = h,
    f = h,
    path = c()
  ))
  visitedSet = list()


  while (1) {
    # find the node in the frontier with the lowest score
    scores=sapply(frontier,function(item)item$f)
    # TODO: breaks ties arbitrarily as of now
    # CAN DO: if one out of the smallest cost ones is the destination then no need to go any further
    expandedIndex = which.min(scores)
    expanded = frontier[[expandedIndex]]
    frontier = frontier[-expandedIndex] # remove the chosen one from the frontier
    visitedSet = append(visitedSet, list(expanded))

    if (expanded$x == destination[1] & expanded$y == destination[2]) {
      return (expanded$path[1])
    }

    if (expanded$y-1 > 0 & !setContains(c(expanded$x, expanded$y-1), visitedSet)) {
      neighbourNode = c(expanded$x, expanded$y-1)
      g = expanded$g + roads$vroads[expanded$x, expanded$y-1]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 2)
    }
    if (expanded$y+1 <= 10 & !setContains(c(expanded$x, expanded$y+1), visitedSet)) {
      neighbourNode = c(expanded$x, expanded$y+1)
      g = expanded$g + roads$vroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 8)
    }
    if (expanded$x-1 > 0 & !setContains(c(expanded$x-1, expanded$y), visitedSet)) {
      neighbourNode = c(expanded$x-1, expanded$y)
      g = expanded$g + roads$hroads[expanded$x-1, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 4)
    }
    if (expanded$x+1 <= 10 & !setContains(c(expanded$x+1, expanded$y), visitedSet)) {
      neighbourNode = c(expanded$x+1, expanded$y)
      g = expanded$g + roads$hroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 6)
    }
  }
}

addNeighbourToFrontier <- function (expanded, g, neighbourNode, destination, frontier, directionNumber) {
  h = distanceBetweenCoordinates(neighbourNode, destination)
  frontierX = sapply(frontier, function(item)item$x)
  frontierY = sapply(frontier, function(item)item$y)
  neighbourIndex = which(frontierX == neighbourNode[1] & frontierY == neighbourNode[2])
  if (length(neighbourIndex) != 0) {
    if (frontier[[neighbourIndex]]$g >= g) {
      frontier[[neighbourIndex]]$g = g
      frontier[[neighbourIndex]]$f = g+h
      frontier[[neighbourIndex]]$path = append(expanded$path, c(directionNumber))
    }
  } else {
    frontier = append(frontier, list(list(
      x = neighbourNode[1],
      y = neighbourNode[2],
      g = g,
      h = h,
      f = g+h,
      path = append(expanded$path, c(directionNumber))
    )))
  }

  return (frontier)
}

setContains = function(node, set) {
  if (length(set) == 0) {
    return (0)
  }
  for (i in 1:length(set)) {
    if (all(node == c(set[[i]]$x, set[[i]]$y))) {
      return (i)
    }
  }
  return (0)
}