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
  packageDistances = c()
  for (packageIndex in availablePackages) {
    packagePos = c(packages[packageIndex, 1], packages[packageIndex, 2])
    dist = distanceBetweenCoordinates(currentPos, packagePos)
    packageDistances = append(packageDistances, c(dist))
  }
  availablePackageIndex = which(packageDistances == min(packageDistances))[1]
  return (availablePackages[availablePackageIndex])
}

distanceBetweenCoordinates <- function (src, dest) {
  return (sqrt(( (src[1] - dest[1])^2 + (src[2] - dest[2])^2 )))
}

astarSearch <- function (roads, car, destination) {
  frontier = list(list(
    x = car$x,
    y = car$y,
    g = 0,
    h = 0,
    f = 0,
    path = c()
  ))

  reached = 0
  path = 0

  run = 1
  while (reached != 1) {
    # find the node in the frontier with the lowest score
    scores=sapply(frontier,function(item)item$f)
    # TODO: breaks ties arbitrarily as of now
    # CAN DO: if one out of the smallest cost ones is the destination then no need to go any further
    expandedIndex = which.min(scores)
    expanded = frontier[[expandedIndex]]
    frontier = frontier[-expandedIndex] # remove the chosen one from the frontier

    if (expanded$x == destination[1] & expanded$y == destination[2]) {
      reached = 1
      return (expanded$path[1])
    }

    if (expanded$y-1 > 0) {
      neighbourNode = c(expanded$x, expanded$y-1)
      g = expanded$g + roads$vroads[expanded$x, expanded$y-1]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 2)
    }
    if (expanded$y+1 <= 10) {
      neighbourNode = c(expanded$x, expanded$y+1)
      g = expanded$g + roads$vroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 8)
    }
    if (expanded$x-1 > 0) {
      neighbourNode = c(expanded$x-1, expanded$y)
      g = expanded$g + roads$hroads[expanded$x-1, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 4)
    }
    if (expanded$x+1 <= 10) {
      neighbourNode = c(expanded$x+1, expanded$y)
      g = expanded$g + roads$hroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, 6)
    }
    run = run+1
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