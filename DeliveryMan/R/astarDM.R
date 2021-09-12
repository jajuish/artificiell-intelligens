astarDM <- function (roads, car, packages) {
  if (car$load == 0) {
    # if car load is 0 and there is no point to reach yet, set a package to pick up
    if (is.null(car$mem$pointToReach)) {
      nextPackageTopick = selectPackage(c(car$x, car$y), packages)
      # print(nextPackageTopick)
      car$mem = list(pointToReach = c(packages[nextPackageTopick, 1], packages[nextPackageTopick, 2]))
    }
    destination = car$mem$pointToReach
  } else {
    # astar search for current point till destination
    destination = c(packages[car$load, 3], packages[car$load, 4])
    car$mem = NULL
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
  #TODO: if current node is dest, go for it ?? or maybe not eh .. check
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
    df = as.data.frame(do.call(rbind, lapply(frontier, unlist)))
    # TODO: breaks ties arbitrarily as of now
    # CAN DO: if one out of the smallest cost ones is the destination then no need to go any further
    expandedIndex = which(df$f==min(df$f))[1]
    expanded = frontier[[expandedIndex]]
    frontier = frontier[-expandedIndex] # remove the chosen one from the frontier
    df = as.data.frame(do.call(rbind, lapply(frontier, unlist)))

    if (expanded$x == destination[1] & expanded$y == destination[2]) {
      reached = 1
      return (expanded$path[1])
    }

    if (expanded$y-1 > 0) {
      neighbourNode = list(x = expanded$x, y = expanded$y-1)
      g = expanded$g + roads$vroads[expanded$x, expanded$y-1]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, df, 2)
    }
    if (expanded$y+1 <= 10) {
      neighbourNode = list(x = expanded$x, y = expanded$y+1)
      g = expanded$g + roads$vroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, df, 8)
    }
    if (expanded$x-1 > 0) {
      neighbourNode = list(x = expanded$x-1, y = expanded$y)
      g = expanded$g + roads$hroads[expanded$x-1, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, df, 4)
    }
    if (expanded$x+1 <= 10) {
      neighbourNode = list(x = expanded$x+1, y = expanded$y)
      g = expanded$g + roads$hroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, g, neighbourNode, destination, frontier, df, 6)
    }
    run = run+1
  }
}

addNeighbourToFrontier <- function (expanded, g, neighbourNode, destination, frontier, df, directionNumber) {
  h = distanceBetweenCoordinates(c(neighbourNode$x,neighbourNode$y), destination)
  neighbourIndex = which(df$x == neighbourNode$x & df$y == neighbourNode$y)
  if (length(neighbourIndex) != 0) {
    if (frontier[[neighbourIndex]]$g >= g) {
      frontier[[neighbourIndex]]$g = g
      frontier[[neighbourIndex]]$f = g+h
      frontier[[neighbourIndex]]$path = append(expanded$path, c(directionNumber))
    }
  } else {
    frontier = append(frontier, list(list(
      x = neighbourNode$x,
      y = neighbourNode$y,
      g = g,
      h = h,
      f = g+h,
      path = append(expanded$path, c(directionNumber))
    )))
  }

  return (frontier)
}