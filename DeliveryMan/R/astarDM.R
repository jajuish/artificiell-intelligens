astarDM <- function (roads, car, packages) {
  if (car$load == 0) {
    # if car load is 0 and there is no point to reach yet, set a package to pick up
    # TODO: optimize this to pick up the nearest package
    nextPackageTopick = which(packages[,5]==0)[1]
    # print(nextPackageTopick)
    car$mem = list(pointToReach = list(x = packages[nextPackageTopick, 1], y = packages[nextPackageTopick, 2]))
    destination = car$mem$pointToReach
  } else {
    # astar search for current point till destination
    destination = list(x = packages[car$load, 3], y = packages[car$load, 4])
  }
  print(paste("current dest==",destination))
  nm = astarSearch(roads, car, destination)
  print("selected next move")
  print(nm)
  car$nextMove = nm
  return(car)
}

distanceBetweenCoordinates <- function (src, dest) {
  return (sqrt(( (src$x - dest$x)^2 + (src$y - dest$y)^2 )))
}

astarSearch <- function (roads, car, destination) {
  #TODO: if current node is dest, go for it ?? or maybe not eh .. check
  frontier = list(list(
    x = car$x,
    y = car$y,
    cost = 0,
    h = 0,
    f = cost+h,
    path = c()
  ))

  reached = 0
  path = 0

  run = 1
  while (reached != 1) {
    df = as.data.frame(do.call(rbind, lapply(frontier, unlist)))
    # TODO: breaks ties arbitrarily as of now
    # CAN DO: is one of smallest cost is of destination then no need to go any further
    expandedIndex = which(df$f==min(df$f))[1]
    expanded = frontier[[expandedIndex]]
    frontier = frontier[-expandedIndex] # remove the chosen one from the frontier
    df = as.data.frame(do.call(rbind, lapply(frontier, unlist)))

    if (expanded$x == destination$x & expanded$y == destination$y) {
      reached = 1
      return (expanded$path[1])
    }

    if (expanded$y-1 > 0) {
      neighbourNode = list(x = expanded$x, y = expanded$y-1)
      cost = expanded$cost + roads$vroads[expanded$x, expanded$y-1]
      frontier = addNeighbourToFrontier(expanded, cost, neighbourNode, destination, frontier, df, 2)
    }
    if (expanded$y+1 <= 10) {
      neighbourNode = list(x = expanded$x, y = expanded$y+1)
      cost = expanded$cost + roads$vroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, cost, neighbourNode, destination, frontier, df, 8)
    }
    if (expanded$x-1 > 0) {
      neighbourNode = list(x = expanded$x-1, y = expanded$y)
      cost = expanded$cost + roads$hroads[expanded$x-1, expanded$y]
      frontier = addNeighbourToFrontier(expanded, cost, neighbourNode, destination, frontier, df, 4)
    }
    if (expanded$x+1 <= 10) {
      neighbourNode = list(x = expanded$x+1, y = expanded$y)
      cost = expanded$cost + roads$hroads[expanded$x, expanded$y]
      frontier = addNeighbourToFrontier(expanded, cost, neighbourNode, destination, frontier, df, 6)
    }
    run = run+1
  }
}

addNeighbourToFrontier <- function (expanded, cost, neighbourNode, destination, frontier, df, directionNumber) {
  h = distanceBetweenCoordinates(list(x = neighbourNode$x, y = neighbourNode$y), destination)
  neighbourIndex = which(df$x == neighbourNode$x & df$y == neighbourNode$y)
  if (length(neighbourIndex) != 0) {
    if (frontier[[neighbourIndex]]$cost > cost) {
      frontier[[neighbourIndex]]$cost = cost
      frontier[[neighbourIndex]]$f = cost+h
      frontier[[neighbourIndex]]$path = append(expanded$path, c(directionNumber))
    }
  } else {
    frontier = append(frontier, list(list(
      x = neighbourNode$x,
      y = neighbourNode$y,
      cost = cost,
      h = h,
      f = cost+h,
      path = append(expanded$path, c(directionNumber))
    )))
  }

  return (frontier)
}