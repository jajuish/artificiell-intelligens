astarDM <- function (roads, car, packages) {
  print("car$load")
  print(car$load)
  if (car$load == 0) {
    # if car load is 0 and there is no point to reach yet, set a package to pick up
    # if (is.null(car$mem$pointToReach)) {
    # TODO: optimize this to pick up the nearest package
    nextPackageTopick = which(packages[,5]==0)[1]
    # print(nextPackageTopick)
    car$mem = list(pointToReach = list(x = packages[nextPackageTopick, 1], y = packages[nextPackageTopick, 2]))
    # }
    destination = car$mem$pointToReach
  } else {
    # astar search for current point till destination
    destination = list(x = packages[car$load, 3], y = packages[car$load, 4])
    # car$mem = list()
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
  # expanded = list(node = list(x = car$x, y = car$y), cost = xxcar$mem$costTillNowFromSrcPkg, h = xxdistBetweenCoordinates, path = xxlist())
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
    # print(paste("run", run))
    df = as.data.frame(do.call(rbind, lapply(frontier, unlist)))
    # print(paste(df))
    # TODO: breaks ties arbitrarily as of now
    # CAN DO: is one of smallest cost is of destination then no need to go any further
    expandedIndex = which(df$f==min(df$f))[1]
    expanded = frontier[[expandedIndex]]
    # print(paste("expandedIndex", expandedIndex))
    # print(paste(expanded))
    frontier = frontier[-expandedIndex] # remove the chosen one from the frontier
    # print("frontier after getting exp")
    # print(paste(frontier))
    df = as.data.frame(do.call(rbind, lapply(frontier, unlist)))

    if (expanded$x == destination$x & expanded$y == destination$y) {
      reached = 1
      return (expanded$path[1])
    }

    if (expanded$y-1 > 0) {
      cost = expanded$cost + roads$vroads[expanded$x, expanded$y-1]
      h = distanceBetweenCoordinates(list(x = expanded$x, y = expanded$y-1), destination)
      
      neighbourIndex = which(df$x == expanded$x & df$y == expanded$y-1)
      # print("neighbourIndex1")
      # print(neighbourIndex)
      # if this neighbour already exists in the frontier
      if (length(neighbourIndex) != 0) {
        # if the current path cost is lesser than the one in the frontier
        if (frontier[[neighbourIndex]]$cost > cost) { # TODO: only replaces if costs have difference, but not when equal ...
          frontier[[neighbourIndex]]$cost = cost
          frontier[[neighbourIndex]]$f = cost+h
          frontier[[neighbourIndex]]$path = append(expanded$path, c(2))
        }
      } else {
        frontier = append(frontier, list(list(
          x = expanded$x,
          y = expanded$y-1,
          cost = cost,
          h = h,
          f = cost+h,
          path = append(expanded$path, c(2))
        )))
      }
    }
    if (expanded$y+1 <= 10) {
      cost = expanded$cost + roads$vroads[expanded$x, expanded$y]
      h = distanceBetweenCoordinates(list(x = expanded$x, y = expanded$y+1), destination)

      neighbourIndex = which(df$x == expanded$x & df$y == expanded$y+1)
      # print("neighbourIndex2")
      # print(neighbourIndex)
      if(length(neighbourIndex) != 0) {
        if (frontier[[neighbourIndex]]$cost > cost) {
          frontier[[neighbourIndex]]$cost = cost
          frontier[[neighbourIndex]]$f = cost+h
          frontier[[neighbourIndex]]$path = append(expanded$path, c(8))
        }
      } else {
        frontier = append(frontier, list(list(
          x = expanded$x,
          y = expanded$y+1,
          cost = cost,
          h = h,
          f = cost+h,
          path = append(expanded$path, c(8))
        )))
      }
    }
    if (expanded$x-1 > 0) {
      cost = expanded$cost + roads$hroads[expanded$x-1, expanded$y]
      h = distanceBetweenCoordinates(list(x = expanded$x-1, y = expanded$y), destination)

      neighbourIndex = which(df$x == expanded$x-1 & df$y == expanded$y)
      # print("neighbourIndex3")
      # print(neighbourIndex)
      if (length(neighbourIndex) != 0) {
        if (frontier[[neighbourIndex]]$cost > cost) {
          frontier[[neighbourIndex]]$cost = cost
          frontier[[neighbourIndex]]$f = cost+h
          frontier[[neighbourIndex]]$path = append(expanded$path, c(4))
        }
      } else {
        frontier = append(frontier, list(list(
          x = expanded$x-1,
          y = expanded$y,
          cost = cost,
          h = h,
          f = cost+h,
          path = append(expanded$path, c(4))
        )))
      }
    }
    if (expanded$x+1 <= 10) {
      cost = expanded$cost + roads$hroads[expanded$x, expanded$y]
      h = distanceBetweenCoordinates(list(x = expanded$x+1, y = expanded$y), destination)
      neighbourIndex = which(df$x == expanded$x+1 & df$y == expanded$y)
      # print("neighbourIndex4")
      # print(neighbourIndex)
      if (length(neighbourIndex) != 0) {
        if (frontier[[neighbourIndex]]$cost > cost) {
          frontier[[neighbourIndex]]$cost = cost
          frontier[[neighbourIndex]]$f = cost+h
          frontier[[neighbourIndex]]$path = append(expanded$path, c(6))
        }
      } else {
        frontier = append(frontier, list(list(
          x = expanded$x+1,
          y = expanded$y,
          cost = cost,
          h = h,
          f = cost+h,
          path = append(expanded$path, c(6))
        )))
      }
    }
    run = run+1
  }
}