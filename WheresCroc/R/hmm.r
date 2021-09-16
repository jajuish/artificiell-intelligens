#' getLike
#'
#' Compares the chemical levels in probs with provided readings to generate
#' percentual likelyhoods of each pool of water.
getLike=function(probs, readings){
  odds = list(
    salinity=NULL,
    phosphate=NULL,
    nitrogen=NULL
  )
  
  for(i in 1:40){
    odds$salinity = append(odds$salinity, 
      dnorm(readings[1],probs$salinity[i,1],probs$salinity[i,2]))
    odds$phosphate = append(odds$phosphate, 
      dnorm(readings[2],probs$phosphate[i,1],probs$phosphate[i,2]))
    odds$nitrogen = append(odds$nitrogen, 
      dnorm(readings[3],probs$nitrogen[i,1],probs$nitrogen[i,2]))
  }
  
  return(odds)
}
