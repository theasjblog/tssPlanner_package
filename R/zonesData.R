#' @title getAllZones
#' @description Contains the definition of training zones according to Matt Fitzgerald https://www.8020endurance.com/8020-zone-calculator/
#' @return A list of the zones
#' @export
getAllZones <- function(){
  zones <- list(list(sport = c('run', 'bike'),
                     metric = 'HR',
                     zones = data.frame(minVal = c(0, 72, 81, 90, 95, 100, 102, 106),
                                        maxVal = c(72, 81, 90, 95, 100, 102, 106, Inf),
                                        zName = c('0', '1', '2', 'X', '3', 'Y', '4', '5'))
  ),
  list(sport = 'swim',
       metric = 'pace',
       zones = data.frame(minVal = c(0, 75, 84, 91, 96, 100, 102, 107),
                          maxVal = c(75, 84, 91, 96, 100, 102, 107, Inf),
                          zName = c('0', '1', '2', 'X', '3', 'Y', '4', '5'))
  ),
  list(sport = c('run', 'bike'),
       metric = 'power',
       zones = data.frame(minVal = c(0, 50, 70, 83, 91, 100, 102, 110),
                          maxVal = c(50, 70, 83, 91, 100, 102, 110, Inf),
                          zName = c('0', '1', '2', 'X', '3', 'Y', '4', '5'))
  ),
  list(sport = 'run',
       metric = 'pace',
       zones = data.frame(minVal = c(0, 60, 76, 87, 93, 100, 102, 111),
                          maxVal = c(60, 76, 87, 93, 100, 102, 111, Inf),
                          zName = c('0', '1', '2', 'X', '3', 'Y', '4', '5'))
  )
  )
  
  return(zones)
}

#' @title getZones
#' @description Get the specific datavrame of zones for a desired combination
#' of sport an metric. If the combination does not exist, the function returns NULL
#' @param sport (character) The sport, i.e. bike
#' @metric (character) The metric, i.e. power
#' @return A dataframe if the combination sport/metric exists, NULL otherwise
#' @export
getZones <- function(sport, metric){
  zones <- getAllZones()
  res <- lapply(zones, function(d){
    if(sport %in% d$sport && metric %in% d$metric){
      return(d$zones)
    }
  })
  
  idx <- NULL
  for(i in 1:length(res)){
    if(is.null(res[[i]])){
      idx <- c(idx, i)
    }
  }
  if (!is.null(idx)){
    res[idx] <- NULL
  }
  
  if (length(res)==1){
    return(res[[1]])
  } else {
    return(NULL)
  }
}
