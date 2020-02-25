#' Create a new training session
#' @title createSession
#' @description Function to set up a user swim threshold
#' @param minTargetZs (list) Number of a minutes at a specified intensity for each sport/session
#' @param targetZs (list) The session intensity for the minutes specified. Numeric for type = bike, character otherwise
#' @param sports (list) One of the sports: bike, run, swim
#' @param description (character) Optional session description. If NULL, it will be set to the type
#' @param userSettings (userSettings) an object of class userSettings
#' @param TSS (list) the known TSS for the session. If this is given (and is not NA (default))
#' it will be used to set the session TSS, even if intervals are given.
#' @param metrics (list) One of 'power', 'pace', 'HR'
#' @examples
#' ##set the run threshold to 4:30
#' myThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
#' ##set the bike threshold to 250
#' myThreshold <- addThreshold(sport = 'bike', metric = 'power', value = 250, userSettings = myThreshold)
#'
#' ##create a brick session
#' newSession <- createSession(sports=list('run', 'bike'),
#' minTargetZ = list(c(20, 30), 60), targetZ = list(c('1:20', '1:40'), 250),
#' metric = list('pace', 'power'), userSettings = myThreshold, TSS = list(NA, NA),
#' description = 'making a test session')
#' @export
createSession <- function(sports, metrics = list(NA),
                          minTargetZs = list(NA),
                          targetZs = list(NA),
                          TSS = list(NA),
                          userSettings = NULL,
                          description = ''){
  
  if(!is.null(userSettings) && class(userSettings) != 'userSettings'){
    stop('User settings must be of class "userSettings')
  }
  if(length(description) !=1 ){
    stop('description must be of length 1')
  }
  if (!is.list(sports) || length(sports) < 1){
    stop('Sports must be a list of length > 0')
  }
  
  sessions <- lapply(seq_along(sports), function(d){
    
    # get the needed input
    thisTSS <- TSS[[d]]
    thisSport <- sports[[d]]
    thisMetric <- metrics[[d]]
    thisTargetZ <- targetZs[[d]]
    thisMinTargetZ <- minTargetZs[[d]]
    
    validateSessionArgs(TSS = thisTSS,
                        sport = thisSport,
                        metric = thisMetric,
                        targetZ = thisTartetZ,
                        targetTime = thisMinTargetZ)
    
    
    if(class(userSettings) == 'userSettings'){
      reference <- slot(userSettings, 'settings') %>% 
        filter(metric == thisMetric & sport == thisSport) %>% 
        select(value)
    } else {
      reference <- data.frame()
    }
    
    # TSS not given: calculate it
    if(any(is.na(thisTSS))){
      #need to find the threshold for the combination of sport/metric
      if (nrow(reference) != 1){
        warning(paste0('Threshold not found for the combination ',
                    thisSport, '/', thisMetric))
        return(new('singleSportSession'))
      }
      
      thisMinTargetZ <- strToMinDec(thisMinTargetZ)
      thisTargetZ <- strToMinDec(thisTargetZ)
      thisTSS <- getTSS(thisMinTargetZ, thisTargetZ,
                        reference$value, thisMetric)
      manualTSS <- FALSE
    
    } else {
      # use user TSS
      manualTSS <- TRUE
      if (length(thisTargetZ) == 0){
        nIntervals <- 1
      } else {
        nIntervals <- length(thisTargetZ)
      }
      thisTSS <- rep(thisTSS/nIntervals, nIntervals)
      
      if (nrow(reference) == 0){
        reference <- data.frame(value = rep(NA, nIntervals))
      }
      
      if(is.na(thisMetric)){
        thisMetric <- rep(NA_character_, nIntervals)
      }
      
    }
    
    # get the table session details
    sessionDetails <- getSessionDetails(sport = thisSport,
                                        metric = thisMetric,
                                        minutes = thisMinTargetZ,
                                        target = thisTargetZ,
                                        threshold = reference$value,
                                        TSS = thisTSS,
                                        manualTSS = manualTSS)
    
    # create and populate session
    newSession <- new('singleSportSession')
    slot(newSession, 'metric') <- thisMetric
    slot(newSession, 'TSS') <- sum(sessionDetails$TSS)
    slot(newSession, 'sport') <- thisSport
    slot(newSession, 'manualTSS') <- manualTSS
    slot(newSession, 'sessionDetails') <-sessionDetails
    
    return(newSession)
  })
  
  
  newSession <- new('session')
  slot(newSession, 'sessions') <- sessions
  slot(newSession, 'description') <- description
  
  return(newSession)
}


