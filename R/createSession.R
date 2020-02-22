#' Create a new training session
#'
#' @description Function to set up a user swim threshold
#' @param minTargetZ (numeric) Number of a minutes at a specified intensity
#' @param targetZ (numeric, character) The session intensity for the minutes specified. Numeric for type = bike, character otherwise
#' @param sport (character) One of the sports: bike, run, swim
#' @param description (character) Optional session description. If NULL, it will be set to the type
#' @param userSettings (userSettings) an object of class userSettings
#' @param TSS (numeric) the known TSS for the session. If this is given (and is not NA (default))
#' it will be used to determine the session TSS, even if intervals are given.
#' @param metric (character) One of 'power', 'pace', 'HR'
#'
#' @usage
#' ##set the swim threshold to 1:30
#' #myThreshold <- addThreshold(sport = 'swim', type = 'pace', value = '1:30')
#'
#' ##create a swim session
#' #newSession <- createSession(minTargetZ = c(20, 30), targetZ = c('1:20', '1:40'),
#' metric = 'pace', userSettings = myThreshold, type = 'swim',
#' description = 'making a test session')
#' @export
createSession <- function(sport, metric = NA, minTargetZ = NA, targetZ = NA,
                          userSettings = NA, TSS = NA, description = NULL){
  
  #errMessage <- validateCreateSession(sport, metric, minTargetZ, targetZ,
  #                                    userSettings, TSS, description)
  #if (errMessage != ''){
  #  stop(errMessage)
  #}
  
  newSession <- new('session')
  
  
  
  
  if(is.na(TSS)){
    idx <- which(slot(userSettings, 'settings')$sport == sport &
                   slot(userSettings, 'settings')$metric == metric)
    reference <- slot(userSettings, 'settings')$value[idx]
    if (metric != 'pace'){
      targetZDec <- targetZ
    } else {
      targetZDec <- rep(NA, length(targetZ))
      for (i in 1:length(minTargetZ)){
        targetZDec[i] <- strToMinDec(targetZ[i])
      }
    }
    TSS_IF <- getTSS(minTargetZ, targetZDec, reference, metric)
    sessionDetails <- data.frame(minutes = minTargetZ,
                                 target = targetZDec,
                                 TSS = TSS_IF$TSS,
                                 IF = TSS_IF$IF)
    TSS <- sum(TSS_IF$TSS)
    if(is.na(TSS) || TSS < 0){
      stop(paste0(
        "Something went wrong when calculationg TSS. ",
        "Check your targetZ/minTargetZ inputs: if give they should not be NA or negative. ",
        "Alternatively, provide a manual TSS. "
        ))
    }
    manualTSS <- FALSE
    slot(newSession, 'sessionDetails') <- sessionDetails
    slot(newSession, 'metric') <- metric
  } else {
    warning("Using user defined TSS for this session.\n")
    targetZDec <- targetZ
    manualTSS <- TRUE
  }
  
  
  if (is.null(description)){
    description <- sport
  }
  
  slot(newSession, 'TSS') <- TSS
  slot(newSession, 'description') <- description
  slot(newSession, 'sport') <- sport
  slot(newSession, 'manualTSS') <- manualTSS
  
  return(newSession)
}
