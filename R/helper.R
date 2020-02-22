#' @title cleanSettings
#' @description remove rows of already used sport/metric combination from userSettings data.frame
#' @param oldData data.frame of data to clean. From the slot settings in userSettings
#' @param sport character the sport to clean
#' @param metric character the metric to clean
cleanSettings <- function(oldData, sport, metric){
  idx <- which(paste0(oldData$sport, oldData$metric) == paste0(sport, metric))
  oldData <- oldData[-idx,]
  return(oldData)
}

#' @title strToMinDec
#' @description Convert a string to decimal
#' @param inputStr (str) a string in the format mm:ss
#' @export
strToMinDec <- function(inputStr) {
  validateInputStr(inputStr, 'inputStr')

  min <- as.numeric(str_split(inputStr, ':')[[1]][1])
  sec <- str_split(inputStr, ':')[[1]][2]
  if (nchar(sec) != 2){
    stop('sec must be in the format ss, i.e. 02, rather than 2')
  }
  sec <- as.numeric(sec)

  minDec <- min+(sec/60)

  return(minDec)
}

#' @title getTSS
#' @description Function to calcualte TSS based on effort parameters and user threshold
#' @param minTargetZ numeric number of minuts at a defined effort
#' @param targetZ numeric defined effort
#' @param reference effort threshold
#' @param metric the kind of threshold, i.e. power, HR or pace
getTSS <- function(minTargetZ, targetZ, reference, metric){

  IF <- switch(metric,
               'power' = targetZ/reference,
               'HR' = targetZ/reference,
               'pace' = reference/targetZ,
               stop('Unkown type'))



  hhtargetZ <- minTargetZ / 60
  TSS <- (IF^2) * hhtargetZ * 100

  return(list(TSS = TSS,
              IF = IF))
}


#' @title viewSessionDetails
#' @description view the slot sessionDetails in a session object
#' @param session An object of calss session
#' @export
viewSessionDetails <- function(session){
  if (class(session) != 'session'){
    stop("'session' is not of class 'session'")
  }
  if (slot(session, 'manualTSS')){
    warning('Cannot return details for a session wher the user defined the TSS.\n')
    return()
  }
  return(slot(session, 'sessionDetails'))
}



#' Create a new weekly plan
#'
#' @description Create a new weekly training plan with no sessions
#'
#' @usage
#' #newPlan <- createPlan()
#'
#' @export
createPlan <- function(){
  newPlan <- new('weeklyPlan')
  return(newPlan)
}


#' Add a session to a plan
#'
#' @description Add a training session to a new or existing weekly plan
#'
#' @param weeklyPlan (weekPlan) An object with the weekly training plan. If NULL, a new one will be created
#' @param session (session) The new training session
#' @param day (character) The day of the week where to add the training session
#'
#' @usage
#' #add a run threshold to settings
#' mySettings <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
#' 
#' # create run session
#' runSession <- createSession(sport = 'run', metric = 'pace', minTargetZ = c(40, 20),
#' targetZ = c('4:50', '4:30'),
#' userSettings =  mySettings,
#' description = 'making a run test session')
#' 
#' # create a new plan
#' newPlan <- storeSettings(mySettings)
#' 
#' # add session to the plan
#' newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
#' day = 'thursday')
#'
#' @export
addSessionToPlan <- function(weeklyPlan, session, day){

  validateAddSessionPlan(weeklyPlan, session, day)

  daySlot <- slot(weeklyPlan, day)
  daySessions <- slot(daySlot, 'sessions')
  
  daySessions <- c(daySessions, session)
  names(daySessions) <- seq(1:length(daySessions))
  slot(daySlot, 'sessions') <- daySessions
  slot(weeklyPlan, day) <- daySlot

  return (weeklyPlan)

}


#' Delete a session from a plan
#'
#' @description Delete a training session from weekly plan
#'
#' @inheritParams addSessionToPlan
#'
#' @usage
#' ##set the swim threshold to 1:30
#' #mySwimTh <- setSwimThreshold('1:30')
#'
#' ##create a swim session
#' #newSession <- createSession(c(20, 30), c('1:20', '1:40'), mySwimTh, 'making a test session')
#'
#' ##create the plan
#' #newPlan <- addSessionToPlan(weeklyPlan = NULL, session = newSession, day = 'monday', sessionNumber = 2)
#'
#' ##delete the session
#' #newPlan <- deleteSession(newPlan, 'monday', 2)
#'
#' @export
deleteSession <- function(weeklyPlan, day, sessionNumber){

  validateSessionArgs(weeklyPlan, day, sessionNumber)

  daySlot <- slot(weeklyPlan, day)
  daySessions <- slot(daySlot, 'sessions')
  daySessions[[sessionNumber]] <- NULL
  if(length(daySessions)>0){
    names(sessions) <- seq(1:length(daySessions))
  }
  slot(daySlot, 'sessions') <- daySessions
  slot(weeklyPlan, day) <- daySlot

  return(weeklyPlan)

}

getSessionName <- function(sessionNumber){
  if(sessionNumber == 1){
    sessionName <- 'session1'
  }
  if(sessionNumber == 2){
    sessionName <- 'session2'
  }

  return (sessionName)
}


#' Reschedule a session within a plan
#'
#' @description Add a training session to a new or existing weekly plan
#'
#' @param weekPlan (weekPlan) An object with the weekly training plan. If NULL, a new one will be created
#' @param fromDay (character) The day of the week where the training session  was scheduled
#' @param fromSessionNumber (numeric) The session number for fromDay, 1 or 2
#' @param toDay (character) The day of the week where to move the training session
#' @param toSessionNumber (numeric) The session number for that toDay, 1 or 2
#' @param swap (logic) If TRUE, replace the TO session with the FROM and the FROM with an epty session, if FALSE swaps sessions.
#'
#' @usage
#' ##set the swim threshold to 1:30
#' #mySwimTh <- setSwimThreshold('1:30')
#'
#' ##create a swim session
#' #newSession <- createSession(c(20, 30), c('1:20', '1:40'), mySwimTh, 'making a test session')
#'
#' ##create the plan
#' #newPlan <- addSessionToPlan(weeklyPlan = NULL, session = newSession, day = 'monday', sessionNumber = 2)
#'
#' ##move the session
#' #newPlan <- moveSession(weeklyPlan, 'monday', 2, 'tuesday', 1)
#' @export
moveSession <- function(weeklyPlan, fromDay, fromSessionNumber,
                        toDay, toSessionNumber = NA,
                        swap = TRUE){
  
  
  if(!is.logical(swap)){
    stop('swap must be logical')
  }
  
  validateSessionArgs(weeklyPlan, fromDay, fromSessionNumber)
  
  originalFromSessions <- slot(slot(weeklyPlan, fromDay), 'sessions')
  originalToSessions <- slot(slot(weeklyPlan, toDay), 'sessions')
  fromSession <- originalFromSessions[[fromSessionNumber]]

  
  if (is.numeric(toSessionNumber) && !swap){
    if(toSessionNumber > length(originalToSessions)){
      stop(paste0('toSessionNumber must be <= ',
                  length(originalToSessions),
                  ' if swap == FALSE'))
    }
  }
  if (is.na(toSessionNumber) && !swap){
    toSessionNumber <- length(originalToSessions)+1
  }
  
  if (swap){
    toSession <- originalToSessions[[toSessionNumber]]
    originalFromSessions[[fromSessionNumber]] <- toSession
    originalToSessions[[toSessionNumber]] <- fromSession
  } else {
    originalToSessions[[toSessionNumber]] <- fromSession
    originalFromSessions[[fromSessionNumber]] <- NULL
  }
  
  if(length(originalToSessions)>0){
    names(originalToSessions) <- seq(length(originalToSessions))
  }
  if(length(originalFromSessions)>0){
    names(originalFromSessions) <- seq(length(originalFromSessions))
  }
  slot(slot(weeklyPlan, fromDay), 'sessions') <- originalFromSessions
  slot(slot(weeklyPlan, toDay), 'sessions') <- originalToSessions

  return(weeklyPlan)

}



#' Get total weekly TSS
#'
#' @description Get total weekly TSS
#' @inheritParams addSessionToPlan
#'
#' @usage
#' ##set the swim threshold to 1:30
#' #mySwimTh <- setSwimThreshold('1:30')
#'
#' ##create a swim session
#' #newSession <- createSession(c(20, 30), c('1:20', '1:40'), mySwimTh, 'making a test session')
#'
#' ##create the plan
#' #newPlan <- addSessionToPlan(weeklyPlan = NULL, session = newSession, day = 'monday', sessionNumber = 2)
#'
#' ##get the totalTSS
#' #getWeekTSS(weeklyPlan)
#' @export
getWeekTSS <- function(weeklyPlan){
  if (class(weeklyPlan) != 'weeklyPlan'){
    stop('WeeklyPlan must be an S4 object of class weeklyPlan')
  }

  TSS <- 0
  for (i in slotNames(weeklyPlan)){
    if (class(slot(weeklyPlan, i)) == 'dayWeek'){
      TSS <- TSS + getDayTSS(slot(weeklyPlan, i))
    }
  }

  return(TSS)
}

#' Get single session TSS
#'
#' @description Get single session TSS
#' @param session (session) A session object
#'
#' @usage
#' ##set the bike HR threshold to 170
#' myTh <- addThreshold(sport = 'bike', metric = 'HR', value = 170)
#'
#' ##create a bike session
#' newSession <- createSession(sport = 'bike', metric = 'HR', minTargetZc(20, 30),
#' targetZ = c(120, 150), userSettings = myTh, description = 'making a test session')
#'
#' ##get the session TSS
#' getSessionTSS(weeklyPlan)
#' @export
getSessionTSS <- function(session){
  if (class(session) != 'session'){
    stop('session must be an S4 object of class session')
  }

  TSS <- session@TSS

  return(TSS)
}

#' Get day TSS
#'
#' @description Get day TSS
#' @param day (dayWeek) A dayWeek object
#'
#' @usage
#' #add a run threshold to settings
#' mySettings <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
#' 
#' # create run session
#' runSession <- createSession(sport = 'run', metric = 'pace', minTargetZ = c(40, 20),
#' targetZ = c('4:50', '4:30'),
#' userSettings =  mySettings,
#' description = 'making a run test session')
#' 
#' # create a new plan
#' newPlan <- storeSettings(mySettings)
#' 
#' # add session to the plan
#' newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
#' day = 'thursday')
#' 
#' getDayTSS(slot(newPlan, 'thursday'))
#' 
#' @export
getDayTSS <- function(day){
  if (class(day) != 'dayWeek'){
    stop('day must be an S4 object of class dayWeek')
  }

  sessions <- slot(day, 'sessions')
  
  TSS <- sum(unlist(lapply(sessions, function(d){slot(d, 'TSS')})))

  return(TSS)

}





#' Convert a decimal to mm:ss string
#' @description Convert a positive integer to a string of mm:ss
#' @param thDec (numeric) a positve numeric
#' @export
convertToMinSec <- function(thDec){
  checkThDec(thDec)

  min <- thDec %/% 1
  sec <- round(60*(thDec - min), digits = 0)
  asString <- paste0(min,':', sec)

  return(asString)
}



#' Store userSettings into a weeklyPlanner object
#' @description Store userSettings into a weeklyPlanner object.
#' If the weeklyplan already has some sessions defined, the TSS will be recalculated
#' @param userSettings (userSettings) An object of class userSettings
#' @param weeklyPlan (weeklyPlan) An object of class weeklyPlan
#' @export
storeSettings <- function(userSettings, weeklyPlan = NULL){
  checkStoreSettings(userSettings, weeklyPlan)
  
  if(!is.null(weeklyPlan)){
    for (i in slotNames(weeklyPlan)){
      day <- slot(weeklyPlan, i)
      if (class(day) == 'dayWeek'){
          sessions <- slot(day, 'sessions')
          sessions <- lapply(sessions, function(d){
            if (!slot(d, 'manualTSS')){
              targetZ <- slot(d, 'sessionDetails')$target
              if(slot(d, 'metric')=='pace'){
                newTarget <- NULL
                for (iii in 1:length(targetZ)){
                  newTarget <- c(newTarget, convertToMinSec(targetZ[iii]))
                }
                targetZ <- newTarget
              }
              createSession(sport = slot(d, 'sport'),
                            metric = slot(d, 'metric'),
                            minTargetZ = slot(d, 'sessionDetails')$minutes,
                            targetZ = targetZ,
                            userSettings = userSettings,
                            description = slot(d, 'description'))
            } else {
              d
            }
            })
          slot(day, 'sessions') <- sessions
        
        slot(weeklyPlan, i) <- day  
      }
      
    }
  } else {
    weeklyPlan <- new('weeklyPlan')
  }
  
  slot(weeklyPlan, 'userSettings') <- userSettings
  return (weeklyPlan)

}

getValues <- function(userSettings, session){

  targetZ <- session@targetZ
  if (session@type == 'bike'){
    reference <- userSettings@FTP
  } else if (session@type == 'swim'){
    reference <- userSettings@swimThreshold
    targetZ <- convertTargezToMinSec(targetZ)
  } else if (session@type == 'run'){
    reference <- userSettings@runThreshold
    targetZ <- convertTargezToMinSec(targetZ)
  }

  return(list(reference = reference,
              targetZ = targetZ))
}

convertTargezToMinSec <- function(targetZ){
  newTargetZ <- NULL
  for (i in 1: length(targetZ)){
    newTargetZ <- c(newTargetZ, convertToMinSec(targetZ[i]))
  }

  return(newTargetZ)
}


#function to get the highest number of session in a single day in a week
getMaxNumberSession <- function(weeklyPlan){
  nSessions <- 0
  for (i in slotNames(weeklyPlan)){
    if (class(slot(weeklyPlan, i)) == 'dayWeek'){
      nSessions <- c(nSessions,
                     length(slot(slot(weeklyPlan, i), 'sessions')))
    }
  }
  
  return(max(nSessions))
}


#' Create a table of TSS for the week
#' @description Create a table of TSS for the week
#' @param weeklyPlan (weeklyPlan) An object of class weeklyPlan
#' @export
tableOfTss <- function(weeklyPlan){
  if (class(weeklyPlan) != 'weeklyPlan'){
    stop('weeklyPlan must be an object of class weeklyPlan')
  }

  tableRows <- getMaxNumberSession(weeklyPlan)
  
  if (tableRows == 0){
    tableRows <- 1
  } else {
    noDay <- rep(0,tableRows)
    WeekTSS <- data.frame(monday = noDay,
                          tuesday = noDay,
                          wednesday = noDay,
                          thursday = noDay,
                          friday = noDay,
                          saturday = noDay,
                          sunday = noDay)
    
    
    for (i in slotNames(weeklyPlan)){
      day <- slot(weeklyPlan, i)
      if (class(day) == 'dayWeek'){
        sessions <- slot(day, 'sessions')
        if (length(sessions) > 0){
          WeekTSS[seq(1:length(sessions)), i] <- unlist(lapply(sessions, function(d){round(d@TSS,digits = 0)}))
        }
      }
    }
    
    
    colnames(WeekTSS) <- c('Mon', 'Tue', 'Wed',
                           'Thu', 'Fri', 'Sat',
                           'Sun')
    
    WeekTSS <- rbind(WeekTSS, colSums(WeekTSS))
    
    rownames(WeekTSS) <- c(seq(1:tableRows), 'Day Tot')
  }
  

  return(WeekTSS)
}


#' plot TSS distribution per day of the week
#' @description plot TSS distribution per day of the week
#' @param x (weeklyPlan) An object of class dayWeek
#' @export
setMethod('plot', signature = c(x = 'weeklyPlan', y = 'missing'), function(x, y){


  df <- getDataForPlot(x)

  g <- ggplot(df, aes(Day, TSS)) +
    geom_col(aes(fill = Sport)) +
    scale_x_discrete(limits = c('monday', 'tuesday', 'wednesday',
                                'thursday', 'friday', 'saturday', 'sunday')) +
    ggtitle(paste0('Total weekly TSS: ', round(getWeekTSS(x), digits = 0)))

  return(g)

})


getDataForPlot <- function(weeklyPlan){

  tablesRows <- getMaxNumberSession(weeklyPlan)

  Day <- NULL
  Sport <- NULL
  TSS <- NULL
  for (i in slotNames(weeklyPlan)){
    day <- slot(weeklyPlan, i)
    if (class(day) == 'dayWeek'){
        session <- slot(day, 'sessions')
        Sport <- c(Sport, sessionSport <- unlist(lapply(session, function(d){d@sport})))
        TSS <- c(TSS, unlist(lapply(session, function(d){d@TSS})))
        Day <- c(Day, rep(i, length(session)))
    }
  }

  df <- data.frame(Day = Day,
                   Sport = Sport,
                   TSS = TSS)

  return(df)
}

handle0Th <- function(th){
  if (th == '0:0' || th == 0 ){
    th <- 'not defined'
  }
  
  return (th)
}