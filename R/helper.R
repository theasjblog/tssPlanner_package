#' @title validateTimes
#' @description return the values in decima format
#' @param metric session metric
#' @param values the valus to convert
validateTimes <- function(metric, values){
  if (is.na(metric) || metric != 'pace'){
    valuesDec <- values
  } else {
    valuesDec <- strToMinDec(values)
  }
  
  return(valuesDec)
}


#' @title getSessionDetails
#' @description get the dataframe summarizing a session
#' @param sport session sport
#' @param metric session metric
#' @param minutes minutes in target zone
#' @param target target zone
#' @param threshold threshold for the sport/metric
#' @param TSS session TSS
#' @param manualTSS if the TSS was user defined or not
getSessionDetails <- function(sport, metric, minutes, target, threshold, TSS, manualTSS){
  
  df <- data.frame(sport = sport,
                   metric = metric,
                   minutes = minutes,
                   target = round(target, digits = 2),
                   threshold = round(threshold, digits = 2),
                   TSS = round(TSS, digits = 2),
                   user_TSS = manualTSS)
  
  df$sport <- as.character(df$sport)
  df$metric <- as.character(df$metric)
  
  return(df)
}

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
#' @description Convert input to decimal. The input is
#' either already numeric or a string. If a string, the
#' format is either nn.nn or mm:ss
#' @param inputStr a vector of string or numeric
#' @export
strToMinDec <- function(inputStr) {
  
  res <- str_split(inputStr, ':')
  
  res <- lapply(res, function(d){
    d <- as.numeric(d)
    if(length(d)>1){
      d <- d[1]+d[2]/60
    }
    if(is.na(d) || d <= 0){
      stop(paste0('Invalid value. Must be either numeric or a ',
                  'string in the format "mm:ss". Must also be > 0'))
    }
    return(d)
  })
  
  return(unlist(res))
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
  
  return(TSS)
}


#' @title viewSessionDetails
#' @description view the slot sessionDetails in a session object
#' @param session An object of calss session
#' @export
viewSessionDetails <- function(session){
  if (class(session) != 'singleSportSession'){
    stop("'session' is not of class 'singleSportSession'")
  }
  
  return(slot(session, 'sessionDetails'))
}



#' Create a new weekly plan
#'
#' @description Create a new weekly training plan with no sessions
#'
#' @examples
#' newPlan <- createPlan()
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
#' @examples 
#' #add a run threshold to settings
#' mySettings <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
#' 
#' # create run session
#' runSession <- createSession(sport = list('run'), metric = list('pace'),
#' minTargetZ = list(c(40, 20)), targetZ = list(c('4:50', '4:30')), TSS = list(NA),
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


#' @title Delete a session from a plan
#' @description Delete a training session from weekly plan
#' @inheritParams addSessionToPlan
#' @param sessionNumber (numeric) the session id to delete
#' @examples
#' ##set the run threshold to 4:30
#' myThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
#' ##set the bike threshold to 250
#' myThreshold <- addThreshold(sport = 'bike', metric = 'power', value = 250, userSettings = myThreshold)
#' ##create a brick session
#' newSession <- createSession(sports=list('run', 'bike'),
#'                            minTargetZ = list(c(20, 30), 60), targetZ = list(c('4:20', '4:40'), 250),
#'                            metric = list('pace', 'power'), userSettings = myThreshold, TSS = list(NA, NA),
#'                            description = 'making a test session')
#' # create a new plan
#' newPlan <- storeSettings(myThreshold)
#' ##add to the plan monday
#' newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = newSession, day = 'monday')
#' ##add to the plan sunday
#' newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = newSession, day = 'sunday')
#' cat(summary(newPlan))
#' ##delete the session
#' newPlan <- deleteSession(weeklyPlan = newPlan, day = 'monday', sessionNumber = 1)
#' cat(summary(newPlan))
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


#' Reschedule a session within a plan
#'
#' @description Add a training session to a new or existing weekly plan
#'
#' @param weeklyPlan (weekPlan) An object with the weekly training plan. If NULL, a new one will be created
#' @param fromDay (character) The day of the week where the training session  was scheduled
#' @param fromSessionNumber (numeric) The session number for fromDay, 1 or 2
#' @param toDay (character) The day of the week where to move the training session
#' @param toSessionNumber (numeric) The session number for that toDay, 1 or 2
#' @param swap (logic) If TRUE, replace the TO session with the FROM and the FROM with an epty session, if FALSE swaps sessions.
#' @examples
#' ##set the run threshold to 4:30
#' myThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
#' ##set the bike threshold to 250
#' myThreshold <- addThreshold(sport = 'bike', metric = 'power', value = 250, userSettings = myThreshold)
#' ##create a brick session
#' newSession <- createSession(sports=list('run', 'bike'),
#'                            minTargetZ = list(c(20, 30), 60), targetZ = list(c('4:20', '4:40'), 250),
#'                            metric = list('pace', 'power'), userSettings = myThreshold, TSS = list(NA, NA),
#'                            description = 'making a test session')
#' # create a new plan
#' newPlan <- storeSettings(myThreshold)
#' ##add to the plan monday
#' newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = newSession, day = 'monday')
#' newSession <- createSession(sports=list('swim'),
#'                            minTargetZ = list(NA), targetZ = list(NA),
#'                            metric = list(NA), userSettings = myThreshold, TSS = list(80),
#'                            description = 'making a swim session')
##' newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = newSession, day = 'sunday')
#' # swap sessions
#' newPlan <- moveSession(weeklyPlan = newPlan, fromDay = 'monday', fromSessionNumber = 1,
#' toDay = 'sunday', toSessionNumber = 1, swap = TRUE)
#' # move a single session
#' newPlan <- moveSession(weeklyPlan = newPlan, fromDay = 'monday', fromSessionNumber = 1,
#' toDay = 'tuesday', swap = FALSE)
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
#' @examples
#' ##set the run threshold to 4:30
#' myThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
#' ##set the bike threshold to 250
#' myThreshold <- addThreshold(sport = 'bike', metric = 'power', value = 250, userSettings = myThreshold)
#' ##create a brick session
#' newSession <- createSession(sports=list('run', 'bike'),
#'                            minTargetZ = list(c(20, 30), 60), targetZ = list(c('4:20', '4:40'), 250),
#'                            metric = list('pace', 'power'), userSettings = myThreshold, TSS = list(NA, NA),
#'                            description = 'making a test session')
#' # create a new plan
#' newPlan <- storeSettings(myThreshold)
#' ##add to the plan monday
#' newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = newSession, day = 'monday')
#' ##get the totalTSS
#' getWeekTSS(newPlan)
#' @export
getWeekTSS <- function(weeklyPlan){
  if (class(weeklyPlan) != 'weeklyPlan'){
    stop('WeeklyPlan must be an S4 object of class weeklyPlan')
  }
  
  TSS <- 0
  for (i in slotNames(weeklyPlan)){
    if (class(slot(weeklyPlan, i)) == 'dayWeek'){
      slot(weeklyPlan, i) <- cleanDay(slot(weeklyPlan, i))
      TSS <- TSS + getDayTSS(slot(weeklyPlan, i))
    }
  }
  
  return(TSS)
}


#' @title Get day TSS
#' @description Get day TSS
#' @param day (dayWeek) A dayWeek object
#' @examples
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
#' getDayTSS(day = slot(newPlan, 'thursday'))
#' @export
getDayTSS <- function(day){
  if (class(day) != 'dayWeek'){
    stop('day must be an S4 object of class dayWeek')
  }
  
  day <- cleanDay(day)
  sessions <- slot(day, 'sessions')
  
  TSS <- sum(unlist(
    lapply(sessions, function(d){
      individualSession <- slot(d, 'sessions')
      getFullSessionTSS(individualSession)})
  ))
  
  
  return(TSS)
  
}


getFullSessionTSS <- function(sessions){
  TSS <- sum(unlist(
    lapply(sessions,function(x){
      slot(x, 'TSS')})
  ))
  
  return(TSS)
}

getAllSports <- function(sessions){
  allSports <- unique(unlist(
    lapply(sessions,function(x){
      slot(x, 'sport')})
  ))
  
  if(length(allSports) != 1){
    allSports <- 'brick'
  }
  
  return(allSports)
}

#' Convert a decimal to mm:ss string
#' @description Convert a positive integer to a string of mm:ss
#' @param thDec (numeric) a positve numeric
#' @export
convertToMinSec <- function(thDec){
  #checkThDec(thDec)
  if (!is.na(thDec)){
    min <- thDec %/% 1
    sec <- round(60*(thDec - min), digits = 0)
    asString <- paste0(min,':', sec)
  } else {
    asString <- NA_character_
  }
  
  
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
        day <- cleanDay(day)
        sessions <- slot(day, 'sessions')
        newSession <- lapply(sessions, function(d){
          dfSummary <- summary(d)
          dfSummary <- split(dfSummary, dfSummary$sport)
          
          args <- lapply(dfSummary, function(x){
            x$TSS[!x$user_TSS] <- NA
            for (ii in 1:nrow(x)){
              targets <- NULL
              if (!is.na(x$metric[ii]) && x$metric[ii] == 'pace'){
                targets <- c(targets, convertTargezToMinSec(x$target[ii]))
              } else {
                targets <- c(targets, x$target[ii])
              }
            }
            
            return(
              list(
                sport = unique(x$sport),
                metric = unique(x$metric),
                minutes = x$minutes,
                target = targets,
                TSS = x$TSS
              )
            )
          })
          
          sport <- lapply(args, function(x){
            as.character(x$sport)
          })
          
          metric <- lapply(args, function(x){
            as.character(x$metric)
          })
          
          minutes <- lapply(args, function(x){
            x$minutes
          })
          
          target <- lapply(args, function(x){
            x$target
          })
          
          TSS <- lapply(args, function(x){
            x$TSS
          })
          
          createSession(sport = sport,
                        metric = metric,
                        minTargetZ = minutes,
                        targetZ = target,
                        TSS = TSS, 
                        userSettings = userSettings,
                        description = slot(d, 'description'))
          
        })
        
        slot(day, 'sessions') <- newSession
        
        slot(weeklyPlan, i) <- day  
      }
      
    }
  } else {
    weeklyPlan <- new('weeklyPlan')
  }
  
  slot(weeklyPlan, 'userSettings') <- userSettings
  return (weeklyPlan)
  
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
      slot(weeklyPlan, i) <- cleanDay(slot(weeklyPlan, i))
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
        day <- cleanDay(day)
        sessions <- slot(day, 'sessions')
        if (length(sessions) > 0){
          WeekTSS[seq(1:length(sessions)), i] <- unlist(lapply(sessions, function(d){
            round(sum(unlist(
              lapply(slot(d, 'sessions'), function(x){
                slot(x, 'TSS')
              })
            )), digits = 2)
            
            
          }))
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
#' @param y other paramenters
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
      day <- cleanDay(day)
      session <- slot(day, 'sessions')
      sessionSport <- unlist(lapply(session, function(d){getAllSports(slot(d, 'sessions'))}))
      sessionTSS <- unlist(lapply(session, function(d){getFullSessionTSS(slot(d, 'sessions'))}))
      Sport <- c(Sport, sessionSport)
      TSS <- c(TSS, sessionTSS)
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

#' @title cleanDay
#' @description Clean a day from empty sessions
#' @param dayWeek An object of class dayWeek
#' @return An object of calss dayWeek without the emoty session that could be generated as a
#' leftover of the create session 
cleanDay <- function(dayWeek){
  
  for (i in seq_len(length(slot(dayWeek, 'sessions')))){
    idx <- NULL
    for (ii in seq_len(length(slot(slot(dayWeek, 'sessions')[[i]], 'sessions')))){
      if (is.null(slot(slot(slot(dayWeek, 'sessions')[[i]], 'sessions')[[ii]], 'sport'))){
        idx <- c(idx, ii)
      }
    }
    if (!is.null(idx)){
      slot(slot(dayWeek, 'sessions')[[i]], 'sessions')[idx] <- NULL
    }
    if(length(slot(slot(dayWeek, 'sessions')[[i]], 'sessions')) == 0){
      slot(dayWeek, 'sessions')[[i]] <- NULL
    }
  }
  
  if(length(slot(dayWeek, 'sessions')) == 0){
    slot(dayWeek, 'sessions') <- list()
  }
  
  return(dayWeek)
}

#' @title getThreshold
#' @description get the threshold valie from the user settings for the specified sport/metric
#' @param sport (character) The sport, i.e. 'bike'
#' @param metric (character) The metric, i.e. 'power'
#' @param userSettings (userSettings) The useSettings objects
getThreshold <- function(sport, metric, userSettings){
  res <- slot(userSettings, 'settings')
  res <- res[which(res$sport == sport & res$metric == metric),]
  if (nrow(res) == 0){
    return(NULL)
  } else {
    return(res$value)
  }
}

#' @title getSessionZones
#' @description Get the time in zones for a single session
#' @param sessionDetails (data.frame) The sesisonDetails data frame of a single session
#' @param userSettings (userSettings) The userSettings object
#' @return A data.frame of times in zones. NULL if the time in zones cannot be calculated
getSessionZones <- function(sessionDetails, userSettings){
  
  if(nrow(sessionDetails) == 0 || is.null(userSettings)){
    return(data.frame())
  }
  zone <- NULL
  time <- NULL
  
  for (i in 1:nrow(sessionDetails)){
    if(!sessionDetails$user_TSS[i]){
      zones <- getZones(sessionDetails$sport[i],
                        sessionDetails$metric[i])
      reference <- getThreshold(sport = sessionDetails$sport[i],
                                             metric = sessionDetails$metric[i],
                                             userSettings = mySettings)
      if (is.null(reference)|| is.null(zones)){
        return(data.frame())
      }
      if(sessionDetails$metric[i] == 'pace'){
        thPerc <- 100*reference/sessionDetails$target[i]
      } else {
        thPerc <- 100*sessionDetails$target[i]/reference
      }
      
      zone <- c(zone, as.character(zones$zName[which(thPerc >= zones$minVal & thPerc < zones$maxVal)]))
      time <- c(time, sessionDetails$minutes[i])
    }
  }
  
  res <- data.frame(zone  = zone,
                    time = time)
  if(nrow(res) > 0){
    res <- formatZones(res)
  }
    
  
  return(res)
}

#' @title formatZones
#' @description Function to summarize the zones by zone name
#' @param zones A dataframe with 2 columns: zone (character) and time (numeric)
#' @return a data.frame with xzones suummarized by name
formatZones <- function(zones){
  res <- split(zones, zones$zone)
  
  resDf <- data.frame()
  
  for (i in 1:length(res)){
    resDf <- rbind(resDf,
                   data.frame(time = sum(res[[i]]$time),
                              zone = names(res)[i]))
  }
  
  return(resDf)
}


#' @title sessisonZones
#' @description function to get the total times in zones for a training session
#' @param object S4 object of class 'session'
#' @return A dataframe of times in zones if it can be calcualted, NULL otherwise 
sessionZones <- function(object){
  res <- data.frame()
  for (i in 1:length(slot(object, 'sessions'))){
    zones <- slot(slot(object, 'sessions')[[i]], 'zones')
    if(nrow(zones) > 0){
      res <- rbind(res, zones)
    }
  }
  if(nrow(res) > 0){
    colnames(res) <- c('time', 'zone')
    res <- formatZones(res)
    return(res)
  } else {
    return(NULL)
  }
}


#' @title dayZones
#' @description function to get the total times in zones for a day
#' @param object S4 object of class 'dayWeek'
#' @return A dataframe of times in zones if it can be calcualted, NULL otherwise 
dayZones <- function(object){
  if(length(slot(object, 'sessions')) == 0){
    return(NULL)
  }
  
  res <- data.frame()
  for (i in 1:length(slot(object, 'sessions'))){
    zones <- sessionZones(slot(object, 'sessions')[[i]])
    if (!is.null(zones)){
      res <- rbind(res, zones)
    }
  }
  
  if(nrow(res) > 0){
    colnames(res) <- c('time', 'zone')
    res <- formatZones(res)
    return(res)
  } else {
    return(NULL)
  }
}

#' @title weekZones
#' @description function to get the total times in zones for a week
#' @param object S4 object of class 'weeklyPlan'
#' @return A dataframe of times in zones if it can be calcualted, NULL otherwise 
weekZones <- function(object){
  res <- data.frame()
  for (i in slotNames(object)){
    day <- slot(object, i)
    if (class(day) == 'dayWeek'){
      zones <- dayZones(day)
      if (!is.null(zones)){
        res <- rbind(res, zones)
      }
    }
  }
  if(nrow(res) > 0){
    colnames(res) <- c('time', 'zone')
    res <- formatZones(res)
    return(res)
  } else {
    return(NULL)
  }
}


#' @title getZonesPercentages
#' @description function to get the total times in zones for a specified object
#' 'session', 'dayWeek' or 'weeklyPlan'
#' @param object S4 object of class 'session', 'dayWeek' or 'weeklyPlan'
#' @return A dataframe of times and percentage of times in zones if it
#' can be calcualted, NULL otherwise 
#' @export
getZonesPercentages <- function(object){
  res <- switch(class(object),
         'session' = sessionZones(object),
         'dayWeek' = dayZones(object),
         weeklyPlan = weekZones(object),
         stop('Invalid object'))
  if(!is.null(res)){
    res$perc <- 100*res$time/sum(res$time)
  }
  
  return(res)
  
}

#' @title plotTimeInZone
#' @description Make a barchart of the percentage of time spent in training zones
#' @param object S4 object of class 'session', 'dayWeek' or 'weeklyPlan'
#' @return A ggplot 
#' @export
plotTimeInZone <- function(object){
  
  zones <- getZonesPercentages(object)
  
  if(is.null(zones)){
    return(NULL)
  }
  
  resDf <- data.frame(zone = as.factor(c('0', '1', '2' , 'X', '3', 'Y', '4', '5')),
                      perc = 0)
  for (i in 1:nrow(zones)){
    idx <- which(as.character(resDf$zone) == zones$zone[i])
    resDf$perc[idx] <- zones$perc[i]
  }
  p <- ggplot(resDf, aes(x=zone, y=perc, fill=zone))+
    geom_bar(width = 1, stat = "identity") +
    ylab('%') +
    ggtitle('Time in zone')
  p
  
}
