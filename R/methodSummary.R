#' define a summary method for session
#' @description define a summary method for session
#' @param object (session) An object of class session
#' @export
setMethod('summary', signature = 'session', function(object){
  text <- paste0('Session sport: ', slot(object, 'sport'),
                 '\nTSS: ', round(slot(object, 'TSS'),digits = 0),
                 '\nUser defined TSS: ', as.character(slot(object, 'manualTSS')),
                 '\nDescription: ', object@description)
  
  if(!slot(object, 'manualTSS')){
    details <- viewSessionDetails(object)
    
    text <- paste0(
      text,
      '\nTotal duration in minutes: ', sum(details$minutes)
    )
  } else {
    details <- NULL
  }
  text <- paste0(text, '\n\n')
  
  
  return(list(text = text,
              details = details))
  
})

#' define a summary method for dayWeek
#' @description define a summary method for dayWeek
#' @param object (dayWeek) An object of class dayWeek
#' @export
setMethod('summary', signature = 'dayWeek', function(object){
  
  
  TSS <- getDayTSS(object)
  
  if (TSS == 0){
    text <- 'No session scheduled for this day\n'
  } else {
    text <- paste0('Total TSS for the day: ', round(TSS, digits = 0), '.\n\nIndividual sessions:\n')
    sessions <- slot(object, 'sessions')
    
    for (i in seq_len(length(sessions))){
      TSSindividual <- getSessionTSS(sessions[[i]])
      text <- paste0(text, 'Session ', i, ':\n', summary(sessions[[i]])$text)
      
    }
  }
  text <- paste0(text,
                 paste(rep('*', 30), collapse = ''),
                 '\n')
  
  return(text)
  
})

#' define a summary method for weeklyPlan
#' @description define a summary method for weeklyPlan
#' @param object (weeklyPlan) An object of class weeklyPlan
#' @export
setMethod('summary', signature = 'weeklyPlan', function(object){
  TSS <- getWeekTSS(object)
  if(TSS == 0){
    text <- ('No session planned for the week')
  } else {
    text <- paste0('Your week has a total TSS of ', round(TSS, digits = 0),
                   '\n\n')
    idx <- 0
    for (i in slotNames(object)){
      day <- slot(object, i)
      if (class(day) == 'dayWeek'){
        idx <- idx + 1
        TSS <- getDayTSS(day)
        if (TSS > 0){
          dayName <- numberToDay(idx)
          text <- paste0(text, dayName, ':\n',
                         summary(day))
        }
      }
    }
  }
  
  
  return(text)
})


numberToDay <- function(number){
  day <- switch(number,
                'Monday',
                'Tuesday',
                'Wednesday',
                'Thursday',
                'Friday',
                'Saturday',
                'Sunday',
                stop('Unknown day of the week'))
  
  return(day)
}

#' define a summary method for usersettings
#' @description define a summary method for usersettings
#' @param object (userSettings) An object of class userSettings
#' @export
setMethod('summary', signature = 'userSettings', function(object){
  slot(object, 'settings')
})

