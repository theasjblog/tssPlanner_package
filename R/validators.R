validateInputStr <- function(inputStr, varName){

  if(length(inputStr) != 1){
    stop(paste0(varName, ' must be of length 1'))
  }

  if(is.null(inputStr)){
    stop(paste0(varName, ' cannot be NULL'))
  }
  if(is.na(inputStr)){
    stop(paste0(varName, ' cannot be NA'))
  }
  if(!is.character(inputStr)){
    stop(paste0(varName, ' must be a character string'))
  }

  numValues <- str_split(inputStr, ':')
  if (length(numValues[[1]]) != 2){
    stop(paste0(varName, ' must be in format mm:ss'))
  }

  num1 <- as.numeric(numValues[[1]][1])
  num2 <- as.numeric(numValues[[1]][2])
  if(is.na(num1) || is.na(num2)){
    stop(paste0(varName, ' must be in format mm:ss'))
  }
  if (num2<0 || num2>=60){
    stop('seconds must be 0<=ss<60')
  }
  if (num1<0){
    stop('minutes must be 0<=ss')
  }
}



validateGetTSS <- function(minTargetZ, targetZ, reference, type){
  if (length(type) != 1){
    stop('type must be one of bike, swim or run')
  }
  if (!type %in% c('bike', 'swim', 'run')){
    stop('type must be one of bike, swim or run')
  }

  if (length(minTargetZ) != length(targetZ) || length(minTargetZ) < 1){
    stop
  }

  if (type != 'bike'){
    for (i in 1:length(targetZ)){
      validateInputStr(targetZ[i], 'targetZ')
    }

  } else {
    for (i in 1:length(targetZ)){
      checkNumeric(targetZ[i], 'targetZ')
    }
  }

  if (is.null(minTargetZ)){
    stop('minTargetZ cannot be NULL')
  }
  for (i in 1: length(minTargetZ)){
    checkNumeric(minTargetZ[i], 'minTargetZ')
  }

}


checkNumeric <- function(isItNumeric, varName){

  if(!is.numeric(isItNumeric)){
    stop(paste0(varName, ' must be numeric'))
  }

  if(is.null(isItNumeric)){
    stop(paste0(varName, ' cannot be NULL'))
  }
  if(is.na(isItNumeric)){
    stop(paste0(varName, ' cannot be NA'))
  }
  if(isItNumeric < 0){
    stop(paste0(varName, ' must be >=0'))

  }
}





validateAddSessionPlan <- function(weeklyPlan, session, day, sessionNumber){

  if (is.null(weeklyPlan)){
    stop('weeklyPlan cannot be NULL')
  }
  if (class(session) != 'session'){
    stop('session must be of class session')
  }
  
  if (length(day) != 1){
    stop('day must be of length 1')
  }
  if (!day %in% c('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday')){
    stop('day must be one of monday, tuesday, wednesday, thursday, friday, saturday or sunday')
  }


}

validateSessionArgs <- function(weeklyPlan, day, sessionNumber){
  if(!is.null(weeklyPlan) && class(weeklyPlan) != 'weeklyPlan'){
      stop('weeklyPlan must be of class weeklyPlan')
    }

  if (!day %in% slotNames(weeklyPlan) || day == 'userSettings'){
    stop("'day' must be one of monday, tuesday, wednesday, thursday, friday, saturday or sunday.")
  }
  
  if (is.na(sessionNumber) || length(sessionNumber) != 1 || !is.numeric(sessionNumber)){
    stop("'sessionNumber' must be a numeric vector of length 1")
  }
  if(sessionNumber <= 0 || sessionNumber > length(slot(slot(weeklyPlan, day), 'sessions'))){
    stop(paste0("'sessionNumber' must be <= ",
                length(slot(slot(weeklyPlan, day), 'sessions'))))
  }
}

validateManulaTSS <- function(TSS, type, description){

  if(length(type) != 1){
    stop ('type must be of length 1')
  }
  if (!type %in% c('bike', 'swim', 'run')){
    stop('type must be one of bike, swim or run')
  }

  if(!is.numeric(TSS) ){
    stop('TSS must be numeric')
  }
  if (is.na(TSS)){
    stop('TSS cannot be NA')
  }
  if (is.infinite(TSS) || TSS < 0){
    stop('TSS must be >=0 and not infinite')
  }

  if (length(description) != 1){
    stop('description must be of length 1')
  }
  if (!is.character(description)){
    stop('description must be a character vector')
  }
}


checkStoreSettings <- function(userSettings, weeklyPlan){
  if (is.null(userSettings)){
    stop('userSettings canot be null')
  }
  if (class(userSettings) != 'userSettings'){
    stop('userSettings must be an object of class userSettings')
  }
  if (!is.null(weeklyPlan) && class(weeklyPlan) != 'weeklyPlan'){
    stop('weeklyPlan must be an object of class weeklyPlan')
  }
}

checkThDec <- function(thDec){
  if (length(thDec) != 1){
    stop('thDec must be of length 1')
  }
  if(!is.numeric(thDec)){
    stop('thDec must be numeric')
  }
  if(is.null(thDec)){
    stop('thDec cannot be NULL')
  }
  if(is.na(thDec)){
    stop('thDec cannot be NA')
  }
  if(thDec < 0){
    stop('thDec must be positive')
  }
}


# Function to validate the inputs to addThreshold
validateUserSettings <- function(sport, type, value, userSettings = NULL){
  errMessage <- list()
  if(length(sport) != 1){
    errMessage <- c(errMessage,
                    "'sport' must be of length 1")
  }
  if(length(type) != 1){
    errMessage <- c(errMessage,
                    "'type' must be of length 1")
  }
  if(length(value) != 1){
    errMessage <- c(errMessage,
                    "'value' must be of length 1")
  }
  if(!is.null(userSettings) && class(userSettings) != 'userSettings'){
    errMessage <- c(errMessage,
                    "'userSettings' must be of class 'userSettings'")
  }
  if(!sport%in%c('bike', 'run', 'swim')){
    errMessage <- c(errMessage,
                    "'sport' must be one of 'bike', 'run', 'swim'")
  }
  if(!type%in%c('power', 'pace', 'HR')){
    errMessage <- c(errMessage,
                    "'type' must be one of 'power', 'pace', 'HR'")
  }
  if(type  == 'pace' && !is.character(value)){
    errMessage <- c(errMessage,
                    "'value' must be character if 'type' is 'pace'")
  }
  if(type  %in% c('power', 'HR') && !is.numeric(value)){
    errMessage <- c(errMessage,
                    "'value' must be numeric if 'type' is 'power' or 'HR'")
  }
  if(is.numeric(value) && value <= 0){
    errMessage <- c(errMessage,
                    "'value' must be > 0")
  }
  
  if(is.character(value)){
    tmpMsg <- tryCatch(validateInputStr(value, 'value'),
                       error = function(e) e$message)
    if(!is.null(tmpMsg)){
      errMessage <- c(errMessage,
                      tmpMsg)
    }
  }
  
  
  errMessage <- paste0(errMessage, collapse = '. ')
  return(errMessage)
}

# function to validate the arguemnts to create a new training session
validateCreateSession <- function(sport, metric, minTargetZ, targetZ,
                                  userSettings, TSS, description){
  
  errMessage <- list()
  
  if(length(sport) != 1 || !sport %in% c('swim', 'bike', 'run')){
    errMessage <- c(errMessage,
                    "'sport' must be of length 1 and one of 'swim', 'bike', 'run'")
  }
  if(!is.na(TSS) && (length(metric) != 1 || !metric %in% c('power', 'HR', 'pace'))){
    errMessage <- c(errMessage,
                    "'metric' must be of length 1 and one of 'power', 'HR', 'pace'")
  }
  if(length(description) > 1){
    errMessage <- c(errMessage,
                    "'description' must be of length 0 (NULL) or 1")
  }
  if(length(TSS) != 1){
    errMessage <- c(errMessage,
                    "'TSS' must be of lenght 1")
  }
  if(!is.na(TSS) && !is.numeric(TSS)){
    errMessage <- c(errMessage,
                    "'TSS' must be numeric or NA")
  }
  if(is.numeric(TSS) && TSS < 0 ){
    errMessage <- c(errMessage,
                    "'TSS' must be >= 0")
  }
  
  if(is.na(TSS)){
    if (length(minTargetZ) != length(targetZ)){
      errMessage <- c(errMessage,
                      "'mintargetZ' and 'targetZ' must be of equal length")
    }
    if (metric == 'pace' && !is.character(targetZ)){
      errMessage <- c(errMessage,
                      "If metric is 'pace', targetZ must be character of format 'mm:ss'")
    }
    if (metric != 'pace' && !is.numeric(targetZ)){
      errMessage <- c(errMessage,
                      "If metric is 'power' or 'HR', targetZ must be numeric")
    }
    if (!is.na(targetZ) && is.numeric(targetZ)){
      if (any(targetZ < 0)){
        errMessage("'targetZ' must be always  >=0")
      }
    }
  }
  
  if(is.na(TSS)){
    if(class(userSettings) !=  'userSettings'){
      errMessage <- c(errMessage,
                      "'userSettings' must be an object of class 'userSettings'")
    }
    
    availableMetrics <- slot(userSettings, 'settings')$metric[slot(userSettings, 'settings')$sport == sport]
    if(!metric %in% availableMetrics && is.na(TSS)){
      errMessage <- c(errMessage,
                      'The metric used does not have a valid threshold associated in the userSettings and TSS is not given')
    }
  }
  
  errMessage <- paste0(errMessage, collapse = '. ')
  return(errMessage)
}