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

validateSessionArgs <- function(TSS,
                                sport,
                                metric,
                                targetZ,
                                targetTime){
  
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
    stop('userSettings cannot be null')
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
  
  
  errMessage <- paste0(errMessage, collapse = '. ')
  return(errMessage)
}
