#' Add threshold to user settings
#' @title addThreshold
#' @description Function to set up a user threshold
#' @param sport Character. One of 'bike', 'run' or 'swim'
#' @param metric Character. One of 'power', pace', 'HR'
#' @param value Numeric (power and HR) or character (pace). For pace, use the format mm:ss
#' @param userSettings (userSettings) An object of class userSettings. Optional.
#' If given the new settings will be added to the existing ones, if NULL (default)
#' a new userSettings object will be created
#' @details The tool is adimentional, as in the calcualtions are not affected by the units used.
#' for instance min/km vs min/mile. However, the user should always use the same units, i.e.
#' if the threshold is in min/km, then all the sessions planned efforts must be entered in
#' min/km for the results to be correct. 
#' @examples
#' #set the bike FTP to 250W
#' mySettings <- addThreshold(sport = 'bike', metric = 'power', value = 250)
#' 
#' #add a run FTP of 300W
#' mySettings <- addThreshold(sport = 'bike', metric = 'power', value = 250,
#' userSettings = mySettings)
#' @export
addThreshold <- function(sport, metric, value, userSettings = NULL){
  #validate inputs
  errMessage <- validateUserSettings(sport, metric, value, userSettings)
  
  if (errMessage != ''){
    stop(errMessage)
  }
  
  
  # create userSettings is needed
  if(is.null(userSettings)){
    userSettings = new('userSettings')
  }
  
  #convert mm:ss to numeric
  if(is.character(value)){
    value <- strToMinDec(value)
  }
  
  # get old Settings and remove possible duplicates of settings
  oldData <- slot(userSettings, 'settings')
  if(paste0(sport, metric) %in% paste0(oldData$sport, oldData$metric)){
    warning(paste0("The combination '",
                   sport,"'/'",metric, "' already exists and will be over-written.\n"))
    oldData <- cleanSettings(oldData, sport, metric)
    
  }
  
  #the settings to add
  newData <- data.frame(sport = sport,
                        metric = metric,
                        value = value)
  
  # put things together
  newData <- rbind(oldData, newData)
  newData <- na.omit(newData)
  newData$sport <- as.character(newData$sport)
  newData$metric <- as.character(newData$metric)
  
  newData <- newData[order(newData$sport, newData$metric),]
  row.names(newData) <- NULL
  slot(userSettings, 'settings') <- newData
  
  return(userSettings)
}
