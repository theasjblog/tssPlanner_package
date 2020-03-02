#' @title exportWeekDetails
#' @description format a table with sesison details for export
#' @param weeklyPlan Object of class weeklyPlan
#' @return Write a table to file
#' @export
exportWeekDetails <- function(weeklyPlan){
  TSS <- getWeekTSS(weeklyPlan)
  
  sumSessionAll <- NULL
  
  if (TSS != 0){
    for (i in slotNames(weeklyPlan)){
      day <- slot(weeklyPlan, i)
      if (class(day) == 'dayWeek'){
        TSS <- getDayTSS(day)
        if (TSS > 0){
          for (ii in 1:length(slot(day, 'sessions'))){
            summSession <- summary(slot(day, 'sessions')[[ii]])
            summSession <- summSession[,
                                       colnames(summSession) %in% c('sport', 'metric',
                                                                   'minutes', 'target')]
            summSession$day <- i
            summSession$session_N <- ii
            sumSessionAll <- rbind(sumSessionAll,
                                   summSession)
          }
        }
      }
    }
  }
  
  return(sumSessionAll)
  
}
