#' userSettings for thresholds used to calculate TSS
#' @slot settings data.frame of settings
setClass('userSettings',
         representation(settings = 'data.frame'),
         prototype(settings = data.frame(sport= NA_character_,
                                         metric = NA_character_,
                                         value = NA)))


#' The individual training session
#' @slot sport character of the sport
#' @slot metric The metric used to calcualte TSS for this session
#' @slot TSS numeric The session TSS
#' @slot manualTSS logical TRUE if the TSS used was provided by the user
#' @slot sessionDetails data.frame Details of each session interval
setClass('singleSportSession',
         representation(sport= 'character',
                        metric = 'character',
                        sessionDetails = 'data.frame',
                        TSS = 'numeric',
                        manualTSS = 'logical'
                        ),
         prototype(sport = NULL,
                   metric = NULL,
                   sessionDetails = NULL,
                   TSS = NULL,
                   manualTSS = FALSE
                   )
)

#' The individual training session
#' @slot sessions list of sessions in the brick session
#' @slot description A character description for the session
setClass('session',
         representation(sessions = 'list',
                        description = 'character'
         ),
         prototype(sessions = list(),
                   description = NULL
         )
)

#' class for the day of the week session
#' @slot sessions list the list of session to do that day
setClass('dayWeek',
         representation(
           sessions = 'list'),
         prototype(
           sessions = list()
         )
)


setClass('weeklyPlan',
         representation(userSettings = 'userSettings',
                        monday = 'dayWeek',
                        tuesday = 'dayWeek',
                        wednesday = 'dayWeek',
                        thursday = 'dayWeek',
                        friday = 'dayWeek',
                        saturday = 'dayWeek',
                        sunday = 'dayWeek'),
         prototype(userSettings = new('userSettings'),
                   monday = new('dayWeek'),
                   tuesday = new('dayWeek'),
                   wednesday = new('dayWeek'),
                   thursday = new('dayWeek'),
                   friday = new('dayWeek'),
                   saturday = new('dayWeek'),
                   sunday = new('dayWeek')))



