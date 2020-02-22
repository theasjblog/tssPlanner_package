context('createSession')


test_that('expected_results',{
  skip('TBC')
  newThreshold <- addThreshold(sport = 'bike', metric = 'HR', value = 55)
  newThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30',
                               userSettings = newThreshold)
  
  singleSportSession <- new('singleSportSession')
  slot(singleSportSession, 'sport') <- 'bike'
  slot(singleSportSession, 'metric') <- 'HR'
  slot(singleSportSession, 'TSS') <- 100
  slot(singleSportSession, 'manualTSS') <- FALSE
  slot(singleSportSession, 'sessionDetails') <- data.frame(sport = 'bike',
                                                           metric = 'HR',
                                                           minutes = 60,
                                                           target = 55,
                                                           threshold = 55,
                                                           TSS = 100,
                                                           user_TSS = FALSE)
  slot(singleSportSession, 'sessionDetails')$sport <- as.character(slot(singleSportSession, 'sessionDetails')$sport)
  slot(singleSportSession, 'sessionDetails')$metric <- as.character(slot(singleSportSession, 'sessionDetails')$metric)
  expected <- new('session')
  slot(expected, 'sessions')[[1]] <- singleSportSession
  slot(expected, 'description') <- 'hi'
  
  actual <- createSession(sport = list('bike'),metric = list('HR'), minTargetZ = list(60), targetZ = list(55),
                          userSettings = newThreshold, TSS = list(NA), description = 'hi')
  expect_equal(expected, actual)
  
  ######################################
  singleSportSession <- new('singleSportSession')
  slot(singleSportSession, 'sport') <- 'bike'
  slot(singleSportSession, 'metric') <- 'power'
  slot(singleSportSession, 'TSS') <- 234
  slot(singleSportSession, 'manualTSS') <- TRUE
  slot(singleSportSession, 'sessionDetails') <- data.frame(sport = 'bike',
                                                           metric = 'power',
                                                           minutes = 60,
                                                           target = 55,
                                                           threshold = NA,
                                                           TSS = 234,
                                                           user_TSS = TRUE)
  
  slot(singleSportSession, 'sessionDetails')$sport <- as.character(slot(singleSportSession, 'sessionDetails')$sport)
  slot(singleSportSession, 'sessionDetails')$metric <- as.character(slot(singleSportSession, 'sessionDetails')$metric)
  slot(singleSportSession, 'sessionDetails')$threshold <- as.numeric(slot(singleSportSession, 'sessionDetails')$threshold)
  expected <- new('session')
  slot(expected, 'sessions')[[1]] <- singleSportSession
  slot(expected, 'description') <- 'hi'
  
  
  actual <- createSession(sport = list('bike'), metric = list('power'), minTargetZ = list(60),
                          targetZ = list(55),
                          userSettings = newThreshold, TSS = list(234), description = 'hi')
  expect_equal(expected, actual)
  
  
  ######################################
  
  singleSportSession <- new('singleSportSession')
  slot(singleSportSession, 'sport') <- 'bike'
  slot(singleSportSession, 'metric') <- 'HR'
  slot(singleSportSession, 'TSS') <- 100
  slot(singleSportSession, 'manualTSS') <- FALSE
  slot(singleSportSession, 'sessionDetails') <- data.frame(sport = c('bike', 'run', 'swim'),
                                                           metric = c('HR', 'pace', NA),
                                                           minutes = c(30, 60, NA),
                                                           target = c(55, 4.5, NA),
                                                           threshold = c(55, 4.5, NA),
                                                           TSS = c(50, 100, 80),
                                                           user_TSS = c(FALSE, FALSE, TRUE))
  slot(singleSportSession, 'sessionDetails')$sport <- as.character(slot(singleSportSession, 'sessionDetails')$sport)
  slot(singleSportSession, 'sessionDetails')$metric <- as.character(slot(singleSportSession, 'sessionDetails')$metric)
  expected <- new('session')
  slot(expected, 'sessions')[[1]] <- singleSportSession
  slot(expected, 'description') <- 'hi'
  
  actual <- createSession(sport = list('bike', 'run', 'swim'),
                          metric = list('HR', 'pace', NA),
                          minTargetZ = list(30, 60, NA),
                          targetZ = list(55, '4:30', NA),
                          userSettings = newThreshold, TSS = list(NA, NA, 80), description = 'hi')
  expect_equal(expected, actual)
})