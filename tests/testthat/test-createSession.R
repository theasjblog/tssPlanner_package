context('createSession')

test_that('errors',{
  newThreshold <- addThreshold(sport = 'bike', metric = 'HR', value = 55)
  newThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30',
                               userSettings = newThreshold)
  
  expect_error(createSession(sport = 'hi',metric = 'HR', minTargetZ = 10, targetZ = 10,
                             userSettings = newThreshold, TSS = 10, description = 'hi'))
  expect_error(createSession(sport = 'bike',metric = 'HR', minTargetZ = 10, targetZ = 10,
                             userSettings = 'hi', TSS = 10, description = 'hi'))
  expect_error(createSession(sport = 'bike',metric = 'hi', minTargetZ = 10, targetZ = 10,
                             userSettings = newThreshold, TSS = 10, description = 'hi'))
  expect_error(createSession(sport = 'bike',metric = 'HR', minTargetZ = 10, targetZ = 10,
                             userSettings = newThreshold, TSS = -10, description = 'hi'))
  expect_error(createSession(sport = 'bike',metric = 'HR', minTargetZ = NA, targetZ = NA,
                             userSettings = newThreshold, TSS = NA, description = 'hi'))
  expect_error(createSession(sport = 'bike',metric = 'HR', minTargetZ = c(10, 20), targetZ = 10,
                             userSettings = newThreshold, TSS = 10, description = 'hi'))
  expect_error(createSession(sport = 'bike',metric = 'power', minTargetZ = 10, targetZ = 10,
                             userSettings = newThreshold, TSS = NA, description = 'hi'))
  expect_error(createSession(sport = 'run',metric = 'pace', minTargetZ = 10, targetZ = 10,
                             userSettings = newThreshold, TSS = NA, description = 'hi'))
  expect_error(createSession(sport = 'bike',metric = 'HR', minTargetZ = 10, targetZ = "4:30",
                             userSettings = newThreshold, TSS = NA, description = 'hi'))
})

test_that('expected_results',{
  newThreshold <- addThreshold(sport = 'bike', metric = 'HR', value = 55)
  newThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30',
                               userSettings = newThreshold)
  expected <- new('session')
  slot(expected, 'sport') <- 'bike'
  slot(expected, 'metric') <- 'HR'
  slot(expected, 'TSS') <- 100
  slot(expected, 'description') <- 'hi'
  slot(expected, 'manualTSS') <- FALSE
  slot(expected, 'sessionDetails') <- data.frame(minutes = 60,
                                                 target = 55,
                                                 TSS = 100,
                                                 IF = 1)
  actual <- createSession(sport = 'bike',metric = 'HR', minTargetZ = 60, targetZ = 55,
                          userSettings = newThreshold, TSS = NA, description = 'hi')
  expect_equal(expected, actual)
  
  expected <- new('session')
  slot(expected, 'sport') <- 'bike'
  slot(expected, 'metric') <- 'power'
  slot(expected, 'TSS') <- 234
  slot(expected, 'description') <- 'hi'
  slot(expected, 'manualTSS') <- TRUE
  actual <- createSession(sport = 'bike', metric = 'power', minTargetZ = 60, targetZ = 55,
                          userSettings = newThreshold, TSS = 234, description = 'hi')
  expect_equal(expected, actual)
  
  expected <- new('session')
  slot(expected, 'sport') <- 'run'
  slot(expected, 'metric') <- 'pace'
  slot(expected, 'TSS') <- 100
  slot(expected, 'description') <- 'making a run test session'
  slot(expected, 'manualTSS') <- FALSE
  slot(expected, 'sessionDetails') <- data.frame(minutes = 60,
                                                 target = 4.5,
                                                 TSS = 100,
                                                 IF = 1)
  actual <- createSession(sport = 'run', metric = 'pace', minTargetZ = 60,
                          targetZ = '4:30',
                          userSettings =  newThreshold,
                          description = 'making a run test session')
  expect_equal(expected, actual)
})