context('weeklyPlan')

test_that("weeklyPlan_is_Right",{
  skip('TBC')
  #add a run threshold to settings
  mySettings <- addThreshold(sport = 'run', metric = 'pace', value = '4:30')
   
  # create run session
  runSession <- createSession(sport = 'run', metric = 'pace', minTargetZ = c(40, 20),
                              targetZ = c('4:50', '4:30'),
                              userSettings =  mySettings,
                              description = 'making a run test session')
  # create a new plan
  newPlan <- storeSettings(mySettings)
  # add session to the plan
  newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
                              day = 'thursday')
  expect_equal(slot(newPlan, 'userSettings'), mySettings)
  expect_equal(length(slot(slot(newPlan, 'monday'), 'sessions')), 0)
  expect_equal(length(slot(slot(newPlan, 'thursday'), 'sessions')), 1)
  newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
                              day = 'thursday')
  newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
                              day = 'monday')
  expect_equal(length(slot(slot(newPlan, 'monday'), 'sessions')), 1)
  expect_equal(length(slot(slot(newPlan, 'thursday'), 'sessions')), 2)
  
})

test_that("add_second_session",{
  skip('skipping as not completed')
  mySettings <- setSwimThreshold('1:30')
  #add a FTP to settings
  mySettings <- setFTP(250,userSettings = mySettings)
  #addUserSettingsToaPlan
  newPlan <- storeSettings(mySettings)
  #create a swim session
  newSession <- createSession(30, '1:30', newPlan, 'swim', 'making a swim test session')
  #create the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'monday', sessionNumber = 2)
  #create a bike session
  newSession <- createSession(30, 250, newPlan, 'bike', 'making a bike test session')
  #create the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'friday', sessionNumber = 1)
  expect_equal(newPlan@userSettings@swimThreshold, 1.5)
  expect_equal(newPlan@userSettings@FTP, 250)
  expect_equal(newPlan@monday@session2@TSS, 50)
  expect_equal(newPlan@friday@session1@TSS, 50)
  expect_equal(getWeekTSS(newPlan), 100)
  df <- data.frame('Mon' = c(0,50, 50),
                   'Tue' = c(0,0, 0),
                   'Wed' = c(0,0, 0),
                   'Thu' = c(0,0, 0),
                   'Fri' = c(50,0, 50),
                   'Sat' = c(0,0, 0),
                   'Sun' = c(0,0, 0))
  rownames(df) <- c('1', '2', 'Day Tot')
  expect_equal(tableOfTss(newPlan), df)
})


test_that("moveASession",{
  skip('skipping as not completed')
  mySettings <- setSwimThreshold('1:30')
  #add a FTP to settings
  mySettings <- setFTP(250,userSettings = mySettings)
  #addUserSettingsToaPlan
  newPlan <- storeSettings(mySettings)
  #create a swim session
  newSession <- createSession(30, '1:30', newPlan, 'swim', 'making a swim test session')
  #create the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'monday', sessionNumber = 2)
  #create a bike session
  newSession <- createSession(30, 250, newPlan, 'bike', 'making a bike test session')
  #create the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'friday', sessionNumber = 1)
  newPlan <- moveSession(newPlan, 'monday', 2, 'tuesday', 1)
  expect_equal(newPlan@monday@session2@TSS, 0)
  expect_equal(newPlan@tuesday@session1@TSS, 50)
  expect_equal(newPlan@tuesday@session2@TSS, 0)
  expect_equal(newPlan@friday@session1@TSS, 50)
  expect_equal(getWeekTSS(newPlan), 100)
})


test_that("removeASession",{
  skip('TBC')
  skip('skipping as not completed')
  mySettings <- setSwimThreshold('1:30')
  #add a FTP to settings
  mySettings <- setFTP(250,userSettings = mySettings)
  #addUserSettingsToaPlan
  newPlan <- storeSettings(mySettings)
  #create a swim session
  newSession <- createSession(30, '1:30', newPlan, 'swim', 'making a swim test session')
  #create the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'monday', sessionNumber = 2)
  #create a bike session
  newSession <- createSession(30, 250, newPlan, 'bike', 'making a bike test session')
  #create the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'friday', sessionNumber = 1)
  newPlan <- moveSession(newPlan, 'monday', 2, 'tuesday', 1)
  newPlan <- tssPlanner::deleteSession(newPlan, 'friday', 1)
  expect_equal(newPlan@tuesday@session1@TSS, 50)
  expect_equal(newPlan@friday@session1@TSS, 00)
  expect_equal(getWeekTSS(newPlan), 50)
})
