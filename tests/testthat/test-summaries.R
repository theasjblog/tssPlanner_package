context('summaries')

test_that("get_summaries",{
  skip('TBC')
  # userSettings
  newThreshold <- addThreshold(sport = 'bike', metric = 'HR', value = 55)
  newThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30',
                               userSettings = newThreshold)
  expect_output(summary(newThreshold))
  
  # session
  runSession <- createSession(sport = 'run', metric = 'pace', minTargetZ = c(40, 20),
                              targetZ = c('4:50', '4:30'),
                              userSettings =  newThreshold,
                              description = 'making a run test session')
  expect_equal("Session sport: run\nTSS: 91\nUser defined TSS: FALSE\nDescription: making a run test session\nTotal duration in minutes: 60\n\n",
               summary(runSession)$text)
  expect_equal(slot(runSession, 'sessionDetails'), summary(runSession)$details)
  

  skip('skipping as not completed')
  mySettings <- setSwimThreshold('1:30')
  #add a FTP to settings
  mySettings <- setFTP(250,userSettings = mySettings)
  #run threshld
  mySettings <- setRunThreshold('4:30',userSettings = mySettings)
  #addUserSettingsToaPlan
  newPlan <- storeSettings(mySettings)
  #create a swim session
  newSession <- createSession(30, '1:30', newPlan, 'swim', 'making a swim test session')
  #create the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'monday', sessionNumber = 2)
  #create a bike session
  newSession <- createSession(30, 250, newPlan, 'bike', 'making a bike test session')
  #add to the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'friday', sessionNumber = 1)
  #create a run session
  newSession <- createSession(30, '4:30', newPlan, 'run', 'making a run test session')
  #add to the plan
  newPlan <- addSessionToPlan(newPlan, session = newSession, day = 'wednesday', sessionNumber = 1)
  
  shouldMatchPlanner <- "Your week has a total TSS of 150\n\nMonday:\nTotal TSS for the day: 50.\n\nIndividual sessions:\nSession 2:\nSession type: swim\nTSS: 50\nDescription: making a swim test session\n\n******************************\nWednesday:\nTotal TSS for the day: 50.\n\nIndividual sessions:\nSession 1:\nSession type: run\nTSS: 50\nDescription: making a run test session\n\n******************************\nFriday:\nTotal TSS for the day: 50.\n\nIndividual sessions:\nSession 1:\nSession type: bike\nTSS: 50\nDescription: making a bike test session\n\n******************************\n"
  shouldMatchSettings <- "Your settings are:\nBike FTP: 250\nRun pace: 4:30\nSwim pace: 1:30"
  expect_equal(shouldMatchPlanner, summary(newPlan))
  expect_equal(shouldMatchSettings, summary(mySettings))
})

test_that('summary_day', {
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
  newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
                              day = 'thursday')
  newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
                              day = 'monday')
  actualMonday <- summary(slot(newPlan, 'monday'))
  actualThursday <- summary(slot(newPlan, 'thursday'))
  expect_equal(actualMonday,
               "Total TSS for the day: 91.\n\nIndividual sessions:\nSession 1:\nSession sport: run\nTSS: 91\nUser defined TSS: FALSE\nDescription: making a run test session\nTotal duration in minutes: 60\n\n******************************\n")
  expect_equal(actualThursday,
               "Total TSS for the day: 182.\n\nIndividual sessions:\nSession 1:\nSession sport: run\nTSS: 91\nUser defined TSS: FALSE\nDescription: making a run test session\nTotal duration in minutes: 60\n\nSession 2:\nSession sport: run\nTSS: 91\nUser defined TSS: FALSE\nDescription: making a run test session\nTotal duration in minutes: 60\n\n******************************\n")
})


test_that('summary_week', {
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
  newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
                              day = 'thursday')
  newPlan <- addSessionToPlan(weeklyPlan = newPlan, session = runSession,
                              day = 'monday')
  actual <- summary(newPlan)
  expect_equal(actual,
               "Your week has a total TSS of 273\n\nMonday:\nTotal TSS for the day: 91.\n\nIndividual sessions:\nSession 1:\nSession sport: run\nTSS: 91\nUser defined TSS: FALSE\nDescription: making a run test session\nTotal duration in minutes: 60\n\n******************************\nThursday:\nTotal TSS for the day: 182.\n\nIndividual sessions:\nSession 1:\nSession sport: run\nTSS: 91\nUser defined TSS: FALSE\nDescription: making a run test session\nTotal duration in minutes: 60\n\nSession 2:\nSession sport: run\nTSS: 91\nUser defined TSS: FALSE\nDescription: making a run test session\nTotal duration in minutes: 60\n\n******************************\n")
})
