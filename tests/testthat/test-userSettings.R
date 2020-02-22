context('user_settings')

test_that("get_errors",{
  expect_error(addThreshold(sport = 'hi', metric = 'HR', value = 55))
  expect_error(addThreshold(sport = 'bike', metric = 'hi', value = 55))
  expect_error(addThreshold(sport = 'bike', metric = 'HR', value = '55'))
  expect_error(addThreshold(sport = 'bike', metric = 'pace', value = 55))
  expect_error(addThreshold(sport = 'bike', metric = 'pace', value = '55'))
  expect_error(addThreshold(sport = 'bike', metric = 'HR', value = 55, userSettings = 'hi'))
})

test_that('right_output',{
  newThreshold <- addThreshold(sport = 'bike', metric = 'HR', value = 55)
  emptyTh <- new('userSettings')
  expectedDf <- na.omit(
    rbind(slot(emptyTh, 'settings'),
          data.frame(sport = 'bike',
                     metric = 'HR',
                     value = 55)))
  expectedDf$sport <- as.character(expectedDf$sport)
  expectedDf$metric <- as.character(expectedDf$metric)
  row.names(expectedDf) <- NULL
  
  expect_equal(slot(newThreshold, 'settings'), expectedDf)
  
  ##
  newThreshold <- addThreshold(sport = 'run', metric = 'pace', value = '4:30',
                               userSettings = newThreshold)
  expectedDf <- na.omit(
    rbind(slot(emptyTh, 'settings'),
          data.frame(sport = c('bike', 'run'),
                     metric = c('HR', 'pace'),
                     value = c(55.0, 4.5))))
  expectedDf$sport <- as.character(expectedDf$sport)
  expectedDf$metric <- as.character(expectedDf$metric)
  row.names(expectedDf) <- NULL
  expect_equal(slot(newThreshold, 'settings'), expectedDf)
  
  ##
  newThreshold <- addThreshold(sport = 'bike', metric = 'HR', value = 55,
                               userSettings = newThreshold)
  row.names(expectedDf) <- NULL
  expect_equal(slot(newThreshold, 'settings'), expectedDf)
})