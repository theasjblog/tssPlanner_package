context('TSS_calculation')

test_that("TSS_is_Right",{
  skip('TBC')
  expect_equal(tssPlanner:::getTSS(minTargetZ = 60, targetZ = 30, reference = 30, metric = 'HR'),
               list(TSS = 100,
                    IF = 1))
  expect_equal(tssPlanner:::getTSS(minTargetZ = 30, targetZ = 30, reference = 30, metric = 'power'),
               list(TSS = 50,
                    IF = 1))
  expect_equal(tssPlanner:::getTSS(minTargetZ = 60, targetZ = 4.5, reference = 4.5, metric = 'pace'),
               list(TSS = 100,
                    IF = 1))
  expect_equal(tssPlanner:::getTSS(minTargetZ = 30, targetZ = 4.5, reference = 4.5, metric = 'pace'),
               list(TSS = 50,
                    IF = 1))
  expect_equal(tssPlanner:::getTSS(minTargetZ = 60, targetZ = 9, reference = 4.5, metric = 'pace'),
               list(TSS = 25,
                    IF = 0.5))
  expect_equal(tssPlanner:::getTSS(minTargetZ = c(30, 15), targetZ = c(1.5, 0.75),
                                   reference = 1.5, metric = 'pace'),
               list(TSS = c(50, 100),
                    IF = c(1, 2)))
})

