context("check_RMW")
library(MVNclean)

dTest <- data.frame(
  x1 = exp((1:100)/15)
  ,x2 =  1:100
  ,x3 = log(1:100)
)

dTest_bad <- data.frame()

RMW_test <- RMW(dTest)

test_that('RMW has expected effects on univariate distributions',{
  expect_lt(max(RMW_test$x1),max(dTest$x1))
  expect_gt(min(RMW_test$x3),min(dTest$x3))
})

test_that('RMW fails or gives warnings with improper variables',{
  expect_error(suppressWarnings( # too few observations
    RMW(dTest[1:8,])
  ))
  expect_warning(RMW(dTest[1:20,])) # few observations (Warning)
  expect_error(suppressWarnings( # only one numeric variable
    RMW(data.frame(x1 = letters[1:20]
                   ,x2 = 1:10))
  ))
  expect_error(suppressWarnings( # Not enough unique values
    RMW(data.frame(x1 = rep(1:4,9)
                   ,x2 = rep(1:3,12)))
  ))
})
