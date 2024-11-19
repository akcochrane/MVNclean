context("check_RUW")
library(MVNclean)

dTest <- data.frame(
  x1 = exp((1:100)/15)
  ,x2 =  1:100
  ,x3 = log(1:100)
)

RUW_test <- data.frame(apply(dTest,2,RUW))

test_that('RUW retained MAD',{
  expect_equal(mad(dTest$x1),mad(RUW_test[,'x1']))
  expect_equal(mad(dTest$x2),mad(RUW_test[,'x2']))
  expect_equal(mad(dTest$x3),mad(RUW_test[,'x3']))
})

test_that('RUW retained medians',{
  expect_equal(median(dTest$x1),median(RUW_test[,'x1']))
  expect_equal(median(dTest$x2),median(RUW_test[,'x2']))
  expect_equal(median(dTest$x3),median(RUW_test[,'x3']))
})

test_that('RUW has expected effects on distributions',{
  expect_lt(max(RUW_test$x1),max(dTest$x1))
  expect_equal(max(RUW_test$x2),max(dTest$x2))
  expect_equal(min(RUW_test$x2),min(dTest$x2))
  expect_gt(min(RUW_test$x3),min(dTest$x3))
})

test_that('RUW fails or gives warnings with improper variables',{
  expect_error(suppressWarnings(RUW(letters[1:20])))
  expect_warning(RUW(4:6))
})

