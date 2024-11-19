context("check_YeoJohn")
library(MVNclean)

dTest <- data.frame(
  x1 = exp((1:100)/15)
  ,x2 =  1:100
  ,x3 = log(1:100)
)

YeoJohn_test <- data.frame(apply(dTest,2,YeoJohn))

test_that('YeoJohn retained MAD',{
  expect_equal(mad(dTest$x1),mad(YeoJohn_test[,'x1']))
  expect_equal(mad(dTest$x2),mad(YeoJohn_test[,'x2']))
  expect_equal(mad(dTest$x3),mad(YeoJohn_test[,'x3']))
})

test_that('YeoJohn retained medians',{
  expect_equal(median(dTest$x1),median(YeoJohn_test[,'x1']))
  expect_equal(median(dTest$x2),median(YeoJohn_test[,'x2']))
  expect_equal(median(dTest$x3),median(YeoJohn_test[,'x3']))
})

test_that('YeoJohn has expected effects on distributions',{
  expect_lt(max(YeoJohn_test$x1),max(dTest$x1))
  expect_gt(min(YeoJohn_test$x3),min(dTest$x3))
  
  expect_lt(abs(skew(YeoJohn_test[,'x1']) ),.025) # only small remaining skew (non-zero due to trimming when optimizing lambda)
  expect_lt(abs(skew(YeoJohn_test[,'x2']) ),.025)
  expect_lt(abs(skew(YeoJohn_test[,'x3']) ),.025) # compare to abs(skew)>1.5 originally
  
})

test_that('YeoJohn fails or gives warnings with improper variables',{
  expect_error(suppressWarnings(YeoJohn(letters[1:20])))
})

