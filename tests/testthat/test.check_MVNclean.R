context("check_MVNclean")
library(MVNclean)

dTest <- data.frame(
  x1 = exp((1:100)/15)
  ,x2 =  1:100
  ,x3 = log(1:100)
)


MVNclean_test <- MVNclean(dTest)

## essentially the same tests as the basic RMW()

test_that('MVNclean has expected effects on univariate distributions',{
  expect_lt(max(MVNclean_test$x1),max(dTest$x1))
  expect_gt(min(MVNclean_test$x3),min(dTest$x3))
})

test_that('MVNclean fails or gives warnings with improper variables',{
  expect_error(suppressWarnings( # too few observations
    MVNclean(dTest[1:8,])
  ))
  expect_warning(MVNclean(dTest[1:20,])) # few observations (Warning)
  expect_error(suppressWarnings( # only one numeric variable
    MVNclean(data.frame(x1 = letters[1:20]
                   ,x2 = 1:10))
  ))
  expect_error(suppressWarnings( # Not enough unique values
    MVNclean(data.frame(x1 = rep(1:4,9)
                   ,x2 = rep(1:3,12)))
  ))
})
