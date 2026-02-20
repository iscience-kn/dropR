test_that("dropout is correctly detected", {
  testdata <- data.frame(var1 = c(1, 2, 3), 
                         var2 = c(1, NA, NA), 
                         var3 = c(NA, 2, 3),
                         var4 = c(1, NA, NA), 
                         var5 = c(1, 2, NA), 
                         var6 = c(NA, 2, NA)
                         ) # idx should be 5, 7, 3
  
  testidx <- add_dropout_idx(testdata, q_pos = 1:6)
  
  expect_equal(testidx$do_idx, c(5,7,3))
})
