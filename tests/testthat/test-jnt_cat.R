
test_that("expected JN interval is found", {
  z <- jnt_cat(X='svl', Y='hl', m='species', data=microlophus, plot.full = T,
               xlab='log(SVL)', ylab='log(head length)')
  expect_equal(as.numeric(z$`lower limit in X`), 4.132542, tolerance = 0.0001)
  expect_equal(as.numeric(z$`upper limit in X`), 4.325789, tolerance = 0.0001)
})
