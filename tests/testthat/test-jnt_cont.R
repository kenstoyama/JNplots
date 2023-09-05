library(testthat)
library(JNplots)

# Test whether the expected intervals are found (significant interaction)
test_that("expected JN interval is found", {
  z <- jnt_cont(X='PHR95_overlap_z', Y='hrsize95', m='degree_z', data=lizard_home_range,
                 xlab = 'home range overlap 95', ylab='home range size 95')
  expect_equal(as.numeric(z$`lower (non)significance limit of moderator`), -3.29628, tolerance = 0.0001)
  expect_equal(as.numeric(z$`upper (non)significance limit of moderator`), -1.360364, tolerance = 0.0001)
})

# Test whether the expected intervals are found (non-significant interaction)
test_that("expected JN interval is found", {
  z <- jnt_cont(X="Sepal.Length",Y="Sepal.Width",m="Petal.Length",data=iris)
  expect_equal(as.numeric(z$`lower (non)significance limit of moderator`), -3.770205, tolerance = 0.0001)
  expect_equal(as.numeric(z$`upper (non)significance limit of moderator`), 27.30096, tolerance = 0.0001)
})

# Test whether the expected intervals are found (non-significant interaction)
test_that("expected JN interval is found", {
  z <- jnt_cont(X="Petal.Length",Y="Sepal.Width",m="Sepal.Length",data=iris)
  expect_equal(as.numeric(z$`lower (non)significance limit of moderator`), -8.107869, tolerance = 0.0001)
  expect_equal(as.numeric(z$`upper (non)significance limit of moderator`), 10.61419, tolerance = 0.0001)
})

# Test whether the output is a list
test_that("jnt_cont() returns a list", {
  z <- jnt_cont(X='PHR95_overlap_z', Y='hrsize95', m='degree_z', data=lizard_home_range,
                xlab = 'home range overlap 95', ylab='home range size 95')
  expect_type(z, "list")
})
