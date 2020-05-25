#### test fars_read

context("make_filename")

test_that("make_filename stops when it should", {

      expect_error(make_filename("abc"))

})

test_that("make_filename for year 2013", {

      expect_equal(make_filename(2013),"accident_2013.csv.bz2")
      expect_equal(make_filename("2013"),"accident_2013.csv.bz2")

})



