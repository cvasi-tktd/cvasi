test_that("basic import", {
  fn <- testthat::test_path(file.path("../data/TOXSWA", "testonil.out"))

  rs <- import_toxswa(fn, time_unit="days", output_unit="ug/L")
  expect_equal(length(rs), 1)
  expect_equal(names(rs), "testonil")
  expect_equal(length(rs[[1]]), 4)
  expect_equal(names(rs[[1]]), c("time","timestamp","substance","metabolite"))
  expect_equal(as.numeric(rs[[1]]$time), c(0, 0.041, 0.083, 0.125, 0.166))
  expect_equal(rs[[1]]$timestamp[1], lubridate::ymd_hms("1992-01-01 00:30:00"))
  expect_equal(rs[[1]]$timestamp[2], lubridate::ymd_hms("1992-01-01 01:30:00"))
  expect_equal(as.numeric(rs[[1]]$substance), c(2000, 0, 0, 0, 0))
  expect_equal(as.numeric(rs[[1]]$metabolite), c(4000, 0, 0, 0, 0))
  expect_equal(units::deparse_unit(rs[[1]]$time), "d")
  expect_equal(units::deparse_unit(rs[[1]]$substance), "ug L-1")
  expect_equal(units::deparse_unit(rs[[1]]$metabolite), "ug L-1")

  # multiple files
  rs <- import_toxswa(c(fn, fn))
  expect_equal(length(rs), 2)
  expect_equal(names(rs), c("testonil", "testonil"))
  expect_equal(rs[[1]], rs[[2]])

  # no file
  expect_error(import_toxswa(NULL))
  expect_error(import_toxswa("foo/bar.baz"))
})

test_that("unit conversion", {
  fn <- testthat::test_path(file.path("../data/TOXSWA", "testonil.out"))
  # time units
  rsd <- import_toxswa(fn, time_unit="days")
  rsh <- import_toxswa(fn, time_unit="hours")
  expect_equal(as.numeric(rsd[[1]]$time) * 24,
               as.numeric(rsh[[1]]$time))
  # output var units
  rsm <- import_toxswa(fn, output_unit="mg/L")
  rsu <- import_toxswa(fn, output_unit="ug/L")
  expect_equal(as.numeric(rsm[[1]]$substance) * 1000,
               as.numeric(rsu[[1]]$substance))
  expect_equal(as.numeric(rsm[[1]]$metabolite) * 1000,
               as.numeric(rsu[[1]]$metabolite))

  # invalid unit
  expect_error(import_toxswa(fn, output_unit="foo"))
})

test_that("filter substance", {
  fn <- testthat::test_path(file.path("../data/TOXSWA", "testonil.out"))

  rs <- import_toxswa(fn, substance="substance")
  expect_equal(names(rs[[1]]), c("time", "timestamp", "substance"))
  rs <- import_toxswa(fn, substance="metabolite")
  expect_equal(names(rs[[1]]), c("time", "timestamp", "metabolite"))

  expect_error(import_toxswa(fn, substance="foo"))
})

test_that("split by substance", {
  fn <- testthat::test_path(file.path("../data/TOXSWA", "testonil.out"))

  rs <- import_toxswa(fn, split=TRUE)
  expect_equal(length(rs), 2)
  expect_equal(names(rs[[1]]), c("time", "timestamp", "substance"))
  expect_equal(names(rs[[2]]), c("time", "timestamp", "metabolite"))
})

test_that("invalid file contents", {
  # no expected contents
  withr::with_file("file1", {
    writeLines("foo", "file1")
    expect_error(import_toxswa("file1"), "version missing")
  })

  # unsupported TOXSWA version
  withr::with_file("file1", {
    writeLines("* FOCUS  TOXSWA version   : 99", "file1")
    expect_error(import_toxswa("file1"), "version not supported")
  })

  # output unit missing
  withr::with_file("file1", {
    writeLines("* FOCUS  TOXSWA version   : 4", "file1")
    expect_error(import_toxswa("file1"), "Unit of output var missing")
  })

  # output unit not found
  withr::with_file("file1", {
    writeLines(c("* FOCUS  TOXSWA version   : 4",
                 "* Unit for ConLiqWatLay is ()"), "file1")
    expect_error(import_toxswa("file1"), "Unit of output var missing")
  })

  # output unit not unique
  withr::with_file("file1", {
    writeLines(c("* FOCUS  TOXSWA version   : 4",
                 "* Unit for ConLiqWatLay is (g.m-3)",
                 "* Unit for ConLiqWatLay is (kg.m-3)"), "file1")
    expect_error(import_toxswa("file1"), "Output variable is not unique")
  })

  # no data for output
  withr::with_file("file1", {
    writeLines(c("* FOCUS  TOXSWA version   : 4",
                 "* Unit for ConLiqWatLay is (g.m-3)"), "file1")
    expect_error(import_toxswa("file1"), "No data")
  })
})

test_that("large number of columns", {
  # It's a frequent issue with most functions such as read.delim(), readr::read_*
  # that not all columns are actually imported into the same data.frame row
  withr::with_file("file1", {
    writeLines(c("* FOCUS  TOXSWA version   : 4",
                 "* Unit for ConLiqWatLay is (g.m-3)",
                 " 0  01-Jan-1992-00h30 ConLiqWatLay_par 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 42"), "file1")
    rs <- import_toxswa("file1", output_unit="g/m3")
    expect_equal(as.numeric(rs[[1]]$par), 42)
  })
})



