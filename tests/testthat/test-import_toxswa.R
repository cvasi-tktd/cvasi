test_that("testfile for TOXSWA import test exists", {
  f <- testthat::test_path(file.path("../data/TOXSWA", "testonil.out"))
  expect_true(file.exists(f))
})

test_that("import of TOXSWA file works", {
  f <- testthat::test_path(file.path("../data/TOXSWA", "testonil.out"))
  toxswa_content <- import_toxswa(f)

  # file name and substance names are as expectet
  expect_equal(names(toxswa_content),
               c("testonil_substance", "testonil_metabolite"))

  # trial column per substance has unique value
  expect_true(
    all(
      unlist(lapply(toxswa_content, function(x) length(unique(x$trial)) == 1))
      )
  )

  # all list elements have the correct column names
  expect_true(
    all(
      unlist(lapply(toxswa_content, function(x) colnames(x) == c("time","conc","trial")))
    )
  )

  # all substances/metabolites have the correct units stored in the attributes
  expect_true(
    all(
      unlist(lapply(toxswa_content,
                    function(x) attributes(x)$units == c(time = "d", conc = "g m-3")))
    )
  )

})
