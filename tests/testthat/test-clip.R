test_that("clip_forcings", {
  # short forcings series
  df <- data.frame(t=0,v=0)
  expect_equal(clip_forcings(df,c(0,1)), df)
  expect_equal(clip_forcings(df,c(10,11)), df)

  df <- data.frame(t=0:1,v=0)
  expect_equal(clip_forcings(df,c(0,1)), df)
  expect_equal(clip_forcings(df,c(10,11)), df)

  # vanilla use case
  df <- data.frame(t=c(0,1,1.5,2,3,4,4.5,5),v=23)
  expect_equal(clip_forcings(df,c(0,1)), data.frame(t=c(0,1,1.5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(0,1.1)), data.frame(t=c(0,1,1.5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(0,1.5)), data.frame(t=c(0,1,1.5,2),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(0,1.6)), data.frame(t=c(0,1,1.5,2),v=23), ignore_attr=TRUE)

  expect_equal(clip_forcings(df,c(1.5,4)), data.frame(t=c(1,1.5,2,3,4,4.5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(1.6,4)), data.frame(t=c(1.5,2,3,4,4.5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(2,4)), data.frame(t=c(1.5,2,3,4,4.5),v=23), ignore_attr=TRUE)

  expect_equal(clip_forcings(df,c(4,5)), data.frame(t=c(3,4,4.5,5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(4.1,5)), data.frame(t=c(4,4.5,5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(4.5,5)), data.frame(t=c(4,4.5,5),v=23), ignore_attr=TRUE)

  # window not (fully) included in time-series
  expect_equal(clip_forcings(df,c(-1,1)), data.frame(t=c(0,1,1.5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(4,5.1)), data.frame(t=c(3,4,4.5,5),v=23), ignore_attr=TRUE)
  expect_equal(clip_forcings(df,c(10,11)), data.frame(t=5,v=23), ignore_attr=TRUE)
})

test_that("clip_scenario", {
  # constant exposure series
  Lemna_Schmitt() %>%
    set_exposure(data.frame(t=0, c=0)) %>%
    set_times(0:10) %>%
    set_forcings(temp=data.frame(t=0,temp=20), rad=data.frame(t=-1:20,rad=42)) -> sc
  clipped <- clip_scenario(sc, c(0,2.1))

  expect_equal(clipped@times, c(0,1,2,2.1))
  expect_equal(clipped@exposure@series, data.frame(t=0,c=0))
  expect_equal(clipped@forcings[["temp"]], data.frame(t=0,temp=20))
  expect_equal(clipped@forcings[["rad"]], data.frame(t=c(-1,0,1,2,3),rad=42), ignore_attr=TRUE)

  # complex exposure series
  sc <- sc %>% set_exposure(data.frame(t=2:4,c=23))
  clipped <- clip_scenario(sc, c(1,3))

  expect_equal(clipped@times, c(1,2,3))
  expect_equal(clipped@exposure@series, data.frame(t=c(2,3,4),c=23))
  expect_equal(clipped@forcings[["temp"]], data.frame(t=0,temp=20))
  expect_equal(clipped@forcings[["rad"]], data.frame(t=c(0,1,2,3,4),rad=42), ignore_attr=TRUE)

  # window is between two exposure time points
  sc <- sc %>% set_exposure(data.frame(t=c(0,10), c=0))
  clipped <- clip_scenario(sc, c(3,4))
  expect_equal(clipped@times, c(3,4))
  expect_equal(clipped@exposure@series, data.frame(t=c(0,10),c=0))
})
