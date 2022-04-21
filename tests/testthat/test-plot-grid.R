test_that("plots grid produces error with wrong input type", {
  expect_error(plotGrid(DataSet$new(name = "DS")))
})

test_that("plots grid is rendered correctly", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("tlf")
  skip_if(getRversion() < "4.1")

  library(tlf)

  set.seed(123)
  ls_plots <- list(
    # first plot
    tlf::plotBoxWhisker(mtcars,
      dataMapping = tlf::BoxWhiskerDataMapping$new(x = "am", y = "wt"), outliers = FALSE
    ),
    # second plot
    tlf::plotBoxWhisker(ToothGrowth,
      dataMapping = tlf::BoxWhiskerDataMapping$new(x = "supp", y = "len")
    )
  )

  plotGridObj <- PlotGridConfiguration$new(ls_plots)

  plotGridObj$title <- "my combined plot"
  plotGridObj$subtitle <- "something clever"
  plotGridObj$caption <- "something dumb"
  plotGridObj$nColumns <- 2L
  plotGridObj$tagLevels <- "A"
  plotGridObj$tagPrefix <- "Plot ("
  plotGridObj$tagSuffix <- ")"

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plotGrid works as expected",
    fig = plotGrid(plotGridObj)
  )
})
