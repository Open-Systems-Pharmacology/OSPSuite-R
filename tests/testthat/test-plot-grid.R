test_that("plots grid is rendered correctly", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  set.seed(123)
  ls_plots <- list(
    tlf::plotHistogram(x = rnorm(100)),
    tlf::plotHistogram(x = rnorm(100, mean = 3)),
    tlf::plotHistogram(x = rnorm(100, mean = 10))
  )

  plotGridObj <- createPlotGridConfiguration(
    plotList = ls_plots,
    title = "my combined plot",
    subtitle = "something clever",
    caption = "something dumb",
    nColumns = 2L,
    tagLevels = "A",
    tagPrefix = "Plot (",
    tagSuffix = ")"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plotGrid works as expected",
    fig = plotGrid(plotGridObj)
  )
})
