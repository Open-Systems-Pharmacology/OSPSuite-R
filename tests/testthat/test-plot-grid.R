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

  library(ggplot2)

  # `{tlf}` plot
  set.seed(123)
  boxData <- data.frame(x = c(rep("A", 500), rep("B", 500)), y = rlnorm(1000))
  p1 <- plotBoxWhisker(data = boxData, dataMapping = BoxWhiskerDataMapping$new(x = "x", y = "y"))

  # custom `{ggplot2}` plot
  p2 <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()

  plotGridObj2 <- createPlotGridConfiguration(
    plotList = list(p1, p2),
    nColumns = 1L,
    tagLevels = "i"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plotGrid allows mixing of tlf and custom ggplot objects",
    fig = plotGrid(plotGridObj2)
  )
})
