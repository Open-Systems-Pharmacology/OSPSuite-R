##' Creates a time profile plot for given data.
#'
#' This function generates a time profile plot using ggplot2, where the data is grouped by a column named "group".
#'
#' @param plotData An object of class DataCombined or a data.table. If it is a data.table it should contain the following columns:
#'   - `xValues`: Numeric values representing the time points.
#'   - `yValues`: Numeric values representing the corresponding observed or simulated values.
#'   - `group`: A factor or character vector indicating the grouping variable for the data.
#'   - `xUnit`: A character string representing the unit of the x-axis values.
#'   - `yUnit`: A character string representing the unit of the y-axis values.
#'   - `dataType`: A character string representing the type of the data, either `observed` or `simulated`
#' @param metaData A list containing metadata for the plot. If NULL, a default list is constructed from the data. Expected structure includes information about dimensions and units for both x and y axes.
#' @param mapping A ggplot2 aesthetic mapping object. Default is `ggplot2::aes()`. This is added or replaces the default mapping constructed by the data.
#' @param observedMapping A ggplot2 aesthetic mapping for observed data. Default is the same as `mapping`.
#' @param ... Additional arguments passed to `ospsuite.plots::plotTimeProfile`.
#'
#' @return A `ggplot2` plot object representing the time profile.
#' @export
#'
#' @examples \dontrun{
#' # Generate a time profile plot for the provided data
#' plotTimeProfile(convertUnits(
#'   myDataCombined,
#'   xUnit = ospUnits$Time$h,
#'   yUnit = ospUnits$`Concentration [mass]`$`mg/l`
#' ))
#' }
plotTimeProfile <- function(plotData, # nolint
                            metaData = NULL,
                            mapping = ggplot2::aes(),
                            observedMapping = mapping,
                            ...) {
  # initialize variables used for data.table to avoid messages during checks
  yDimension <- yUnit <- dataType <- NULL

  plotData <- .validateAndConvertData(plotData = plotData, predictedIsNeeded = FALSE)
  checkmate::assertNames(names(plotData), must.include = c("xUnit"))

  # Capture additional arguments
  additionalArgs <- list(...)

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData,nYunit = 2)
  }

  if (any(names(metaData) %in% "y2")) {
    if (!("yDimension" %in% names(plotData))) {
      plotData[, yDimension := ""]
      for (yUnitLoop in unique(plotData$yUnit)) {
        plotData[yUnitLoop == yUnit, yDimension := ospsuite::getDimensionForUnit(yUnitLoop)]
      }
    }
  }
  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotTimeProfile,
    args = c(
      list(
        data = plotData[dataType == "simulated"],
        mapping = .getMappingForTimeprofiles(
          plotData = plotData[dataType == "simulated"],
          metaData = metaData,
          userMapping = mapping
        ),
        observedMapping = .getMappingForTimeprofiles(
          plotData = plotData[dataType == "observed"],
          metaData = metaData,
          userMapping = observedMapping
        ),
        metaData = metaData,
        observedData = plotData[dataType == "observed"]
      ),
      additionalArgs
    )
  )

  return(plotObject)
}
#' Plots predicted vs observed data, grouped by "group".
#'
#' This function visualizes the relationship between predicted and observed values, allowing for easy identification of discrepancies.
#'
#' @inheritParams plotTimeProfile
#' @param xyscale A character string specifying the scale for the x and y-axis.
#'          Default is 'log'.
#' @param ... Additional arguments passed to `ospsuite.plots::plotPredVsObs`.
#'
#' @return A `ggplot2` plot object representing predicted vs observed values, including aesthetics for the x and y axes.
#' @export
#'
#' @examples \dontrun{
#' # Generate a predicted vs observed plot for the provided data
#' plotPredictedVsObserved(myDataCombined)
#' }
plotPredictedVsObserved <- function(plotData, # nolint
                                    metaData = NULL,
                                    mapping = ggplot2::aes(),
                                    xyscale = 'log',
                                    ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(plotData = plotData,
                                      predictedIsNeeded = TRUE,
                                      scaling = xyscale)

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- .getMappingForPredictedVsObserved(plotData = plotData,
                                               userMapping = mapping)

  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }

  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotPredVsObs,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        xyscale = xyscale
      ),
      additionalArgs
    )
  )

  return(plotObject)
}
#' Plots residuals vs Time, grouped by "group".
#'
#' This function visualizes the residuals over time, providing insights into the accuracy of the model predictions.
#'
#' @inheritParams plotTimeProfile
#' @param residualScale A character string specifying the scale for the x and y-axis. Default is 'log'.
#' @param... Additional arguments passed to `ospsuite.plots::plotResVsCov`.
#'
#' @return A `ggplot2` plot object representing residuals vs time.
#' @export
plotResidualsVsTime <- function(plotData, # nolint
                                metaData = NULL,
                                mapping = ggplot2::aes(),
                                residualScale = 'log',
                                  ...) {
  # initialize variables used for data.table to avoid messages during checks
  xValues <- predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(plotData = plotData,
                                      predictedIsNeeded = TRUE,
                                      scaling = residualScale)

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        x = xValues,
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
  )


  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }

  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotResVsCov,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        residualScale = residualScale
      ),
      additionalArgs
    )
  )

  return(plotObject)
}

#' Plots residuals vs observed values, grouped by "group".
#'
#' This function visualizes the residuals against observed values, helping to assess model performance.
#'
#' @inheritParams plotTimeProfile
#' @inheritParams plotResidualVsTime
#' @param... Additional arguments passed to `ospsuite.plots::plotResVsCov`.
#'
#' @return A `ggplot2` plot object representing residuals vs observed values.
#' @export
#'
#' @examples \dontrun{
#' # Generate a residuals vs observed plot for the provided data
#' plotResidualsVsObserved(convertUnits(
#'   myDataCombined,
#'   xUnit = ospUnits$Time$h,
#'   yUnit = ospUnits$`Concentration [mass]`$`µg/l`
#' ))
#' }
plotResidualsVsObserved <- function(plotData,
                                    metaData = NULL,
                                    mapping = ggplot2::aes(),
                                    residualScale = 'log',
                                             ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(plotData = plotData,
                                      predictedIsNeeded = TRUE,
                                      scaling = residualScale)

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        x = yValues,
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
  )


  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }

  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotResVsCov,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        residualScale = residualScale
      ),
      additionalArgs
    )
  )

  return(plotObject)
}

#' Plots residuals as a histogram, grouped by "group".
#'
#' This function generates a histogram of the residuals, providing a visual representation of their distribution.
#'
#' @inheritParams plotTimeProfile
#' @inheritParams plotResidualVsTime
#' @param distribution parameter passed to `ospsuite.plots::plotHistogram`.
#' @param... Additional arguments passed to `ospsuite.plots::plotHistogram`.
#'
#' @return A `ggplot2` plot object representing the histogram of residuals.
#' @export
plotResidualsAsHistogram <- function(plotData,
                                     metaData = NULL,
                                     mapping = ggplot2::aes(),
                                     distribution = 'normal',
                                     residualScale = 'log',
                                     ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(plotData = plotData,
                                      predictedIsNeeded = TRUE,
                                      scaling = residualScale)

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
  )


  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }
  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotHistogram,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        residualScale = residualScale
      ),
      additionalArgs
    )
  )

  return(plotObject)
}


#' Plots a Quantile-Quantile plot, grouped by "group".
#'
#' This function visualizes the distribution of predicted vs observed values using a Q-Q plot.
#'
#' @inheritParams plotTimeProfile
#' @inheritParams plotResidualVsTime
#' @param... Additional arguments passed to `ospsuite.plots::plotQQ`.
#'
#' @return A `ggplot2` plot object representing the Q-Q plot.
#' @export
plotQuantileQuantilePlot <- function(plotData,
                                     metaData = NULL,
                                     mapping = ggplot2::aes(),
                                     residualScale = 'log',
                                              ...) {
  # initialize variables used for data.table to avoid messages during checks
  predicted <- yValues <- group <- NULL

  plotData <- .validateAndConvertData(plotData = plotData,
                                      predictedIsNeeded = TRUE,
                                      scaling = residualScale)

  # Capture additional arguments
  additionalArgs <- list(...)

  mapping <- structure(
    utils::modifyList(
      ggplot2::aes(
        predicted = predicted,
        observed = yValues,
        groupby = group
      ),
      mapping
    ),
    class = "uneval"
  )


  if (is.null(metaData)) {
    metaData <- .constructMetDataForTimeProfile(plotData)
    metaData$predicted <- metaData$yValues
  }

  # create plot Object
  plotObject <- do.call(
    what = ospsuite.plots::plotQQ,
    args = c(
      list(
        data = plotData,
        mapping = mapping,
        metaData = metaData,
        residualScale = residualScale
      ),
      additionalArgs
    )
  )

  return(plotObject)
}

#' Validates observed data and converts it to appropriate format.
#'
#' This function checks the input data for required columns and formats it for plotting. If predicted values are needed,
#' it calculates them based on the observed and simulated data.
#'
#' @param plotData Either a data.table with columns 'xValues', 'yValues', 'group' or an object of class 'DataCombined'.
#' @param predictedIsNeeded If TRUE, only observed data are returned. If FALSE and the "predicted" column does not exist,
#'        predicted values are calculated.
#' @param scaling A character string specifying the scale for the data. Used in the conversion process.
#'
#' @return A `data.table` with data formatted for plotting.
#' @keywords internal
.validateAndConvertData <- function(plotData, predictedIsNeeded,scaling) {
  # initialize variables used for data.table to avoid messages during checks
  dataType <- NULL

  if ("DataCombined" %in% class(plotData)) {

    if (predictedIsNeeded){
      plotData <- calculateResiduals(dataCombined = plotData,
                                     scaling = scaling) %>%
        data.table::setDT()
      if (nrow(plotData) == 0) stop("No data for this plot available")
      plotData <- plotData %>%
        data.table::setnames(old = c("yValuesSimulated",'yValuesObserved'),
                             new = c('predicted','yValues')) %>%
        dplyr::mutate(dataType = 'observed')

    } else {

      plotData <- plotData$toDataFrame() %>%
        data.table::setDT()
      # check if we have datatype observed AND simulated and  units differ between dataType
      if (plotData[,uniqueN(dataType)]==2 &&
          plotData[,.(nUnit = uniqueN(xUnit)*uniqueN(yUnit))]$nUnit > 1){

        if (!all(plotData[,.(nUnit = uniqueN(xUnit)*uniqueN(yUnit)),by = 'dataType']$nUnit == 1)){
          stop('units have to be consistent within one datatype')
        }
        plotData <- .unitConverter(data = plotData,
                                   xUnit = plotData[dataType == 'observed']$xUnit[1],
                                   yUnit = plotData[dataType == 'observed']$yUnit[1])
      }
    }
  }
  checkmate::assertDataFrame(plotData)
  checkmate::assertNames(names(plotData), must.include = c("xValues", "yValues", "group", "dataType"))

  # create a copy, so changes to columns will stay inside function
  plotData <- data.table::copy(data.table::setDT(plotData))

  if (predictedIsNeeded) {
    if (!("predicted" %in% names(plotData))) {
      stop("Please use combinedDataFormat or a data.frame with column 'predicted'")
    } else {
      plotData <- plotData[dataType == "observed"]
    }
    plotData <- plotData[!is.na(predicted)]
  }

  if (nrow(plotData) == 0) {
    stop("No data for this plot available")
  }

  return(plotData)
}

#' Construct Metadata for Time Profile
#'
#' This function generates metadata for time profile plots based on the provided plot data.
#' It extracts and validates the dimensions and units for both x and y values, ensuring
#' that there is no ambiguity in the units.
#'
#' @param plotData A data.table containing the following relevant columns:
#'   - `xUnit`: The unit of the x-axis values.
#'   - `xDimension`: (optional) The dimension of the x values, if already specified.
#'   - `yUnit`: The unit(s) of the y-axis values (can be one or two units).
#'   - `yDimension`: (optional) The dimension of the y values, if already specified.
#' @param nYunit Maximal number of different y-units that the plot can handle.
#'
#' @details
#' The function checks for ambiguities in the x and y units and retrieves the corresponding
#' dimensions. If two y units are provided, it constructs separate metadata for each.
#' The resulting metadata is returned as a list, which includes dimensions and units for
#' both x and y values.
#'
#' @return A list containing metadata with the following structure:
#' - `xValues`: A list with `dimension` and `unit` for the x-axis.
#' - `yValues`: A list with `dimension` and `unit` for the primary y-axis.
#' - `y2`: (optional) A list with `dimension` and `unit` for the secondary y-axis if applicable.
#' @keywords internal
.constructMetDataForTimeProfile <- function(plotData, nYunit = 1) {
  xUnit <- unique(plotData$xUnit)
  if (length(xUnit) > 1) stop("x Unit ambiguous")
  if ("xDimension" %in% names(plotData)) {
    xDimension <- unique(plotData$xDimension)
  } else {
    xDimension <- ospsuite::getDimensionForUnit(xUnit)
  }
  yUnit <- unique(plotData$yUnit)
  if (length(yUnit) > nYunit) stop("y Unit ambiguous")
  if ("yDimension" %in% names(plotData)) {
    yDimension <- unique(plotData$yDimension)
  } else {
    yDimension <- unlist(lapply(yUnit, function(yUnit) {
      ospsuite::getDimensionForUnit(yUnit)
    }))
  }

  metaData <- list(
    xValues = list(
      dimension = xDimension,
      unit = xUnit
    ),
    yValues = list(
      dimension = yDimension[1],
      unit = yUnit[1]
    )
  )
  if (length(yUnit) == 2) {
    metaData[["y2"]] <- list(
      dimension = yDimension[2],
      unit = yUnit[2]
    )
  }

  return(metaData)
}


#' Creates mapping for plotData.
#'
#' This function generates a mapping for the plotting based on the provided plot data and metadata.
#'
#' @param plotData Data to map.
#' @param metaData A list with metadata for plotData.
#' @param userMapping Mapping provided by the user; this will update the internal mapping.
#'
#' @return A mapping object for ggplot2.
#' @keywords internal
.getMappingForTimeprofiles <- function(plotData, metaData, userMapping) {
  # initialize variables used for data.table to avoid warnings during checks
  xValues <- yValues <- group <- yErrorType <- yErrorValues <- yMin <- yMax <- lloq <- NULL

  mapping <- ggplot2::aes(x = xValues, y = yValues)

  if (any(!is.na(plotData$group))){
    mapping <- structure(c(mapping, ggplot2::aes(groupby = group)), class = "uneval")
  }

  # delete columns not needed
  plotData <- plotData[, which(colSums(is.na(plotData)) != nrow(plotData)), with = FALSE]

  if ("yErrorType" %in% names(plotData) &&
    any(plotData[["yErrorType"]] %in% unlist(ospsuite::DataErrorType))) { # nolint
    checkmate::assertNames(names(plotData),
      must.include = c("yErrorValues"), # nolint
      disjunct.from = c("yMin", "yMax"),
      .var.name = "columns needed for yErrorValues"
    )

    if (length(unique(plotData[!is.na(yErrorType)][["yErrorType"]])) > 1) {
      stop("Please do not mix different error Types in one plot")
    }

    if (any(plotData[["yErrorType"]] == ospsuite::DataErrorType$ArithmeticStdDev,na.rm = TRUE)) {
      mapping <- structure(c(mapping, ggplot2::aes(error = yErrorValues)), class = "uneval")
    }

    if (any(plotData[["yErrorType"]] == ospsuite::DataErrorType$GeometricStdDev,na.rm = TRUE)) {
      mapping <- structure(c(mapping, ggplot2::aes(error_relative = yErrorValues)), class = "uneval")
    }
  } else if (any(c("yMin", "yMax") %in% names(plotData))) {
    checkmate::assertNames(names(plotData), must.include = c("yMin", "yMax"))
    mapping <- structure(c(mapping, ggplot2::aes(ymin = yMin, ymax = yMax)), class = "uneval")
  }
  if (any(names(plotData) %in% "lloq")) {
    mapping <- structure(c(mapping, ggplot2::aes(lloq = lloq)), class = "uneval")
  }

  if (any(names(metaData) %in% "y2")) {
    mapping <- structure(
      c(
        mapping,
        eval(parse(
          text = paste0(
            "aes( y2axis = yUnit == '",
            metaData[["y2"]][["unit"]], "')"
          )
        ))
      ),
      class = "uneval"
    )
  }

  if (!is.null(userMapping)) {
    mapping <- structure(utils::modifyList(mapping, userMapping), class = "uneval")
  }

  return(mapping)
}
#' Creates mapping for plotData.
#'
#' This function generates a mapping for the plotting based on the provided plot data and metadata.
#'
#' @param plotData Data to map.
#' @param metaData A list with metadata for plotData.
#' @param userMapping Mapping provided by the user; this will update the internal mapping.
#'
#' @return A mapping object for ggplot2.
#' @keywords internal
.getMappingForPredictedVsObserved <- function(plotData,userMapping){
  defaultMapping <- ggplot2::aes(
    predicted = predicted,
    observed = yValues,
    groupby = group
  )

  if ('lloq' %in% names(plotData) & any(!is.na(plotData$lloq))){
    defaultMapping <- structure(
      utils::modifyList(
        defaultMapping,
        ggplot2::aes(lloq = lloq)
      ),
      class = "uneval"
    )
  }

  mapping <- structure(
    utils::modifyList(
      defaultMapping,
      userMapping
    ),
    class = "uneval"
  )

  return(mapping)

}

