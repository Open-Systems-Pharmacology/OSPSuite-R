benchmarkFunction <- function(quantityOrDimension, values, targetUnit, sourceUnit = NULL,
                              nrIterations = 1000) {
  for (i in 1:nrIterations) {
    converted <- toUnit(
      quantityOrDimension = quantityOrDimension,
      values = values,
      targetUnit = targetUnit,
      sourceUnit = sourceUnit
    )
  }
  return(converted)
}

quantityOrDimension <- ospDimensions$Time

values <- seq(1, 100000)

#### toUnit####
# same unit
sourceUnit <- ospUnits$Time$min
targetUnit <- ospUnits$Time$min

# Same unit without source specified
# user  system elapsed
# 41.18    1.75   74.96

# New version
# 14.75    1.21   18.73
system.time(
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    nrIterations = 5000
  )
)

# 40k ms
# 10556 mb

# New version
# 7k ms
# 2290 mb
profvis::profvis({
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    nrIterations = 5000
  )
})

# Same unit with source specified
# user  system elapsed
# 39.67    0.74  107.83

# New version
# 15.42    1.35   18.58
system.time(
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    sourceUnit = sourceUnit,
    nrIterations = 5000
  )
)

# 70k ms
# 14753mb

# New version
# 8k ms
# 2440 mb
profvis::profvis({
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    sourceUnit = sourceUnit,
    nrIterations = 5000
  )
})

# Diff units with no source specified
sourceUnit <- ospUnits$Time$s
targetUnit <- ospUnits$Time$h

# user  system elapsed
# 35.54    1.00   64.55

# New version
# 36.15    1.25   67.63
system.time(
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    nrIterations = 5000
  )
)

# 36k ms
# 10627mb

# New version
# 33k ms
# 10326 mb
profvis::profvis({
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    nrIterations = 5000
  )
})


#########
# Diff units with no source specified
sourceUnit <- ospUnits$Time$s
targetUnit <- ospUnits$Time$h

# user  system elapsed
# 58.33    1.63  122.89

# New version
# 35.24    1.40   90.70
system.time(
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    sourceUnit = sourceUnit,
    nrIterations = 5000
  )
)

# 70k ms
# 14680mb

# New version
# 64k ms
# 14458 mb
profvis::profvis({
  converted <- benchmarkFunction(
    quantityOrDimension = quantityOrDimension,
    values = values,
    targetUnit = targetUnit,
    sourceUnit = sourceUnit,
    nrIterations = 5000
  )
})
