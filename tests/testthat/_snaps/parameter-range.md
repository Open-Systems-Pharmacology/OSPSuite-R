# It can print parameter range

    Code
      paramRange$print()
    Output
      <ParameterRange>
        * Min: NULL
        * Max: NULL
        * Unit: NULL

# It prints a filled parameter range

    Code
      paramRange$print()
    Output
      <ParameterRange>
        * Min: 10
        * Max: 20
        * Unit: kg

# It returns a one-line print value

    Code
      ParameterRange$new(min = 10, max = 20, unit = "kg")$getPrintValue()
    Output
      [1] "[10 kg..20 kg]"
    Code
      ParameterRange$new(max = 20, unit = "kg")$getPrintValue()
    Output
      [1] "]-Inf..20 kg]"
    Code
      ParameterRange$new(min = 10, unit = "kg")$getPrintValue()
    Output
      [1] "[10 kg..+Inf["

