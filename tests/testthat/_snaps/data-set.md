# it can add a new meta data

    Code
      print(dataSet)
    Output
      <DataSet>
        * Name: ObservedData
        * X dimension: Time
        * X unit: min
        * Y dimension: Concentration (mass)
        * Y unit: mg/l
        * Error type: ArithmeticStdDev
        * Error unit: mg/l
        * Molecular weight: NULL
        * LLOQ: NULL
        * Subject count: NULL
      Meta data:
        * Source: C:\temp\RanorexTestData\ObservedData.xlsx
        * Sheet: Tabelle1
        * DoubleValue: 5
        * StringValues: hello
        * IntegerValue: 4
        * Meta: Value

# it throws an error for a subjectCount that is not one positive whole number

    Code
      dataSet$subjectCount <- 0
    Condition
      Error:
      ! subjectCount must be one positive whole number, not 0.

---

    Code
      dataSet$subjectCount <- 2.5
    Condition
      Error:
      ! subjectCount must be one positive whole number, not 2.5.

---

    Code
      dataSet$subjectCount <- c(12, 8)
    Condition
      Error:
      ! subjectCount must be one positive whole number, not 12 and 8.

---

    Code
      dataSet$subjectCount <- "twelve"
    Condition
      Error:
      ! subjectCount must be one positive whole number, not "twelve".

---

    Code
      dataSet$subjectCount <- Inf
    Condition
      Error:
      ! subjectCount must be one positive whole number, not Inf.

---

    Code
      dataSet$subjectCount <- -1
    Condition
      Error:
      ! subjectCount must be one positive whole number, not -1.

---

    Code
      dataSet$subjectCount <- TRUE
    Condition
      Error:
      ! subjectCount must be one positive whole number, not TRUE.

# subjectCount does not accept text that is not only digits

    Code
      dataSet$subjectCount <- "1e1"
    Condition
      Error:
      ! subjectCount must be one positive whole number, not "1e1".

---

    Code
      dataSet$subjectCount <- "0x0C"
    Condition
      Error:
      ! subjectCount must be one positive whole number, not "0x0C".

---

    Code
      dataSet$subjectCount <- factor("twelve")
    Condition
      Error:
      ! subjectCount must be one positive whole number, not "twelve".

# addMetaData with the name subjectCount needs text, like all other entries

    Code
      dataSet$addMetaData("subjectCount", 18)
    Condition
      Error in `validateIsOfType()`:
      ! `test()`: argument "value" is of type <numeric>, but expected <character>!

---

    Code
      dataSet$addMetaData("subjectCount", NA_character_)
    Condition
      Error in `dataSet$addMetaData()`:
      ! subjectCount must be one positive whole number, not NA.

# addMetaData with the name subjectCount throws an error for an invalid value

    Code
      dataSet$addMetaData("subjectCount", "n=24")
    Condition
      Error in `dataSet$addMetaData()`:
      ! subjectCount must be one positive whole number, not "n=24".

# subjectCount is NULL with a warning when the stored meta data entry is not a whole number

    Code
      subjectCount <- dataSet$subjectCount
    Condition
      Warning:
      The meta data entry subjectCount of the data set "MyDataSet" is "n=24", which is not a positive whole number.
      The number of subjects is treated as not set.

# print shows subjectCount once, not again as meta data

    Code
      print(dataSet)
    Output
      <DataSet>
        * Name: ObservedData
        * X dimension: Time
        * X unit: min
        * Y dimension: Concentration (mass)
        * Y unit: mg/l
        * Error type: ArithmeticStdDev
        * Error unit: mg/l
        * Molecular weight: NULL
        * LLOQ: NULL
        * Subject count: 15
      Meta data:
        * Source: C:\temp\RanorexTestData\ObservedData.xlsx
        * Sheet: Tabelle1
        * DoubleValue: 5
        * StringValues: hello
        * IntegerValue: 4

