# it can print a data repository

    Code
      print(obsData)
    Output
      <DataRepository>

# setSubjectCount throws an error for a value that is not a positive whole number

    Code
      dataRepository$setSubjectCount(2.5)
    Condition
      Error in `dataRepository$setSubjectCount()`:
      ! subjectCount must be one positive whole number, not 2.5.

