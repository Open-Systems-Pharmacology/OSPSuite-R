# Convert `.NET` object to wrapper class in R

Transforms a single or a list of `.NET` object(s) to their corresponding
wrapper class in R. Note that if the object is a single object, `NULL`
will be returned if the `.NET` object is `null`. This allows semantic
equivalence between `.NET` and R.

## Usage

``` r
.toObjectType(netObject, class)
```

## Arguments

- netObject:

  The `.NET` object instances (single or list) to wrap.

- class:

  The class definition that will be used to convert the parameter.

## Value

The wrapped object (single or a list).
