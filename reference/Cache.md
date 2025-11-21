# Cache

A class that provides structure for caching objects.

## Public fields

- `type`:

  Type of the object that are cached. All objects must be of the same
  type.

## Active bindings

- `keys`:

  List all key in the cache

## Methods

### Public methods

- [`Cache$new()`](#method-Cache-new)

- [`Cache$reset()`](#method-Cache-reset)

- [`Cache$set()`](#method-Cache-set)

- [`Cache$hasKey()`](#method-Cache-hasKey)

- [`Cache$get()`](#method-Cache-get)

- [`Cache$dropKey()`](#method-Cache-dropKey)

- [`Cache$print()`](#method-Cache-print)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    Cache$new(type = NULL)

#### Arguments

- `type`:

  Optional type constraints for object stored in the cache.

#### Returns

A new `Cache` object.

------------------------------------------------------------------------

### Method `reset()`

Clear cache

#### Usage

    Cache$reset()

------------------------------------------------------------------------

### Method `set()`

Store the given value in cache with the given key

#### Usage

    Cache$set(key, value)

#### Arguments

- `key`:

  Key to use to store the `value` in the case

- `value`:

  Value to store in the cache

------------------------------------------------------------------------

### Method `hasKey()`

Check if an entry with the given `key` exists in cache

#### Usage

    Cache$hasKey(key)

#### Arguments

- `key`:

  Key to check

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get cached object by key

#### Usage

    Cache$get(key)

#### Arguments

- `key`:

  Key to use to retrieve the value

------------------------------------------------------------------------

### Method `dropKey()`

Remove the value associated with the key from cache.

#### Usage

    Cache$dropKey(key)

#### Arguments

- `key`:

  Key used to find the value to remove. If the key is not found, nothing
  happens.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Cache$print(...)

#### Arguments

- `...`:

  Rest arguments.

## Examples

``` r
# Create a new instance of the class
myCache <- ospsuite:::Cache$new()

# Adding new key-value pairs
myCache$set("data1", iris)
myCache$set("data2", BOD)

# Print to see the current pairs
myCache
#> Cache for objects of type: 
#> Cached keys:
#> data1
#> data2

# Get value using a key
myCache$get("data2")
#>   Time demand
#> 1    1    8.3
#> 2    2   10.3
#> 3    3   19.0
#> 4    4   16.0
#> 5    5   15.6
#> 6    7   19.8
```
