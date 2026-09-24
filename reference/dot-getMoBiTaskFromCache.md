# .getMoBiTaskFromCache

Get an instance of the specified `.NET` Task from MoBi.R that is
retrieved from cache if already initiated. Otherwise a new task will be
initiated and cached in the `tasksEnv`.

## Usage

``` r
.getMoBiTaskFromCache(taskName)
```

## Arguments

- taskName:

  The name of the task to retrieve (**without** `Get` prefix).

## Value

returns an instance of of the specified `.NET` task.
