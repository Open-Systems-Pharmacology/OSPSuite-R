# Entity

Abstract wrapper for an OSPSuite.Core Entity class

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ObjectBase.md)
-\> `Entity`

## Active bindings

- `path`:

  The path of the entity in the container hierarchy without the
  simulation name. (read-only)

- `fullPath`:

  Same as `path`, but with the simulation name. (read-only)

- `parentContainer`:

  Returns a new wrapper instance to the .NET parent container. Multiple
  call to this method will always return the same instance. However two
  children of the same parent will return two different instances of
  `Container` pointing to the same .NET container
