{.experimental.}
type ColMajor* = object
type RowMajor* = object
type Options* = concept x
  x is ColMajor or
    x is RowMajor
