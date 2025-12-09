# Get angle between vectors in radians or degrees

**\[experimental\]**

Get the angle between two vectors specified by their end points using
law of cosines. Vector 1 is defined as the vector pointing from the
point `(x1_i, y1_i)` to `(x1_f, y1_f)` and vector 2 is defined as the
vector point from the point `(x2_i, y2_i)` to `(x2_f, y2_f)`. The angle
is defined as the angle produced if the two vectors are joined at the
initial endpoints (rather than head-to-tail). The resulting angle is
always positive. It is only necessary to either provide the points or
the x/y vector elements.

## Usage

``` r
get_angle_between_vectors(
  x1_i = NA,
  y1_i = NA,
  x1_f = NA,
  y1_f = NA,
  x2_i = NA,
  y2_i = NA,
  x2_f = NA,
  y2_f = NA,
  dx1 = NA,
  dy1 = NA,
  dx2 = NA,
  dy2 = NA,
  degrees = F
)
```

## Arguments

- x1_i:

  x coordinate of initial point of vector 1

- y1_i:

  y coordinate of initial point of vector 1

- x1_f:

  x coordinate of final point of vector 1

- y1_f:

  y coordinate of final point of vector 1

- x2_i:

  x coordinate of initial point of vector 2

- y2_i:

  y coordinate of initial point of vector 2

- x2_f:

  x coordinate of final point of vector 2

- y2_f:

  y coordinate of final point of vector 2

- dx1:

  x element in vector 1

- dy1:

  y element in vector 1

- dx2:

  x element in vector 2

- dy2:

  y element in vector 2

- degrees:

  if `T`, angle will be returned in degrees (otherwise it will be
  returned in radians)

## Value

Returns the angle between the two specified vectors

## Author

Ariana Strandburg-Peshkin

NOT YET CODE REVIEWED
