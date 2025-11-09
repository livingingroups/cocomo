# Plot network matrix

Plot a matrix with color representing the edge weights of a network

## Usage

``` r
plot_network_matrix(
  mat,
  mat_col_palette = NULL,
  ind_names = NULL,
  ind_cols = NULL,
  zlim = NULL,
  xlab = "",
  ylab = "",
  main = ""
)
```

## Arguments

- mat:

  the matrix to plot (matrix of dimensions `n_inds x n_inds`)

- mat_col_palette:

  color palette to use for the matrix plot

- ind_names:

  the names for the x and y labels (vector of length `n_inds`)

- ind_cols:

  colors to use for the labels (vector of length `n_inds`)

- zlim:

  a vector of length 2 giving the numerical limits of z (the color
  scale)

- xlab:

  x axis label (rows of matrix)

- ylab:

  y axis label (columns of matrix)

- main:

  title for plot

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
