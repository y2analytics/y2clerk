# Convert a variable to haven labelled

Convert a character or factor vector into a labelled (haven labelled)
vector. Useful for when you need to either extract labels or attach
underlying numbers to each label.

## Usage

``` r
to_haven_y2(variable, ...)
```

## Arguments

- variable:

  The vector you wish to convert to haven labelled

- ...:

  Reserved for future expansion. Must be empty.

## Examples

``` r

test <- tibble::tibble(
  color_vec = c('Blue', 'Blue', 'Red', 'Yellow'),
  color_factor = forcats::as_factor(color_vec)
)

test$color <- to_haven_y2(test$color_factor)

test <- test |>
  dplyr::mutate(
    color = to_haven_y2(color_vec)
  )
```
