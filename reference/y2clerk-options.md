# y2clerk package options

y2clerk's behavior can be controlled with the following global options,
set via [`base::options()`](https://rdrr.io/r/base/options.html). Each
option has a built-in default that is used when the option is unset.

## Options

### `y2clerk.quiet`

**Default:** `FALSE`

Whether to suppress y2clerk's informational messages (e.g. progress
notes from
[`multi_freqs()`](https://y2analytics.github.io/y2clerk/reference/multi_freqs.md)
and
[`sig_test_y2()`](https://y2analytics.github.io/y2clerk/reference/sig_test_y2.md),
or the hints emitted by
[`freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md)
about ignored arguments). Warnings and errors are unaffected. Set to
`TRUE` to silence informational output.

    # Silence informational messages
    options(y2clerk.quiet = TRUE)

### `y2clerk.mcc_correction`

**Default:** `"fdr"`

The multiple comparison correction algorithm passed to
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) in
[`sig_test_y2()`](https://y2analytics.github.io/y2clerk/reference/sig_test_y2.md).
Must be one of: holm, hochberg, hommel, bonferroni, BH, BY, fdr, none.

    # Use Bonferroni correction instead of the default FDR
    options(y2clerk.mcc_correction = "bonferroni")

### `y2clerk.quantile_algorithm`

**Default:** `"hf8"`

The quantile algorithm (`qrule`) passed to
[`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
when
[`freqs()`](https://y2analytics.github.io/y2clerk/reference/freqs.md) is
called with `stat = "quantile"` and a weight variable. See
[`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
for the full list of supported rules (e.g. `"hf1"` through `"hf9"`,
`"math"`, `"school"`).

    # Use the "school" quantile rule
    options(y2clerk.quantile_algorithm = "school")
