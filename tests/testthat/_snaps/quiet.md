# informational messages are emitted by default

    Code
      invisible(freqs(df, x, stat = "mean", percentile = 50))
    Message
      i `percentile` only affects output when `stat = 'quantile'`.
      i Current `stat` is "mean", so `percentile` (50) is ignored.

# y2clerk.quiet = TRUE suppresses informational messages

    Code
      invisible(freqs(df, x, stat = "mean", percentile = 50))

