fix_recoveries_low <- function(x) {
    for (i in seq_along(x)[-1]) {
        if(x[i] < x[i - 1]) x[i] <- x[i - 1]
    }
    x
}

fix_recoveries_high <- function(x) {
    y <- rev(x)
    for (i in seq_along(y)[-length(y)]) {
        if(y[i] < y[i + 1]) y[i + 1] <- y[i]
    }
    rev(y)
}
