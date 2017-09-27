#' Data transformations 
#'
#' @param x A matrix, data.frame, or data.table with the data of
#' interest.
#' @param type A character list of the transformations desired. See
#' 'Details'
#' @param t98 A boolean variable to define if the data are truncated to
#' the inner 98\% before continuing.
#' @param ... Arguments passed to the rank function.
#' @return A list of the transformed data as data.tables.
#' @details If there are negatives present, using any of log transforms
#' will generate an error. Types of transformations include 'Zscore' -- 
#' transforms to z-scores, 'rank' -- passes to ranks, '1e3' -- scales 
#' the data between \eqn{[0,1e3]}, 'log10' -- checks for negatives
#' and then removes 0's and takes the \eqn{\log_10} of the data, 'log' --
#' same as 'log10' but for base \eqn{e}.  'slog10' -- checks for negatives
#' and then removes 0's and takes \eqn{\log_10}, then zscores. 'slog' --
#' same as 'slog10' but for base \eqn{e}. And 'all' performs all of the
#' above.
#'
#' @importFrom data.table data.table
#' 
#' @examples
#' set.seed(13)
#' x <- exp(3 + rnorm(100))
#' y <- exp(5 + rnorm(100))
#' z <- abs(x*y + rnorm(100, mean = 3, sd = 2))
#' X <- data.frame(x, y, z)
#'
#' tx <- transformData(X, type = c('1e3'),
#' ties.method = 'average')
#'
#' @export
transformData <- function(x, type = c("1e3"), t98 = FALSE, ...) {
    x <- as.data.table(x)
    
    if (any(type == "all")) {
        message("type = 'all' is not recommended for large datasets, continue? [y/n]:")
        ans <- readLines(n = 1)
        if (!(ans %in% c("y", "Y", "yes", "Yes", "YES"))) 
            stop("Exiting: select different type and retry.")
        
        type <- c("zscore", "rank", "1e3", "log10", "log", "slog10", "slog")
    } else {
        type
    }
    
    if (t98) {
        qs <- apply(x, 2, quantile, probs = c(0.01, 0.99))
        
        x <- x[, lapply(.SD, function(x) {
            qs <- quantile(x, probs = c(0.01, 0.99))
            x[x < qs[1]] <- NA
            x[x > qs[2]] <- NA
            return(x)
        })]
        x <- x[complete.cases(x), ]
    }
    
    out <- list()
    
    if ("zscore" %in% type) {
        dZscore <- x[, lapply(.SD, scale, center = TRUE, scale = TRUE)]
        out[["zscore"]] <- dZscore
    }
    
    if ("1e3" %in% type) {
        d01e3 <- x[, lapply(.SD, function(a) {
            (a - min(a)) * 1000/(max(a) - min(a))
        })]
        out[["d01e3"]] <- d01e3
    }
    
    if ("rank" %in% type) {
        dRank <- x[, lapply(.SD, rank, ...)]
        out[["Rank"]] <- dRank
    }
    
    negs <- any(x < 0)
    
    if ("log10" %in% type) {
        if (negs) 
            stop("There are negatives!")
        zs <- apply(x, 1, function(row) {
            any(row == 0)
        })  ## logical of which rows have 0's present.
        
        ## Select only rows greater than zero
        dLog10 <- x[which(!zs == TRUE), lapply(.SD, log10)]
        out[["log10"]] <- dLog10
    }
    
    if ("log" %in% type) {
        if (negs) 
            stop("There are negatives!")
        zs <- apply(x, 1, function(row) {
            any(row == 0)
        })  ## logical of which rows have 0's present.
        
        ## Select only rows greater than zero
        dLog <- x[which(!zs == TRUE), lapply(.SD, log)]
        out[["log"]] <- dLog
    }
    
    if ("slog10" %in% type) {
        if (negs) 
            stop("There are negatives!")
        zs <- apply(x, 1, function(row) {
            any(row == 0)
        })  ## logical of which rows have 0's present.
        
        ## Select only rows greater than zero
        dlog10 <- x[which(!zs == TRUE), lapply(.SD, log10)]
        dSlog10 <- data.table(scale(dlog10, center = TRUE, scale = TRUE))
        
        out[["slog10"]] <- dSlog10
    }
    
    if ("slog" %in% type) {
        if (negs) 
            stop("There are negatives!")
        zs <- apply(x, 1, function(row) {
            any(row == 0)
        })  ## logical of which rows have 0's present.
        
        ## Select only rows greater than zero
        dlog <- x[which(!zs == TRUE), lapply(.SD, log)]
        
        dSlog <- data.table(scale(dlog, center = TRUE, scale = TRUE))
        out[["slog"]] <- dSlog
    }

    out$raw <- x   
    return(out)
    
}

# Time: About 3 hours. Working status: It works!  Comments: Might need work to be
# robust against messy data.  Soli Deo Gloria
