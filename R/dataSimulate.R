dataSimulate <-
function (flag = 1, nCol = 20, nRow = 100) 
{
    if (nCol <= 2) 
        stop("nCol must be >= 2.")
    X <- matrix(runif(nRow * nCol, min = -2, max = 2), ncol = nCol)
    target <- rep(-1, nRow)
    if (flag == 4) {
        ix1 <- which(X[, 1] >= -1 & X[, 1] <= 1)
        ix2 <- which(X[, 2] >= -1 & X[, 2] <= 1)
        ix <- intersect(ix1, ix2)
        target[ix] <- 1
    }
    if (flag == 3) {
        target <- (X[, 1]) + (X[, 2])
        ix <- which(target > quantile(target, 1/2))
        target <- target * 0 - 1
        target[ix] <- 1
    }
    if (flag == 2) {
        target <- (X[, 1])^2 + 1 * (X[, 2])^2
        ix <- which(target > quantile(target, 6/10))
        ix <- c(ix, which(target < quantile(target, 1/10)))
        target <- target * 0 - 1
        target[ix] <- 1
    }
    if (flag == 1) {
        X <- matrix(0, nRow, nCol)
        for (ii in 1:nrow(X)) {
            ix <- sample(1:nCol, nCol/2, replace = FALSE)
            X[ii, ix] <- 1
        }
        target <- (xor(X[, 1], X[, 2]))
        X <- data.frame(X)
        for (jj in 1:ncol(X)) {
            X[, jj] <- as.factor(X[, jj])
        }
    }
    return(list(X = X, target = target))
}
