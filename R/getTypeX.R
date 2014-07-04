getTypeX <-
function (X) 
{
    typeX = rep(0, ncol(X))
    for (i in 1:ncol(X)) {
        if (is.numeric(X[, i])) {
            typeX[i] = 1
        }
        else {
            typeX[i] = 2
        }
    }
    return(typeX)
}
