measureRule <-
function (ruleExec, X, target, pred = NULL) 
{
    len <- length(unlist(strsplit(ruleExec, split = " & ")))
    origRule <- ruleExec
    ruleExec <- paste("which(", ruleExec, ")")
    ixMatch <- eval(parse(text = ruleExec))
    if (length(ixMatch) == 0) {
        v <- c("-1", "-1", "-1", "", "")
        names(v) <- c("len", "freq", "err", "condition", "pred")
        return(v)
    }
    ys <- target[ixMatch]
    freq <- round(length(ys)/nrow(X), digits = 3)
    if (is.numeric(target)) {
        ysMost <- mean(ys)
        err <- sum((ysMost - ys)^2)/length(ys)
    }
    else {
        if (length(pred) > 0) {
            ysMost = as.character(pred)
        }
        else {
            ysMost <- names(which.max(table(ys)))
        }
        ly <- sum(as.character(ys) == ysMost)
        conf <- round(ly/length(ys), digits = 3)
        err <- 1 - conf
    }
    rule <- origRule
    v <- c(len, freq, err, rule, ysMost)
    names(v) <- c("len", "freq", "err", "condition", "pred")
    return(v)
}
