getRuleMetric <-
function (ruleExec, X, target) 
{
    ruleMetric <- t(sapply(ruleExec[, "condition", drop = FALSE], 
        measureRule, X, target))
    rownames(ruleMetric) = NULL
    colnames(ruleMetric) <- c("len", "freq", "err", "condition", 
        "pred")
    dIx <- which(ruleMetric[, "len"] == "-1")
    if (length(dIx) > 0) {
        ruleMetric <- ruleMetric[-dIx, ]
        print(paste(length(dIx), " paths are ignored.", sep = ""))
    }
    return(ruleMetric)
}
