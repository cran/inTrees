pruneSingleRule <-
function (rule, X, target, maxDecay = 0.05) 
{
    newRuleMetric <- measureRule(rule["condition"], X, target)
    errOrig <- as.numeric(newRuleMetric["err"])
    ruleV <- unlist(strsplit(rule["condition"], split = " & "))
    pred <- rule["pred"]
    if (length(ruleV) == 1) 
        return(newRuleMetric)
    for (i in length(ruleV):1) {
        restRule <- ruleV[-i]
        restRule <- paste(restRule, collapse = " & ")
        metricTmp <- measureRule(restRule, X, target, pred)
        errNew <- as.numeric(metricTmp["err"])
        if ((errNew - errOrig)/max(errOrig, 1e-06) <= maxDecay) {
            ruleV <- ruleV[-i]
            newRuleMetric <- metricTmp
            if (length(ruleV) <= 1) 
                break
        }
    }
    return(newRuleMetric)
}
