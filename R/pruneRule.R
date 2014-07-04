pruneRule <-
function (rules, X, target) 
{
    newRuleMetric <- NULL
    for (i in 1:nrow(rules)) {
        newRuleMetric <- rbind(newRuleMetric, pruneSingleRule(rules[i, 
            ], X, target))
    }
    return(newRuleMetric)
}
