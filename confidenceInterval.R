

# cbind(CIlower = mean(Y) - 1.96 * std / sqrt(n), CIupper = mean(Y) + 1.96 * std / sqrt(n))
test <- cbind(CIlower = covariateSummary$CovariateMean - 1.96 * covariateSummary$CovariateStDev / sqrt(covariateSummary$CovariateCount),
      CIupper = covariateSummary$CovariateMean + 1.96 * covariateSummary$CovariateStDev / sqrt(covariateSummary$CovariateCount))
