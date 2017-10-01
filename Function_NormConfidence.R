normal_confidence <- function(xbar, ssd, n, ci){
  alpha <- ci/100
  alpha2 <- 1-((1-alpha)/2)
  
  moe <- qnorm(alpha2)*ssd/sqrt(n)
  
  left_bound <- xbar - moe
  right_bound <- xbar + moe
  
  your_ci_is <- c(left_bound, right_bound)
  print(your_ci_is)
}