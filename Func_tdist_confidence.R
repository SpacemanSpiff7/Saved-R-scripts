tdist_confidence <- function(xbar, ssd, n, ci){
  alpha <- ci/100
  alpha2 <- 1-((1-alpha)/2)
  dof <- n-1
  
  moe <- qt(alpha2, dof)*ssd/sqrt(n)
  
  your_ci_is <- c(xbar-moe, xbar+moe)
  print(your_ci_is)
}