
##### FIGURE 6 #####

var <- seq(from = 0,to = 2,by = 0.05) # Variance parameters (as specified on the paper)
x <- 0  # Initial value of x (to start the search)
n_others <- c(2,10,100) # Number of other companies
reps <- 5000 # Number of "simulations" (sample size to calculate p*)


results_fig_6 <- 
  lapply (n_others, function(N){
  
x_values <- 
  sapply (var, function(v){
  i <- 1 # Counter

  repeat{
  
  print(paste("Attempt with x =", x))
  
  iterations <- sapply(1:reps, function(i){
    f_org <- rnorm(n = 1,mean = x,sd = v) # Performance of the focus organization
    o_org <- rnorm(n = N,mean = 0,sd = 1) # Vector with the performance of the other organizations
    
    rank_1 <- sum(f_org > o_org) == N # Is the performance of the focus org better than that of all the others?
    return(rank_1)
  })
  
  p <- sum(iterations)/reps # Probability of the focal org outperforming all the others
  yardstick <- 1/(1 + N) # Theoretical probability of focal org outperforming all if the distributions were the same
  
  p_r <- round(p,digits = 2) # Rounded p
  y_r <- round(yardstick,digits = 2) # Rounded y
  
  if (p_r == y_r) { 
    print (paste("Success!", x)) 
    return(x)
    x <- 0 # Set it up to 0 again
    i <- i + 1 # Counter
    break}

  else if (p_r > y_r) {x <- x - 0.025} 
  else if (p_r < y_r) {x <- x + 0.025}
 }

})

df <- tibble(x_value = x_values, v_value = var, N = N)

})


fig_6 <- bind_rows(results_fig_6)

vroom::vroom_write(fig_6, "March_1991_fig6.csv")


##### SENSITIVITY ANALYSIS ##### 

# With the same probability distribution, p = 1/(N + 1). Because there is some
#  randomness in p, this is a test to see the level of sensitivity (true positive rate)
#  with a certain number of reps and rounding to x digits

# 
# true_positives <- 
#   pbapply::pbsapply(1:100, function(a){
#   
#   iterations <- sapply(1:reps, function(i){
#     f_org <- rnorm(n = 1,mean = 0,sd = 1) # Performance of the focus organization
#     o_org <- rnorm(n = N,mean = 0,sd = 1) # Vector with the performance of the other organizations
#     
#     rank_1 <- sum(f_org > o_org) == N # Is the performance of the focus org better than that of all the others?
#     return(rank_1)
#   })
#   
#   p <- sum(iterations)/reps
#   yardstick <- 1/(1 + N)
#   
#   result <- round(p,digits = 2) == round(yardstick,digits = 2)
#   
#   return(result)
# })
#    
# true_positives_rate <- sum(true_positives)/100    # Percentage of true positives     


