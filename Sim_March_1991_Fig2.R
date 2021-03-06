# March 1991
# This is a recreation of the March 1991 model written in R.

# @author: Maciej Workiewicz (ESSEC)
# date: August 10, 2017

# This is a recreation of the computational model from March JG. 1991. Exploration
# and Exploitation in Organizational Learning, Organization Science. 2(1):71-87
# The model has been recreated without access to the original code. The purpose
# of the recreation was to assess the ease of reproducing the original results.

# Technical notes:
# Interestingly, the original code cannot be followed exactly as described in
# the paper. The text says that the simulation runs until it reaches a steady state,
# i.e., "at which all individuals and the code share the same (not necesarilly
# accurate) beliefe with respect to each dimension" p. 75
# However, because individuals don't learn when code is 0 and the code only learns 
# from superior individuals (page 74), it is possible that such equilibrium
# will never be reached. Consider reality R[-1, 1, -1, 1], C[0, 1, 1, 1], and
# all Individuals are I[1, 1, 1, 1]. No further learning is possible and there
# will be no convergence in beliefs between the individuals and the code

# The code reproduces Figure 1. but to a different scaling (does not reach the
# 0.95 value). This is a well known problem with that model - it is relatively
# easy to reproduce the code, but difficult to reproduce the figures. Doing
# requires making additional assumptions that cannot be found in the paper itself.

# FUNCTIONS ----------

getvote <- function(v) {
  # The purpose of this function is to produce a vote (majority belief of the
  # individuals with superior knowledge compared with the code) and the
  # difference between the two votes (k). 
  # Here I assume that the individuals with beliefs = 0 do not vote (abstain),
  # which is a natural proposition when viewed from the political science
  # perspective (if one doesn't have an opinion about a certain dimension, 
  # one abstains from the vote).
  # Specifically, the function takes a vector (v) of arbitrary length and
  # produces the majority vote and advantage over the second choice. It only
  # works with two non-neutral (not 0) beliefs. It can be generalized to more
  # values, but further assumption about the rate of code learning (effect of n
  # and k) need to be made.
  uniqv <- sort(unique(v[v!=0]))  # only sort the non 0 elements
  freq <- tabulate(match(v, uniqv))  # find number of occurences
  indx_max <- which(freq == max(freq))
  k <- max(freq) - min(freq)
  if(length(uniqv) == 1) {
    # Here we need to eliminate an situation where with only one vote
    # the max == min and thus k would be 0 (no code learning)
    k <- max(freq)
  }
  output_ = c(uniqv[indx_max], k)
  return(output_)
}

# MODEL VARIABLES  ------------------------
iterations = 50  # number of iteration, originally set to 80 in the paper
time = 50  # iterations until equilibrium, a lazy solution to the steady state problem
m <- 30  # number of dimensions
n <- 50  # number of people

#--- HETEROGENEOUS LEARNING RATES
prob <- seq(from = 0.1,to = 0.9,by = 0.1) #--- Probability of learning rate 0.1

params_hetero <- lapply(prob, function(p){  
q <- 1 - p #--- Probability of learning rate 0.9
p1 <- matrix(sample(x = c(0.1,0.9),size = n, prob = c(p,q), replace = TRUE)) #--- Individual learning rates

})

mean_lr <- sapply(params_hetero, function(i){mean(i)})
names(params_hetero) <-  paste0("hetero_" ,mean_lr ) #--- 
#The names indicate the Average Socialization Rate (x axis in figure 2)

#---- HOMOGENEOUS LEARNING RATES
params_homo <- lapply(mean_lr, function(p){matrix (p, nrow=n)})
names(params_homo) <-  paste0("homo_" , mean_lr)
#--- The learning rates for the homo group coincide with the mean of the hetero group


# p2 <- 0.5  # speed of code learning
# end vestigal code ###

p3 <- 0  # turnover
p4 <- 0  # environmental turbulence


# lists forming the parmeter space for the learning rates
P1_list <- c(params_hetero, params_homo)
P2_list <- c(0.5)

# PREPARING THE OUTPUT MATRIX ------
OUTPUT = matrix(0, nrow=length(P1_list), ncol=length(P2_list))
c_p1 <- 0  # counter for recording rows in the OUTPUT file
scenario <- 0  # counter to report progress

# SIMULATION  -----------------
for(p1 in P1_list) {
  c_p1 <- c_p1 + 1
  c_p2 <- 0  # counter for recording columns in the OUTPUT file
  for(p2 in P2_list) {
    c_p2 <- c_p2 + 1
    scenario <- scenario + 1
    for(i in 1:iterations) {
      cat("\r","Scenario: ", toString(scenario), " out of ", toString(length(P1_list)*length(P2_list)), ", iteration: ", toString(i))
      
      # Generating starting objects
      external_reality <- 2*(floor(runif(m, min=0, max=2))) - 1
      beliefs <- matrix(floor(runif(n*m, min=-1, max=2)), nrow=n, ncol=m)
      org_code <- numeric(m)

      for(t in 1:time) {
        # turnover =====
        for(x3 in 1:n) {
          if(runif(1) < p3) {
            beliefs[, x3] <- matrix(floor(runif(m, min=-1, max=2)), nrow=m)
          }
        }

        # environmental turbulence  =====
        for(x4 in 1:m) {
          if(runif(1) < p4) {
            external_reality[x4] <- external_reality[x4]*(-1)
          }
        }

        # socialization ======
        for(n_ in 1:n) {
          for(m_ in 1:m) {
            if(runif(1) < p1[[n_]]) {
              # individuals don't learn if code is 0 [page 74, line 27:28]
              if(org_code[m_] != 0)  {
                beliefs[n_, m_] <- org_code[m_]
              }
            }
          }
        }
        # end: socialization

        # code learning ======
        # * choosing the chosen ones (superior knowledge) ######
        # find out the quality of knowledge for the code and individuals

        knowl_code <- sum(org_code == external_reality)
        knowl_wkrs <- numeric(n)  # vector with num of correct dimensions

        for(person in 1:n) {
          knowl_wkrs[person] <- sum(beliefs[person,] == external_reality)
        }

        chosen_ones <- numeric(n)  # individuals smarter than the org code
        # in the beginning pretty much all workers are better than the code
        # which has zero knowledge

        for(person in 1:n) {
          if(knowl_code < knowl_wkrs[person]) {
            chosen_ones[person] <- 1
          }
        }

        # *superior matrix  ######
        if(sum(chosen_ones) > 0) {
          knowl_matrix <- matrix(0, nrow=sum(chosen_ones), ncol=m)
          p <- 1  # counter for the superior people
          for(person in 1:n) {
            if(chosen_ones[person] == 1) {
              knowl_matrix[p,] <- beliefs[person,]
              p <- p + 1
            }
          }

          # *learning #####
          # Setting up the superior group and its knowledge for the code to learn from it
          # As per the paper, org code learns from the majority vote of superior
          # group.
          for(dimension in 1:m) {
            result <- getvote(knowl_matrix[,dimension])
            vote <- result[1]
            k <- result[2]
            if(runif(1) > ((1- p2)^k)) {
              org_code[dimension] <- vote
            }
          }
        }
        # end: code learning from the individuals
      }
      
      # recording results  ======
      # For now a crude but working version.
      knowl_code <- sum(org_code == external_reality)
      OUTPUT[c_p1, c_p2] <- OUTPUT[c_p1, c_p2] + (knowl_code/m)
    }
  }
}
OUTPUT <- OUTPUT/iterations  # taking the average

# SAVING RESULTS TO CSV ----
rownames(OUTPUT) <- names(P1_list)
colnames(OUTPUT) <- P2_list

# setwd("/Users/workiewicz/My Documents/R/")
write.csv(OUTPUT, file = "March_1991_fig2")

# END OF LINE

