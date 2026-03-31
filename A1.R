#a
library(markovchain)
library(diagram)

P1 <- matrix(c(1.0, 0, 0, 0, 0,
               0.5, 0, 0, 0, 0.5,
               0.2, 0, 0, 0, 0.8,
               0, 0, 1.0, 0, 0,
               0, 0, 0, 1.0, 0), 
             nrow = 5, byrow = TRUE)

mc1 <- new("markovchain", transitionMatrix = P1, states = c("S1", "S2", "S3", "S4", "S5"))

# Plotting
plot(mc1, package = "diagram", main = "A1: 5-State Markov Chain")

# Classification
transient_states <- transientStates(mc1)
recurrent_classes <- recurrentClasses(mc1)
absorbing_states <- absorbingStates(mc1)
periods <- period(mc1)

print(list(Transient = transient_states, Recurrent = recurrent_classes, 
           Absorbing = absorbing_states, Periods = periods))

#-----------------------------END OF (a)-----------------------------
#b
set.seed(123)
for(i in 1:3) {
  traj <- rmarkovchain(n = 20, object = mc1, t0 = sample(mc1@states, 1))
  print(paste("Trajectory", i, ":", paste(traj, collapse = " -> ")))
}
#-----------------------------END OF (b)-----------------------------
#c
# A chain is aperiodic if its period is 1
chain_period <- period(mc1)
is_aperiodic <- (chain_period == 1)

print(paste("The period of the chain is:", chain_period))
print(paste("Is the chain aperiodic?:", is_aperiodic))
#-----------------------------END OF (c)-----------------------------
#d
library(expm)
# Plotting probability of being in each state over 20 steps
steps <- 20
probs <- matrix(NA, nrow = steps, ncol = 5)
initial_dist <- c(0, 1, 0, 0, 0) # Starting at S2

for(n in 1:steps) {
  probs[n,] <- initial_dist %*% (P1 %^% n)
}

matplot(probs, type = "l", lty = 1, col = 1:5, xlab = "Time n", ylab = "Probability")
legend("right", legend = mc1@states, col = 1:5, lty = 1)
#-----------------------------END OF (d)-----------------------------