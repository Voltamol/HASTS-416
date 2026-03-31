#a
library(markovchain)
library(diagram)
library(expm)

# Force the plot window to be a single panel so A2 fills the screen
par(mfrow=c(1,1))

# --- QUESTION A2 ---
P2 <- matrix(c(0, 1, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0.4, 0.2, 0.2, 0.2,
               0, 0, 0, 0, 0.2, 0.4, 0.4,
               0.3, 0, 0, 0.1, 0.3, 0.1, 0.2,
               0, 0, 0, 0.2, 0.2, 0.3, 0.3,
               0, 0, 0, 0.5, 0.2, 0.2, 0.1), 
             nrow = 7, byrow = TRUE)

mc2 <- new("markovchain", transitionMatrix = P2, states = paste0("S", 1:7))

# (a) Plotting A2
# Note: box.size and cex.txt help prevent the overlapping labels
plot(mc2, package = "diagram", main = "A2: 7-State Chain", 
     box.size = 0.04, cex.txt = 0.8)

# (b) Identification
cat("\n--- A2 Analysis ---\n")
print(recurrentClasses(mc2))
print(transientStates(mc2))
cat("Periods for A2 states:", period(mc2), "\n")

# --- QUESTION A3 ---
# Using the corrected matrix (0.5 -> 0.4)
cat("\n--- A3 Traffic Results ---\n")
Pa <- matrix(c(0.4, 0.4, 0.2, 
               0.3, 0.4, 0.3, 
               0, 0.1, 0.9), nrow = 3, byrow = TRUE)

Pb <- matrix(c(0.1, 0.5, 0.4, 
               0.1, 0.3, 0.6, 
               0, 0.1, 0.9), nrow = 3, byrow = TRUE)

# Calculate distribution at 6PM
initial_dist <- c(1, 0, 0) # Start at Light
dist_6pm <- (initial_dist %*% (Pa %^% 9)) %*% (Pb %^% 6)

# Rounding for readability
print(round(dist_6pm, 4))
#----------------------------- END OF (a) -----------------------------
#b
P2 <- matrix(c(0, 1, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0.4, 0.2, 0.2, 0.2,
               0, 0, 0, 0, 0.2, 0.4, 0.4,
               0.3, 0, 0, 0.1, 0.3, 0.1, 0.2,
               0, 0, 0, 0.2, 0.2, 0.3, 0.3,
               0, 0, 0, 0.5, 0.2, 0.2, 0.1), 
             nrow = 7, byrow = TRUE)

mc2 <- new("markovchain", transitionMatrix = P2)
# Clean version of the A2 plot
plot(mc2, 
     package = "diagram", 
     main = "A2: 7-State Markov Chain Structure",
     box.size = 0.04,  # Shrinks the circles
     cex.txt = 0.7,    # Shrinks the probability numbers
     arr.width = 0.1,  # Thinner arrows
     edge.curv = 0.3)  # Curves the lines so they don't overlap as much
#----------------------------- END OF (b) -----------------------------
#c
set.seed(42) 

# Get states directly from the object
st_names <- states(mc2)

# Simulate 50 steps (this results in a vector of length 50)
traj1 <- rmarkovchain(n = 50, object = mc2, t0 = sample(st_names, 1))
traj2 <- rmarkovchain(n = 50, object = mc2, t0 = sample(st_names, 1))

# Convert "S1", "S2" etc. to numeric 1, 2 for plotting
y1 <- as.numeric(gsub("S", "", traj1))
y2 <- as.numeric(gsub("S", "", traj2))
x_axis <- 1:50 # Match the length of the trajectories

# Plotting
plot(x_axis, y1, type = "b", col = "blue", pch = 16,
     ylim = c(1, 7), ylab = "State Number", xlab = "Time Step (n)", 
     main = "A2(c): Markov Chain Trajectories")
lines(x_axis, y2, type = "b", col = "red", pch = 16)
legend("topright", legend = c("Trajectory 1", "Trajectory 2"), 
       col = c("blue", "red"), lty = 1, pch = 16)
#----------------------------- END OF (c) -----------------------------
#d
P_lim <- P2 %^% 100
# Print the first row to see where an initial state eventually ends up
print(round(P_lim[1,], 4))
#----------------------------- END OF (d) -----------------------------