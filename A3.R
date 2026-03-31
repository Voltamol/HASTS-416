#a
# P_A (1PM - 4PM)
Pa <- matrix(c(0.4, 0.4, 0.2,
               0.3, 0.4, 0.3, # Correction: 0.5 -> 0.4 (adj jammed to 0.3)
               0, 0.1, 0.9), nrow = 3, byrow = TRUE)

# P_B (4PM - 6PM)
Pb <- matrix(c(0.1, 0.5, 0.4,
               0.1, 0.3, 0.6,
               0, 0.1, 0.9), nrow = 3, byrow = TRUE)

initial_state <- c(1, 0, 0) # Start at "Light"

# Distribution at 4PM
dist_4pm <- initial_state %*% (Pa %^% 9)

# Distribution at 6PM
dist_6pm <- dist_4pm %*% (Pb %^% 6)
#----------------------------- END OF (a) -----------------------------
#b
# Final Simulation Verification
sim_traffic <- replicate(10000, {
  # 9 steps using Pa
  state_at_4pm <- rmarkovchain(n = 9, object = mcA, t0 = "L")[9]
  # 6 steps using Pb starting from where we left off
  state_at_6pm <- rmarkovchain(n = 6, object = mcB, t0 = state_at_4pm)[6]
  return(state_at_6pm)
})

# Should match your dist_6pm values
prop.table(table(sim_traffic))
#----------------------------- END OF (b) -----------------------------