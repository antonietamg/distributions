library(Matrix)
library(ggplot2)
library(dplyr)

set.seed(123456)
rn <- rnorm(250000)

M1 <- matrix(rn, 500)
A <- forceSymmetric(M1)
A = as.matrix(A)
diag(A) <- 1

ev <- eigen(A)
ev = (values <- ev$values)
ev = sort(ev)
evdiff = diff(ev)

# Estimate the mode using kernel density estimation
density_estimation <- density(evdiff)
mode_value <- density_estimation$x[which.max(density_estimation$y)]

# Subtract the mode to align the peak to 0
evdiff_centered <- evdiff - mode_value

EVdiff <- data.frame(ed = evdiff_centered)

# Plot the density and ECDF
ggplot(EVdiff, aes(x=ed)) +
  theme_bw() +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

ggplot(EVdiff, aes(x=ed)) +
  theme_bw() +
  stat_ecdf()



#######################################


library(Matrix)
library(ggplot2)
library(dplyr)

set.seed(123)
rn <- rnorm(250000)

M1 <- matrix(rn, 500)
A <- forceSymmetric(M1)
A = as.matrix(A)
diag(A) <- 1

ev <- eigen(A)
ev = (values <- ev$values)
ev = sort(ev)
evdiff = diff(ev)

# Estimate the mode using kernel density estimation
density_estimation <- density(evdiff)
mode_value <- density_estimation$x[which.max(density_estimation$y)]

# Subtract the mode to align the peak to 0
evdiff_centered <- evdiff - mode_value

# Move the distribution to the right such that its rightmost point is at 5
max_shift <- 2 - max(evdiff_centered)
evdiff_shifted <- evdiff_centered + max_shift

EVdiff <- data.frame(ed = evdiff_shifted)

# Plot the density and ECDF
ggplot(EVdiff, aes(x=ed)) +
  theme_bw() +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

ggplot(EVdiff, aes(x=ed)) +
  theme_bw() +
  stat_ecdf()
