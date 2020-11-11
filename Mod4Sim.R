library(ggplot2)
library(gganimate)
library(gifski)
#Brownian Motion Simulation
Term = 1; N = 500
mu = 0.05; sigma = 0.05
dt = Term/N
dW = rnorm(N, mean = 0, sd = sqrt(dt))
W = c(0, cumsum(dW))
Time = seq(from = 0, to = 1, by = dt)
S = exp((mu - 0.5*sigma^2)*Time + sigma*W)

#Plotting
df <- data.frame(Time, W, S)
p <- ggplot(df) +
  geom_line(aes(x = Time, y = S), colour = "black") +
  ggtitle("Geometric Brownian Motion")
  #geom_line(aes(x = Time, y = W), colour = "red")

#Animate plot.
p + transition_reveal(Time)

