library(ggplot2)
library(gganimate)
library(gifski)
library(reshape2)
set.seed(0)

#Brownian Motion Simulation
Term = 1; N = 500
mu = 0.05; sigma = 0.01
dt = Term/N
dW = rnorm(N, mean = 0, sd = sqrt(dt))
W = c(0, cumsum(dW))
t = seq(from = 0, to = Term, by = dt)
S = exp((mu - 0.5*sigma^2)*t + sigma*W)


#Plotting
S_mean <- exp(mu*t)
df <- data.frame(t, S, S_mean)
colnames(df) <- c("t", "GBM", "Mean")
df_melted <- melt(df, id.vars="t", variable.name="Process")

p <- ggplot(df_melted) +
  geom_line(aes(x=t, y=value, group=Process, colour=Process), lwd=1) +
  ggtitle("Geometric Brownian Motion vs Mean return")

#Animate plot.
p + theme(legend.position="right") + transition_reveal(t)



