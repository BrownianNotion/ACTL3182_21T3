library(ggplot2)
library(reshape2)
library(gganimate)
library(gifski)

set.seed(1)
#Parameters
Term = 1; N = 500
mu = 0.05; sigma = 0.05; alpha = 0.05
dt = Term/N
dW = rnorm(N, mean = 0, sd = sqrt(dt))
W = c(0, cumsum(dW))
Time = seq(from = 0, to = 1, by = dt)

#Merton Model
r0 = 0.01
r_Merton = r0 + alpha*Time + sigma*W

#Vasicek Model
r_Vasicek = rep(0, 501)
r_Vasicek[1] = r0
for (i in 1:500) {
  r_Vasicek[i + 1] = r_Vasicek[i] + alpha*(mu - r_Vasicek[i])*dt + sigma*dW[i]
}

#Cox-Ingersoll Ross
r_CIR = rep(0, 501)
r_CIR[1] = r0
for (i in 1:500) {
  r_CIR[i + 1] = r_CIR[i] + alpha*(mu - r_CIR[i])*dt + sigma*sqrt(r_CIR[i])*dW[i]
}

df <- data.frame(Time, r_Merton, r_Vasicek, r_CIR)
colnames(df) <- c("Time", "Merton", "Vasicek", "CIR")
df_melted <- melt(df, id.vars = c("Time"))
colnames(df_melted) <- c("Time", "Model", "r_t")
head(df_melted)

p <- ggplot(df_melted) +
  geom_line(aes(x = Time, y = r_t, colour = Model)) + 
  labs(
    x = "Time",
    y = "Short rate r(t)",
    title = "Short rate simulations"
  )
  
p + transition_reveal(Time)
