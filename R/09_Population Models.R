# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Sara's Last class - Population Models
# Logistic Growth and Lotka-Volterra
# First version 2022-07-22
# jfb
# --------------------------------------------------#

##### Libs #####
library(deSolve)
library(ggplot2)
library(tidyr)
##### Population Models #####

# Creating a function for logistic growth
logGrowth <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1 - a * N)
    return(list(dN.dt))
  })
}

# named vector with parameters
p <- c(r = 1, a = 0.001)
# initial condition
y0 <- c(N = 10)
# time steps
t <- 1:20

# give the function and the parameters to the ode function
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)

head(out_log)

df_log <- as.data.frame(out_log)
ggplot(df_log) +
  geom_line(aes(x = time, y = N)) +
  theme_classic()

# Lotka-Volterra Competition Model

LVComp <- function(t, y, p) {
  N <- y
  with(as.list(p), {
    dN1.dt <- r[1] * N[1] * (1 - a[1, 1] * N[1] - a[1, 2] * N[2])
    dN2.dt <- r[2] * N[2] * (1 - a[2, 1] * N[1] - a[2, 2] * N[2])
    return(list(c(dN1.dt, dN2.dt)))
  })
}

# LV parameters
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2) #alpha matrix with competition coefficients a11, a12, a21 and a22
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)

# run ode with LotkaVolterra function
out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv) # Notice that now we have two columns because we have two state variables


# We can again plot the solution. But first, we have to convert out data in a format in which every variable is represented in a column and every observation is represented in a row. We will use the function pivot_longer() from the package tidyr



df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3)

head(df_lv) ; tail(df_lv)

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()


