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


# Trying Rosenzweig-MacArthur for fun
RwMcPred <- function(t, y, p) {
  N <- y[1] ; P <- y[2]
  with(as.list(p), {
    dN.dt <- (r1 * N * (1-N/K)) - ((c*N*P)/(1+c*tau*N))
    dP.dt <- ((b * N * P) / (1+c*tau*N))- d * P
    return(list(c(dN.dt, dP.dt)))
  })
}

# RwMc parameters
c <- 2 #PconsumptionRate
b <- 1 #c convertion coef
tau <- 0.2 #manipulationTime
d <- 0.2 #P deathRate
a1 <- list(c(c,b,tau,d))
# here we have to add the predation coefficients, c, b, tau and d which are consumption, converted consumption and manipulationTime and PdeathRate respectively
r1 <- 10 # growthRate N
p3 <- list(r1, a1)
N1 <- c(5000, 100)
t2 <- c(1:1000)
K <- 15000

out_RwMc <- ode(y = N1, times = t2, func = RwMcPred, parms = p3)

#head(out_RwMc)

df_RwMc <- pivot_longer(as.data.frame(out_RwMc), cols = 2:3)

#head(df_RwMc)

#tb_RwMc <- tibble(df_RwMc)
#tb_RwMc$name == 1
#prey <- list(tb_RwMc$name == 1)
#predator <- list(tb_RwMc$name == 2)
#replace(tb_RwMc$name,list = prey,values = 'Prey')
# trying to change the name of 1 to prey and 2 to predator

ggplot(tb_RwMc) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()

# now is just tweaking parameters so the predators don't all die
