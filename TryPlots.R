# packages
load("./disl_temp_salt.RData")

# Demo data
time <- disl_temp_salt$Time[1:50]
temp <- disl_temp_salt$Temperature[1:50]
salt <- disl_temp_salt$Salinity[1:50]

t_min <- min(temp[1:50])
t_max <- max(temp[1:50])
t_mean <- mean(temp[1:50])

plot(time,temp,pch='.',main="title",sub="subtitle",xlab="",ylab="Temperature")
abline(h = c(t_min,t_mean,t_max), col = c("#D1D0DE","#636D97","#D1D0DE"))
