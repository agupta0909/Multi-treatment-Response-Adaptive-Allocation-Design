#----Allocation RUle---

set.seed(8)

#---R_k_hat for proposed tule---
R <- function(theta_a_hat, theta_b_hat, theta_c_hat){
  del_a = 0
  del_b = 0
  del_c = 0
  
  A = rexp(500, theta_a_hat)
  B = rexp(500, theta_b_hat)
  C = rexp(500, theta_c_hat)
  
  for(i in 1:500){
    if(A[i] > max(B[i], C[i])){
      del_a = del_a + 1
    }
    else if(B[i] > max(A[i], C[i])){
      del_b = del_b + 1
    }
    else{
      del_c = del_c + 1
    }
  }
  R_a_hat = del_a/500
  R_b_hat = del_b/500
  R_c_hat = del_c/500
  return(c(R_a_hat, R_b_hat, R_c_hat))
}





prop_a = numeric(1000)
prop_b = numeric(1000)
prop_c = numeric(1000)
d1 = numeric(1000)#---Statistic to condlude from, d_1,n--
d2 = numeric(1000)#---Statistic to condlude from, d_2,n--
d = numeric(1000)
#f = numeric(1000)

for(b in 1: 1000){
  
  X = numeric(800)
  
  delta_a = numeric(800)#---Indicator variable for 200 allocations
  delta_b = numeric(800)
  delta_c = numeric(800)
  
  X[1:5] = rexp(5, 1/250)#--A
  X[6:10] = rexp(5, 1/80)#--B
  X[11:15] = rexp(5, 1/30)#--C
  
  delta_a[1:5] <- c(rep(1,5))
  delta_b[6:10] <- c(rep(1,5))
  delta_c[11:15] <- c(rep(1,5))
  
  theta_a = 1/mean(X[1:5])
  theta_b = 1/mean(X[6:10])
  theta_c = 1/mean(X[11:15])#---15 patients allocated
  
  del_a = 0
  del_b = 0
  del_c = 0
  
  A = rexp(1000, theta_a)
  B = rexp(1000, theta_b)
  C = rexp(1000, theta_c)
  
  for(i in 1:1000){
    if(A[i] > max(B[i], C[i])){
      del_a = del_a + 1
    }
    else if(B[i] > max(A[i], C[i])){
      del_b = del_b + 1
    }
    else{
      del_c = del_c + 1
    }
  }
  R_a_hat = del_a/1000
  R_b_hat = del_b/1000
  R_c_hat = del_c/1000
  
  
  for(i in 16:800){
    r = runif(1)
    if(r <= R_a_hat){
      X[i] = rexp(1, theta_a)
      delta_a[i] = 1
      theta_a = 1/(((1/theta_a)*(sum(delta_a)-1) + X[i])/sum(delta_a))
      R_estim = R(theta_a_hat = theta_a, theta_b_hat = theta_b, theta_c_hat = theta_c)
      R_a_hat = R_estim[1]
      R_b_hat = R_estim[2]
      R_c_hat = R_estim[3]
    }
    else if((r > R_a_hat )&&(r <= (R_a_hat + R_b_hat))){
      X[i] = rexp(1, theta_b)
      delta_b[i] = 1
      theta_b = 1/(((1/theta_b)*(sum(delta_b)-1) + X[i])/sum(delta_b))
      R_estim = R(theta_a_hat = theta_a, theta_b_hat = theta_b, theta_c_hat = theta_c)
      R_a_hat = R_estim[1]
      R_b_hat = R_estim[2]
      R_c_hat = R_estim[3]
    }
    else{
      X[i] = rexp(1, theta_c)
      delta_c[i] = 1
      theta_c = 1/(((1/theta_c)*(sum(delta_c)-1) + X[i])/sum(delta_c))
      R_estim = R(theta_a_hat = theta_a, theta_b_hat = theta_b, theta_c_hat = theta_c)
      R_a_hat = R_estim[1]
      R_b_hat = R_estim[2]
      R_c_hat = R_estim[3]
    }
  }
  prop_a[b] = mean(delta_a)
  prop_b[b] = mean(delta_b)
  prop_c[b] = mean(delta_c)
  theta_hat = (sum(delta_a)*(1/theta_a) + sum(delta_b)*(1/theta_b) + sum(delta_c)*(1/theta_c))/800
  d1[b] =  sqrt(800)*((1/theta_a - 1/theta_b)/(sqrt(6)*theta_hat))
  d2[b] =  sqrt(800)*((1/theta_a - 1/theta_c)/(sqrt(6)*theta_hat))
  d[b] = max(d1[b], d2[b])
  #if(d[b] > 12.30528){
    #f[b] = 1 
  #}else{
   # f[b] = 0
  #} 
}

quantile(d, p = 0.95)

sum(as.numeric(d>=11.75263))/1000

mean(prop_a)
mean(prop_b)
mean(prop_c)


hist(d)
shapiro.test(d)



mean(f)



#---Competitor-1---
prop_a = numeric(1000)
prop_b = numeric(1000)
prop_c = numeric(1000)
d1 = numeric(1000)#---Statistic to condlude from, d_1,n--
d2 = numeric(1000)#---Statistic to condlude from, d_2,n--
d = numeric(1000)
f = numeric(1000)
#ta = numeric(5000)
#tb = numeric(5000)
#tc = numeric(5000)

for(b in 1:1000){
  X = numeric(1200)
  X[1:5] = rexp(5, 1/250)
  X[6:10] = rexp(5, 1/80)
  X[11:15] = rexp(5, 1/30)
  
  #Estimating Parameters
  theta_a = 1/mean(X[1:5])
  theta_b = 1/mean(X[6:10])
  theta_c = 1/mean(X[11:15])#---15 patients allocated
  
  
  delta_a = numeric(1200)#---Indicator variable for 200 allocations
  delta_b = numeric(1200)
  delta_c = numeric(1200)
  
  
  delta_a[1:5] <- c(rep(1,5))#---Initializing the indicators
  delta_b[6:10] <- c(rep(1,5))
  delta_c[11:15] <- c(rep(1,5))
  
  
  sum_a = sum(X[1:5])#---Initiliazing sum variables
  sum_b = sum(X[6:10])
  sum_c = sum(X[11:15])
  
  R_a = (sum_a/sum(delta_a))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
  R_b = (sum_b/sum(delta_b))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
  R_c = (sum_c/sum(delta_c))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
  
  for(i in 16:1200){
    r = runif(1)
    if(r <= R_a){
      X[i] = rexp(1, theta_a)
      delta_a[i] = 1
      theta_a = 1/(((1/theta_a)*(sum(delta_a)-1) + X[i])/sum(delta_a))
      sum_a = sum_a + X[i]
      R_a = (sum_a/sum(delta_a))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
      R_b = (sum_b/sum(delta_b))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
      R_c = (sum_c/sum(delta_c))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
    }else if((r > R_a)&&(r <= (R_a + R_b))){
      X[i] = rexp(1, theta_b)
      delta_b[i] = 1
      theta_b = 1/(((1/theta_b)*(sum(delta_b)-1) + X[i])/sum(delta_b))
      sum_b = sum_b + X[i]
      R_a = (sum_a/sum(delta_a))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
      R_b = (sum_b/sum(delta_b))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
      R_c = (sum_c/sum(delta_c))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
    }else{
      X[i] = rexp(1, theta_c)
      delta_c[i] = 1
      theta_c = 1/(((1/theta_c)*(sum(delta_c)-1) + X[i])/sum(delta_c))
      sum_c = sum_c + X[i]
      R_a = (sum_a/sum(delta_a))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
      R_b = (sum_b/sum(delta_b))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
      R_c = (sum_c/sum(delta_c))/(sum_a/sum(delta_a) + sum_b/sum(delta_b) + sum_c/sum(delta_c))
    }
  }
  prop_a[b] = sum(delta_a)/1200
  prop_b[b] = sum(delta_b)/1200
  prop_c[b] = sum(delta_c)/1200
  theta_hat = (sum(delta_a)*(1/theta_a) + sum(delta_b)*(1/theta_b) + sum(delta_c)*(1/theta_c))/200
  d1[b] =  sqrt(1200)*((1/theta_a - 1/theta_b)/(sqrt(6)*theta_hat))
  d2[b] =  sqrt(1200)*((1/theta_a - 1/theta_c)/(sqrt(6)*theta_hat))
  d[b] = max(d1[b], d2[b])
  #if(d[b] > 7.522993){
   # f[b] = 1 
  #}else{
    #f[b] = 0
  #} 
}

sum(as.numeric(d>=2.488799))/1000

mean(prop_a)
sd(prop_a)

mean(prop_b)
mean(prop_c)


quantile(d, p = 0.95)

mean(f)



#--- Competitor 2 --- 

R_a_hat = 0.3333
R_b_hat = 0.3334
R_c_hat = 0.3333
prop_a = numeric(1000)
prop_b = numeric(1000)
prop_c = numeric(1000)
d1 = numeric(1000)#---Statistic to condlude from, d_1,n--
d2 = numeric(1000)#---Statistic to condlude from, d_2,n--
d = numeric(1000)
#f = numeric(5000)

for(b in 1:1000){
  X = numeric(200)
  X[1:5] = rexp(5, 1/250)
  X[6:10] = rexp(5, 1/80)
  X[11:15] = rexp(5, 1/30)
  
  theta_a = 1/mean(X[1:5])
  theta_b = 1/mean(X[6:10])
  theta_c = 1/mean(X[11:15])
  
  delta_a = numeric(200)#---Indicator variable for 200 allocations
  delta_b = numeric(200)
  delta_c = numeric(200)
  
  
  delta_a[1:5] <- c(rep(1,5))#---Initializing the indicators
  delta_b[6:10] <- c(rep(1,5))
  delta_c[11:15] <- c(rep(1,5))
  
  for(i in 16:200){
    r = runif(1)
    if(r <= R_a_hat){
      X[i] = rexp(1, theta_a)
      delta_a[i] = 1
      theta_a = 1/(((1/theta_a)*(sum(delta_a)-1) + X[i])/sum(delta_a))
    }else if((r > R_a_hat)&&(r <= (R_a_hat + R_b_hat))){
      X[i] = rexp(1, theta_b)
      delta_b[i] = 1
      theta_b = 1/(((1/theta_b)*(sum(delta_b)-1) + X[i])/sum(delta_b))
    }else{
      X[i] = rexp(1, theta_c)
      delta_c[i] = 1
      theta_c = 1/(((1/theta_c)*(sum(delta_c)-1) + X[i])/sum(delta_c))
    }
  }
  prop_a[b] = sum(delta_a)/200
  prop_b[b] = sum(delta_b)/200
  prop_c[b] = sum(delta_c)/200
  theta_hat = (sum(delta_a)*(1/theta_a) + sum(delta_b)*(1/theta_b) + sum(delta_c)*(1/theta_c))/200
  d1[b] =  sqrt(200)*((1/theta_a - 1/theta_b)/(sqrt(6)*theta_hat))
  d2[b] =  sqrt(200)*((1/theta_a - 1/theta_c)/(sqrt(6)*theta_hat))
  d[b] = max(d1[b], d2[b])
  #if(d[b] > 8.408722){
    #f[b] = 1 
  #}else{
   # f[b] = 0
  #} 
}

sum(as.numeric(d >= 8.652025))/1000

mean(prop_a)
sd(prop_a)
 mean(prop_b)
mean(prop_c)

quantile(d, p = 0.95)


mean(f)


#---Reading from Proposed allocation-- Competitor1--- Comperitor2--

mean_prop_A_B_C<-c(0.54273,0.67967,0.74521,0.78517,0.810775,0.83141)
power_A_B_C<-c(0.421,0.734,0.874,0.93,0.959,0.98)

mean_prop_A_BC<-c(0.58036,0.680405,0.76378,0.80832,0.83667,0.87885)
power_A_BC<-c(0.431,0.643,0.842,0.91,0.955,0.984)

mean_prop_A_C_B<-c(0.516565,0.660066,0.728325,0.772035,0.801395)
power_A_C_B<-c(0.311,0.612,0.789,0.877,0.908)

power_samp<-c(0.683,0.695,0.746,0.75)

#--------------------------------------------------------------------------------
  
comp1_mean_prop_A_B_C<-c(0.51465,0.65402,0.724505,0.764635,0.796255,0.812305)
comp1_power_A_B_C<-c(0.396,0.724,0.861,0.91,0.925,0.906)

comp1_mean_prop_A_BC<-c(0.55935,0.65499,0.73262,0.787465,0.81481,0.85858)
comp1_power_A_BC<-c(0.449,0.692,0.867,0.943,0.967,0.986)

comp1_mean_prop_A_C_B<-c(0.489465,0.63367,0.700595,0.74304,0.774765)
comp1_power_A_C_B<-c(0.284,0.61,0.762,0.824,0.855)

comp1_power_samp <- c(0.679, 0.682, 0.69, 0.671)

#--------------------------------------------------------------------------------
  
comp2_mean_prop_A_B_C<-c(0.33292,0.33209,0.33488,0.3334,0.3345,0.332105)
comp2_power_A_B_C<-c(0.451,0.767,0.891,0.95,0.976,0.986)

comp2_mean_prop_A_BC<-c(0.333365,0.332485,0.332565,0.33454,0.33252,0.33095)
comp2_power_A_BC<-c(0.483,0.735,0.915,0.955,0.979,0.996)

comp2_mean_prop_A_C_B<-c(0.331455,0.331655,0.33258,0.333385,0.333575)
comp2_power_A_C_B<-c(0.304,0.652,0.823,0.902,0.938)

comp2_power_samp <- c(0.669, 0.706, 0.730, 0.726)

#---Plots for mean proportion of allocations
par(mfrow = c(1,3))

#--Table-1
plot(mean_prop_A_B_C, type = "b" ,main = "TABLE 1", xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", col = "red", ylim = c(0.3, 0.9))
par(new = TRUE)
plot(comp1_mean_prop_A_B_C, xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", type = "b", col = "blue", ylim = c(0.3, 0.9))
par(new = TRUE)
plot(comp2_mean_prop_A_B_C, xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", type = "b", col = "green", ylim = c(0.3, 0.9))
legend(x = 2, y = 0.6, legend = c("Proposed Rule", "Competitor1", "Competitor2"), fill = c("red", "blue", "green"))

#--Table-2

plot(mean_prop_A_BC, type = "b",main = "TABLE 2", xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", col = "red", ylim = c(0.3, 0.9))
par(new = TRUE)
plot(comp1_mean_prop_A_BC, xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", type = "b", col = "blue", ylim = c(0.3, 0.9))
par(new = TRUE)
plot(comp2_mean_prop_A_BC, xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", type = "b", col = "green", ylim = c(0.3, 0.9))
legend(x = 2, y = 0.6, legend = c("Proposed Rule", "Competitor1", "Competitor2"), fill = c("red", "blue", "green"))

#---Table-3

plot(mean_prop_A_C_B, type = "b",main = "TABLE 3", xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", col = "red", ylim = c(0.3, 0.9))
par(new = TRUE)
plot(comp1_mean_prop_A_C_B, xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", type = "b", col = "blue", ylim = c(0.3, 0.9))
par(new = TRUE)
plot(comp2_mean_prop_A_C_B, xlab = "Choices of Parameters", ylab = "Mean Proportion of allocation", type = "b", col = "green", ylim = c(0.3, 0.9))
legend(x = 1.9, y = 0.6, legend = c("Proposed Rule", "Competitor1", "Competitor2"), fill = c("red", "blue", "green"))

#---Plots for power
par(mfrow = c(1,3))

#--Table-1--
plot(power_A_B_C, type = "b" , main = "TABLE 1", xlab = "Choices of Parameters", ylab = "Power", col = "red", ylim = c(0.3, 1))
par(new = TRUE)
plot(comp1_power_A_B_C, xlab = "Choices of Parameters", ylab = "Power", type = "b", col = "blue", ylim = c(0.3, 1))
par(new = TRUE)
plot(comp2_power_A_B_C, xlab = "Choices of Parameters", ylab = "Power", type = "b", col = "green", ylim = c(0.3, 1))
legend(x = 2, y = 0.6, legend = c("Proposed Rule", "Competitor1", "Competitor2"), fill = c("red", "blue", "green"))

#--Table-2--
plot(power_A_BC, type = "b" ,main = "TABLE 2", xlab = "Choices of Parameters", ylab = "Power", col = "red", ylim = c(0.4, 1))
par(new = TRUE)
plot(comp1_power_A_BC, xlab = "Choices of Parameters", ylab = "Power", type = "b", col = "blue", ylim = c(0.4, 1))
par(new = TRUE)
plot(comp2_power_A_BC, xlab = "Choices of Parameters", ylab = "Power", type = "b", col = "green", ylim = c(0.4, 1))
legend(x = 2, y = 0.6, legend = c("Proposed Rule", "Competitor1", "Competitor2"), fill = c("red", "blue", "green"))

#--Table-2--
plot(power_A_C_B, type = "b" ,main = "TABLE 3", xlab = "Choices of Parameters", ylab = "Power", col = "red", ylim = c(0.2, 1))
par(new = TRUE)
plot(comp1_power_A_C_B, xlab = "Choices of Parameters", ylab = "Power", type = "b", col = "blue", ylim = c(0.2, 1))
par(new = TRUE)
plot(comp2_power_A_C_B, xlab = "Choices of Parameters", ylab = "Power", type = "b", col = "green", ylim = c(0.2, 1))
legend(x = 1.9, y = 0.5, legend = c("Proposed Rule", "Competitor1", "Competitor2"), fill = c("red", "blue", "green"))

#--Plot of power with sample size--
sample_size = c(200, 400, 1000, 1200)
par(mfrow = c(1,1))
plot(sample_size , power_samp, main = "Plot showing Power varying with Sample size", xlab = "Sample size", ylab = "Power", col = "red", type = "b", ylim = c(0.6, 0.8))
par(new = TRUE)
plot(sample_size , comp1_power_samp, xlab = "Sample size", ylab = "Power", col = "blue", type = "b", ylim = c(0.6, 0.8))
par(new = TRUE)
plot(sample_size , comp2_power_samp, xlab = "Sample size", ylab = "Power", col = "green", type = "b", ylim = c(0.6, 0.8))
legend(x = 210, y = 0.8, legend = c("Proposed Rule", "Competitor1", "Competitor2"), fill = c("red", "blue", "green"))

