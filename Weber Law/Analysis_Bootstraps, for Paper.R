
#----------------------
# Analysis of Weber's Law
# Marcin Penconek
#----------------------

# reduced number of levels and noise

#------------------------
# Estimating 67% cut level: Weibull Model


Weibull_function <- function(c, a, b){
  return(1 - 0.5*exp(-(c/a)^b))
} 

Reverse_Weibull_function <- function(v, a, b){
  return((-log(2-2*v))^(1/b)*a)
} 

Weibull_Estimate_B <- function(data, nA, v) {
  choice <- -(winning - 1)/2
  B_value <- data$n_B
  A_value <- data$n_A
  lambda0 <- A_value + B_value
  coherence <- 100*(2*B_value/lambda0 - 1)
  
  equation <- choice ~ Weibull_function(coherence,a,b)
  model <- nls(equation, start=list(a=12.5, b=1.2))
  
  a <- coef(model)[1]
  b <- coef(model)[2]
  c <- Reverse_Weibull_function(v, a, b)/100
  
  return((c + 1)*nA/(1 - c))
}

#----------
# Estimates

b_iterations <- 1000
#b_iterations <- 100

e5_67 <- numeric(10)
e5_75 <- numeric(10)
e5_85 <- numeric(10)

e4_67 <- numeric(10)
e4_75 <- numeric(10)
e4_85 <- numeric(10)

e3_67 <- numeric(10)
e3_75 <- numeric(10)
e3_85 <- numeric(10)

ciag <- numeric(10)
datasize_level <- numeric(10)


for(l in 1:10) {
  
  if(l == 1) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_1.csv") }
  if(l == 2) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_2.csv") }
  if(l == 3) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_3.csv") }
  if(l == 4) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_4.csv") }
  if(l == 5) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_5.csv") }
  if(l == 6) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_6.csv") }
  if(l == 7) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_7.csv") }
  if(l == 8) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_8.csv") }
  if(l == 9) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_9.csv") }
  if(l == 10) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_10.csv") }
#  if(l == 11) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_11.csv") }
#  if(l == 12) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_12.csv") }

  bdata <- subset(dane, dane$which_won_50 != 0)
  data_size <- length(bdata[ ,1])
  datasize_level[l] <- data_size
  nA <- bdata$n_A[1]
  winning <- bdata$which_won_50
  
  e5_67[l] <- round(Weibull_Estimate_B(bdata, nA, 2/3),2)
  e5_75[l] <- round(Weibull_Estimate_B(bdata, nA, 0.75),2)
  e5_85[l] <- round(Weibull_Estimate_B(bdata, nA, 0.85),2)
 
  bdata <- subset(dane, dane$which_won_40 != 0)
  winning <- bdata$which_won_40
  
  if(l < 10) { e4_67[l] <- round(Weibull_Estimate_B(bdata, nA, 2/3),2) }
  if(l < 10) { e4_75[l] <- round(Weibull_Estimate_B(bdata, nA, 0.75),2) }
  if(l < 10) { e4_85[l] <- round(Weibull_Estimate_B(bdata, nA, 0.85),2) }

  bdata <- subset(dane, dane$which_won_30 != 0)
  winning <- bdata$which_won_30
  
  if(l < 9) { e3_67[l] <- round(Weibull_Estimate_B(bdata, nA, 2/3),2) }
  if(l < 9) { e3_75[l] <- round(Weibull_Estimate_B(bdata, nA, 0.75),2) }
  if(l < 9) { e3_85[l] <- round(Weibull_Estimate_B(bdata, nA, 0.85),2) }
  
  ciag[l] <- nA
}

e5_67
e5_75
e5_85

e4_67
e4_75
e4_85

e3_67
e3_75
e3_85



#-----------
# Bootstrap
  
  boot_e5_67 <- numeric(10)
  low_e5_67 <- numeric(10)
  high_e5_67 <- numeric(10)
  
  boot_e5_75 <- numeric(10)
  low_e5_75 <- numeric(10)
  high_e5_75 <- numeric(10)
  
  boot_e5_85 <- numeric(10)
  low_e5_85 <- numeric(10)
  high_e5_85 <- numeric(10)

  
  boot_e4_75 <- numeric(10)
  low_e4_75 <- numeric(10)
  high_e4_75 <- numeric(10)

  boot_e3_75 <- numeric(10)
  low_e3_75 <- numeric(10)
  high_e3_75 <- numeric(10)
  
  boot_estimates <- matrix(NA, nrow = b_iterations, ncol = 10*3)
  
  set.seed(123456)
  print(timestamp())
  
  for(l in 1:10) {
    
    if(l == 1) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_1.csv") }
    if(l == 2) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_2.csv") }
    if(l == 3) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_3.csv") }
    if(l == 4) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_4.csv") }
    if(l == 5) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_5.csv") }
    if(l == 6) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_6.csv") }
    if(l == 7) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_7.csv") }
    if(l == 8) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_8.csv") }
    if(l == 9) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_9.csv") }
    if(l == 10) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_10.csv") }
#    if(l == 11) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_11.csv") }
#    if(l == 12) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_12.csv") }
    
    nB_vector_67 <-numeric(b_iterations)
    nB_vector_75 <-numeric(b_iterations)
    nB_vector_85 <-numeric(b_iterations)
    
    
  bdata <- subset(dane, dane$which_won_50 != 0)
  data_size <- length(bdata[ ,1])
  nA <- bdata$n_A[1]
  
  for(i in 1:b_iterations) {
    index <- sample(data_size, data_size, replace = TRUE)
    subdata <- bdata[index, ]
    winning <- bdata[index, ]$which_won_50
    
    nB_vector_67[i]<- Weibull_Estimate_B(subdata, nA, 2/3)
    nB_vector_75[i]<- Weibull_Estimate_B(subdata, nA, 0.75)
    nB_vector_85[i]<- Weibull_Estimate_B(subdata, nA, 0.85)

  }
  
  boot_e5_67[l] <- mean(nB_vector_67, na.rm = TRUE)
  low_e5_67[l] <- quantile(nB_vector_67, 0.025, na.rm = TRUE)
  high_e5_67[l] <- quantile(nB_vector_67, 0.975, na.rm = TRUE)

  boot_e5_75[l] <- mean(nB_vector_75, na.rm = TRUE)
  low_e5_75[l] <- quantile(nB_vector_75, 0.025, na.rm = TRUE)
  high_e5_75[l] <- quantile(nB_vector_75, 0.975, na.rm = TRUE)
  
  boot_e5_85[l] <- mean(nB_vector_85, na.rm = TRUE)
  low_e5_85[l] <- quantile(nB_vector_85, 0.025, na.rm = TRUE)
  high_e5_85[l] <- quantile(nB_vector_85, 0.975, na.rm = TRUE)
  
  boot_estimates[ , 3*l - 2] <- nB_vector_67
  boot_estimates[ , 3*l - 1] <- nB_vector_75
  boot_estimates[ , 3*l] <- nB_vector_85
  
  if(l <= 9) {
  
    bdata <- subset(dane, dane$which_won_40 != 0)
    data_size <- length(bdata[ ,1])
    
    for(i in 1:b_iterations) {
      index <- sample(data_size, data_size, replace = TRUE)
      subdata <- bdata[index, ]
      winning <- bdata[index, ]$which_won_40
      
      nB_vector_75[i]<- Weibull_Estimate_B(subdata, nA, 0.75)
      
    }
    
    boot_e4_75[l] <- mean(nB_vector_75, na.rm = TRUE)
    low_e4_75[l] <- quantile(nB_vector_75, 0.025, na.rm = TRUE)
    high_e4_75[l] <- quantile(nB_vector_75, 0.975, na.rm = TRUE)
  }
  
  if(l <= 6) {
    
    bdata <- subset(dane, dane$which_won_30 != 0)
    data_size <- length(bdata[ ,1])
    
    for(i in 1:b_iterations) {
      index <- sample(data_size, data_size, replace = TRUE)
      subdata <- bdata[index, ]
      winning <- bdata[index, ]$which_won_30
      
      nB_vector_75[i]<- Weibull_Estimate_B(subdata, nA, 0.75)
      
    }
    
    boot_e3_75[l] <- mean(nB_vector_75, na.rm = TRUE)
    low_e3_75[l] <- quantile(nB_vector_75, 0.025, na.rm = TRUE)
    high_e3_75[l] <- quantile(nB_vector_75, 0.975, na.rm = TRUE)
  }
  
}

print(timestamp())
  
boot_e5_67 
low_e5_67 
high_e5_67 
  
boot_e5_75 
low_e5_75 
high_e5_75 
  
boot_e5_85 
low_e5_85 
high_e5_85 

boot_e4_75 
low_e4_75 
high_e4_75 

boot_e3_75 
low_e3_75 
high_e3_75 


#--------------
# Plot

opis <- 1.2
small <- 1.2


# Linear Coding
#-----------
# Diff Plot


diff_67 <- e5_67 - ciag
diff_75 <- e5_75 - ciag
diff_85 <- e5_85 - ciag

diff_67
diff_75
diff_85

plot(ciag, diff_67, type = "p", xlim = c(1.5,11.5), ylim = c(0,6), pch =17, 
     xlab = "Stimulus Intensity", ylab = "Stimulus Difference", cex.axis = opis, cex.lab = opis, cex = opis, 
     main = "a                                                                                                       "
)

points(ciag, diff_75, pch = 19, cex = small)

points(ciag, diff_85, pch = 15, cex = small)

lines(ciag, 0.129333 + ciag*0.121333, lty = 2)
lines(ciag, 0.08145 + ciag*0.22055, lty = 2)
lines(ciag[1:10], -0.13145 + ciag[1:10]*0.42345, lty = 2)


legend(2, 6, legend = c("Choice Prob. 85%", "75%", "66.7%"), 
       pch = c(15, 19, 17), cex = small, bty = "n")


model <- lm(diff_67 ~ ciag)
summary(model)

model <- lm(diff_75 ~ ciag)
summary(model)

model <- lm(diff_85[1:10] ~ ciag[1:10])
summary(model)

#----------
# Confidence Intervals with Bootstrap


low_d_67 <- (low_e5_67 - ciag) 
high_d_67 <- (high_e5_67 - ciag) 

low_d_75 <- (low_e5_75 - ciag) 
high_d_75 <- (high_e5_75 - ciag) 

low_d_85 <- (low_e5_85 - ciag) 
high_d_85 <- (high_e5_85 - ciag) 

low_d_67
high_d_67

low_d_75
high_d_75

low_d_85
high_d_85 

#low_d_85[11] <- NA
#high_d_85[11] <- NA


for(d in 1:12) {
  lines(c(ciag[d], ciag[d]), c(low_d_67[d], high_d_67[d]), lwd = 1)
  lines(c(ciag[d], ciag[d]), c(low_d_75[d], high_d_75[d]), lwd = 1)
  lines(c(ciag[d], ciag[d]), c(low_d_85[d], high_d_85[d]), lwd = 1)
}

points(ciag, low_d_67, cex = opis, pch = 45)
points(ciag, high_d_67, cex = opis, pch = 45)

points(ciag, low_d_75, cex = opis, pch = 45)
points(ciag, high_d_75, cex = opis, pch = 45)

points(ciag, low_d_85, cex = opis, pch = 45)
points(ciag, high_d_85, cex = opis, pch = 45)


#-----------
# Ratio Plot (67%)

szum <- 0

ratio_67 <- (e5_67-ciag) / (ciag - szum)

plot(ciag - szum, ratio_67, type = "l", lty = 3, xlim = c(1.5,11.5), ylim = c(0,0.8), 
     xlab = "Stimulus Intensity", ylab = "Weber Fractions", cex.axis = opis, cex.lab = opis, cex = opis, 
     main = "b                                                                                                       "
)

points(ciag - szum, ratio_67, pch = 17, cex = small)


#legend(2, 0.07, legend = c("Choice Prob. 66.7% (95% CI)", "Weber's Law"), 
#       pch = c(17, NaN), lty = c(3,2), cex = small, bty = "n")

#----------
# Confidence Intervals with Bootstrap

low_r_67 <- (low_e5_67 - ciag) / (ciag - szum)
high_r_67 <- (high_e5_67 - ciag) / (ciag - szum)

for(d in 1:12) {
  lines(c(ciag[d] - szum, ciag[d] - szum), c(low_r_67[d], high_r_67[d]), lwd = 1)
}

points(ciag - szum, low_r_67, cex = opis, pch = 45)
points(ciag - szum, high_r_67, cex = opis, pch = 45)
#abline(h = mean(ratio_67), lty = 2)


#-----------
# Ratio Plot (75%)

e5_75 <- e5_75

ratio_75 <- (e5_75-ciag) / (ciag - szum)

lines(ciag - szum, ratio_75, lty = 3)
points(ciag - szum, ratio_75, pch = 19, cex = small)


#legend(2, 0.12, legend = c("Choice Prob. 75% (95% CI)", "Weber's Law"), 
#       pch = c(19, NaN), lty = c(3,2), cex = small, bty = "n")

#----------
# Confidence Intervals with Bootstrap

low_r_75 <- (low_e5_75 - ciag) / (ciag - szum)
high_r_75 <- (high_e5_75 - ciag) / (ciag - szum)

for(d in 1:12) {
  lines(c(ciag[d] - szum, ciag[d] - szum), c(low_r_75[d], high_r_75[d]), lwd = 1)
}

points(ciag - szum, low_r_75, cex = opis, pch = 45)
points(ciag - szum, high_r_75, cex = opis, pch = 45)
#abline(h = mean(ratio_75), lty = 2)


#-----------
# Ratio Plot (85%)

ratio_85 <- (e5_85-ciag) / (ciag - szum)

lines(ciag - szum, ratio_85, lty = 3) 
points(ciag - szum, ratio_85, pch = 15, cex = small)


#legend(2, 1.2, legend = c("Choice Prob. 85% (95% CI)", "Weber's Law"), 
#       pch = c(15, NaN), lty = c(3,2), cex = small, bty = "n")


#----------
# Confidence Intervals with Bootstrap

low_r_85 <- (low_e5_85 - ciag) / (ciag - szum)
high_r_85 <- (high_e5_85 - ciag) / (ciag - szum)

for(d in 1:12) {
  lines(c(ciag[d] - szum, ciag[d] - szum), c(low_r_85[d], high_r_85[d]), lwd = 1)
}

points(ciag - szum, low_r_85, cex = opis, pch = 45)
points(ciag - szum, high_r_85, cex = opis, pch = 45)
#abline(h = mean(ratio_85), lty = 2)

legend(5, 0.8, legend = c("Choice Prob. 85%", "75%", "66.7%"), 
       pch = c(15, 19, 17), lty = c(3,3,3), cex = small, bty = "n")


#-------------------
# Estimating Non-Linearity based on Bootstrap

model_results <- matrix(NA, nrow = b_iterations, ncol = 9)

for(i in 1:b_iterations) {
  
  est_67 <- c(boot_estimates[i, 1], boot_estimates[i, 4], boot_estimates[i, 7], boot_estimates[i, 10], 
              boot_estimates[i, 13], boot_estimates[i, 16], boot_estimates[i, 19], boot_estimates[i, 22], 
              boot_estimates[i, 25], boot_estimates[i, 28])
  est_75 <- c(boot_estimates[i, 2], boot_estimates[i, 5], boot_estimates[i, 8], boot_estimates[i, 11], 
              boot_estimates[i, 14], boot_estimates[i, 17], boot_estimates[i, 20], boot_estimates[i, 23], 
              boot_estimates[i, 26], boot_estimates[i, 29])
  est_85 <- c(boot_estimates[i, 3], boot_estimates[i, 6], boot_estimates[i, 9], boot_estimates[i, 12], 
              boot_estimates[i, 15], boot_estimates[i, 18], boot_estimates[i, 21], boot_estimates[i, 24], 
              boot_estimates[i, 27], boot_estimates[i, 30])
  jnd_67 <- est_67 - ciag
  jnd_75 <- est_75 - ciag
  jnd_85 <- est_85 - ciag
  sq_ciag <- ciag^2
  
  equation_67 <- jnd_67 ~ ciag + sq_ciag
  model_67 <- lm(equation_67)
  model_results[i, 1] <- model_67[["coefficients"]][["(Intercept)"]]
  model_results[i, 2] <- model_67[["coefficients"]][["ciag"]]
  model_results[i, 3] <- model_67[["coefficients"]][["sq_ciag"]]
  
  equation_75 <- jnd_75 ~ ciag + sq_ciag
  model_75 <- lm(equation_75)
  model_results[i, 4] <- model_75[["coefficients"]][["(Intercept)"]]
  model_results[i, 5] <- model_75[["coefficients"]][["ciag"]]
  model_results[i, 6] <- model_75[["coefficients"]][["sq_ciag"]]
  
  equation_85 <- jnd_85 ~ ciag + sq_ciag
  model_85 <- lm(equation_85)
  model_results[i, 7] <- model_85[["coefficients"]][["(Intercept)"]]
  model_results[i, 8] <- model_85[["coefficients"]][["ciag"]]
  model_results[i, 9] <- model_85[["coefficients"]][["sq_ciag"]]
  

}

mean(model_results[ ,3])
quantile(model_results[ ,3], 0.025, na.rm = TRUE)
quantile(model_results[ ,3], 0.975, na.rm = TRUE)

mean(model_results[ ,6])
quantile(model_results[ ,6], 0.025, na.rm = TRUE)
quantile(model_results[ ,6], 0.975, na.rm = TRUE)

mean(model_results[ ,9])
quantile(model_results[ ,9], 0.025, na.rm = TRUE)
quantile(model_results[ ,9], 0.975, na.rm = TRUE)


#-----------------
# The same on Weber Fractions


model_fra <- matrix(NA, nrow = b_iterations, ncol = 9)

for(i in 1:b_iterations) {
  
  est_67 <- c(boot_estimates[i, 1], boot_estimates[i, 4], boot_estimates[i, 7], boot_estimates[i, 10], 
              boot_estimates[i, 13], boot_estimates[i, 16], boot_estimates[i, 19], boot_estimates[i, 22], 
              boot_estimates[i, 25], boot_estimates[i, 28])
  est_75 <- c(boot_estimates[i, 2], boot_estimates[i, 5], boot_estimates[i, 8], boot_estimates[i, 11], 
              boot_estimates[i, 14], boot_estimates[i, 17], boot_estimates[i, 20], boot_estimates[i, 23], 
              boot_estimates[i, 26], boot_estimates[i, 29])
  est_85 <- c(boot_estimates[i, 3], boot_estimates[i, 6], boot_estimates[i, 9], boot_estimates[i, 12], 
              boot_estimates[i, 15], boot_estimates[i, 18], boot_estimates[i, 21], boot_estimates[i, 24], 
              boot_estimates[i, 27], boot_estimates[i, 30])
  
  fra_67 <- (est_67 - ciag) / ciag
  fra_75 <- (est_75 - ciag) / ciag
  fra_85 <- (est_85 - ciag) / ciag
  sq_ciag <- ciag^2
  
  equation_67 <- fra_67 ~ ciag + sq_ciag
  model_67 <- lm(equation_67)
  anova_67 <- anova(model_67)
  
  model_fra[i, 1] <- anova_67[2,5]
  model_fra[i, 2] <- anova_67[2,4]
  model_fra[i, 3] <- model_67[["coefficients"]][["sq_ciag"]]
  
  equation_75 <- fra_75 ~ ciag + sq_ciag
  model_75 <- lm(equation_75)
  anova_75 <- anova(model_75)
  
  model_fra[i, 4] <- anova_75[2,5]
  model_fra[i, 5] <- anova_75[2,4]
  model_fra[i, 6] <- model_75[["coefficients"]][["sq_ciag"]]
  
  equation_85 <- fra_85 ~ ciag + sq_ciag
  model_85 <- lm(equation_85)
  anova_85 <- anova(model_85)
  
  model_fra[i, 7] <- anova_85[2,5]
  model_fra[i, 8] <- anova_85[2,4]
  model_fra[i, 9] <- model_85[["coefficients"]][["sq_ciag"]]
  

}

mean(model_fra[ ,3])
quantile(model_fra[ ,3], 0.025, na.rm = TRUE)
quantile(model_fra[ ,3], 0.975, na.rm = TRUE)

sum(model_fra[ ,3] > 0)
sum(model_fra[ ,1] < 0.01)

mean(model_fra[ ,6])
quantile(model_fra[ ,6], 0.025, na.rm = TRUE)
quantile(model_fra[ ,6], 0.975, na.rm = TRUE)

sum(model_fra[ ,6] > 0)
sum(model_fra[ ,4] < 0.01)

mean(model_fra[ ,9])
quantile(model_fra[ ,9], 0.025, na.rm = TRUE)
quantile(model_fra[ ,9], 0.975, na.rm = TRUE)

sum(model_fra[ ,9] > 0)
sum(model_fra[ ,7] < 0.01)


#------------

fra_67 <- (est_67 - ciag) / ciag
fra_75 <- (est_75 - ciag) / ciag
fra_85 <- (est_85 - ciag) / ciag
sq_ciag <- ciag^2

equation_67 <- fra_67 ~ ciag + sq_ciag
model_67 <- lm(equation_67)

summary(model_67)

equation_75 <- fra_75 ~ ciag + sq_ciag
model_75 <- lm(equation_75)

summary(model_75)

equation_85 <- fra_85 ~ ciag + sq_ciag
model_85 <- lm(equation_85)

summary(model_85)


