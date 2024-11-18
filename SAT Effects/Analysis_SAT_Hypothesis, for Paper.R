
#----------------------
# Analysis of Weber's Law
# Marcin Penconek
#----------------------

# Hypothetical SAT Scenario
# SAT condition 30 Hz for first level
# SAT condition 40 Hz for second

correct <- numeric(10)
mean_RT <- numeric(10)
median_RT <- numeric(10)
correct_RT <- numeric(10)
error_RT <- numeric(10)
min_RT <- numeric(10)
cdatasize_level <- numeric(10)
ccdatasize_level <- numeric(10)
ecdatasize_level <- numeric(10)
cnA <- numeric(10)
cnB <- numeric(10)


for(l in 1:10) {
  
  if(l == 1) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_1.csv") }
  if(l == 2) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_2.csv") }
  if(l == 3) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_3.csv") }
  if(l == 4) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_4.csv") }
  if(l == 5) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_5.csv") }
  if(l == 6) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_6.csv") }
  if(l == 7) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_7.csv") }
  if(l == 8) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_8.csv") }
  if(l == 9) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_9.csv") }
  if(l == 10) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_10.csv") }
#  if(l == 11) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_11.csv") }
#  if(l == 12) { cdane <- read.csv2("~/Marcin/Studia/Weber Law/Scale Invariance/Check_2000_12.csv") }
  
  cdata <- subset(cdane, (cdane$which_won_50 != 0 & cdane$dec_time_50 > 500))
  cdatasize_level[l] <- length(cdata[ ,1])
  cnA[l] <- cdata$n_A[1]
  cnB[l] <- cdata$n_B[1]
  correct[l] <- sum(cdata$which_won_50 == -1)/cdatasize_level[l]
  mean_RT[l] <- mean(cdata$dec_time_50[cdata$dec_time_50 > 500]) - 500
  median_RT[l] <- median(cdata$dec_time_50[cdata$dec_time_50 > 500]) - 500
  correct_RT[l] <- mean(cdata$dec_time_50[cdata$which_won_50 == -1 & cdata$dec_time_50 > 500]) - 500
  error_RT[l] <- mean(cdata$dec_time_50[cdata$which_won_50 == 1 & cdata$dec_time_50 > 500]) - 500
  min_RT[l] <- min(cdata$dec_time_50) - 500
  
  if(l == 1) { RT_1 <- cdata$dec_time_50 - 500}
  if(l == 2) { RT_2 <- cdata$dec_time_50 - 500}
  if(l == 3) { RT_3 <- cdata$dec_time_50 - 500}
  if(l == 4) { RT_4 <- cdata$dec_time_50 - 500}
  if(l == 5) { RT_5 <- cdata$dec_time_50 - 500}
  if(l == 6) { RT_6 <- cdata$dec_time_50 - 500}
  if(l == 7) { RT_7 <- cdata$dec_time_50 - 500}
  if(l == 8) { RT_8 <- cdata$dec_time_50 - 500}
  if(l == 9) { RT_9 <- cdata$dec_time_50 - 500}
  if(l == 10) { RT_10 <- cdata$dec_time_50 - 500}
#  if(l == 11) { RT_11 <- cdata$dec_time_50 - 500}
#  if(l == 12) { RT_12 <- cdata$dec_time_50 - 500}

  ccdatasize_level[l] <- length(cdata[ ,1][cdata$which_won_50 == -1])
  
  if(l == 1) { cRT_1 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 2) { cRT_2 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 3) { cRT_3 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 4) { cRT_4 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 5) { cRT_5 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 6) { cRT_6 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 7) { cRT_7 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 8) { cRT_8 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 9) { cRT_9 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
  if(l == 10) { cRT_10 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
#  if(l == 11) { cRT_11 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}
#  if(l == 12) { cRT_12 <- cdata$dec_time_50[cdata$which_won_50 == -1] - 500}

  ecdatasize_level[l] <- length(cdata[ ,1][cdata$which_won_50 == 1])  
  if(l == 1) { eRT_1 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 2) { eRT_2 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 3) { eRT_3 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 4) { eRT_4 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 5) { eRT_5 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 6) { eRT_6 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 7) { eRT_7 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 8) { eRT_8 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 9) { eRT_9 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  if(l == 10) { eRT_10 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
#  if(l == 11) { eRT_11 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
#  if(l == 12) { eRT_12 <- cdata$dec_time_50[cdata$which_won_50 == 1] - 500}
  

}

cdatasize_level
ccdatasize_level
ecdatasize_level

SAT_mean_RT <- mean_RT
SAT_median_RT <- median_RT
SAT_correct_RT <- correct_RT
SAT_error_RT <- error_RT

cdane <- read.csv2("~/Marcin/Studia/Weber Law/SAT Effects/Check_2000_1_30.csv")

cdata <- subset(cdane, (cdane$which_won_30 != 0 & cdane$dec_time_30 > 500))
SAT_size_1 <- length(cdata[ ,1])
SAT_mean_RT[1] <- mean(cdata$dec_time_30[cdata$dec_time_30 > 500]) - 500
SAT_median_RT[1] <- median(cdata$dec_time_30[cdata$dec_time_30 > 500]) - 500
SAT_correct_RT[1] <- mean(cdata$dec_time_30[cdata$which_won_30 == -1 & cdata$dec_time_30 > 500]) - 500
SAT_error_RT[1] <- mean(cdata$dec_time_30[cdata$which_won_30 == 1 & cdata$dec_time_30 > 500]) - 500

SAT_RT_1 <- cdata$dec_time_30 - 500
SAT_cRT_1 <- cdata$dec_time_30[cdata$which_won_30 == -1] - 500
SAT_eRT_1 <- cdata$dec_time_30[cdata$which_won_30 == 1] - 500

cdane <- read.csv2("~/Marcin/Studia/Weber Law/SAT Effects/Check_2000_2_40.csv")

cdata <- subset(cdane, (cdane$which_won_40 != 0 & cdane$dec_time_40 > 500))
SAT_size_2 <- length(cdata[ ,1])
SAT_mean_RT[2] <- mean(cdata$dec_time_40[cdata$dec_time_40 > 500]) - 500
SAT_median_RT[2] <- median(cdata$dec_time_40[cdata$dec_time_40 > 500]) - 500
SAT_correct_RT[2] <- mean(cdata$dec_time_40[cdata$which_won_40 == -1 & cdata$dec_time_40 > 500]) - 500
SAT_error_RT[2] <- mean(cdata$dec_time_40[cdata$which_won_40 == 1 & cdata$dec_time_40 > 500]) - 500

SAT_RT_2 <- cdata$dec_time_40 - 500
SAT_cRT_2 <- cdata$dec_time_40[cdata$which_won_40 == -1] - 500
SAT_eRT_2 <- cdata$dec_time_40[cdata$which_won_40 == 1] - 500

SAT_size_1
SAT_size_2
length(SAT_RT_1)
length(SAT_RT_2)

#---------------
# Wykres

low <- 2:11

do_wykresu <- c(rep(low[1], SAT_size_1),
                rep(low[2], SAT_size_2),
                rep(low[3], cdatasize_level[3]),
                rep(low[4], cdatasize_level[4]),
                rep(low[5], cdatasize_level[5]),
                rep(low[6], cdatasize_level[6]),
                rep(low[7], cdatasize_level[7]),
                rep(low[8], cdatasize_level[8]),
                rep(low[9], cdatasize_level[9]),
                rep(low[10], cdatasize_level[10])) 


length(do_wykresu)

set.seed(123456)
do_wykresu <- do_wykresu + rnorm(length(do_wykresu), 0, 0.15)

opis <- 1.2

plot(do_wykresu, c(SAT_RT_1, SAT_RT_2, RT_3, RT_4, RT_5, RT_6, RT_7, RT_8, RT_9, RT_10), 
     type = "p", xlim = c(1.5,11.5), ylim = c(0,2500), pch = 16, cex = 0.4, 
     xlab = "Stimulus Level", ylab = "Reaction Time (ms)", 
     col = "light grey", cex.lab = opis, cex.axis = opis, 
     main = "c                                                                                                       "
)

lines(low, mean_RT, lty = 3)
lines(low, SAT_mean_RT, lty = 3)

sd_RT <- c(sd(SAT_RT_1), sd(SAT_RT_2), sd(RT_3), sd(RT_4), sd(RT_5), 
           sd(RT_6), sd(RT_7), sd(RT_8), sd(RT_9), sd(RT_10))

#points(low, mean_RT, pch = 16, cex = opis, col = "grey")
points(low, mean_RT, pch = 1, cex = opis)
points(low, SAT_mean_RT, pch = 15, cex = opis)


for(d in 1:10) {
  lines(c(low[d], low[d]), c(SAT_mean_RT[d] - sd_RT[d], SAT_mean_RT[d] + sd_RT[d]), lwd = 1)
}
points(low, SAT_mean_RT - sd_RT, cex = 1.2, pch = 45, col = "black")
points(low, SAT_mean_RT + sd_RT, cex = 1.2, pch = 45, col = "black")


legend(2.75, 2500, legend = c("SAT Scenario: Mean (+/-SD)", "Default Scenario: Mean"), 
       pch = c(15, 1), lty = c(3,3), lwd = c(1,1), cex = opis, bty = "n")

SAT_mean_RT
SAT_median_RT
sd_RT
SAT_correct_RT
SAT_error_RT
diff_RT <- SAT_error_RT - SAT_correct_RT
diff_RT
min_RT

#--------------
# Correct & Errors

cSAT_size_1 <- length(SAT_cRT_1)
cSAT_size_2 <- length(SAT_cRT_2)
eSAT_size_1 <- length(SAT_eRT_1)
eSAT_size_2 <- length(SAT_eRT_2)

cdo_wykresu <- c(rep(low[1], cSAT_size_1),
                rep(low[2], cSAT_size_2),
                rep(low[3], ccdatasize_level[3]),
                rep(low[4], ccdatasize_level[4]),
                rep(low[5], ccdatasize_level[5]),
                rep(low[6], ccdatasize_level[6]),
                rep(low[7], ccdatasize_level[7]),
                rep(low[8], ccdatasize_level[8]),
                rep(low[9], ccdatasize_level[9]),
                rep(low[10], ccdatasize_level[10]))

edo_wykresu <- c(rep(low[1], eSAT_size_1),
                 rep(low[2], eSAT_size_2),
                 rep(low[3], ecdatasize_level[3]),
                 rep(low[4], ecdatasize_level[4]),
                 rep(low[5], ecdatasize_level[5]),
                 rep(low[6], ecdatasize_level[6]),
                 rep(low[7], ecdatasize_level[7]),
                 rep(low[8], ecdatasize_level[8]),
                 rep(low[9], ecdatasize_level[9]),
                 rep(low[10], ecdatasize_level[10]))

set.seed(123456)
cdo_wykresu <- cdo_wykresu + rnorm(length(cdo_wykresu), 0, 0.15)
edo_wykresu <- edo_wykresu + rnorm(length(edo_wykresu), 0, 0.08)


plot(cdo_wykresu, c(SAT_cRT_1, SAT_cRT_2, cRT_3, cRT_4, cRT_5, cRT_6, cRT_7, cRT_8, cRT_9, cRT_10), 
     type = "p", xlim = c(1.5,11.5), ylim = c(0,2500), pch = 16, cex = 0.4, 
     xlab = "Stimulus Level", ylab = "Reaction Time (ms)", 
     col = "light blue", cex.lab = opis, cex.axis = opis, 
     main = "d                                                                                                       "
)

points(edo_wykresu, c(SAT_eRT_1, SAT_eRT_2, eRT_3, eRT_4, eRT_5, eRT_6, eRT_7, eRT_8, eRT_9, eRT_10), 
       pch = 17, cex = 0.3, col = "pink")

#plot(low, correct_RT, lty = 3, lwd = 1, 
#     type = "l", xlim = c(2,13), ylim = c(0,2500), 
#     xlab = "Stimulus Level", ylab = "Reaction Time (ms)", 
#     cex.lab = opis, cex.axis = opis, 
#     main = "b                                                                                                       "
#)

lines(low, SAT_correct_RT, lty = 3, lwd = 1, col = "blue")
lines(low, SAT_error_RT, lty = 3, lwd = 1, col = "red")

#lines(low, correct_RT, col = "blue")
#lines(low, error_RT, col = "red")

#sd_RT <- c(sd(RT_1), sd(RT_2), sd(RT_3), sd(RT_4), sd(RT_5), 
#           sd(RT_6), sd(RT_7), sd(RT_8), sd(RT_9), sd(RT_10),
#           sd(RT_11), sd(RT_12))

points(low, SAT_correct_RT, pch = 16, cex = opis, col = "blue")
points(low, SAT_error_RT, pch = 17, cex = opis, col = "red")

legend(4, 2500, "SAT Scenario: Mean RTs", cex = opis, bty = "n")

legend(4.8, 2250, legend = c("Correct Decisions", "Erroneous Decisions"), 
       pch = c(16, 17), lty = c(3,3), col = c("blue", "red"), cex = opis, bty = "n")

diff_RT
mean(diff_RT)
max(diff_RT)
min(diff_RT)


#------------------------
# Ten fragment pochodzi z dysertacji doktorskiej i korzysta z danych

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
#b_iterations <- 2

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
  
  if(l < 11) { e4_67[l] <- round(Weibull_Estimate_B(bdata, nA, 2/3),2) }
  if(l < 11) { e4_75[l] <- round(Weibull_Estimate_B(bdata, nA, 0.75),2) }
  if(l < 11) { e4_85[l] <- round(Weibull_Estimate_B(bdata, nA, 0.85),2) }
  
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
#  if(l == 11) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_11.csv") }
#  if(l == 12) { dane <- read.csv2("~/Marcin/Studia/Dysertacja/Weber (no Channel)/Fractions_5000_12.csv") }
  
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

#-----------------
# Weber's Law with SAT

estimate_SAT <- e5_75
boot_e_SAT <- boot_e5_75
low_e_SAT <- low_e5_75
high_e_SAT <- high_e5_75

estimate_SAT[1] <- e3_75[1]
estimate_SAT[2] <- e4_75[2]

boot_e_SAT[1] <- boot_e3_75[1]
boot_e_SAT[2] <- boot_e4_75[2]

low_e_SAT[1] <- low_e3_75[1]
low_e_SAT[2] <- low_e4_75[2]

high_e_SAT[1] <- high_e3_75[1]
high_e_SAT[2] <- high_e4_75[2]

estimate_SAT 
e5_75
boot_e_SAT 
boot_e5_75
low_e_SAT 
low_e5_75
high_e_SAT 
high_e5_75


#-----------
# Ratio Plot with SAT (75%)

kolor <- 1
kol2 <- 1
opis <- 1.2

ratio_75 <- (e5_75-ciag) / ciag
ratio_SAT <- (estimate_SAT-ciag) / ciag

plot(ciag, ratio_75, type = "l", lty = 3, xlim = c(1.5,11.5), ylim = c(0,0.6), col = kolor, 
     xlab = "Stimulus Intensity", ylab = "Weber Fractions", cex.axis = opis, cex.lab = opis, cex = opis, 
     main = "b                                                                                                       "
)

lines(ciag, ratio_SAT, lty = 3)
points(ciag[1:2], ratio_75[1:2], pch = 1, cex = opis, col = kolor)
points(ciag[3:12], ratio_75[3:12], pch = 15, cex = opis)
points(ciag[1:2], ratio_SAT[1:2], pch = 15, cex = opis, col = kol2)


legend(5.5, 0.6, legend = c("SAT Scenario", "Default Scenario"), 
       pch = c(15, 1), lty = c(3,3), cex = opis, col = c(kol2, kolor), bty = "n")


legend(1.5, 0.49, legend = "30 Hz", cex = opis, col = kol2, bty = "n")
legend(2.5, 0.35, legend = "40 Hz", cex = opis, col = kol2, bty = "n")

#----------
# Confidence Intervals with Bootstrap

low_r_75 <- (low_e5_75 - ciag) / ciag
high_r_75 <- (high_e5_75 - ciag) / ciag

low_r_SAT <- (low_e_SAT - ciag) / ciag
high_r_SAT <- (high_e_SAT - ciag) / ciag



for(d in 1:2) {
  lines(c(ciag[d], ciag[d]), c(low_r_75[d], high_r_75[d]), lwd = 1, col = kolor)
  lines(c(ciag[d], ciag[d]), c(low_r_SAT[d], high_r_SAT[d]), lwd = 1, col = kol2)
}

for(d in 3:10) {
  lines(c(ciag[d], ciag[d]), c(low_r_75[d], high_r_75[d]), lwd = 1)
}

points(ciag[1:2], low_r_75[1:2], cex = opis, pch = 45, col = kolor)
points(ciag[1:2], high_r_75[1:2], cex = opis, pch = 45, col = kolor)
points(ciag[3:12], low_r_75[3:12], cex = opis, pch = 45)
points(ciag[3:12], high_r_75[3:12], cex = opis, pch = 45)
points(ciag[1:2], low_r_SAT[1:2], cex = opis, pch = 45, col = kol2)
points(ciag[1:2], high_r_SAT[1:2], cex = opis, pch = 45, col = kol2)

#abline(h = mean(ratio_SAT), lty = 2)



#-----------
# Ratio Plot with SAT (75%)

e4_75
e3_75

ratio_75 <- (e5_75-ciag) / ciag
r4_75 <- (e4_75-ciag) / ciag
r3_75 <- (e3_75-ciag) / ciag

plot(ciag, ratio_75, type = "l", lty = 3, xlim = c(1.5,11.5), ylim = c(0,0.6), 
     xlab = "Stimulus Intensity", ylab = "Weber Fractions", cex.axis = opis, cex.lab = opis, cex = opis, 
     main = "a                                                                                                       "
)

points(ciag, ratio_75, pch = 1, cex = opis)
lines(ciag[1:8], r4_75[1:8], lty = 3)
points(ciag[1:8], r4_75[1:8], pch = 18, cex = opis)
lines(ciag[1:6], r3_75[1:6], lty = 3)
points(ciag[1:6], r3_75[1:6], pch = 17, cex = opis)

#?pch

legend(2, 0.175, legend = c("SAT Condition: 50 Hz", "40 Hz", "30 Hz"), 
       pch = c(1, 18, 17), lty = c(3,3,3), cex = opis, bty = "n")

#----------
# Confidence Intervals with Bootstrap


low_r_75 <- (low_e5_75 - ciag) / ciag
high_r_75 <- (high_e5_75 - ciag) / ciag

low_r4_75 <- (low_e4_75 - ciag) / ciag
high_r4_75 <- (high_e4_75 - ciag) / ciag

low_r3_75 <- (low_e3_75 - ciag) / ciag
high_r3_75 <- (high_e3_75 - ciag) / ciag




for(d in 1:10) {
  lines(c(ciag[d], ciag[d]), c(low_r_75[d], high_r_75[d]), lwd = 1)
}

points(ciag, low_r_75, cex = opis, pch = 45)
points(ciag, high_r_75, cex = opis, pch = 45)

for(d in 1:8) {
  lines(c(ciag[d], ciag[d]), c(low_r4_75[d], high_r4_75[d]), lwd = 1)
}

points(ciag[1:8], low_r4_75[1:8], cex = opis, pch = 45)
points(ciag[1:8], high_r4_75[1:8], cex = opis, pch = 45)

for(d in 1:6) {
  lines(c(ciag[d], ciag[d]), c(low_r3_75[d], high_r3_75[d]), lwd = 1)
}

points(ciag[1:6], low_r3_75[1:6], cex = opis, pch = 45)
points(ciag[1:6], high_r3_75[1:6], cex = opis, pch = 45)



