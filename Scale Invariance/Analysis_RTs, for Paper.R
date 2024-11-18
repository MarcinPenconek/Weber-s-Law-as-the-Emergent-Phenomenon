
#----------------------
# Analysis of Weber's Law
# Marcin Penconek
#----------------------

# Scale Invariance

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


#---------------
# Wykres

low <- 2:11

do_wykresu <- c(rep(low[1], cdatasize_level[1]),
                rep(low[2], cdatasize_level[2]),
                rep(low[3], cdatasize_level[3]),
                rep(low[4], cdatasize_level[4]),
                rep(low[5], cdatasize_level[5]),
                rep(low[6], cdatasize_level[6]),
                rep(low[7], cdatasize_level[7]),
                rep(low[8], cdatasize_level[8]),
                rep(low[9], cdatasize_level[9]),
                rep(low[10], cdatasize_level[10]))

set.seed(123456)
do_wykresu <- do_wykresu + rnorm(sum(cdatasize_level), 0, 0.15)

opis <- 1.2

plot(do_wykresu, c(RT_1, RT_2, RT_3, RT_4, RT_5, RT_6, RT_7, RT_8, RT_9, RT_10), 
     type = "p", xlim = c(1.5,11.5), ylim = c(0,2500), pch = 16, cex = 0.4, 
     xlab = "Stimulus Level", ylab = "Reaction Time (ms)", 
     col = "light grey", cex.lab = opis, cex.axis = opis, 
     main = "a                                                                                                       "
)

lines(low, mean_RT, lty = 3)
#lines(low, correct_RT, col = "blue")
#lines(low, error_RT, col = "red")

sd_RT <- c(sd(RT_1), sd(RT_2), sd(RT_3), sd(RT_4), sd(RT_5), 
           sd(RT_6), sd(RT_7), sd(RT_8), sd(RT_9), sd(RT_10))

points(low, mean_RT, pch = 19, cex = opis)

for(d in 1:10) {
  lines(c(low[d], low[d]), c(mean_RT[d] - sd_RT[d], mean_RT[d] + sd_RT[d]), lwd = 1)
}
points(low, mean_RT - sd_RT, cex = 1.2, pch = 45, col = "black")
points(low, mean_RT + sd_RT, cex = 1.2, pch = 45, col = "black")


legend(8, 2500, legend = c("Mean", "SD"), 
       pch = c(19, NaN), lty = c(3,1), lwd = c(1,1), cex = opis, bty = "n")

round(mean_RT)
median_RT
round(sd_RT)
correct_RT
error_RT
diff_RT <- error_RT - correct_RT
diff_RT
mean(diff_RT)
round(diff_RT)
min_RT


#--------------
# Correct & Errors

cdo_wykresu <- c(rep(low[1], ccdatasize_level[1]),
                rep(low[2], ccdatasize_level[2]),
                rep(low[3], ccdatasize_level[3]),
                rep(low[4], ccdatasize_level[4]),
                rep(low[5], ccdatasize_level[5]),
                rep(low[6], ccdatasize_level[6]),
                rep(low[7], ccdatasize_level[7]),
                rep(low[8], ccdatasize_level[8]),
                rep(low[9], ccdatasize_level[9]),
                rep(low[10], ccdatasize_level[10])) 


edo_wykresu <- c(rep(low[1], ecdatasize_level[1]),
                 rep(low[2], ecdatasize_level[2]),
                 rep(low[3], ecdatasize_level[3]),
                 rep(low[4], ecdatasize_level[4]),
                 rep(low[5], ecdatasize_level[5]),
                 rep(low[6], ecdatasize_level[6]),
                 rep(low[7], ecdatasize_level[7]),
                 rep(low[8], ecdatasize_level[8]),
                 rep(low[9], ecdatasize_level[9]),
                 rep(low[10], ecdatasize_level[10])) 

set.seed(123456)
cdo_wykresu <- cdo_wykresu + rnorm(sum(ccdatasize_level), 0, 0.15)
edo_wykresu <- edo_wykresu + rnorm(sum(ecdatasize_level), 0, 0.08)


plot(cdo_wykresu, c(cRT_1, cRT_2, cRT_3, cRT_4, cRT_5, cRT_6, cRT_7, cRT_8, cRT_9, cRT_10), 
     type = "p", xlim = c(1.5,11.5), ylim = c(0,2500), pch = 16, cex = 0.4, 
     xlab = "Stimulus Level", ylab = "Reaction Time (ms)", 
     col = "light blue", cex.lab = opis, cex.axis = opis, 
     main = "b                                                                                                       "
)

points(edo_wykresu, c(eRT_1, eRT_2, eRT_3, eRT_4, eRT_5, eRT_6, eRT_7, eRT_8, eRT_9, eRT_10), 
       pch = 17, cex = 0.3, col = "pink")

#plot(low, correct_RT, lty = 3, lwd = 1, 
#     type = "l", xlim = c(2,13), ylim = c(0,2500), 
#     xlab = "Stimulus Level", ylab = "Reaction Time (ms)", 
#     cex.lab = opis, cex.axis = opis, 
#     main = "b                                                                                                       "
#)

lines(low, correct_RT, lty = 3, lwd = 1, col = "blue")
lines(low, error_RT, lty = 3, lwd = 1, col = "red")

#lines(low, correct_RT, col = "blue")
#lines(low, error_RT, col = "red")

#sd_RT <- c(sd(RT_1), sd(RT_2), sd(RT_3), sd(RT_4), sd(RT_5), 
#           sd(RT_6), sd(RT_7), sd(RT_8), sd(RT_9), sd(RT_10),
#           sd(RT_11), sd(RT_12))

points(low, correct_RT, pch = 16, cex = opis, col = "blue")
points(low, error_RT, pch = 17, cex = opis, col = "red")

legend(4.5, 2500, "Mean RTs", cex = opis, bty = "n")

legend(5, 2250, legend = c("Correct Decisions", "Erroneous Decisions"), 
       pch = c(16, 17), lty = c(3,3), col = c("blue", "red"), cex = opis, bty = "n")

diff_RT
mean(diff_RT)
max(diff_RT)
min(diff_RT)

#---------------
# RT Distributions

kolors <- c("dark red", "red", "orange",  "green",
            "dark green", "light blue", 5, 4, "blue", "dark blue")

g <- 2

plot(density(RT_10[RT_10 > 0]), xlim = c(0,5000), ylim = c(0, 0.0021), col = kolors[10],
     xlab = "RT Distributions", ylab = "Density", 
     cex.lab = opis, cex.axis = opis, lwd = g,
     main = "c                                                                                                       "
)

lines(density(RT_9[RT_9 > 0]), col = kolors[9], lwd = g)
lines(density(RT_8[RT_8 > 0]), col = kolors[8], lwd = g)
lines(density(RT_7[RT_7 > 0]), col = kolors[7], lwd = g)
lines(density(RT_6[RT_6 > 0]), col = kolors[6], lwd = g)
lines(density(RT_5[RT_5 > 0]), col = kolors[5], lwd = g)
lines(density(RT_4[RT_4 > 0]), col = kolors[4], lwd = g)
lines(density(RT_3[RT_3 > 0]), col = kolors[3], lwd = g)
lines(density(RT_2[RT_2 > 0]), col = kolors[2], lwd = g)
lines(density(RT_1[RT_1 > 0]), col = kolors[1], lwd = g)

#wykres <- c("1.80 - 2.17", "2.17 - 2.61", "2,61 - 3.11", "3.11 - 3.63", "3.63 - 4.21", "4.21 - 4.87", "4.87 - 5.55", 
#            "5.55 - 6.33", "6.33 - 7.17", "7.17 - 8.17", "8.17 - 9.35", "9.35 - 10.87", "10.87 - 13.25")

legend(2100, 0.0021, "Stimulus Level", cex = opis, bty = "n")

wykres <- low[1:5]

legend(2400, 0.0019, legend = wykres, 
       lwd = c(2,2,2,2,2), col = kolors[1:5], cex = opis, bty = "n")

wykres <- low[6:10]

legend(3500, 0.0019, legend = wykres, 
       lwd = c(2,2,2,2,2), col = kolors[6:10], cex = opis, bty = "n")

#legend(3500, 0.003, legend = wykres, 
#       lwd = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2), col = kolors, cex = opis, bty = "n")



#---------------
# Scale Invariance

#w <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

#Q <- matrix(NA, nrow = 12, ncol = 9)
#correction <- numeric(12)

#Q[12, ] <- quantile((RT_12[RT_12 > 0])/median_RT[12], probs = w)
#Q[11, ] <- quantile((RT_11[RT_11 > 0])/median_RT[11], probs = w)
#Q[10, ] <- quantile((RT_10[RT_10 > 0])/median_RT[10], probs = w)
#Q[9, ] <- quantile((RT_9[RT_9 > 0])/median_RT[9], probs = w)
#Q[8, ] <- quantile((RT_8[RT_8 > 0])/median_RT[8], probs = w)
#Q[7, ] <- quantile((RT_7[RT_7 > 0])/median_RT[7], probs = w)
#Q[6, ] <- quantile((RT_6[RT_6 > 0])/median_RT[6], probs = w)
#Q[5, ] <- quantile((RT_5[RT_5 > 0])/median_RT[5], probs = w)
#Q[4, ] <- quantile((RT_4[RT_4 > 0])/median_RT[4], probs = w)
#Q[3, ] <- quantile((RT_3[RT_3 > 0])/median_RT[3], probs = w)
#Q[2, ] <- quantile((RT_2[RT_2 > 0])/median_RT[2], probs = w)
#Q[1, ] <- quantile((RT_1[RT_1 > 0])/median_RT[1], probs = w)

#Q

#for(i in 1:12) {
#  x <- Q[i, ]
#  y <- Q[7, ]
#  m <- lm(y ~ x - 1)
#  correction[i] <- m[["coefficients"]][["x"]]
#}

#correction <- numeric(12) + 1
#correction


#--------------------------
# Scaling based on K-S test

ilosc <- 2000
from <- 0.9
to <- 1.1
reference_RT <- RT_8[RT_8 > 0]/median_RT[8]
ks_c <- matrix(NA, nrow = ilosc, ncol = 10)

for(m in 1:ilosc) {
  correction <- from + m*(to-from)/ilosc
  
  ks_c[m, 1] <- as.numeric(ks.test((RT_1[RT_1 > 0]/median_RT[1])/correction, reference_RT)['statistic'])
  ks_c[m, 2] <- as.numeric(ks.test((RT_2[RT_2 > 0]/median_RT[2])/correction, reference_RT)['statistic'])
  ks_c[m, 3] <- as.numeric(ks.test((RT_3[RT_3 > 0]/median_RT[3])/correction, reference_RT)['statistic'])
  ks_c[m, 4] <- as.numeric(ks.test((RT_4[RT_4 > 0]/median_RT[4])/correction, reference_RT)['statistic'])
  ks_c[m, 5] <- as.numeric(ks.test((RT_5[RT_5 > 0]/median_RT[5])/correction, reference_RT)['statistic'])
  ks_c[m, 6] <- as.numeric(ks.test((RT_6[RT_6 > 0]/median_RT[6])/correction, reference_RT)['statistic'])
  ks_c[m, 7] <- as.numeric(ks.test((RT_7[RT_7 > 0]/median_RT[7])/correction, reference_RT)['statistic'])
  ks_c[m, 8] <- as.numeric(ks.test((RT_8[RT_8 > 0]/median_RT[8])/correction, reference_RT)['statistic'])
  ks_c[m, 9] <- as.numeric(ks.test((RT_9[RT_9 > 0]/median_RT[9])/correction, reference_RT)['statistic'])
  ks_c[m, 10] <- as.numeric(ks.test((RT_10[RT_10 > 0]/median_RT[10])/correction, reference_RT)['statistic'])

}

#ks_c

cor_m <- numeric(10)

for(k in 1:10) {
  min_KS <- min(ks_c[ ,k])
  for(m in 1:ilosc) {
    if(ks_c[m, k] == min_KS) { cor_m[k] <- from + m*(to-from)/ilosc }
  }
}

cor_m
min(cor_m)
max(cor_m)

ks.test((RT_1[RT_1 > 0]/median_RT[1])/cor_m[1], reference_RT)
ks.test((RT_2[RT_2 > 0]/median_RT[2])/cor_m[2], reference_RT)
ks.test((RT_3[RT_3 > 0]/median_RT[3])/cor_m[3], reference_RT)
ks.test((RT_4[RT_4 > 0]/median_RT[4])/cor_m[4], reference_RT)
ks.test((RT_5[RT_5 > 0]/median_RT[5])/cor_m[5], reference_RT)
ks.test((RT_6[RT_6 > 0]/median_RT[6])/cor_m[6], reference_RT)
ks.test((RT_7[RT_7 > 0]/median_RT[7])/cor_m[7], reference_RT)
ks.test((RT_8[RT_8 > 0]/median_RT[8])/cor_m[8], reference_RT)
ks.test((RT_9[RT_9 > 0]/median_RT[9])/cor_m[9], reference_RT)
ks.test((RT_10[RT_10 > 0]/median_RT[10])/cor_m[10], reference_RT)

suma <- as.numeric(ks.test((RT_1[RT_1 > 0]/median_RT[1])/cor_m[1], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_2[RT_2 > 0]/median_RT[2])/cor_m[2], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_3[RT_3 > 0]/median_RT[3])/cor_m[3], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_4[RT_4 > 0]/median_RT[4])/cor_m[4], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_5[RT_5 > 0]/median_RT[5])/cor_m[5], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_6[RT_6 > 0]/median_RT[6])/cor_m[6], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_7[RT_7 > 0]/median_RT[7])/cor_m[7], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_8[RT_8 > 0]/median_RT[8])/cor_m[8], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_9[RT_9 > 0]/median_RT[9])/cor_m[9], reference_RT)['statistic']) +
  as.numeric(ks.test((RT_10[RT_10 > 0]/median_RT[10])/cor_m[10], reference_RT)['statistic'])

suma

#---------------

#install.packages("goftest")
library(goftest)
#?goftest


tRT_1 <- (RT_1[RT_1 > 0]/median_RT[1])/cor_m[1]
tRT_2 <- (RT_2[RT_2 > 0]/median_RT[2])/cor_m[2]
tRT_3 <- (RT_3[RT_3 > 0]/median_RT[3])/cor_m[3]
tRT_4 <- (RT_4[RT_4 > 0]/median_RT[4])/cor_m[4]
tRT_5 <- (RT_5[RT_5 > 0]/median_RT[5])/cor_m[5]
tRT_6 <- (RT_6[RT_6 > 0]/median_RT[6])/cor_m[6]
tRT_7 <- (RT_7[RT_7 > 0]/median_RT[7])/cor_m[7]
tRT_8 <- (RT_8[RT_8 > 0]/median_RT[8])/cor_m[8]
tRT_9 <- (RT_9[RT_9 > 0]/median_RT[9])/cor_m[9]
tRT_10 <- (RT_10[RT_10 > 0]/median_RT[10])/cor_m[10]



#-----------
# Wykres



plot(density((RT_10[RT_10 > 0]/median_RT[10])/cor_m[10]), xlim = c(0,3), ylim = c(0, 1.2), col = kolors[10],
     xlab = "Rescaled RT Distributions", ylab = "Density", 
     cex.lab = opis, cex.axis = opis, lwd = g,
     main = "d                                                                                                       "
)

lines(density((RT_9[RT_9 > 0]/median_RT[9])/cor_m[9]), col = kolors[9], lwd = g)
lines(density((RT_8[RT_8 > 0]/median_RT[8])/cor_m[8]), col = kolors[8], lwd = g)
lines(density((RT_7[RT_7 > 0]/median_RT[7])/cor_m[7]), col = kolors[7], lwd = g)
lines(density((RT_6[RT_6 > 0]/median_RT[6])/cor_m[6]), col = kolors[6], lwd = g)
lines(density((RT_5[RT_5 > 0]/median_RT[5])/cor_m[5]), col = kolors[5], lwd = g)
lines(density((RT_4[RT_4 > 0]/median_RT[4])/cor_m[4]), col = kolors[4], lwd = g)
lines(density((RT_3[RT_3 > 0]/median_RT[3])/cor_m[3]), col = kolors[3], lwd = g)
lines(density((RT_2[RT_2 > 0]/median_RT[2])/cor_m[2]), col = kolors[2], lwd = g)
lines(density((RT_1[RT_1 > 0]/median_RT[1])/cor_m[1]), col = kolors[1], lwd = g)


legend(1.3, 1.2, "Stimulus Level", cex = opis, bty = "n")

wykres <- low[1:5]

legend(1.5, 1.08, legend = wykres, 
       lwd = c(2,2,2,2,2), col = kolors[1:5], cex = opis, bty = "n")

wykres <- low[6:10]

legend(2.2, 1.08, legend = wykres, 
       lwd = c(2,2,2,2,2), col = kolors[6:10], cex = opis, bty = "n")



#------------------
# Skewness
# Use correction for reference level 8

#sRT_1 <- (RT_1[RT_1 > 0]/median_RT[1])/cor_m[1]
#sRT_2 <- (RT_2[RT_2 > 0]/median_RT[2])/cor_m[2]
#sRT_3 <- (RT_3[RT_3 > 0]/median_RT[3])/cor_m[3]
#sRT_4 <- (RT_4[RT_4 > 0]/median_RT[4])/cor_m[4]
#sRT_5 <- (RT_5[RT_5 > 0]/median_RT[5])/cor_m[5]
#sRT_6 <- (RT_6[RT_6 > 0]/median_RT[6])/cor_m[6]
#sRT_7 <- (RT_7[RT_7 > 0]/median_RT[7])/cor_m[7]
#sRT_8 <- (RT_8[RT_8 > 0]/median_RT[8])/cor_m[8]
#sRT_9 <- (RT_9[RT_9 > 0]/median_RT[9])/cor_m[9]
#sRT_10 <- (RT_10[RT_10 > 0]/median_RT[10])/cor_m[10]
#sRT_11 <- (RT_11[RT_11 > 0]/median_RT[11])/cor_m[11]
#sRT_12 <- (RT_12[RT_12 > 0]/median_RT[12])/cor_m[12]

sRT_1 <- tRT_1
sRT_2 <- tRT_2
sRT_3 <- tRT_3
sRT_4 <- tRT_4
sRT_5 <- tRT_5
sRT_6 <- tRT_6
sRT_7 <- tRT_7
sRT_8 <- tRT_8
sRT_9 <- tRT_9
sRT_10 <- tRT_10

library(e1071)


skew_est <- numeric(10)
skew_est[1] <- skewness(sRT_1, type = 2)
skew_est[2] <- skewness(sRT_2, type = 2)
skew_est[3] <- skewness(sRT_3, type = 2)
skew_est[4] <- skewness(sRT_4, type = 2)
skew_est[5] <- skewness(sRT_5, type = 2)
skew_est[6] <- skewness(sRT_6, type = 2)
skew_est[7] <- skewness(sRT_7, type = 2)
skew_est[8] <- skewness(sRT_8, type = 2)
skew_est[9] <- skewness(sRT_9, type = 2)
skew_est[10] <- skewness(sRT_10, type = 2)

skew_est

b_iterations <- 10000
#b_iterations <- 10

skew_vector <- matrix(NA, nrow = b_iterations, ncol = 10)

set.seed(123456)

for(i in 1:b_iterations){
  index <- sample(cdatasize_level[1], cdatasize_level[1], replace = TRUE)
  sub_RT_1 <- sRT_1[index]
  skew_vector[i, 1]<- skewness(sub_RT_1, type = 2)
  index <- sample(cdatasize_level[2], cdatasize_level[2], replace = TRUE)
  sub_RT_2 <- sRT_2[index]
  skew_vector[i, 2]<- skewness(sub_RT_2, type = 2)
  index <- sample(cdatasize_level[3], cdatasize_level[3], replace = TRUE)
  sub_RT_3 <- sRT_3[index]
  skew_vector[i, 3]<- skewness(sub_RT_3, type = 2)
  index <- sample(cdatasize_level[4], cdatasize_level[4], replace = TRUE)
  sub_RT_4 <- sRT_4[index]
  skew_vector[i, 4]<- skewness(sub_RT_4, type = 2)
  index <- sample(cdatasize_level[5], cdatasize_level[5], replace = TRUE)
  sub_RT_5 <- sRT_5[index]
  skew_vector[i, 5]<- skewness(sub_RT_5, type = 2)
  index <- sample(cdatasize_level[6], cdatasize_level[6], replace = TRUE)
  sub_RT_6 <- sRT_6[index]
  skew_vector[i, 6]<- skewness(sub_RT_6, type = 2)
  index <- sample(cdatasize_level[7], cdatasize_level[7], replace = TRUE)
  sub_RT_7 <- sRT_7[index]
  skew_vector[i, 7]<- skewness(sub_RT_7, type = 2)
  index <- sample(cdatasize_level[8], cdatasize_level[8], replace = TRUE)
  sub_RT_8 <- sRT_8[index]
  skew_vector[i, 8]<- skewness(sub_RT_8, type = 2)
  index <- sample(cdatasize_level[9], cdatasize_level[9], replace = TRUE)
  sub_RT_9 <- sRT_9[index]
  skew_vector[i, 9]<- skewness(sub_RT_9, type = 2)
  index <- sample(cdatasize_level[10], cdatasize_level[10], replace = TRUE)
  sub_RT_10 <- sRT_10[index]
  skew_vector[i, 10]<- skewness(sub_RT_10, type = 2)

}

boot_skew <- numeric(10)
low_skew <- numeric(10)
high_skew <- numeric(10)

for(l in 1:10) {
  boot_skew[l] <- mean(skew_vector[, l])
  low_skew[l] <- quantile(skew_vector[, l], 0.025, na.rm = TRUE)
  high_skew[l] <- quantile(skew_vector[, l], 0.975, na.rm = TRUE)
}

skew_est
boot_skew
low_skew
high_skew

plot(low, skew_est, 
     type = "l", xlim = c(1.5,11.5), ylim = c(0,4), lty = 2,  
     xlab = "Stimulus Intensity", ylab = "Skewness of RT Distributions", 
     cex.lab = opis, cex.axis = opis, 
     main = "e                                                                                                       "
)

points(low, skew_est, pch = 19, cex = opis)

for(d in 1:10) {
  lines(c(low[d], low[d]), c(low_skew[d], high_skew[d]), lwd = 1)
}
points(low, low_skew, cex = 1.2, pch = 45, col = "black")
points(low, high_skew, cex = 1.2, pch = 45, col = "black")


legend(2.5, 4, legend = c("Skewness", "95% CI"), 
       pch = c(19, NaN), lty = c(2,1), lwd = c(1,1), cex = opis, bty = "n")


#------------------
# Cullen and Frey Graph

# Kurtosis as implemented in R: Delignette-Muller and Dutang, 2015

Kurt <- function(x) {
  n <- length(x)
  m2 <- (1/n)*sum((x - mean(x))^2)
  m4 <- (1/n)*sum((x - mean(x))^4)
  estym <- (n-1)/((n-2)*(n-3))*((n+1)*m4/(m2^2)-3*(n-1)) + 3
  return(estym)
}


library(fitdistrplus)
descdist(RT_8)

kurt_est <- numeric(10)
kurt_est[1] <- Kurt(tRT_1)
kurt_est[2] <- Kurt(tRT_2)
kurt_est[3] <- Kurt(tRT_3)
kurt_est[4] <- Kurt(tRT_4)
kurt_est[5] <- Kurt(tRT_5)
kurt_est[6] <- Kurt(tRT_6)
kurt_est[7] <- Kurt(tRT_7)
kurt_est[8] <- Kurt(tRT_8)
kurt_est[9] <- Kurt(tRT_9)
kurt_est[10] <- Kurt(tRT_10)


square <- skew_est^2
square
kurt_est

#line for lognormal distribution

s <- numeric(1000)
k <- numeric(1000)
for(j in 1:1000) {
  y <- sqrt(exp((j/1000)^2)-1)
  s[j] <- (3*y+y^2)^2
  k[j] <- 16*y^2+15*y^4+6*y^6+y^8+3
}

#line for ex-Gaussian distribution

ss <- numeric(1000)
kk <- numeric(1000)
for(l in 1:1000) {
  y <- 1+log(1/l)
  ss[l] <- ((2/(y^3))*((1+1/(y^2))^(-3/2)))^2
  kk[l] <- 3*(1+2/(y^2)+3/(y^4))/((1+1/(y^2))^2)
}


#wykres


plot(c(4, 0, 0, 0), c(9, 1.5, 3, 4.5), pch = c(7, 2, 8, 3),
     type = "p", xlim = c(0,13), ylim = c(25,1), 
     xlab = "Square of Skewness", ylab = "Kurtosis", cex = 1.8, 
     cex.lab = opis, cex.axis = opis,
     main = "f                                                                                                       "
)

polygon(c(0,0,24,24), c(1,3,39,25), density = 1000, angle = 30,
        border = NULL, col = "light grey", lty = par("lty"))

lines(c(0, 24), c(3, 39), lty = 2)
lines(s, k, lty = 3)
#lines(ss, kk, lty = 4)

points(square, kurt_est, col = kolors, pch = 19, cex = 1.5)
points(c(4, 0, 0, 0), c(9, 1.5, 3, 4.5), pch = c(7, 2, 8, 3), cex = 1.8)

wykres <- low[1:5]

legend(9, 0, legend = wykres, 
       pch = 19, col = kolors[1:5], cex = opis, bty = "n")

wykres <- low[6:10]

legend(11, 0, legend = wykres, 
       pch = 19, col = kolors[6:10], cex = opis, bty = "n")

legend(0, 12, legend = c("normal", "uniform", "exponential", "logistic", "beta", "gamma", "lognormal"), 
       pch = c(8, 2, 7, 3, 15, NA, NA), lty = c(NA, NA, NA, NA, NA, 2, 3), 
       col = c(1, 1, 1, 1, "dark grey", 1, 1), cex = 1.1, bty = "n")


# ---------------------------------------------
# Which reference distribution is best

sum_cor <- numeric(10)

ilosc <- 2000
from <- 0.9
to <- 1.1
ks_c <- matrix(NA, nrow = ilosc, ncol = 10)

for(i in 1:12) {
  if(i == 1) { reference_RT <- RT_1[RT_1 > 0]/median_RT[i] }
  if(i == 2) { reference_RT <- RT_2[RT_2 > 0]/median_RT[i] }
  if(i == 3) { reference_RT <- RT_3[RT_3 > 0]/median_RT[i] }
  if(i == 4) { reference_RT <- RT_4[RT_4 > 0]/median_RT[i] }
  if(i == 5) { reference_RT <- RT_5[RT_5 > 0]/median_RT[i] }
  if(i == 6) { reference_RT <- RT_6[RT_6 > 0]/median_RT[i] }
  if(i == 7) { reference_RT <- RT_7[RT_7 > 0]/median_RT[i] }
  if(i == 8) { reference_RT <- RT_8[RT_8 > 0]/median_RT[i] }
  if(i == 9) { reference_RT <- RT_9[RT_9 > 0]/median_RT[i] }
  if(i == 10) { reference_RT <- RT_10[RT_10 > 0]/median_RT[i] }

  for(m in 1:ilosc) {
    correction <- from + m*(to-from)/ilosc
    
    ks_c[m, 1] <- as.numeric(ks.test((RT_1[RT_1 > 0]/median_RT[1])/correction, reference_RT)['statistic'])
    ks_c[m, 2] <- as.numeric(ks.test((RT_2[RT_2 > 0]/median_RT[2])/correction, reference_RT)['statistic'])
    ks_c[m, 3] <- as.numeric(ks.test((RT_3[RT_3 > 0]/median_RT[3])/correction, reference_RT)['statistic'])
    ks_c[m, 4] <- as.numeric(ks.test((RT_4[RT_4 > 0]/median_RT[4])/correction, reference_RT)['statistic'])
    ks_c[m, 5] <- as.numeric(ks.test((RT_5[RT_5 > 0]/median_RT[5])/correction, reference_RT)['statistic'])
    ks_c[m, 6] <- as.numeric(ks.test((RT_6[RT_6 > 0]/median_RT[6])/correction, reference_RT)['statistic'])
    ks_c[m, 7] <- as.numeric(ks.test((RT_7[RT_7 > 0]/median_RT[7])/correction, reference_RT)['statistic'])
    ks_c[m, 8] <- as.numeric(ks.test((RT_8[RT_8 > 0]/median_RT[8])/correction, reference_RT)['statistic'])
    ks_c[m, 9] <- as.numeric(ks.test((RT_9[RT_9 > 0]/median_RT[9])/correction, reference_RT)['statistic'])
    ks_c[m, 10] <- as.numeric(ks.test((RT_10[RT_10 > 0]/median_RT[10])/correction, reference_RT)['statistic'])

  }
  
  cor_m <- numeric(10)
  index <- numeric(10)
  
  for(k in 1:10) {
    min_KS <- min(ks_c[ ,k])
    for(m in 1:ilosc) {
      if(ks_c[m, k] == min_KS) { 
        cor_m[k] <- from + m*(to-from)/ilosc
        index[k] <- m
      }
    }
  }
  
  for(k in 1:10) {
    sum_cor[i] <- ks_c[index[k], k] + sum_cor[i]
  }
  
}

sum_cor

