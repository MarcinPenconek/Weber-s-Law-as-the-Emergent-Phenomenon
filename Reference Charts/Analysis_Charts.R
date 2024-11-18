
#----------------------
# Analysis of Weber's Law
# Marcin Penconek
#----------------------

# Experimental data from the paper Jesteadt et al., 1977

opis <- 1.2

dII_8000 <- c(0.731, 0.393, 0.299, 0.406, 0.132)
dII_4000 <- c(0.455, 0.262, 0.257, 0.236, 0.124)
dII_2000 <- c(0.419, 0.347, 0.393, 0.298, 0.112)
dII_1000 <- c(0.503, 0.425, 0.354, 0.250, 0.099)
dII_800 <- c(0.506, 0.339, 0.326, 0.227, 0.107)
dII_600 <- c(0.414, 0.357, 0.374, 0.190, 0.128)
dII_400 <- c(0.406, 0.401, 0.319, 0.266, 0.132)
dII_200 <- c(0.407, 0.357, 0.269, 0.228, NA)

dB_levels <- c(5, 10, 20, 40, 80)

plot(dB_levels, dII_8000,  
     type = "p", xlim = c(0,90), ylim = c(0,0.8), 
     pch = 5, cex = opis, 
     xlab = "Stimulus Intensity (dB)", ylab = "Weber Fractions", 
     cex.lab = opis, cex.axis = opis, 
     main = "b                                                                                                       "
)

points(dB_levels, dII_4000, pch = 1, cex = opis)
points(dB_levels, dII_2000, pch = 0, cex = opis)
points(dB_levels, dII_1000, pch = 2, cex = opis)
points(dB_levels, dII_800, pch = 6, cex = opis)
points(dB_levels, dII_600, pch = 18, cex = opis)
points(dB_levels, dII_400, pch = 16, cex = opis)
points(dB_levels, dII_200, pch = 15, cex = opis)

lines(dB_levels, dII_8000, lty = 3)
lines(dB_levels, dII_4000, lty = 3)
lines(dB_levels, dII_2000, lty = 3)
lines(dB_levels, dII_1000, lty = 3)
lines(dB_levels, dII_800, lty = 3)
lines(dB_levels, dII_600, lty = 3)
lines(dB_levels, dII_400, lty = 3)
lines(dB_levels, dII_200, lty = 3)


#all_lev <- rep(dB_levels, 8)
#all_data <- log(c(dII_200, dII_400, dII_600, dII_800, dII_1000, dII_2000, dII_4000, dII_8000))

#dBs <- c(5, 7, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80)

#lines(dBs, exp(-0.758074 + dBs*(-0.016911)))

#legend(60, 1, legend = c("8000 Hz", "4000 Hz", "2000 Hz", "1000 Hz", "800 Hz", "600 Hz", "400 Hz", "200 Hz"), 
#       pch = c(5, 1, 0, 2, 6, 18, 16, 15), cex = opis, bty = "n")

legend(-5, 0.1, legend = c("Jesteadt et al., 1977"), cex = opis, bty = "n")
legend(60, 0.8, legend = c("200 Hz", "400 Hz", "600 Hz", "800 Hz", "1000 Hz", "2000 Hz", "4000 Hz", "8000 Hz"), 
       pch = c(15, 16, 18, 6, 2, 0, 1, 5), cex = opis, bty = "n")


model <- lm(all_data ~ all_lev)
summary(model)



#a1 <- rep(36.3, 5)
#a2 <- rep(22.7, 5)
#a3 <- rep(16.3, 5)
#a4 <- rep(13.7, 5)
#a5 <- rep(14.7, 5)
#a6 <- rep(18, 5)
#a7 <- rep(20, 5)
#a8 <- rep(27, 5)

#correction <- c(a1, a2, a3, a4, a5, a6, a7, a8)
#cor_lev <- log(all_lev/correction ) # - correction)

#model <- lm(all_data ~ cor_lev)
#summary(model)



# Experimental data from the paper Hecht, 1924


dII_Aubert <- (0.7/11.5)*c(5.5, 4.2, 4.2, 2.1, 1.6, 0.75, 0.75, 0.75, 0.6, 0.5, 0.6, 0.65, 0.6, 0.5, 0.45, 0.3, 0.2, 0.15, 0.15, 0.15, 0.1)
I_Aubert <- (5/6.3)*c(-4.2, -4.1, -3.6, -3.3, -2.8, -2.3, -1.9, -1.7, -1.1, -0.8, -0.7, -0.3, 0, 0.2, 0.5, 0.95, 1.25, 1.75, 1.95, 2.25, 2.7) 

plot(I_Aubert, dII_Aubert,
     type = "p", xlim = c(-5, 4), ylim = c(0, 0.8), pch = 2, cex = opis, 
     xlab = "Log of Intensity (millilamberts)", ylab = "Weber Fractions", 
     cex.lab = opis, cex.axis = opis, 
     main = "a                                                                                                       "
)

dII_Koenig <- (0.7/11.5)*c(11.4, 7.9, 6.25, 4.7, 3.1, 2.1, 1.65, 1, 0.9, 0.75, 0.6, 0.55, 0.45, 0.5, 0.25, 0.25, 0.3, 0.25, 0.3, 0.25, 0.3, 0.5, 0.5, 0.7)
I_Koenig <- (5/6.3)*c(-5.4, -4.85, -4.45, -4.1, -3.5, -3.1, -2.7, -2.2, -1.85, -1.45, -1, -0.6, -0.1, 0.45, 0.8, 1.2, 1.7, 2.05, 2.4, 2.9, 3.3, 3.7, 4.2, 4.5)

points(I_Koenig, dII_Koenig, pch = 1, cex = opis)

dII_Brodhun <- (0.7/11.5)*c(10.85, 8.2, 5.5, 4.0, 2.8, 2.0, 1.55, 0.9, 0.6, 0.5, 0.4, 0.3, 0.3, 0.2, 0.4, 0.5, 0.6, 0.7)
I_Brodhun <- (5/6.3)*c(-5.4, -4.85, -4.4, -4.1, -3.5, -3.15, -2.7, -2.2, -1.5, -1, -0.6, 0, 0.45, 2.5, 2.9, 3.3, 3.7, 4.2)

points(I_Brodhun, dII_Brodhun, pch = 16, cex = opis)

dII_Blanchard <- (0.7/11.5)*c(9.75, 7.3, 5.6, 5.4, 2.6, 1.2, 0.75, 0.7, 0.5)
I_Blanchard <- (5/6.3)*c(-5.3, -4.8, -4.4, -4.0, -3.15, -2.2, -1.9, -1.4, -0.5)

points(I_Blanchard, dII_Blanchard, pch = 17, cex = opis)

line_y <- (0.7/11.5)*c(12, 9.75, 8.5, 8.1, 6.4, 5.75, 4.9, 3.1, 1.6, 1.3, 0.8, 0.5, 0.4, 0.3, 0.2, 0.15, 0.15, 0.2, 0.3, 0.4, 0.6, 0.8)
line_x <- (5/6.3)*c(-5.5, -5.2, -5.0, -4.9, -4.55, -4.4, -4.15, -3.5, -2.8, -2.4, -1.8, -1.2, -0.6, 0, 0.6, 1.2, 1.8, 2.4, 3, 3.6, 4.2, 4.8)

lines(line_x, line_y)

legend(-1, 0.8, legend = c("Aubert, 1865", "Koenig, 1889", "Brodhun, 1889", "Blanchard, 1918"), 
       pch = c(2, 1, 16, 17), cex = opis, bty = "n")

#legend(-1.5, 0.55, legend = c("after Hecht, 1924"), cex = opis, bty = "n")
