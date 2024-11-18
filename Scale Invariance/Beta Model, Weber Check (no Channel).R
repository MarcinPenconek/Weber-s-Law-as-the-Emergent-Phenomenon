#-------------------------------------------------------------#
#                  Neural Network Decison Model               #
#                        Marcin Penconek                      #
#-------------------------------------------------------------#

#Beta model

# No Channel
# Time Bins: 30 ms

# density: 0.55 i 0.36

# fixed firing 
# 0.07
# 0.006



#----------------------------------------------
# SECTION 1: Functions
#----------------------------------------------

Grouping_variable = function() {
  
  size <- n_groups*n_size + N_size
  ggroups <- c(rep(1:n_groups, each = n_size, len = n_groups*n_size), rep(n_groups + 1, len = N_size))
  groups <- matrix(0, nrow = size, ncol = n_groups + 2)
  groups[ ,1] <- ggroups
  for(i in 1:size) {
    groups[i, ggroups[i] + 1] <- 1
  }
  return(groups)
}


#----------------------------------------------
# Defining network connections


Network_definition = function() {
  
  network_connections <- matrix(FALSE, nrow = size, ncol = size)
  u <- runif(size*size, min = 0, max = 1)
  
  for(i in 1:size) {
    for(j in 1:size) {
      if((groups[i, 1] == n_groups + 1) & (groups[j, 1] == n_groups + 1)) {
        if(u[size*(i-1) + j] < N_density) {
          network_connections[i, j] <- TRUE
        }
      } else if(groups[i, 1] == groups[j, 1]) {
        if(u[size*(i-1) + j] < n_density) {
          network_connections[i, j] <- TRUE
        }
      } else {
        if(u[size*(i-1) + j] < n_to_N_density) {
          network_connections[i, j] <- TRUE
        }
      }
    }
  }
  return(network_connections)
}



#----------------------------------------------
# Initiating the network with random inputs

Network_init = function() {
  
  prob_s <- runif(size, min = 0, max = 1)
  t <- rexp(size, rate = r_default)
  s <- logical(size)
  
  for(i in 1:size) {
    if(prob_s[i] < inhibition_level) {
      s[i] <- TRUE
      t[i] <- rexp(1, rate = r_active)
    }
    else {
      s[i] <- FALSE
    }
  }
  state <- matrix(0, nrow = size, ncol = 2)
  state[ ,1] <- s
  state[ ,2] <- t
  
  return(state)
}

#-----------------------------------------------
# Defining stimulus (time-random) - VERSION #4 (n_A, n_B can be non-integer)

Stimulus_definition_t_random = function(duration, input_A, input_B) {
  
  stimulus <- matrix(0, nrow = duration, ncol = size)
  
  connect_A <- rep(1, len = to_A)
  connect_B <- rep(1, len = to_B)
  
  r_A <- rpois(duration, lambda = input_A)
  r_B <- rpois(duration, lambda = input_B)
  
  for(k in 1:duration) {
    stimulus[k, ] <- c(connect_A*r_A[k], numeric(n_size - to_A), 
                       connect_B*r_B[k], numeric(n_size - to_B), numeric(size - 2*n_size))
  }
  return(stimulus)
}


#----------------------------------------
# Network evolution function

Network_evolution = function() {
  
  AB_max <- 0
  max_level <- 80
  current_time <- 0

  current_time <- 0
  current_stimulus <- numeric(size)
  stimulus_finished <- 0

  
  s <- state[ ,1]
  t <- state[ ,2]
  k <- 1
  
  time_vector <- rexp(number_of_iterations, rate = r_default)
  simulation <- matrix(0, nrow = number_of_iterations, ncol = (1 + n_groups + 1 + 4))
  
  j <- 0
  while(j < number_of_iterations & AB_max < max_level) {
    
    j <- j + 1
    
    # Defining the neuron with the lowest t
    # Changing its state based on the activity of other neurons it is connected to
    
    current_time <- min(t)
    for(i in 1:size) if(t[i] == current_time) { Min_i <- i }
    
    # Stimulus
    
    if(current_time > event_starts & current_time < event_ends) {
      current_stimulus <- stimulus[k,]
      k <- round((current_time - event_starts)/time_bins) + 1
    }
    
    if(stimulus_finished == 0 & current_time > event_ends) {
      current_stimulus <- numeric(size)
      stimulus_finished <- 1
    }
    
    # Evolution
    
    
    active_groups <- numeric(n_groups + 1)
    for(i in 1:(n_groups + 1)) {
      active_groups[i] <- sum(s*groups[ ,i + 1])
    }
    
    simulation[j, ] <- c(current_time, active_groups, Min_i, 0, 0, 0)
    AB_max <- max(active_groups[1], active_groups[2])
    
    active <- sum(s)    
    cutoff <- (active/size)^2/inhibition_level
    
    active_connections <- sum(s*network_connections[Min_i, ])
    all_connections <- sum(network_connections[Min_i, ])
    
    change_factor <- (current_stimulus[Min_i] + active_connections)/(current_stimulus[Min_i] + all_connections)
    
    if(change_factor > cutoff) {
      s[Min_i] <- 1
      t[Min_i] <- current_time + 1/r_active
    } else {
      s[Min_i] <- 0
      t[Min_i] <- current_time + time_vector[j]
    }
    simulation[j, 6] <- s[Min_i]*(Min_i <= 100)
    simulation[j, 7] <- s[Min_i]*((Min_i > 100) & (Min_i <= 200))
    simulation[j, 8] <- s[Min_i]*(Min_i > 200)
  }
  simulation <- subset(simulation, simulation[ ,1] != 0)
  return(simulation)
}

#----------------------------------------------
# Visualization
  
Population_plot <- function(x, y1, y2, y3) {
  

  plot(x, y3, ylim = c(0,100), xlim = c(0,6000), type = "l",  
       #xlab = "Time (ms)", ylab = "% active", 
       xlab = "", ylab = "", 
       cex.main = 1.5, cex.axis = 1.2)
  lines(x, y1, type = "l", col = "red", lwd=1.5)
  lines(x, y2, type = "l", col = "blue", lwd=1.5)
  if(decision[5] != 0) {abline(v = decision[2])}
  if(n_A1 + n_A2 +n_A3 + n_B1 + n_B2 + n_B3 >0) {
    abline(v = event_starts, lwd=1, lty=2)
    abline(v = event_ends, lwd=1, lty=2)
  } else {
      legend(200, 100, legend = c("A", "B"), cex = 1.5, lwd=c(3,3), col=c("red","blue"), bty = "n")
    }
}


#----------------------------------------------
# Analysis of a decision

Freq_Analysis <- function(beg_index, end_index) {
  
  A_count <- sum(simulation[beg_index:end_index, 6])
  B_count <- sum(simulation[beg_index:end_index, 7])
  time_int <- (simulation[end_index, 1] - simulation[beg_index, 1])/1000
  
  return(c((A_count/100)/time_int, (B_count/100)/time_int, time_int))
}

#----------------------------------
# Decision function for the reaction-time paradigm
# bound = 50Hz

Which_wins_when <- function(bound) {
  
  bound_level <- 0
  bound_time <- 0
  pool_A <- 0
  pool_B <- 0
  which_wins <- 0
  
  b <- start_b
  if(b < length(simulation[, 1])) { 
    freq <- Freq_Analysis(b - updates_per_30ms, b)
    } else { freq <- c(0,0,0) }
  
  while(max(freq[1], freq[2]) < bound & b + freq_step < length(simulation[, 1])) {
    b <- b + freq_step
    freq <- Freq_Analysis(b - updates_per_30ms, b)
  }
  
  if(b + freq_step + 1 < length(simulation[, 1])) { 
    bound_level <- max(freq[1], freq[2])
    bound_time <- simulation[b, 1]
    pool_A <- freq[1]
    pool_B <- freq[2]
    which_wins <- sign(freq[1] - freq[2])
  }
  
  return(c(bound_level, bound_time, pool_A, pool_B, which_wins))
}



#----------------------------------------------
# SECTION 2.PARAMETERS
#----------------------------------------------

# Network Parameters

N_size <- 800
n_groups <- 2
n_size <- 100
size <- n_groups*n_size + N_size

n_density <- 0.55
n_to_N_density <- 0.36
N_density <- 0.36


#----------------------------------------------
# Global parameters

inhibition_level <- 0.13
r_default <- 0.006
r_active <- 0.07


#----------------------------------------------
# Simulation parameters

number_of_iterations <- 100000
threshold_level <- 50

to_A <- 50
to_B <- 50

event_starts <- 500
event_ends <- 5500

start_b <- round(event_starts*15)

#----------------------------------------------
# Stimulus parameters

time_bins <- 30
duration <- round((event_ends - event_starts)/time_bins + 1)


#-----------------------------------
# Parameters for Frequency Analysis

updates_per_30ms <- 440
freq_step <- 145



#------------------------------------------------
# SECTION 3: Running the model
#------------------------------------------------

#------------------------------------------------
# APPENDIX: MODEL RUNS

#set.seed(1234) 


#-----------------------------------------------


#for(i in 1:1) {
  
#  groups <- Grouping_variable()
#  network_connections <- Network_definition()
#  state <- Network_init()
#  s <- state[ ,1]
#  t <- state[, 2]
  
#  intensity_A <- Expected_intensity_A(duration)
#  channel_A <- Channel_evolution(intensity_A)
  
#  intensity_B <- Expected_intensity_B(duration)
#  channel_B <- Channel_evolution(intensity_B)
  
#  simulation <- Network_evolution()
#  decision <- Which_wins_when(threshold_level)
  
#  Population_plot(simulation[ ,1], simulation[ ,2], simulation[ ,3], simulation[ ,4]*(100/N_size))
  
#}


#------------------------------------------------
# SECTION 4: Generating a sample of simulations
#------------------------------------------------


bound_30 <- 0
dec_time_30 <- 0
freq_A_30 <- 0
freq_B_30 <- 0
which_won_30 <- 0

bound_40 <- 0
dec_time_40 <- 0
freq_A_40 <- 0
freq_B_40 <- 0
which_won_40 <- 0

bound_50 <- 0
dec_time_50 <- 0
freq_A_50 <- 0
freq_B_50 <- 0
which_won_50 <- 0

n_A <- 0
n_B <- 0

over_time_A <- 0
over_time_B <- 0


dane <- data.frame(N_size, n_groups, n_size, n_density, n_to_N_density, N_density, 
          inhibition_level, r_default, r_active, number_of_iterations, threshold_level, time_bins, 
          to_A, to_B, n_A, n_B, over_time_A, over_time_B, event_starts, event_ends, 
          bound_50, dec_time_50, freq_A_50, freq_B_50, which_won_50)

groups <- Grouping_variable()


# sequence: 2-13
#-------------

# 2.61  3.85  4.98  6.21  7.29  8.53  9.57 11.01 12.27 13.83 15.51 18.19
# 30 Hz: 2 vs. 2.88; 40Hz: 3 vs 3.90


cases <- 2000

n_A <- 2
n_B <- 2.88


for(i in 1:cases) {

  network_connections <- Network_definition()
  state <- Network_init()
  s <- state[ ,1]
  t <- state[, 2]

  stimulus <- Stimulus_definition_t_random(duration, n_A, n_B)
  
  over_time_A <- mean(stimulus[ ,1:100])
  over_time_B <- mean(stimulus[ ,101:200])
  
  simulation <- Network_evolution()
  
  dec_50 <- Which_wins_when(50)
#  dec_30 <- Which_wins_when(30)
  
  dane[i, ] <- c(N_size, n_groups, n_size, n_density, n_to_N_density, N_density, 
                 inhibition_level, r_default, r_active, number_of_iterations, threshold_level, time_bins, 
                 to_A, to_B, n_A, n_B, over_time_A, over_time_B, event_starts, event_ends, 
                 dec_50)
  
  if(i == 1000 | i == 2000 | i == 3000 | i == 4000 | i == 5000) { timestamp() }

}


write.csv2(dane,'Check_2000_.csv')



#--------------------

sum(dane$which_won_50 == 0)
a <- sum(dane$which_won_50 == 1)
b <- sum(dane$which_won_50 == -1)

b/(a+b)


