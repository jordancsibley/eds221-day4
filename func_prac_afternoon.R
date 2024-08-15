# # August 15, 2024 
# EDS 221 
# Afternoon lecture coding 

library(tidyverse)

# function to show ground water flow rate ----

# function 
gw_rate <- function(site) {
 
  if(!site %in% c("mountain","prairie", "desert", "beach")) {
    warning("site not included")   # warning message 
  }
  
   gw_depths <- data.frame(sitename = c("mountain",
                                       "prairie",
                                       "desert",
                                       "beach"),
                          depth = c(32, 41, 63, 2),
                          slope = c(11.2, 0.4, 0.8, 2.6))
  
  site_select <- filter(gw_depths, sitename == site)
  
  transport_rate <- 1.4 * site_select$slope + 3.6 * site_select$depth
  
  return(transport_rate)
}

gw_rate(site = "desert")




# logistic growth ----

# first create function that spits out Nt using logistic growth model 
# next create vector of times 
# write out an example pop_1 using population of vector indexing for time and inputting in the other variables with values 

# log growth function 
logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K - N0) / N0) * exp(-r * time))
  return(Nt)
}

# logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)  # seeing if function works 

time_vec <- seq(from = 0, to = 50, by = 0.1)  # creating vector for times 

# pop_1 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

pop_1_vec <- vector(mode = "numeric", length = length(time_vec)) # creating vector to store the population in the for loop that will be calculated from the function 

# for loop for pop_1
for (i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
  pop_1_vec[i] <- population
}

# data frame to plot this data 
pop_time_1 <- data.frame(time_vec, pop_1_vec)

# plotting the logistic growth of pop_1 over time_vec
ggplot(data = pop_time_1, aes(x = time_vec, y = pop_1_vec)) +
  geom_line()


# sequence of different growth rate values we want to test 
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

# create an output matrix 
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

# putting it all together to go through multiple time steps and multiple growth rate steps ----

for (i in seq_along(r_seq)) { # outer loop for growth rates 
  for (j in seq_along(time_vec)) { # inner loop of time steps 
    population <- logistic_growth(N0 = 100, K = 6000, r = r_seq[i], time = time_vec[j])
    out_matrix[j, i] <- population # remember that time is the rows and rates is the columns
  }
}

# wrangling to make it easier to plot ----

out_df <- data.frame(out_matrix, time= time_vec)
colnames(out_df) <- c(paste0("growth_rate_", r_seq), "time")

# tidy structure 
out_df_long <- out_df %>% 
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "population_size")
# we will learn more about this next week but cols = -time means don't get rid of this, names_to = growth rate means create column growth rate, and values_to = pop size makes it so the values in the df are now put in their own column that match up with their time and rate 


# plotting this 
ggplot(data = out_df_long, aes(x = time, y = population_size)) +
  geom_line(aes(color = growth_rate), show.legend = FALSE) +
  theme_minimal()

