# August 15, 2024 
# EDS 221 
# Morning lecture coding 


# nested for loop ----
file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1, 2, 3, 4, 5)

for (i in 1:length(file_prefix)) {
  for (j in 1:length(file_suffix)) {
    print(paste0(file_prefix[i], "_", file_suffix[j]))
  }
}

# if you dont add the second for loop it only does the prefixs 1 through 3.
# with the addition of the second for loop, it goes through each prefix and adds the five numbers (suffix)
# important to note for the second for loop you need a different letter/variable to be the iterator. 



# functions ----

# func ex 1 from slides ----

birddog_sum <- function(bird, dog) {
  pets <- bird + dog 
  return(pets)
}
# this now shows up in a functions tab in your global environment 

birddog_sum(bird = 2, dog = 5)

# gave me 7 
# can assign this to a object like 'x'


# func double any number we give it ----

double_it <- function(x) {
  print(2*x)
}

double_it(4)
double_it(1:4)

# func ex 2 from slides ----

exclaim_age<- function(age) {
  print(paste("I am", age, "years old!"))
}

exclaim_age(age = 10)
# I am 10 years old! 


# func comparing two values ----

find_max <- function(val1, val2) {
  if (val1 > val2) {
    return(val1)
  } else if (val2 > val1)
    return(val2)
}

find_max(7, 3)

# print versus return. In this case, either would work but it is best practice to use return. Using returns allow the answer of the function to show up in your global environment. 
# we have been using print in a learning setting, it is rarely used in scripts/workflow 


# quarter spilts for loop practice 

quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)

for (i in seq_along(quarter_splits)) {
  print(quarter_splits[i] + quarter_splits[i + 1])
}

# instead of seq_along we also could have used 1:length(quarter_splits)


# functions with conditionals ----

animal_age <- function(animal, age) {
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age(animal = "dog",age = 8) # 56
animal_age(animal = "dog", age = "yellow") # error bc yellow is non-numeric 
animal_age(animal = "cow", age = 5) # no print 


# function pulling info from data frame ----

dog_choice <- data.frame(dog_name = c("khora",
                                      "teddy",
                                      "waffle",
                                      "banjo"),
                         food = c("everything",
                                  "salmon",
                                  "pancakes",
                                  "chicken"))

library(tidyverse)

dog_menu <- function(name) {
  my_sub <- dog_choice %>% 
    filter(dog_name == name)
  print(paste("My name is", my_sub$dog_name, "and I like to eat", my_sub$food))
}

dog_menu("waffle")


# writing your own error messages ----

animal_age <- function(animal, age) {
  
  if (!animal %in% c("dog", "goat")) {    # error if any animal is not dog or goat
    stop("Oops! Animal must be a dog or goat")
  }  
  
  if (is.numeric(age) == FALSE) {  # error message 
    stop("The age must be a number")
  }
  
  if(age <= 0) {  # error must be a real positive age 
    stop("Age must be greater than 0")
  }
   
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}


# writing your own warning messages ----

# energy generated from a wind turbine 


calc_windpower <- function(rho, radius, windspeed) {
  
  if (windspeed > 130) {
    warning("wow, that's fast! are you sure?")
  }
  
  if (rho > 1.225) {
    warning("that air density is suspiscious. are you sure?")
  }
  
  if (radius < 0) {
    stop("rotor radius must be a positive value (meters)")
  }
  
  print(0.3*rho*pi*(radius^2)*(windspeed^3))
}

# lets test it out 
calc_windpower(2, -1, 50)




