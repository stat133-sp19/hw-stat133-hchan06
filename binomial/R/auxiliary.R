# title aux_mean
# description calculates the mean of the binomial distribution
# param trials numeric vector
# param prob numeric vector
# return value
aux_mean <- function(trials, prob) {
  return(trials*prob)
}


# title aux_variance 
# description calculates the variance of the binomial distribution
# param trials numeric vector
# param prob numeric vector
# return value variance of the binomial distribution
aux_variance <- function(trials, prob) {
  return(aux_mean(trials, prob)*(1 - prob))
}


# title aux_mode
# description calculates the mode of the binomial distribution
# param trials numeric vector
# param prob numeric vector
# return value the mode of the binomial distribution
aux_mode <- function(trials, prob) {
  return(as.integer(aux_mean(trials, prob)+prob))
}


# title bin_skewness
# description calculates the skewness of the binomial distribution
# param trials numeric vector
# param prob numeric vector
# return value the skewness of the binomial distribution
aux_skewness <- function(trials, prob) {
  return((1-2*prob)/sqrt(aux_variance(trials, prob)))
}


# title bin_kurtosis
# description calculates the kurtosis of the binomial distribution
# param trials numeric vector
# param prob numeric vector
# return value the kurtosis of the binomial distribution
aux_kurtosis <- function(trials, prob) {
  return((1 -6*prob*(1-prob))/aux_variance(trials, prob))
}