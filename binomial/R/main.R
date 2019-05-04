source("R/checker.R")
source("R/auxiliary.R")

#' @title bin_choose
#' @description all possible combinations of k successes 
#' @param n numeric vector
#' @param k numeric vector
#' @return a numeric vctor of all the possible combinations of successes
#' @export
#' @examples
#' 
#' bin_choose(5, 4)
#' 
#' 
#' example <- bin_choose(4, 0:4)
#' 
#' 
bin_choose <- function(n,k) {
  return(factorial(n)/(factorial(k)*factorial(n-k)))
}


#' @title bin_probability
#' @description calculates the binomial probability
#' @param success numeric vector
#' @param trials numeric vector
#' @param prob numeric vector
#' @return the resulted binomial probability depending on the param
#' @export
#' @examples
#' 
#' bin_probability(success = 2, trails = 5, prob = 0.5)
#' 
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' 
#' 
bin_probability <- function(success, trials, prob){
  if(check_trials(trials) & check_prob(prob) & check_success(success, trials)) {
    return(bin_choose(trials, success)*(prob^success)*((1-prob)^(trials - success)))
  }else{
    stop("Invalid value from success, trials, or prob")
  }
}


#' @title bin_distribution
#' @description calculates the binomial distribution 
#' @param trials numeric vector
#' @param prob numeric vector
#' @return the dataframe of the binomial distribution
#' @export
#' @examples
bin_distribution <- function(trials, prob){
  
  x <- 0:trials
  
  bin <- data.frame(
    success = x,
    probability = bin_probability(x, trials = trials, prob = prob))
  
  class(bin) <- c("bindis", "data.frame")
  
  invisible(class(bin))
  
  return(bin)
}

#' @export
plot.bindis <- function(x){
  barplot(x$probability,
          xlab = "successes",
          ylab = "probability",
          names.arg = x$success,
          las = 1)
}

#' @title bin_cumulative
#' @description cumulates the binomial distribution
#' @param trials numeric vector
#' @param prob numeric vector
#' @return
#' @export
#' @examples
#' 
#' bin_cumulative(trials = 5, prob = 0.5)
#' 
#' x <- class(c("binvar", "data.frame"))
#' 
#' x <- bin_cumulative(trials = 6, prob = 0.3)
#' 
#' 
bin_cumulative <- function(trials, prob){
  bin <- bin_distribution(trials, prob)
  
  x <- bin$probability
  
  for(i in 2:length(x)){
    x[i] <- x[i - 1] + x[i]
  }
  
  x <- data.frame(
    success = bin$success,
    probability = bin$probability,
    cumulative = x
  )
  
  
  class(x) <- c("bincum", "data.frame")
  
  invisible(class(x))
  
  return(x)
  
}

#' @export
plot.bincum <- function(x){
  plot(x = x$success,
       y = x$cumulative,
       type = "o",
       las = 1,
       xlab = "successes",
       ylab = "probability")
}


#' @title bin_variable
#' @description binomial random variable object
#' @param trials numeric vector
#' @param prob numeric vector
#' @return binvar object
#' @export
#' @examples
#' 
#' bin_variable(trials = 3, prob = 0.4)
#' 
#' 
#' bin1 <- bin_variable(trials = 10, prob = 0.3)
#' 
#' 
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  
  temp <- list(
    trials = trials,
    prob = prob
  )
  
  class(temp) <- "binvar"
  
  invisible(class(temp))
  
  return(temp)
}

#' @export
print.binvar <- function(x){
  cat('"Binomial Variable"',
      "\n\n",
      "Parameters\n",
      "- number of trials: ",
      x$trials,
      "\n",
      "- prob of success : ",
      x$prob)
  
  invisible(x)
}

#' @export
summary.binvar <- function(x){
  
  temp <- list(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials, x$prob),
    variance = aux_variance(x$trials, x$prob),
    mode = aux_mode(x$trials, x$prob),
    skewness = aux_skewness(x$trials, x$prob),
    kurtosis = aux_kurtosis(x$trials, x$prob)
  )
  
  class(temp) <- "summary.binvar"
  
  invisible(class(temp))
  
  return(temp)
}


#' @export
print.summary.binvar <- function(x){
  
  cat('"Binomial Variable"', "\n\n",
      "Parameters\n", "- number of trials: ", x$trials,"\n",
      "- prob of success : ", x$prob,
      "\n\n", "Measures\n", "- mean    : ", x$mean, "\n",
      "- variance: ", x$variance, "\n",
      "- mode    : ", x$mode, "\n",
      "- skewness: ", x$skewness, "\n",
      "- kurtosis: ", x$kurtosis)
  invisible(x)
  
}


#' @title bin_mean
#' @description calculates the mean of the binomial distribution
#' @param trials numeric vector
#' @param prob numeric vector
#' @return the mean of the binomial distribution
#' @export
#' @examples
#' 
#' bin_mean(10, 0.3)
#' 
#' bin1 <- bin_mean(10, 0.3)
#' 
#' 
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title bin_variance
#' @description calculates the variance of the binomial distribution
#' @param trials numeric vector
#' @param prob numeric vector
#' @return variance of the binomial distribution
#' @export
#' @examples
#' 
#' bin_variance(10, 0.3)
#' 
#' 
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}


#' @title bin_mode
#' @description calculates the mode of the binomial distribution
#' @param trials numeric vector
#' @param prob numeric vector
#' @return the mode of the binomial distribution
#' @export
#' @examples
#' 
#' bin_mode(10 , 0.3)
#' 
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials,prob))
}


#' @title bin_skewness
#' @description calculates the skewness of the binomial distribution
#' @param trials numeric vector
#' @param prob numeric vector
#' @return the skewness of the binomial distribution
#' @export
#' @examples
#' 
#' bin_skewness(10, 0.3)
#' 
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials,prob))
}


#' @title bin_kurtosis
#' @description calculates the kurtosis of the binomial distribution
#' @param trials numeric vector
#' @param prob numeric vector
#' @return the kurtosis of the binomial distribution
#' @export
#' @examples
#' 
#' bin_kurtosis(10, 0.3)
#' 
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials,prob))
}


