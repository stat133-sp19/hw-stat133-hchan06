# title check_prob
# description checks whether the prob valid
# param prob numeric vector
# return value logical to indicate validness
check_prob <- function(prob){
  if(prob >= 0 & prob <= 1) {
    return(TRUE)
  }else {
    stop("Invalid probability value.")
  }
}

# title check_trials
# description checks whether the trials are valid nonnegative integer values
# param trials numeric vector
# return value logical to indicate validness
check_trials <- function(trials){
  if (trials > 0){
    return(TRUE)
  }else {
    stop("Invalid trials value")
  }
}


# title check_success
# description checks whether success is within range of trials
# param success numeric vector
# param trials numeric vector
# return value logical to indicate validness
check_success <- function(success, trials){
  if(success >= 0 & success <= trials) {
    return(TRUE)
  }else {
    stop("Invalid success value")
  }
}
