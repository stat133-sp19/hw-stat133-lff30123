#1.1)Private Checker Functions

#Write a private auxiliary function check_prob() to test if an input prob is a valid probability value (i.e. 0 < p < 1). check_prob() takes one input: prob. If prob is valid, then check_prob() should return TRUE. If prob is invalid, then check_prob() should stop() execution with an error—e.g. something like 'invalid prob value', or 'p has to be a number betwen 0 and 1'.

check_prob <- function(prob) {
  if (!is.numeric(prob)) {
    stop("\n'prob' must be a numeric value")
  }
  if (length(prob)!=1) {
    stop("\n'prob' must have length of 1")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  TRUE
}

#Function check trials. A private auxiliary function check_trials() to test if an input trials is a valid value for number of trials (i.e. n is a non-negative integer). Check_trials() takes one input: trials.If trials is valid, then check_trials() should return TRUE. If trials is invalid, then check_trials() should stop() execution with an error—e.g. something like 'invalid trials value'.

check_trials <- function(trials){
  if (!is.numeric(trials))
  {
    stop("trials need to be a numerical number")
  }
  if (trials < 0 | trials%%1 != 0){
    stop("invalid trials value")
  }
  TRUE
}


#Function check_success(). Write a private auxiliary function check_success() to test if an input success is a valid value for number of successes (i.e. 0 < k < n). Check_success() takes two inputs: success and trials. Success should be a vector of non-negative integer(s) less than or equal to trials. Notice that success can be of length greater than 1 (i.e. multiple successes). If success is valid, then check_success() should return TRUE. If success is invalid, then check_success() should stop() execution with an error—e.g. something like 'invalid success value' or if k > n then 'success cannot be greater than trials'.


check_success <- function(success,trials){
  if (any(sapply(success,function(success){success%%1}) != 0)) {
    stop("\number of successes must be integers")}
  if (any(success > trials)|any(success<0)) {
    stop("sucess cannot be greater than trials or smaller than zero")
  }
  TRUE
}


#1.2) Private Auxiliary Functions

#Expected number of successes
aux_mean <- function(trials,prob){
  result <- trials*prob
  return(result)
}

#Expected variance of number of success.
aux_variance <- function(trials,prob){
  result <- trials*prob*(1-prob)
  return(result)
}

#The most likely number of success in n independent trials with probability p
aux_mode <- function(trials,prob){
  m <- floor(trials*prob+prob)
  return(m)
}

#Skewness is a measure of the asymmetry of the probability distribution of a random variable about its mean.
aux_skewness <- function(trials,prob){
  skewness <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  return(skewness)
}

#kurtosis is a descriptor of the shape of a probability distribution
aux_kurtosis <- function(trials,prob){
  kurtosis <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  return(kurtosis)
}


#1.3) bin_choose()
#'@title future value
#'@description the function calculates the number of combinations in which k successes can occur in n trials.
#'@param n number of trials
#'@param k number of success
#'@return the number of combinations in which k successes can occur in n trials
#'@export
#'@examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)


bin_choose<- function(n,k){
  if (any (k > n)){
    stop("k cannot be greater than n")
  }
  combinations <- sapply(k,function(k){factorial(n)/(factorial(k)*factorial(n-k))})
  return(combinations)
}
bin_choose(5, 1:3)

#1.4) Function bin_probability()
#'@title binomial probability
#'@description the function calculates the probability of  certain number successes can occur in a fixed number of trials
#'@param success number of success
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return probability of success
#'@export
#'@examples
#'bin_probability(success = 2, trials = 5, prob = 0.5)
#'bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability<- function(success,trials,prob)
{
    check_trials(trials)
    check_prob(prob)
    check_success(success,trials)
    result <- bin_choose(trials,success)*prob^success*(1-prob)^(trials-success)
    return(result)
  }

#1.5)Function bin_distribution()
#'@title create a binomial distribution table
#'@description the primary class is "bindis" indicating that this is an object of class binomial distribution.This function should return a data frame with the probability distribution: sucesses in the first column, probability in the second column.
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return return a data frame with the probability distribution: sucesses in the first column, probability in the second column.
#'@export
#'@examples
#'bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials,prob){
  success <- c(0:trials)
  probability <- bin_probability(success,trials,prob)
  dat <- data.frame(success,probability)
  class(dat) <- c("bindis","data.frame")
  return(dat)
}
# binomial probability distribution
bin_distribution(trials = 5, prob = 0.5)

# plotting binomial probability distribution
#'@export
plot.bindis <- function(datframe){barplot(datframe$probability,xlab="success",ylab="probability",names.arg = datframe$success,col="blue")}

# plotting binomial probability distribution
dis1 <- bin_distribution(trials = 5, prob = 0.5)
plot(dis1)

#1.6)Function plot.bincum()
#'@title create a binomial cumulative distribution table
#'@description the primary class is "bincum" indicating that this is an object of class binomial cumulative distribution. Additionally, to keep this object as a data frame, we still need to include a class "data.frame".
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return a data frame with the cumulative probability distribution: sucesses in the first column, probability in the second column, cumulative distribution probability in the third column.
#'@export
#'@examples
#'bin_cumulative(trials = 5, prob = 0.5)

bin_cumulative <- function(trials,prob){
  success <- 0:trials
  probability <- bin_probability(success,trials,prob)
  cumulative <- cumsum(probability)
  dat <- data.frame('success' = success, "probability"=probability,"cumulative"= cumulative)
  class(dat) <- c("bincum","data.frame")
  dat
}

bin_cumulative(trials = 5, prob = 0.5)

#'@export
plot.bincum <- function(datframe){
  result <- plot(datframe$success,datframe$cumulative,xlab="success",ylab="probability",type="o")
  return(result)
}
# plotting binomial cumulative distribution
dis2 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis2)

#1.7)Function bin_variable()
#'@title bin_variable
#'@description This is a main function that takes two arguments: trials and prob.The function should return a binomial random variable object
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return an object of class "binvar", that is, a binomial random variable object.
#'@export
bin_variable <- function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  result <- list("trials"=trials,"prob"=prob)
  class(result) <- "binvar"
  return(result)
}
bin1 <- bin_variable(trials = 10, prob = 0.3)
bin1

#'@export
# print method for object of class "toss"
print.binvar <- function(x) {
  cat(sprintf("Binomial variable"))
  cat(sprintf("\n"))
  cat(sprintf("\n"))
  cat("Parameters","\n")
  cat("- number of trials:", x$trials,"\n")
  cat("- prob of success:", x$prob, "\n")
  invisible(x)
}
print.binvar(bin_variable(trials = 10, prob = 0.3))
#'@export
#Methods summary.binvar() and print.summary.binvar()

summary.binvar <- function(x) {
  summary <- list(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials,x$prob),
    variance = aux_variance(x$trials,x$prob),
    mode = aux_mode(x$trials,x$prob),
    skewness = aux_skewness(x$trials,x$prob),
    kurtosis = aux_kurtosis(x$trials,x$prob))
  class(summary) <- "summary.binvar"
  return(summary)
}

bin1 <- bin_variable(trials = 10, prob = 0.3)
binsum1 <- summary(bin1)
binsum1

#'@export
print.summary.binvar <- function(x) {
  cat(sprintf("Summary Binomial"))
  cat(sprintf("\n"))
  cat(sprintf("\n"))
  cat("Parameters","\n")
  cat("- number of trials:", x$trials,"\n")
  cat("- prob of success:", x$prob, "\n")
  cat(sprintf("\n"))
  cat(sprintf("\n"))
  cat("Measures","\n")
  cat("- mean    :", aux_mean(x[[1]],x[[2]]),"\n")
  cat("- variance:", aux_variance(x$trials,x$prob),"\n")
  cat("- mode    :", aux_mode(x$trials,x$prob), "\n")
  cat("- skewness:", aux_skewness(x$trials,x$prob), "\n")
  cat("- kurtosis:", aux_kurtosis(x$trials,x$prob), "\n")
  invisible(x)
}

#1.8)Functions of measures
#'@title binomial mean
#'@description the function calculates mean number of success
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return number of success
#'@export
#'@examples
#'bin_mean(10, 0.3)

bin_mean<- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials,prob)
}

#'@title binomial variance
#'@description the function calculates variance of the number of success
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return variance
#'@export
#'@examples
#'bin_variance(10, 0.3)
bin_variance<- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials,prob)
}



#'@title binomial model
#'@description the function calculates mode of the number of success
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return mode
#'@export
#'@examples
#'bin_mode(10, 0.3)

bin_mode<- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials,prob)
}

#'@title binomial skewness
#'@description the function calculates skewness of the number of success
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return skewness
#'@export
#'@examples
#'bin_skewness(10, 0.3)
bin_skewness<- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials,prob)
}

#'@title binomial kurtosis
#'@description the function measures the "tailedness" of the probability distribution
#'@param trials number of trials
#'@param prob probability of success for each trials
#'@return tailedness
#'@export
#'@examples
#'bin_kurtosis(10, 0.3)

bin_kurtosis<- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials,prob)
}




