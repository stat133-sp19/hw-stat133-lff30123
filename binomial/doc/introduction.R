## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(binomial)

## ----eval=FALSE----------------------------------------------------------
#  check_prob <- function(prob) {
#    if (!is.numeric(prob)) {
#      stop("\n'prob' must be a numeric value")
#    }
#    if (length(prob)!=1) {
#      stop("\n'prob' must have length of 1")
#    }
#    if (any(prob < 0) | any(prob > 1)) {
#      stop("\n'prob' values must be between 0 and 1")
#    }
#    TRUE
#  }

## ------------------------------------------------------------------------
aux_mean <- function(trials,prob){
  result <- trials*prob
  return(result)
}
aux_mean(10, 0.3)


#Expected variance of number of success.
aux_variance <- function(trials,prob){
  result <- trials*prob*(1-prob)
  return(result)
}
aux_variance(10, 0.3)

#The most likely number of success in n independent trials with probability p
aux_mode <- function(trials,prob){
  m <- floor(trials*prob+prob)
  return(m)
}
aux_mode(10, 0.3)
#Skewness is a measure of the asymmetry of the probability distribution of a random variable about its mean.
aux_skewness <- function(trials,prob){
  skewness <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  return(skewness)
}
aux_skewness(10, 0.3)
#kurtosis is a descriptor of the shape of a probability distribution
aux_kurtosis <- function(trials,prob){
  kurtosis <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  return(kurtosis)
}
aux_kurtosis(10, 0.3)




## ------------------------------------------------------------------------
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

plot.bindis <- function(datframe){barplot(datframe$probability,xlab="success",ylab="probability",names.arg = datframe$success,col="blue")}

# plotting binomial probability distribution
dis1 <- bin_distribution(trials = 5, prob = 0.5)
plot(dis1)

## ------------------------------------------------------------------------
bin_cumulative <- function(trials,prob){
  success <- 0:trials
  probability <- bin_probability(success,trials,prob)
  cumulative <- cumsum(probability)
  dat <- data.frame('success' = success, "probability"=probability,"cumulative"= cumulative)
  class(dat) <- c("bincum","data.frame")
  dat
}

bin_cumulative(trials = 5, prob = 0.5)

## ------------------------------------------------------------------------
plot.bincum <- function(datframe){
  result <- plot(datframe$success,datframe$cumulative,xlab="success",ylab="probability",type="o")
  return(result)
}
# plotting binomial cumulative distribution
dis2 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis2)

## ------------------------------------------------------------------------
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

