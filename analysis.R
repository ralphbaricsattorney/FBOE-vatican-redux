library(MASS)
library(ggplot2)
library(dplyr)

births <- read.csv("data/clean/birth_results.csv")
cardinals <- read.csv("data/clean/all_cardinals.csv")
data <- left_join(cardinals, births, by = "cardinal")
modernData <- data[!data$century %in% c("fifteenth_century", "sixteenth_century"), ]
oldData <- data[data$century %in% c("fifteenth_century", "sixteenth_century"), ]


cleanBirthStatistic <- function(statistic) {
  if (!(startsWith(statistic, ">") || (statistic == "") || (is.na(statistic)))) {
    return(as.integer(statistic))
  } else {
    return(NA)
  }
}


isEldestSib <- function(birthOrder) {
  if (is.na(birthOrder) || (birthOrder == "")) {
    return(NA)
  }
  else if (birthOrder == "1") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


isSecondSib <- function(birthOrder) {
  if (is.na(birthOrder) || (birthOrder == "")) {
    return(NA)
  } else if (birthOrder == "2") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


isThirdSib <- function(birthOrder) {
  if (is.na(birthOrder) || (birthOrder == "")) {
    return(NA)
  } else if (birthOrder == "3") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


isFourthSib <- function(birthOrder) {
  if (is.na(birthOrder) || (birthOrder == "")) {
    return(NA)
  } else if (birthOrder == "4") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


numericallyCastAndSubsetField <- function(data, field) {
  numericData <- unlist(lapply(data[[field]], cleanBirthStatistic))
  numericData <- numericData[!is.na(numericData)]
  return(numericData)
}


eldestSibs <- function(data) {
  booleanData <- unlist(lapply(data[["birth_order"]], isEldestSib))
  booleanData <- booleanData[!is.na(booleanData)]
  return(booleanData)
}


secondSibs <- function(data) {
  booleanData <- unlist(lapply(data[["birth_order"]], isSecondSib))
  booleanData <- booleanData[!is.na(booleanData)]
  return(booleanData)
}


thirdSibs <- function(data) {
  booleanData <- unlist(lapply(data[["birth_order"]], isThirdSib))
  booleanData <- booleanData[!is.na(booleanData)]
  return(booleanData)
}


fourthSibs <- function(data) {
  booleanData <- unlist(lapply(data[["birth_order"]], isFourthSib))
  booleanData <- booleanData[!is.na(booleanData)]
  return(booleanData)
}




youngestSibs <- function(data) {
  data$total_sibs <- lapply(data$total_sibs, cleanBirthStatistic)
  data <- data[!is.na(data$total_sibs), ]
  data <- data[data$total_sibs > 1, ]
  data$birth_order <- lapply(data$birth_order, cleanBirthStatistic)
  data <- data[!is.na(data$birth_order), ]
  return((unlist(data$birth_order) - unlist(data$total_sibs)) == 0)
}


plotNegativeBinomialFit <- function(data) {
  numericTotalSibs <- numericallyCastAndSubsetField(data, "total_sibs")
  fit <- fitdistr(numericTotalSibs, "Negative Binomial")
  size <- fit$estimate["size"]
  mu <- fit$estimate["mu"]

  numericTotalSibsData <- data.frame(value = numericTotalSibs)
  
  # Generate predicted probabilities
  xValues <- 1:max(numericTotalSibs)
  probabilities <- dnbinom(xValues, size = size, mu = mu)
  
  # plotting stuff
  ggplot(numericTotalSibsData, aes(x = value)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "green", alpha = 0.6, bins = 20) +
  geom_density(color = "red", size = 1.2, alpha = 0.7) +  
  geom_point(data = data.frame(x = xValues, prob = probabilities), aes(x = x, y = prob), color = "blue", size = 1) +
  geom_line(data = data.frame(x = xValues, prob = probabilities), aes(x = x, y = prob), color = "blue", size = 1) +
  labs(title = "Distribution of completed family size (green), fit negative binomial distribution (blue), and KDE estimate (red)", x = "Family Size", y = "Density") +
  theme_minimal()
}


calculateActualBirthOrder <- function(data) {
  return(mean(numericallyCastAndSubsetField(data, "birth_order")))
}


calculateExpectedBirthOrder <- function(data, outOfSample = TRUE) {
  if (outOfSample == FALSE) {
    data$cleanBirthOrder <- lapply(data[["birth_order"]], cleanBirthStatistic)
    data <- data[!is.na(data$cleanBirthOrder), ]
  }
  numericTotalSibs <- numericallyCastAndSubsetField(data, "total_sibs")
  fit <- fitdistr(numericTotalSibs, "Negative Binomial")
  size <- fit$estimate["size"]
  mu <- fit$estimate["mu"]
  xValues <- 1:max(numericTotalSibs)
  probabilities <- dnbinom(xValues, size = size, mu = mu)
  return(sum(((xValues + 1) / 2) * probabilities))
}
