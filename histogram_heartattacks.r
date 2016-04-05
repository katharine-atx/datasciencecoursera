# R Programming: Assignment 3 - Federal Hospital Stats and Patient Outcome Rankings
# Part 1: Plot 30-day mortality rates for heart attacks
# Data file: "outcome-of-care-measures.csv"

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
str(outcome)
# column 11 contains heart attack mortality rate:
outcome[, 11] <- as.numeric(outcome[, 11]) 
# plotting histogram...
hist(outcome[, 11]) 
