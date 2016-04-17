# R Programming: Assignment 3 - Federal Hospital Stats and Patient Outcome Rankings
# Part 2: Function to find best hospital in a state

#best function takes two arguments: 2-character state abbreviation and an
#outcome name and returns a character vector with the name of the hospital that 
#has the lowest 30-day mortality for the specifed outcome in that state.
  
  #Data file: "outcome-of-care-measures.csv"
	#Input arguments:
	   #State (character), i.e. "AL"
	   #outcome (character), one of "heart attack", "heart failure", or "pneumonia"
	#Output:
		#Hospital.Name (character)

#Note: Hospitals that do not have data on a particular outcome will be excluded.
#If an invalid state value is passed to best, the function should throw an 
#error via the stop function with the message "invalid state". 
#If an invalid outcome value is passed to best, the function should throw 
#an error via the stop function with the message "invalid outcome".

#Tie-breaker: In case of tie for the best hospital for a given outcome,the 
#hospital names will be sorted in alphabetical order and the first hospital in 
#that set will be chosen.


best <- function(state, outcome) {
#Reading in the data...
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#Checking that state and outcome inputs are valid...
if ((state %in% unique(data$State)) == FALSE){
	stop("invalid state") 
	}
if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) 
	== FALSE){
	stop("invalid outcome")
	}
#If valid, proceed to subset data for chosen state...
state_subset <- data[which(data$State == state),]

#Column 11 contains heart attack mortality rate:
if (outcome %in% {"heart attack"}){
	colnames(state_subset)[11] <- "outcome"
	}
#Column 17 contains heart failure mortality rate:
if (outcome %in% {"heart failure"}){
	colnames(state_subset)[17] <- "outcome"
	}
#Column 23 contains pneumonia mortality rate:
if (outcome %in% {"pneumonia"}){
	colnames(state_subset)[23] <- "outcome"
	}

## Return hospital name in that state with lowest 30-day death
## rate for the outcome
min_rate <- min(as.numeric(state_subset$outcome[!is.na(as.numeric(state_subset$outcome))]))
hospital <- state_subset$Hospital.Name[which(as.numeric(state_subset$outcome) == min_rate)]

if (length(hospital) > 1){
	sorted <- sort(hospital) 
	return(sorted[1])
	stop()
	}
return(hospital)
 }








