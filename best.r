# R Programming: Assignment 3 - Federal Hospital Stats and Patient Outcome Rankings
# Part 2: Function to fund best hospital in a state

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

#Note: If an invalid state value is passed to best, the function should throw an 
#error via the stop function with the message "invalid state". 

#Note: If an invalid outcome value is passed to best, the function should throw 
#an error via the stop function with the message "invalid outcome".

#Tie-breaker: In case of tie for the best hospital for a given outcome,the 
#hospital names will be sorted in alphabetical order and the first hospital in 
#that set will be chosen.


best <- function(state, outcome) {
#Reading in the data...
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#Checking that state and outcome inputs are valid...
if as.character(state) !%in% unique(outcome$state){
	message("invalid state") ### how to stop the function????
	#stop
	}
if as.character(outcome) !%in% c("heart attack", "heart failure", "pneumonia"){
	message("invalid outcome")
	#stop
	}
#If valid, proceed to subset data for chosen state...
state_subset <- outcome[which(outcome$State == state),]

#How to map the input values to the columns???

# column 11 contains heart attack mortality rate:
"heart attack" <- as.numeric(outcome[, 11]) 
# column 17 contains heart failure mortality rate:
"heart failure" <- as.numeric(outcome[, 17]) 
# column 23 contains pneumonia mortality rate:
"pneumonia" <- as.numeric(outcome[, 23]) 

hospital <- state_subset[which(outcome_chosen == min(outcome_chosen), Hospital.Name]
hospital_v <- hospital ######

if length(hospital) > 1{
	sorted <- sort(hospital) #
	return sorted[1]
else
	return hospital}
 }



## Return hospital name in that state with lowest 30-day death
## rate






