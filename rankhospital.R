# R Programming: Assignment 3 - Federal Hospital Stats and Patient Outcome Rankings
# Part 3: Return hospital name given outcome metric, state and rank in that state

# Data file: "outcome-of-care-measures.csv"
	#Input arguments:
	   #State (character), i.e. "AL"
	   #Rank in state (integer), i.e. 4
	   #outcome (character), one of "heart attack", "heart failure", or "pneumonia"
	#Output:
		#Hospital.Name (character)
		

rankhospital <- function(state, outcome, num = 1) {
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

## Return hospital name in that state with the given rank
## 30-day death rate

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
	
#Create a simplified data frame for ranking and rank by outcome value and then hospital name...
#Note, putting NAs last is the default setting for order()... using that here...
rank_this <- data.frame(state_subset$Hospital.Name, as.numeric(state_subset$outcome), state_subset$State)
colnames(rank_this) <- c("Hospital.Name", "outcome", "State")
ranked <- rank_this[order(rank_this$outcome, rank_this$Hospital.Name),]
rank_df <- data.frame(ranked, seq_along(ranked$Hospital.Name))
colnames(rank_df) <- c("Hospital.Name", "outcome", "State", "Rank")

#Specify index for "best" and "worst", else return the value for the specified integer index...
if (num %in% {"best"}){
	return(rank_df$Hospital.Name[1])
	}
if (num %in% {"worst"}){
	return(rank_df$Hospital.Name[max(rank_df$Rank)])
	}
return(rank_df$Hospital.Name[num])

}