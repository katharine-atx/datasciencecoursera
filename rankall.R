# R Programming: Assignment 3 - Federal Hospital Stats and Patient Outcome Rankings
# Part 4: Return hospital name for each state given outcome metric and rank
# Output should be a 2-column data frame with columns "hospital" and "state"

# Data file: "outcome-of-care-measures.csv"
	#Input arguments:
	   #State (character), i.e. "AL"
	   #Rank in state (integer), i.e. 4
	   #outcome (character), one of "heart attack", "heart failure", or "pneumonia"
	#Output:
		#Hospital.Name (character)

rankall <- function(outcome, num = "best") {
#Reading in the data...
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#Checking that outcome input is valid...
if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) 
	== FALSE){
	stop("invalid outcome")
	}
#Column 11 contains heart attack mortality rate:
if (outcome %in% {"heart attack"}){
	colnames(data)[11] <- "outcome"
	}
#Column 17 contains heart failure mortality rate:
if (outcome %in% {"heart failure"}){
	colnames(data)[17] <- "outcome"
	}
#Column 23 contains pneumonia mortality rate:
if (outcome %in% {"pneumonia"}){
	colnames(data)[23] <- "outcome"
	}

#Data frame with fields for ranking...
rank_this <- data.frame(data$Hospital.Name, as.numeric(data$outcome), data$State)
colnames(rank_this) <- c("hospital", "outcome", "state")

#For each state, find the hospital of the given rank...
#A ranking function to be applied by state:
ranked <- function(x){
	x[order(x$outcome, x$hospital),]
	}
rank_df <- by(rank_this,rank_this$state, ranked)

if (num %in% {"best"}){
	num <- 1
	}
if (num %in% {"worst"}){
### FIX THIS NEXT LINE
	num <- [[nrow(rank_df$state)]]
	}

	
## Testigng...

max_rank <- by(rank_df, rank_df$state, seq_len(nrow(rank_df$state)))

added <- cbind(rank_df, max_rank)
## TESTING - doesn't work
rankle <- do.call(rbind,by(rank_df,rank_df$state,function(x) x[num,]))

#How to add an index to each data frame in the list so we can call the nth element of each????
data.frame(
  id=1:n,                          # we know the index is just 1:n
  nrows=sapply(df.list, nrow) 

add_index <- lapply(1:length(add_index)),     
               function(i) cbind(my_list[[i]], my_list[[i]]["SubCat"] <- as.character(""))) 
lapply(x, split(x, x$state), x[x$hospital[[num]], c()
lapply(rank_df, )
#Now how to pick the rank by state....
rank_index <- function(x){
	x[x$hospital[[num]], c("hospital", "state")]
	}

output <- lapply(rank_df, rank_index)
colnames(output) <- c("hospital", "state")

lapply()



	

## Return a data frame with the hospital names and the
## (abbreviated) state name
}