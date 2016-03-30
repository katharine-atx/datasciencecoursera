#Part 3

#Write a function that takes a directory of data files and a threshold for 
#complete cases and calculates the correlation between sulfate and nitrate for monitor
#locations where the number of completely observed cases (on all variables) is greater
#than the threshold. The function should return a vector of correlations for the 
#monitors that meet the threshold requirement. If no monitors meet the threshold 
#requirement, then the function should return a numeric vector of length 0. A 
#prototype of this function follows

#For this function you will need to use the 'cor' function in R which calculates 
#the correlation between two vectors. Please read the help page for this function 
#via '?cor' and make sure that you know how to use it.


corr <- function(directory, threshold = 0){
	setwd(directory)
#List and read .csv files and append via rbind()...
	specdata <- list.files(pattern = '\\.csv')
	read_data <- lapply(specdata, read.csv, header = TRUE)
	monitor_data <- do.call(rbind, read_data)
	
#Subset records, removing any observations with an NA value...
	monitor_data_rmna <- na.omit(monitor_data)
	
#Counting the number of records for each monitor ID, total and completes...
	nobs_total <- tapply(monitor_data$ID, monitor_data$ID, length)
	nobs_rmna <- tapply(monitor_data_rmna$ID, monitor_data_rmna$ID, length)
	
#Convert count tables to dataframes with a named column for ID...
	nobs_total_df <- data.frame(unique(monitor_data$ID), nobs_total)
	colnames(nobs_total_df) <- c("id", "nobs_total")
	nobs_rmna_df <- data.frame(unique(monitor_data_rmna$ID), nobs_rmna)
	colnames(nobs_rmna_df) <- c("id", "nobs_rmna")
	
#Merge on id to a single table with ID, total records, completes...
	nobs_merge <- merge(nobs_total_df, nobs_rmna_df, by.x = "id", by.y = "id", all.x)
	
#Calculate percent completes for each monitor ID into column: completes...
	threshold_df <- data.frame(nobs_merge$id, nobs_merge$nobs_rmna / nobs_merge$nobs_total)
	colnames(threshold_df) <- c("id", "completes")

#Merged data frame with pollutant data for monitors meeting completes threshold...
	good_ids <- data.frame(threshold_df[which(threshold_df$completes > threshold),])
	good_ids_merge <- merge(monitor_data_rmna, good_ids, by.x = "ID", by.y = "id", all.y)
	
#Calculate correlation vector ... 
### FIX - is returning same correlation value for all good_ids
	correlations <- sapply(split(good_ids_merge, good_ids_merge$ID), function(x,y) cor(good_ids_merge$sulfate, good_ids_merge$nitrate))
	correlations_vector <- as.numeric(correlations)
	correlations_vector
	