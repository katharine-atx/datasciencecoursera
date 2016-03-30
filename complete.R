#Part 2: Counting complete observation cases

# complete function reports the number of completely observed cases in each data file. 
	#directory (character): local directory path for .csv monitor data files
	#id (integer): monitor ID #. Each .csv file contains the data for a unique monitor.
	#Note: return dataframe with [,1] = file name and [,2] = # completes

complete <- function(directory, id = 1:332){
	setwd(directory)
	#List and read .csv files and append via rbind()...
	specdata <- list.files(pattern = '\\.csv')
	read_data <- lapply(specdata, read.csv, header = TRUE)
	monitor_data <- do.call(rbind, read_data)
	#Subset table by IDs, removing any observations with an NA value...
	id_subset_rmna <- na.omit(monitor_data[which(monitor_data$ID %in% id),])
	#Counting the number of records for each monitor ID
	nobs <- tapply(id_subset_rmna$ID, id_subset_rmna$ID, length)
	#Return as a data frame with 2 columns: id and nobs
	nobs_summary <- data.frame(unique(id_subset_rmna$ID), nobs)
	#Simplify column names for clarity...
	colnames(nobs_summary) <- c("id", "nobs")
	nobs_summary
}
