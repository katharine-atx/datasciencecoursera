#Part 3: Calculating pollutant correlations by monitor ID, given # complete obs threshold

# corr function calculates the correlation between sulfate and nitrate for monitor
# locations where the number of completely observed cases (on all variables) is greater
# than the threshold.
	#directory (character): local directory path for .csv monitor data files.
	#id (integer): monitor ID #. Each .csv file contains the data for a unique monitor.
	#threshold: Monitor inclusion criteria (# complete observations).
	#Note: return numeric vector of correlation value for each included monitor.
	
corr <- function(directory, threshold = 0){
	setwd(directory)
#List and read .csv files and append via rbind()...
	specdata <- list.files(pattern = '\\.csv')
	read_data <- lapply(specdata, read.csv, header = TRUE)
	monitor_data <- do.call(rbind, read_data)
	
#Subset records, removing any observations with an NA value...
	monitor_data_rmna <- na.omit(monitor_data)
	
#Counting the number of completes for each monitor ID...
	nobs_rmna <- tapply(monitor_data_rmna$ID, monitor_data_rmna$ID, length)
	
#Convert count table to dataframe with a named column for ID...
	nobs_rmna_df <- data.frame(unique(monitor_data_rmna$ID), nobs_rmna)
	colnames(nobs_rmna_df) <- c("id", "completes")
	
#Merged data frame with pollutant data for monitors meeting completes threshold...
	good_ids <- data.frame(nobs_rmna_df[which(nobs_rmna_df$completes > threshold),])
	good_ids_merge <- merge(monitor_data_rmna, good_ids, by.x = "ID", by.y = "id", all.y)
	
#Calculate correlation vector, splitting by id using by() and then converting to numeric vector... 
	correlations <- by(good_ids_merge[,3:4], good_ids_merge$ID, function(x) {cor(x$sulfate, x$nitrate)})
	correlations_vector <- as.numeric(correlations)
	correlations_vector
}
