#Variation on Part3: Calculating correlations by monitor ID, given complete obs % threshold

# corr2 function calculates the correlation between sulfate and nitrate for monitor
# locations where the number of completely observed cases (on all variables) is greater
# than the threshold - here expressed as % completes instead of # completes.
	#directory (character): local directory path for .csv monitor data files.
	#id (integer): monitor ID #. Each .csv file contains the data for a unique monitor.
	#threshold: Monitor inclusion criteria (% complete observations as value between 0 and 1).
	#Note: return numeric vector of correlation value for each included monitor.
	
corr2 <- function(directory, threshold = 0){
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
	
#Calculate correlation vector, splitting by id using by() and then converting to numeric vector... 
	correlations <- by(good_ids_merge[,3:4], good_ids_merge$ID, function(x) {cor(x$sulfate, x$nitrate)})
	correlations_vector <- as.numeric(correlations)
	correlations_vector
}
