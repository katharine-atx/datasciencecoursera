#Part 1: Pollutant monitoring data...

#pollutantmean function to calculate mean sulfate or nitrate across a defined monitor list
	#directory (character): local directory path for .csv monitor data files
	#pollutant (character): column name: "sulfate" or "nitrate"
	#id (integer): monitor ID #
	#Note: remove NA values for mean calculation and return numeric
		
pollutantmean <- function(directory, pollutant, id = 1:332, removeNA = TRUE) {
	setwd(directory)
	#List and read .csv files and then append via rbind()
	specdata <- list.files(pattern = '\\.csv')
	read_data <- lapply(specdata, read.csv, header = TRUE)
	monitor_data <- do.call(rbind, read_data)
	id_subset <- monitor_data[which(monitor_data$ID %in% id),]
	#taking the mean...
	val <- mean(id_subset[, pollutant], na.rm = removeNA)
	val
}