#Part 1: Pollutant monitoring data...

#pollutantmean function to calculate mean sulfate or nitrate across a defined monitor list
	#directory (character): local location of monitor data
	#pollutant (character): column name: "sulfate" or "nitrate"
	#id (integer): monitor ID #

pollutantmean <- function(directory, pollutant, id = 1:332, removeNA = TRUE) {
	val <- numeric(mean(pollutant, na.rm = removeNA))
	val
}