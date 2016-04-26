#Course: Exploratory Data Analysis (R)
#Plotting PM2.5 emmissions trends
#--------------------------
#Files used for analysis:

#PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame 
#with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each 
#year, the table contains number of tons of PM2.5 emitted from a specific type 
#of source for the entire year. 
    #fips: A five-digit number (represented as a string) indicating the U.S. 
		#county
    #SCC: The name of the source as indicated by a digit string (see source code 
		#classification table)
    #Pollutant: A string indicating the pollutant
    #Emissions: Amount of PM2.5 emitted, in tons
    #type: The type of source (point, non-point, on-road, or non-road)
    #year: The year of emissions recorded

#Source Classification Code Table (Source_Classification_Code.rds): This table 
#provides a mapping from the SCC digit strings in the Emissions table to the 
#actual name of the PM2.5 source. For example, source “10100101” is known 
#as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.
#--------------------

pm25 <- readRDS("summarySCC_PM25.rds")
source.types <- readRDS("Source_Classification_Code.rds")
#Datasets summary...
str(pm25); str(source.types)
with(pm25, class(Emissions)) #numeric
with(pm25, class(year)) #integer

#Plot1.R: Have total emissions from PM2.5 decreased in the United States from 
#1999 to 2008? 

emm.total <- tapply(pm25$Emissions, pm25$year, sum)
emm.total.scale <- emm.total / 1000  # For y-axis readability, scaling to 1000s of tons...
plot1 <- barplot(emm.total.scale, main = "Total U.S. PM2.5 Emissions", ylab = "PM2.5 Emissions (1000s of tons)")
dev.copy(png,'plot1.png')
dev.off()

# Answer: Yes, PM2.5 emissions decreased substantially.

#Plot2.R: Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to 
#make a plot answering this question.

#Plot3.R: Of the four types of sources indicated by the type (point, nonpoint, 
#onroad, nonroad) variable, which of these four sources have seen decreases in 
#emissions from 1999–2008 for Baltimore City? Which have seen increases in 
#emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer 
#this question.

#Plot4.R: Across the United States, how have emissions from coal combustion-
#related sources changed from 1999–2008?

#Plot5.R: How have emissions from motor vehicle sources changed from 1999–2008 
#in Baltimore City?

#Plot6.R: Compare emissions from motor vehicle sources in Baltimore City with 
#emissions from motor vehicle sources in Los Angeles County, California 
#(fips == "06037"). Which city has seen greater changes over time in motor 
#vehicle emissions?