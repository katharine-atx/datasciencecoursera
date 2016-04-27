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
#Maryland (fips == "24510") from 1999 to 2008? 

baltimore <- pm25[which(pm25$fips == "24510"),]
emm.baltimore <- tapply(baltimore$Emissions, baltimore$year, sum)
plot2 <- barplot(emm.baltimore, main = "Total Baltimore PM2.5 Emissions", ylab = "PM2.5 Emissions (tons)")
dev.copy(png,'plot2.png')
dev.off()

# Answer: Overall, yes, but 2005 saw an increase (over 2002) before emissions fell to their lowest level in 2008.

#Plot3.R: Of the four types of sources indicated by the type (point, nonpoint, 
#onroad, nonroad) variable, which of these four sources have seen decreases in 
#emissions from 1999–2008 for Baltimore City? Which have seen increases in 
#emissions from 1999–2008? 

library(ggplot2)
baltimore$type <- as.factor(baltimore$type)
baltimore$year <- as.factor(as.character(baltimore$year))
plot3 <- ggplot(baltimore, aes(year, Emissions))
plot3 <- plot3 + geom_bar(stat = "sum") + facet_wrap(~type) + theme(legend.position = "none") + ggtitle("Baltimore Emissions (tons) by Source Type")
dev.copy(png, 'plot3.png')
dev.off()

# Answer: Point sources accounted for the 2005 spike; other sources steadily decreased.

#Alternate method to ggplot2: tapply() and then use base plotting...i.e.
#emm.baltimore.type <- with(baltimore, tapply(Emissions, list(year, type), sum))...

#Plot4.R: Across the United States, how have emissions from coal combustion-
#related sources changed from 1999–2008?

unique(source.types$EI.Sector)

source.types$EI.Sector <- as.character(source.types$EI.Sector)
coal.comb <- c("Fuel Comb - Electric Generation - Coal", "Fuel Comb - Comm/Institutional - Coal", "Fuel Comb - Industrial Boilers, ICEs - Coal")
coal.SCC <- as.character(source.types[which(source.types$EI.Sector %in% coal.comb), 1])
coal.emm.us <- pm25[which(pm25$SCC %in% coal.SCC),]
coal.emm.total <- tapply(coal.emm.us$Emissions, coal.emm.us$year, sum)
plot4 <- barplot(coal.emm.total/1000, main = "U.S. PM2.5 Emissions from Coal Combustion", ylab = "PM2.5 Emissions (1000s of tons)")
dev.copy(png,'plot4.png')
dev.off()

# Answer: The bulk of the decrease in U.S. coal-combustion emissions occured between 2005 and 2008.

#Plot5.R: How have emissions from motor vehicle sources changed from 1999–2008 
#in Baltimore City?

library(ggplot2)
baltimore <- pm25[which(pm25$fips == "24510"),]
baltimore$year <- as.factor(as.character(baltimore$year))
baltimore.mv <- baltimore[which(baltimore$type == "ON-ROAD"),]
plot5 <- ggplot(baltimore.mv, aes(year, Emissions))
plot5 <- plot5 + geom_bar(stat = "sum") + theme(legend.position = "none") + ggtitle("Baltimore Motor Vehicle Emissions (tons)")
dev.copy(png, 'plot5.png')
dev.off()

# Answer: Baltimore Motor Vehicle emissions dropped dramatically between 1999 and 2002.

#Plot6.R: Compare emissions from motor vehicle sources in Baltimore City with 
#emissions from motor vehicle sources in Los Angeles County, California 
#(fips == "06037"). 

baltimore <- pm25[which(pm25$fips == "24510"),]
baltimore.mv <- baltimore[which(baltimore$type == "ON-ROAD"),]
balt.mv.emm <- tapply(baltimore.mv$Emissions, baltimore.mv$year, sum)
los.angeles <- pm25[which(pm25$fips == "06037"),]
los.angeles.mv <- los.angeles[which(los.angeles$type == "ON-ROAD"),]
LA.mv.emm <- tapply(los.angeles.mv$Emissions, los.angeles.mv$year, sum)
rng <- range(balt.mv.emm, LA.mv.emm)
par(mfrow = c(1,2))
barplot(balt.mv.emm, main = "Baltimore MV Emissions", ylab = "PM2.5 Emissions (tons)", ylim = rng)
barplot(LA.mv.emm, main = "L.A MV Emissions", ylim = rng)
dev.copy(png, 'plot6.png')
dev.off()

# Answer: Large difference in magnitude of emissions between the two cities. Baltimore
# has steadily reduced PM2.5 emissions while L.A.'s total emissions increased 1999-2005.
# Though L.A. saw a substantial reduction after 2005, emissions their remain above 1999 levels.