
ptm <- proc.time()

########################################

# A program that scrapes the current year's MLS standings table
# Author: Paul James
# Year: 2014

########################################

# Load in the packages
library(XML)

# Create our custom vectors and objects
header <- c("Year", "Conf", "Club", "GP", "PPG", "Wpct", "Lpct", "Dpct", "GFPG", "GAPG", "GDPG", "HGpct", "RGpct", "W", "L", "D", "GF", "GA", "GD", "HG", "HGD", "RG", "RGD")

conf <- c("East", "West")

# Pull in the current MLS standings
mlsRaw <- readHTMLTable("http://www.mlssoccer.com/standings/2014", header = TRUE, stringsAsFactors = FALSE, as.data.frame = TRUE, which = 1:2)

# Add the Conf and Year columns to each table
for(i in 1:length(mlsRaw)){
    mlsRaw[[i]]$Conf <- conf[i]
    mlsRaw[[i]]$Year <- "2014"
}

# Combine the conference standings to edit all at once
mlsNow <- do.call("rbind", mlsRaw)
mlsNow <- mlsNow[ , -1]
row.names(mlsNow) <- NULL

# Fix any extra spaces in the default column names
colNames <- names(mlsNow)
colNames <- gsub(" ", "", colNames, fixed = TRUE)
names(mlsNow) <- colNames

# Change the club names to their abbreviations
clubNames <- read.table("clubNames.csv", sep = "|", header = TRUE)

mlsNow <- merge(mlsNow, clubNames, by = "Club", all.x = TRUE)

# Change the col's to the appropriate data type/class
mlsNow[ , 2:14] <- lapply(mlsNow[ , 2:14], as.numeric)
mlsNow[ , 15:17] <- lapply(mlsNow[ , 15:17], as.factor)

# Make some calculated columns for better apples to apples comparisons
mlsNow <- within(mlsNow, {RGpct <- round(RG / GF, 2)
                          HGpct <- round(HG / GF, 2)
                          Dpct  <- round(T  / GP, 2)
                          Lpct  <- round(L  / GP, 2)
                          Wpct  <- round(W  / GP, 2)
                          GDPG  <- round(GD / GP, 2)
                          GAPG  <- round(GA / GP, 2)
                          GFPG  <- round(GF / GP, 2)})

# Reorder and rename the columns
mlsNow <- mlsNow[c(16, 15, 17, 3:4, 21:23, 18:20, 24:25, 5:14)]

colnames(mlsNow) <- header

########################################

# Write the data to a csv file
write.table(mlsNow, file = "../data/csv/mlsNow.csv", sep = "|", row.names = FALSE, col.names = TRUE)

########################################

proc.time() - ptm # laptop = 2.81 elapsed; work = 0.61 elapsed
