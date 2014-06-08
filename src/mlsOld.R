
ptm <- proc.time()

########################################

# A program that scrapes all past years' MLS standings tables
# Author: Paul James
# Year: 2014

########################################

# Create our custom vectors and objects
Year1 <- as.character(2012:2013)
Year2 <- as.character(c(1996:1999, 2002:2011))
Year3 <- as.character(2000:2001)

Conf1 <- c("East", "West")
Conf2 <- Conf1
Conf3 <- c("East", "Cent", "West")

tableUrls <- function(Years){
    paste("http://www.mlssoccer.com/standings/", Years, sep = "")
}

url1 <- tableUrls(Year1)
url2 <- tableUrls(Year2)
url3 <- tableUrls(Year3)

mlsRaw1 <- list()
mlsRaw2 <- list()
mlsRaw3 <- list()

########################################

# Make the function to get the stats
mlsStats <- function(url.Num, mlsRaw.Num, Conf.Num, Year.Num){
    # Load the packages
    require(XML)

    # Pull in the standings
    if(length(grep("3", url.Num)) > 0){
        for(i in 1:length(url.Num)){
            mlsRaw.Num[[i]] <- readHTMLTable(url.Num[i], header = TRUE, stringsAsFactors = FALSE, as.data.frame = TRUE, which = 1:2)
        }

        # readHTMLTable makes nested lists, un-nest them
        mlsRaw.Num <- do.call("c", mlsRaw.Num)

        # Add the Conf and Year columns to each standings list
        for(i in 1:length(mlsRaw.Num)){
            mlsRaw.Num[[i]]$Conf <- rep(Conf.Num, length(Year.Num))[i]
            mlsRaw.Num[[i]]$Year <- sort(rep(Year.Num, 2))[i]
        }
    } else {
        for(i in 1:length(url.Num)){
            mlsRaw.Num[[i]] <- readHTMLTable(url.Num[i], header = TRUE, stringsAsFactors = FALSE, as.data.frame = TRUE, which = 1:3)
        }

        # readHTMLTable makes nested lists, un-nest them
        mlsRaw.Num <- do.call("c", mlsRaw.Num)

        # Add the Conf and Year columns to each standings list
        for(i in 1:length(mlsRaw.Num)){
            mlsRaw.Num[[i]]$Conf <- rep(Conf.Num, length(Year.Num))[i]
            mlsRaw.Num[[i]]$Year <- sort(rep(Year.Num, 3))[i]
        }
    }
    # Combine the list into a dataframe and remove some annoyances
    mlsRaw.Num <- do.call("rbind", mlsRaw.Num)
    mlsRaw.Num <- mlsRaw.Num[ , -1]
    row.names(mlsRaw.Num) <- NULL

    return(mlsRaw.Num)
}
# Run the standings function
mlsOld1 <- mlsStats(url1, mlsRaw1, Conf1, Year1)
mlsOld2 <- mlsStats(url2, mlsRaw2, Conf2, Year2)
mlsOld3 <- mlsStats(url3, mlsRaw3, Conf3, Year3)

# Remove the raw data and it's objects from memory
rm(mlsRaw1, mlsRaw2, mlsRaw3, Year1, Year2, Year3, Conf1, Conf2, Conf3)
gc()

########################################

# Make a function to further mold the data so it can be combined
mlsGroom <- function(mlsOld.Num){
    # Fix any extra spaces in the default column names
    colNames <- names(mlsOld.Num)
    colNames <- gsub(" ", "", colNames, fixed = TRUE)
    names(mlsOld.Num) <- colNames

    # Change the club names to their abbreviations
    clubNames <- read.table("clubNames.csv", sep = "|", header = TRUE)
    mlsOld.Num <- merge(mlsOld.Num, clubNames, by = "Club", all.x = TRUE)

    # Change the col's to the appropriate data type/class
    if(length(grep("Home", names(mlsOld.Num))) == 0){
        mlsOld.Num[ , 2:10] <- lapply(mlsOld.Num[ , 2:10], as.numeric)
        mlsOld.Num[ , 15:17] <- lapply(mlsOld.Num[ , 15:17], as.factor)
    } else {
        mlsOld.Num[ , 2:9] <- lapply(mlsOld.Num[ , 2:9], as.numeric)
        mlsOld.Num[ , 13:15] <- lapply(mlsOld.Num[ , 13:15], as.factor)
    }

    # Make calculated columns for better apples to apples comparisons
    newStats <- function(data){
        within(data, {PPG   <- round(PTS / GP, 2)
                      Dpct  <- round(T   / GP, 2)
                      Lpct  <- round(L   / GP, 2)
                      Wpct  <- round(W   / GP, 2)
                      GDPG  <- round(GD  / GP, 2)
                      GAPG  <- round(GA  / GP, 2)
                      GFPG  <- round(GF  / GP, 2)})
    }
    mlsOld.Num <- newStats(mlsOld.Num)

    # Reorder the data
    if(length(grep("Home", names(mlsOld.Num))) == 0){
        mlsOld.Num <- mlsOld.Num[c(16, 15, 17, 3:4, 21:23, 18:20, 5:10)]
    } else {
        mlsOld.Num <- mlsOld.Num[c(14, 13, 15, 3, 22, 19:21, 16:18, 4:9)]
    }

    # Rename the columns
    header <- c("Year", "Conf", "Club", "GP", "PPG", "Wpct", "Lpct", "Dpct", "GFPG", "GAPG", "GDPG", "W", "L", "D", "GF", "GA", "GD")
    colnames(mlsOld.Num) <- header

    return(mlsOld.Num)
}
# Run the grooming function
mlsOld1 <- mlsGroom(mlsOld1)
mlsOld2 <- mlsGroom(mlsOld2)
mlsOld3 <- mlsGroom(mlsOld3)

# Combine the data to one dataset
mlsOld <- rbind(mlsOld1, mlsOld2, mlsOld3)

# Remove the individual datasets from memory
rm(mlsOld1, mlsOld2, mlsOld3)
gc()

# Write the data to a csv file
write.table(mlsOld, file = "../data/csv/mlsOld.csv", sep = "|", row.names = FALSE, col.names = TRUE)

########################################

proc.time() - ptm # laptop = 18.38 elapsed
