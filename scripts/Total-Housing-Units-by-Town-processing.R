library(dplyr)
library(datapkg)
library(acs)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Total Housing Units by Town
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

# ACS B25115
# Get geography object for CT and subcounty divisions

options(scipen=999)
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2017,
    table = "B25002"
)

dataset <- data.table()
for(data in acsdata) {
    year <- data@endyear
    print(paste("Processing: ", year))
    year <- paste(year-4, year, sep = "-")

    # Total housing units
    # this total also used as denominator
    total <- data[, 1]
    acs.colnames(total) <- "Number:Total"

    occupied <- data[, 2]
    acs.colnames(total) <- "Number:Occupied"
    percent.occupied <- divide.acs(occupied, total, method = "proportion")
    acs.colnames(percent.occupied) <- "Percent:Occupied"

    vacant <- data[, 3]
    acs.colnames(total) <- "Number:Vacant"
    percent.vacant <- divide.acs(vacant, total, method = "proportion")
    acs.colnames(percent.vacant) <- "Percent:Vacant"

    # merge in fips
    datafips <- data.table(geography(data)[2])

    # Cast to separate data frames
    numberEstimates <- data.table(
        datafips$Id2,
        estimate(total),
        estimate(occupied),
        estimate(vacant),
        year,
        "Number",
        "Housing Units"
    )
    numberMOES <- data.table(
        datafips$Id2,
        standard.error(total) * 1.645,
        standard.error(occupied) * 1.645,
        standard.error(vacant) * 1.645,
        year,
        "Number",
        "Margins of Error"
    )
    numberNames <- c(
            "FIPS",
            "Number:Total",
            "Number:Occupied",
            "Number:Vacant",
            "Year",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
        rbind(numberEstimates, numberMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Housing Units",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    percentEstimates <- data.table(
        datafips$Id2,
        estimate(percent.occupied),
        estimate(percent.vacant),
        year,
        "percent",
        "Housing Units"
    )
    percentMOES <- data.table(
        datafips$Id2,
        standard.error(percent.occupied) * 1.645,
        standard.error(percent.vacant) * 1.645,
        year,
        "percent",
        "Margins of Error"
    )
    percentNames <- c(
        "FIPS",
        "Percent:Occupied",
        "Percent:Vacant",
        "Year",
        "Measure Type",
        "Variable"
     )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentsData.melt <- melt(
        rbind(percentEstimates, percentMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Housing Units",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    dataset <- rbind(dataset, numbersData.melt, percentsData.melt)
}

#Final Additions, processing
# Split Measure type and Occupancy out of "Housing Units" column, then drop that column
dataset[,c("Measure Type", "Occupancy Status"):=do.call(Map, c(f = c, strsplit(`Housing Units`, ":", fixed = T)))]
dataset[,`Housing Units` := NULL]

# Round Values according to type/variable
dataset[`Measure Type` == "Number", Value := round(Value, 0)]
dataset[`Measure Type` != "Number", Value := round(Value*100, 2)]

dataset$FIPS <- gsub("^", "0", dataset$FIPS)

# Join town names by FIPS code
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

dataset$`Occupancy Status` <- factor(dataset$`Occupancy Status`, levels = c("Total", "Vacant", "Occupied"))

#set final column order
dataset <- dataset %>% 
  select(Town, FIPS, Year, `Occupancy Status`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Occupancy Status`, `Measure Type`, Variable)

# Write to File
write.table(
    dataset,
    file.path(getwd(), "data", "total-housing-units-by-town-2017.csv"),
    sep = ",",
    row.names = F,
    na = "-9999"
)

