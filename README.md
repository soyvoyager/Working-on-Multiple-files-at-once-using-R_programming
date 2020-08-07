# Working-on-Multiple-files-at-once-using-R_programming
The zip file contains 332 CSV files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. In this section we are going to operate on those Multiple files in simple commands using R programming.
# Each file contains three variables:
1) Date: the date of the observation in YYYY-MM-DD format (year-month-day)
2) Sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
3) Nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

# Part 1-
Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.
Here you will get answer to part 1 statement(https://github.com/soyvoyager/Working-on-Multiple-files-at-once-using-R_programming/blob/master/Part%201.R)


# part 2-
Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.


# part 3-
Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.
