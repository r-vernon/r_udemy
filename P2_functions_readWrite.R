# make sure required packages installed
# using pacman to automatically check and install packages
if (!require('pacman')) install.packages('pacman'); library(pacman)
p_load('readxl','writexl') # for reading/writing excel files

# create csv to play around with
write.csv(mtcars,file = 'dataFiles/example_csv.csv')

# read in the csv
example_csv <- read.csv('dataFiles/example_csv.csv')

# read in excel file (commands from readxl)
excel_sheets('dataFiles/Sample-Sales-Data.xlsx') # list sheets
df <- read_excel('dataFiles/Sample-Sales-Data.xlsx',sheet='Sheet1')

# write excel file (commands from writexl)
write_xlsx(mtcars,'dataFiles/example_xlsx.xlsx')
