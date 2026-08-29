## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Drivers")

# fatal crash data (NHTSA Motor Vehicle Crash Data Querying and Reporting)
crashes = data.frame(
  year = 2010:2024,
  total_crashes = c(30296, 29867, 31006, 30202, 30056, 32538, 34748, 34560,
                     33919, 33487, 35935, 39785, 39422, 37769, 36297))

print(crashes)

write_csv(crashes, "data/output/fatalcrashes.csv")
