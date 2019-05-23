#

library(tidyverse)

file_name <- "DairyBaseForPowerBIsoils.csv"
data_all <- read_csv(file_name, na=c("","NaN","NA"), col_types=cols(Concatbest="c")) # force "Concatbest" to chr

data <- data_all %>%
  select(Region, Season,
         "Pasture and Crop eaten t DM/ha",
         SupplyClean, long, lat,
         nzsc_order) %>%
  mutate(long=long+runif(n(), -0.1, 0.1),
         lat=lat+runif(n(), -0.1, 0.1))

file_name <- "DairyBaseForPowerBIsoilsEdmar.csv"
write_csv(data, "DairyBaseForPowerBIsoilsEdmar.csv")
