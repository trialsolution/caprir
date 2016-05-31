# Model nomenclatures
library(dplyr)

# list of member states
ms_list <- read.csv(file="inst/extdata/MS_codes.csv")


# product list - agricultural commodities
product_list <- read.csv(file="inst/extdata/prod_codes_capri.csv")
product_list <- tbl_df(product_list)


# activities
activity_list <- read.csv(file = "inst/extdata/activity_codes_capri.csv")
activity_list <- tbl_df(activity_list)

save(activity_list, file = "data/activity_list.RData")
save(ms_list, file = "data/ms_list.RData")
save(product_list, file = "data/product_list.RData")
