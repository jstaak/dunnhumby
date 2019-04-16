#Read in files 
dftrans <- read.csv("dh_transactions.csv")
df_store <- read.csv("dh_store_lookup.csv")
df_product <- read.csv("dh_product_lookup.csv")
df_causal <- read.csv("dh_causal_lookup.csv")


#a.	What are the top 5 products in each commodity?
library(dplyr)

#Left outer join to include product information with transactional data
df_tp <- merge(x = dftrans, y = df_product, by = "upc", all.x = TRUE)

#Create a new variable "revenue" by multiplying units and sales price
df_tp$revenue <- df_tp$dollar_sales * df_tp$units

#Aggregate revenue on product description, commodity, brand
df_ag <- df_tp %>% group_by(product_description, commodity, brand) %>%
  dplyr::summarize(revenue = sum(revenue))

#PASTA
#New dataframe with only pasta products
pasta_index <- which(df_ag$commodity == "pasta")
df_pasta_ag <- df_ag[pasta_index, ]

#Sort by revenue to find top sellers
df_pasta_ag <- df_pasta_ag[order(-df_pasta_ag$revenue),]

#PASTA SAUCE 
#New dataframe with only pasta sauce products
pasta_sauce_index <- which(df_ag$commodity == "pasta sauce")
df_sauce_ag <- df_ag[pasta_sauce_index, ]

#Sort by revenue to find top sellers
df_sauce_ag <- df_sauce_ag[order(-df_sauce_ag$revenue),]

#PANCAKES
#New dataframe with only pancake mix products
pancake_index <- which(df_ag$commodity == "pancake mixes")
df_pancake_ag <- df_ag[pancake_index, ]

#Sort by revenue to find top sellers
df_pancake_ag <- df_pancake_ag[order(-df_pancake_ag$revenue),]

#SYRUP
#New dataframe with only syrup products
syrup_index <- which(df_ag$commodity == "syrups")
df_syrup_ag <- df_ag[syrup_index, ]

#Sort by revenue to find top sellers
df_syrup_ag <- df_syrup_ag[order(-df_syrup_ag$revenue),]




  #b.	What are the top 5 brands in each commodity?

#Aggregate by brand for pasta commodity, sort by revenue
df_ag_pasta <- df_pasta_ag %>% group_by(brand) %>%
  dplyr::summarize(revenue = sum(revenue))

df_ag_pasta <- df_ag_pasta[order(-df_ag_pasta$revenue),]

#Aggregate by brand for pasta sauce commodity, sort by revenue
df_ag_sauce <- df_sauce_ag %>% group_by(brand) %>%
  dplyr::summarize(revenue = sum(revenue))

df_ag_sauce <- df_ag_sauce[order(-df_ag_sauce$revenue),]


#Aggregate by brand for pancake commodity, sort by revenue
df_ag_pancake <- df_pancake_ag %>% group_by(brand) %>%
  dplyr::summarize(revenue = sum(revenue))

df_ag_pancake <- df_ag_pancake[order(-df_ag_pancake$revenue),]

#Aggregate by brand for pancake syrup commodity, sort by revenue
df_ag_syrup <- df_syrup_ag %>% group_by(brand) %>%
  dplyr::summarize(revenue = sum(revenue))

df_ag_syrup <- df_ag_syrup[order(-df_ag_syrup$revenue),]





#c.	What drives my sales? Which brands and which customers

#Create a dataframe with only pasta transactions for visualization in Tableau
pasta_rowindex <- which(df_tp$commodity == "pasta")
df_pasta <- df_tp[pasta_rowindex, ]

write.csv(df_pasta, "pasta_tableau.csv")



#d.	What is the repeat rate for each commodity? 

#% of households that made a repeat purchase for a commodity 

#Aggregate reveue and number of transactions by commodity and household
df_ag2 <- df_tp %>% group_by(commodity, household) %>%
  dplyr::summarize(revenue = sum(revenue), num_records = n())

#Find the number of households who have purchased in pasta category & # of households
#who have repeat (>1) transactions in the pasta category 

#Total pasta customers 
pasta_index2 <- which(df_ag2$commodity == "pasta")
df_pasta_ag2 <- df_ag2[pasta_index2, ]
total_pasta_customers <- nrow(df_pasta_ag2)

#Find repeat customers
df_repeat_pasta <- which(df_pasta_ag2$num_records > 1)
repeat_pasta_customers <- length(df_repeat_pasta)

#Pasta repeat rate
pasta_repeat_rate <- repeat_pasta_customers / total_pasta_customers 




#Find the number of households who have purchased in pasta sauce category & # of households
#who have repeat (>1) transactions in the pasta sauce category 

#Total pasta sauce customers 
sauce_index2 <- which(df_ag2$commodity == "pasta sauce")
df_sauce_ag2 <- df_ag2[sauce_index2, ]
total_sauce_customers <- nrow(df_sauce_ag2)

#Find repeat customers
df_repeat_sauce <- which(df_sauce_ag2$num_records > 1)
repeat_sauce_customers <- length(df_repeat_sauce)

#Pasta sauce repeat rate
sauce_repeat_rate <- repeat_sauce_customers / total_sauce_customers 




#Find the number of households who have purchased in pancake category & # of households
#who have repeat (>1) transactions in the pancake category 

#Total pancake customers 
pancake_index2 <- which(df_ag2$commodity == "pancake mixes")
df_pancake_ag2 <- df_ag2[pancake_index2, ]
total_pancake_customers <- nrow(df_pancake_ag2)

#Find repeat customers
df_repeat_pancake <- which(df_pancake_ag2$num_records > 1)
repeat_pancake_customers <- length(df_repeat_pancake)

#Pancake repeat rate
pancake_repeat_rate <- repeat_pancake_customers / total_pancake_customers 




#Find the number of households who have purchased in syrup category & # of households
#who have repeat (>1) transactions in the syrup category 

#Total pancake customers 
syrup_index2 <- which(df_ag2$commodity == "syrups")
df_syrup_ag2 <- df_ag2[syrup_index2, ]
total_syrup_customers <- nrow(df_syrup_ag2)

#Find repeat customers
df_repeat_syrup <- which(df_syrup_ag2$num_records > 1)
repeat_syrup_customers <- length(df_repeat_syrup)

#Pancake repeat rate
syrup_repeat_rate <- repeat_syrup_customers / total_syrup_customers 





#e.	How is the health of the category?

#Analysis for health of categories conducted in Tableau using aggregated pasta dataframe



 # f.	Any other recommendations based on what you have looked at the data


#Finalize full dataset merge for additional recommendations (prep for Tableau)
df_all <- merge(x = df_tp, y = df_causal, by = c("upc", "store", "week"), all.x = TRUE)

#Manipulate time of transaction... add trailing zeros using stringi
install.packages("stringi")
library(stringi)
df_all$time_of_transaction <- as.character(df_all$time_of_transaction)
df_all$time <- stri_pad_left(df_all$time_of_transaction, 4, 0)
df_all$time <- as.POSIXct(df_all$time, format = '%H%M')
df_all$hour = format(as.POSIXct(df_all$time,format="%H:%M:%S"),"%H")

#Join store dataframe with df_all (df_all include all datasets except "store")
df_all2 <- merge(x = df_all, y = df_store, by = "store", all.x = TRUE)

#Derive State and City from zipcode
install.packages("zipcode")
library(zipcode)

store_zip_code <- df_all2$store_zip_code
zip_df <- data.frame(store_zip_code)
data("zipcode")
zipdf <- merge(zip_df, zipcode)
zipdf$store_zip_code <- zipdf$zip

write.csv(zipdf, "zip_df.csv")


write.csv(df_all, "df_all.csv")
write.csv(df_all2, "df_all2.csv")


