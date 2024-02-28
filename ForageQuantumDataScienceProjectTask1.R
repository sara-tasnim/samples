# loading related packages
library(data.table)
library(ggplot2)
library(readr)
library(readxl)

# loading datasets
transactionData <- read_excel("QVI_transaction_data.xlsx")
purchaseData <- read.csv("QVI_purchase_behaviour.csv")

# exploring datasets
head(purchaseData)
head(transactionData)

# checking datatype for transaction data
class(transactionData$DATE)
class(transactionData$STORE_NBR)
class(transactionData$LYLTY_CARD_NBR)
class(transactionData$TXN_ID)
class(transactionData$PROD_NBR)
class(transactionData$PROD_NAME)
class(transactionData$PROD_QTY)
class(transactionData$TOT_SALES)

# coverting date column
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
class(transactionData$DATE)

# examining product names
summary(transactionData$PROD_NAME)
unique(transactionData$PROD_NAME)

productwords <- data.table(unlist(strsplit(unique(as.character(transactionData[, "PROD_NAME"])), " ")))
setnames(productwords, "words")

productwords <- productwords[!grepl("\\d", words)]
productwords <- productwords[!grepl("[^A-Za-z]", words)]
wordCounts <- table(productwords$words)
wordCountsSorted <- sort(wordCounts, decreasing = TRUE)

# removing salsa
transactionData <- transactionData[!grepl("salsa", tolower(transactionData$PROD_NAME), ignore.case = TRUE), ]

# checking for nulls and possible outliers
summary(transactionData)

# Filter the dataset to find the outlier
filtered_prodQTY <- transactionData[transactionData$PROD_QTY > 100, ]
filtered_prodQTY

# checking all the transactions of this customer
cust226000 <- transactionData[transactionData$LYLTY_CARD_NBR == 226000, ]
cust226000

# removing 226000
transactionData_filtered <- transactionData[transactionData$LYLTY_CARD_NBR!= 226000, ]

# Counting the number of transactions by date
library(dplyr)
transaction_summary <- transactionData %>%
  group_by(DATE) %>%
  summarise(Transactions = n())
transaction_summary

# Creating a sequence of dates and join this the count of transactions by date
library(tidyr)
date_seq <- seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day")
date_seq_df <- data.frame(DATE = date_seq)

transaction_by_day <- date_seq_df %>%
  left_join(transaction_summary, by = "DATE")
transaction_by_day$Transactions[is.na(transaction_by_day$Transactions)] <- 0

transactionData_filtered <- date_seq_df %>%
  left_join(transactionData_filtered, by = "DATE")

# handling nas'
transactionData_filtered$PROD_QTY[is.na(transactionData_filtered$PROD_QTY)] <- 0
transactionData_filtered$LYLTY_CARD_NBR[is.na(transactionData_filtered$LYLTY_CARD_NBR)] <- 0
transactionData_filtered$TXN_ID[is.na(transactionData_filtered$TXN_ID)] <- 0
transactionData_filtered$PROD_NBR[is.na(transactionData_filtered$PROD_NBR)] <- 0
transactionData_filtered$TOT_SALES[is.na(transactionData_filtered$TOT_SALES)] <- 0
summary(transactionData_filtered)

# setting plot themes
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Plot transactions over time
ggplot(transaction_by_day, aes(x = DATE, y = Transactions)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Filter to December and look at individual days
december_transaction <- transaction_by_day %>%
  filter(month(DATE) == 12)

ggplot(december_transaction, aes(x = DATE, y = Transactions)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# making pack sizes
setDT(transactionData)
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]
# Let's check if the pack sizes look sensible 
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

# histogram of PACK_SIZE
ggplot(transactionData, aes(x = factor(PACK_SIZE))) +
  geom_bar() +
  labs(x = "Pack Size", y = "Number of Transactions", title = "Histogram of Pack Size")

# brands
library(stringr)
transactionData$BRAND <- str_extract(transactionData$PROD_NAME, "\\w+")
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "Red", BRAND := "RRD"]
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]
transactionData[BRAND == "Grain", BRAND := "GrnWves"]
transactionData[BRAND == "Dorito", BRAND := "Doritos"]
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]
transactionData[BRAND == "WW", BRAND := "Woolworths"]
transactionData[BRAND == "Natural", BRAND := "NCC"]
transactionData[BRAND == "Smith", BRAND := "Smiths"]
unique(transactionData$BRAND)

# Examining customer dataset
summary(purchaseData)
unique(purchaseData$LIFESTAGE)
unique(purchaseData$PREMIUM_CUSTOMER)

# Merging transaction data to customer data
data <- merge(transactionData, purchaseData, all.x = TRUE)

# checking for missing value
missing_customers <- data[is.na(data$PREMIUM_CUSTOMER), ]
missing_customers

# saving dataset as csv
fwrite(data, paste0("QVI_data.csv"))

#Who spends the most on chips (total sales)
salesBycust <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(SALES = sum(TOT_SALES))
ggplot(salesBycust, aes(x = LIFESTAGE, y = SALES, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lifestage", y = "Total Sales", title = "Total Sales by Lifestage and Premium Customer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
nbrCustLft <- data %>%
  group_by(LIFESTAGE) %>%
  summarise(NbrOfCustomer = n_distinct(LYLTY_CARD_NBR))
nbrCustPrm <- data %>%
  group_by(PREMIUM_CUSTOMER) %>%
  summarise(NbrOfCustomer = n_distinct(LYLTY_CARD_NBR))
nbrOfCust <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(CustomerNbr = n_distinct(LYLTY_CARD_NBR))
ggplot(nbrOfCust, aes(x = LIFESTAGE, y = CustomerNbr, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lifestage", y = "Number of Customers", title = "Number of customer by lifestage and premium customers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set4")

# Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
unitsByCustL <- data %>%
  group_by(LIFESTAGE) %>%
  summarise(TOT_UNITS = sum(PROD_QTY))
unitsByCustP <- data %>%
  group_by(PREMIUM_CUSTOMER) %>%
  summarise(TOT_UNITS = sum(PROD_QTY))
unitsByCust <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(TOT_UNITS = mean(PROD_QTY))
ggplot(unitsByCust, aes(x = LIFESTAGE, y = TOT_UNITS, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lifestage", y = "Total units bought", title = "Units bought by customer by lifestage and premium customers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set10")

# Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
priceByCustL <- data %>%
  group_by(LIFESTAGE) %>%
  summarise(AVG_Price = mean(TOT_SALES/PROD_QTY))
priceByCustP <- data %>%
  group_by(PREMIUM_CUSTOMER) %>%
  summarise(AVG_Price = mean(TOT_SALES/PROD_QTY))
PriceByCust <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(AVG_Price = mean(TOT_SALES/PROD_QTY))
ggplot(PriceByCust, aes(x = LIFESTAGE, y = AVG_Price, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Lifestage", y = "Total units bought", title = "Units bought by customer by lifestage and premium customers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Performing an independent t-test between mainstream vs premium and budget midage 
# and young singles and couples
mainstreamCust <- PriceByCust[PriceByCust$LIFESTAGE %in%
                         c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES") &
                         PriceByCust$PREMIUM_CUSTOMER == "Mainstream", ]
premBdgCust <- PriceByCust[PriceByCust$LIFESTAGE %in%
                      c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES") &
                      PriceByCust$PREMIUM_CUSTOMER != "Mainstream", ]
t_test <- t.test(mainstreamCust$AVG_Price, premBdgCust$AVG_Price)

# Deep dive into specific customer segments for insights
# Deep dive into Mainstream, young singles/couples
mainstream_young <- data[data$LIFESTAGE %in% c("YOUNG SINGLES/COUPLES") & 
                                data$PREMIUM_CUSTOMER == "Mainstream", ]
brand_preferences <- mainstream_young %>%
  group_by(BRAND) %>%
  summarise(Total_Purchases = sum(TOT_SALES))
brand_preferences <- brand_preferences[order(-brand_preferences$Total_Purchases), ]
head(brand_preferences,5)
### kettle is the most preferred brand by this specific segment of customers 

# Preferred pack size compared to the rest of the population
pack_preferences <- mainstream_young %>%
  group_by(PACK_SIZE) %>%
  summarise(Preferred_pack = sum(PACK_SIZE))
pack_preferences <- pack_preferences[order(-pack_preferences$Preferred_pack), ]
head(pack_preferences, 5)
### we can see that the most preferred pack size is 175 followed by 150.

