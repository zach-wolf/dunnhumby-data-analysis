library(caret)
library(dplyr)
library(ggplot2)
library(descr)
library(dummies)
library(Metrics)

setwd("~/Desktop/dunnhumby-data-analysis/data/")
campaign_desc <- read.csv("campaign_desc.csv", header = T)
campaign_table <- read.csv("campaign_table.csv", header = T)
causal_data <- read.csv("causal_data.csv", header = T)
coupon_redempt <- read.csv("coupon_redempt.csv", header = T)
coupon <- read.csv("coupon.csv", header = T)
hh_demographic <- read.csv("hh_demographic.csv", header = T)
product <- read.csv("product.csv", header = T)
transaction_data <- read.csv("transaction_data.csv", header = T)

#### EDA ####

# ggplot(data=campaign_desc, aes(x=CAMPAIGN, y=END_DAY-START_DAY, fill = DESCRIPTION)) +
#   geom_histogram(stat="identity")

head(transaction_data)
table(transaction_data$WEEK_NO)
ggplot(data=transaction_data, aes(x=WEEK_NO)) +
  geom_histogram(fill = "orange", color = "black", binwidth = 1) +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "Transactions per Week", x = "Week", y = "Transaction Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# Calculate revenue
transaction_data$REVENUE <- transaction_data$QUANTITY*transaction_data$SALES_VALUE
transaction_data$household_key <- as.character(transaction_data$household_key)
transaction_data$BASKET_ID <- as.character(transaction_data$BASKET_ID)
transaction_data$PRODUCT_ID <- as.character(transaction_data$PRODUCT_ID)
transaction_data$STORE_ID <- as.character(transaction_data$STORE_ID)

ggplot(data=transaction_data, aes(x=REVENUE)) +
  geom_histogram()

# exclude outlier transactions
transaction_data <- transaction_data[transaction_data$REVENUE <= quantile(transaction_data$REVENUE, 0.99),]

weekly_revenue <- transaction_data %>%
  group_by(WEEK_NO) %>%
  summarise(REVENUE = sum(REVENUE),
            CUSTOMERS = length(unique(household_key)),
            BASKETS = length(unique(BASKET_ID)))

# the first 4 months of data looks incomplete so we will remove from analysis
weekly_revenue <- weekly_revenue[-c(1:16),]

# weekly revenue plot
ggplot(data=weekly_revenue, aes(x=WEEK_NO, y=REVENUE)) +
  geom_line(stat="identity") +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "Revenue per Week", x = "Week", y = "Revenue") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# weekly revenue change 
weekly_revenue <- weekly_revenue %>%
  mutate(PCT_CHANGE = (REVENUE/lag(REVENUE) - 1) * 100) 

# weekly revenue pct change plot
ggplot(data=weekly_revenue, aes(x=WEEK_NO, y=PCT_CHANGE)) +
  geom_line(stat="identity") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "Revenue % Change per Week", x = "Week", y = "Revenue % Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# weekly customers plot
ggplot(data=weekly_revenue, aes(x=WEEK_NO, y=CUSTOMERS)) +
  geom_line(stat="identity") +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "Customers per Week", x = "Week", y = "Customers") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# weekly baskets plot
ggplot(data=weekly_revenue, aes(x=WEEK_NO, y=BASKETS)) +
  geom_line(stat="identity") +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "Baskets per Week", x = "Week", y = "Baskets") +
  theme(plot.title = element_text(hjust = 0.5))

weekly_revenue <- weekly_revenue %>%
  mutate(REVENUE_BASKET = REVENUE/BASKETS)

# weekly revenue per basket plot
ggplot(data=weekly_revenue, aes(x=WEEK_NO, y=REVENUE_BASKET)) +
  geom_line(stat="identity") +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "Basket Revenue per Week", x = "Week", y = "Revenue per Basket") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# NEW CUSTOMERS
min_date <- transaction_data %>%
  group_by(household_key) %>%
  summarise(MinDate = min(WEEK_NO))

transaction_data <- left_join(transaction_data,
                              min_date,
                              by = "household_key")

transaction_data$NEW_CUSTOMER <- ifelse(transaction_data$MinDate==transaction_data$WEEK_NO, 1, 0)

new_vs_existing <- transaction_data %>%
  group_by(WEEK_NO,NEW_CUSTOMER) %>%
  summarise(REVENUE = sum(REVENUE),
            CUSTOMERS = length(unique(household_key))) 

new_ratio <- transaction_data %>%
  group_by(WEEK_NO) %>%
  summarise(NEW_RATIO = length(unique(household_key[NEW_CUSTOMER==1]))/length(unique(household_key)))

ggplot(data=new_ratio, aes(x=WEEK_NO, y=NEW_RATIO)) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "New Customer Ratio per Week", x = "Week", y = "New Customer Ratio") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# RETENTION 
weekly_customers <- transaction_data %>%
  group_by(WEEK_NO, household_key) %>%
  summarise(sum(REVENUE))

retention_matrix <- crosstab(weekly_customers$household_key, weekly_customers$WEEK_NO, dnn = c("household_key", "WEEK_NO"), plot = FALSE)

retention_table = data.frame()
for(i in 1:101){
  selected_week <- i+1
  prev_week <- i
  retention_data <- data.frame(matrix(data = NA, nrow = 1, ncol = 3))
  colnames(retention_data) <- c("WEEK_NO","TotalCustomers","RetainedCustomers")
  retention_data$WEEK_NO[1] <- selected_week
  retention_data$TotalCustomers[1] <- sum(retention_matrix$tab[,selected_week])
  retention_data$RetainedCustomers[1] <- sum(retention_matrix$tab[retention_matrix$tab[,selected_week]>0 & retention_matrix$tab[,prev_week]>0, selected_week])
  retention_table <- rbind(retention_table, retention_data)
}

retention_table$RetentionRate <- retention_table$RetainedCustomers/retention_table$TotalCustomers

# weekly retention rate
ggplot(data=retention_table, aes(x=WEEK_NO, y=RetentionRate)) +
  geom_line(stat="identity") +
  scale_x_continuous(breaks = seq(0,102,10)) +
  labs(title = "Retention per Week", x = "Week", y = "Retention") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))


#### SEGMENTATION ####

# recency
max_day <- max(transaction_data$DAY)

recency_table <- transaction_data %>%
  group_by(household_key) %>%
  summarise(MaxPurchaseDate = max(DAY),
            Recency = max_day - max(DAY))

# recency plot
ggplot(data=recency_table, aes(x=Recency)) +
  geom_histogram(fill = "orange", color = "black", binwidth = 7) +
  scale_x_continuous(breaks = seq(0,1000,50)) +
  labs(title = "Customer Recency", x = "Days Since Last Visit", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# kmeans cluster into 4 groups
set.seed(1234)
recency_k <- kmeans(recency_table$Recency, centers = 4)
recency_table$Cluster <- recency_k$cluster
recency_table$Score <- ifelse(recency_table$Cluster==2,1,
                              ifelse(recency_table$Cluster==3,2,
                                     ifelse(recency_table$Cluster==4,3,4)))

# frequency
frequency_table <- transaction_data %>%
  group_by(household_key) %>%
  summarise(Frequency = length(unique(BASKET_ID)))

# frequency plot
ggplot(data=frequency_table, aes(x=Frequency)) +
  geom_histogram(fill = "orange", color = "black", binwidth = 25) +
  scale_x_continuous(breaks = seq(0,2000,100)) +
  labs(title = "Customer Frequency", x = "Visits", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# kmeans cluster into 4 groups
set.seed(1234)
frequency_k <- kmeans(frequency_table$Frequency, centers = 4)
frequency_table$Cluster <- frequency_k$cluster
frequency_table$Score <- ifelse(frequency_table$Cluster==2,1,
                              ifelse(frequency_table$Cluster==3,2,
                                     ifelse(frequency_table$Cluster==4,3,4)))


# revenue
revenue_table <- transaction_data %>%
  group_by(household_key) %>%
  summarise(Revenue = sum(REVENUE))

# revenue plot
ggplot(data=revenue_table, aes(x=Revenue)) +
  geom_histogram(fill = "orange", color = "black", binwidth = 1000) +
  scale_x_continuous(breaks = seq(0,40000,5000)) +
  labs(title = "Customer Revenue", x = "Revenue", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# kmeans cluster into 4 groups
set.seed(1234)
revenue_k <- kmeans(revenue_table$Revenue, centers = 4)
revenue_table$Cluster <- revenue_k$cluster
revenue_table$Score <- ifelse(revenue_table$Cluster==3,1,
                              ifelse(revenue_table$Cluster==2,2,
                                     ifelse(revenue_table$Cluster==4,3,4)))


# Customer Profiles
customer_profiles <- cbind(recency_table$household_key,
                           recency_table$Recency,
                           recency_table$Score,
                           frequency_table$Frequency,
                           frequency_table$Score,
                           revenue_table$Revenue,
                           revenue_table$Score)
customer_profiles <- as.data.frame(customer_profiles)
colnames(customer_profiles) <- c("household_key",
                                 "Recency",
                                 "RecencyCluster",
                                 "Frequency",
                                 "FrequencyCluster",
                                 "Revenue",
                                 "RevenueCluster")
customer_profiles$Recency <- as.numeric(as.character(customer_profiles$Recency))
customer_profiles$RecencyCluster <- as.numeric(as.character(customer_profiles$RecencyCluster))
customer_profiles$Frequency <- as.numeric(as.character(customer_profiles$Frequency))
customer_profiles$FrequencyCluster <- as.numeric(as.character(customer_profiles$FrequencyCluster))
customer_profiles$Revenue <- as.numeric(as.character(customer_profiles$Revenue))
customer_profiles$RevenueCluster <- as.numeric(as.character(customer_profiles$RevenueCluster))

customer_profiles$OverallScore <- customer_profiles$RecencyCluster + customer_profiles$FrequencyCluster + customer_profiles$RevenueCluster

scores <- customer_profiles %>%
  group_by(OverallScore) %>%
  summarise(Count = n(),
            Recency = mean(as.numeric(as.character(Recency))),
            Frequency = mean(as.numeric(as.character(Frequency))),
            Revenue = mean(as.numeric(as.character(Revenue))))

customer_profiles$ScoreGroup <- ifelse(
  customer_profiles$OverallScore <= 5, "Low-Value",
  ifelse(customer_profiles$OverallScore > 5 & customer_profiles$OverallScore <= 8, "Mid-Value", "High-Value")
)
customer_profiles$ScoreGroup <- as.factor(customer_profiles$ScoreGroup)
customer_profiles$ScoreGroup <- factor(customer_profiles$ScoreGroup, levels = c("High-Value","Mid-Value","Low-Value"))

# recency vs revenue color by group 
ggplot(data = customer_profiles, aes(x = Recency, y = Revenue, color = ScoreGroup)) +
  geom_point(size = 3) +
  labs(title = "Customer Segments: Recency vs Revenue", color = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# frequency vs revenue color by group 
ggplot(data = customer_profiles, aes(x = Frequency, y = Revenue, color = ScoreGroup)) +
  geom_point(size = 3) +
  labs(title = "Customer Segments: Frequency vs Revenue", color = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# frequency vs revenue color by group 
ggplot(data = customer_profiles, aes(x = Recency, y = Frequency, color = ScoreGroup)) +
  geom_point(size = 3) +
  labs(title = "Customer Segments: Recency vs Frequency", color = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

customer_profiles$household_key <- as.character(customer_profiles$household_key)
hh_demographic$household_key <- as.character(hh_demographic$household_key)

# 801 customers w demographic data
customer_profiles_dem <- inner_join(customer_profiles,
                                    hh_demographic,
                                    by = "household_key")

# Only 6 "Low-Value" customers with loyalty reward cards
ggplot(data = customer_profiles_dem, aes(x = ScoreGroup, fill = ScoreGroup)) +
  geom_histogram(stat = "count") +
  labs(title = "Customers with Demographic Data",
       x = "Customer Segment",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.position = 'none')
  

table(customer_profiles_dem$ScoreGroup)

# distribution of score groups by age
ggplot(data = customer_profiles_dem, aes(x = AGE_DESC, fill = ScoreGroup)) +
  geom_histogram(stat = "count", position = position_dodge2(preserve = "single")) +
  labs(title = "Customer Segments: Age", 
       x = "Age Group",
       y = "Count",
       fill = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# distribution of score groups by marital status
ggplot(data = customer_profiles_dem, aes(x = MARITAL_STATUS_CODE, fill = ScoreGroup)) +
  geom_histogram(stat = "count", position = position_dodge2(preserve = "single")) +
  labs(title = "Customer Segments: Marital Status", 
       x = "Marital Status",
       y = "Count",
       fill = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# distribution of score groups by income
customer_profiles_dem$INCOME_DESC <- factor(customer_profiles_dem$INCOME_DESC,
                                            levels = c("Under 15K",
                                                       "15-24K",
                                                       "25-34K",
                                                       "35-49K",
                                                       "50-74K",
                                                       "75-99K",
                                                       "100-124K",
                                                       "125-149K",
                                                       "150-174K",
                                                       "175-199K",
                                                       "200-249K",
                                                       "250K+"))

ggplot(data = customer_profiles_dem, aes(x = INCOME_DESC, fill = ScoreGroup)) +
  geom_histogram(stat = "count", position = position_dodge2(preserve = "single")) +
  labs(title = "Customer Segments: Income", 
       x = "Income Group",
       y = "Count",
       fill = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# distribution of score groups by homeownership
ggplot(data = customer_profiles_dem, aes(x = HOMEOWNER_DESC, fill = ScoreGroup)) +
  geom_histogram(stat = "count", position = position_dodge2(preserve = "single")) +
  labs(title = "Customer Segments: Homeownership", 
       x = "Homeownership",
       y = "Count",
       fill = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# distribution of score groups by household composition
ggplot(data = customer_profiles_dem, aes(x = HH_COMP_DESC, fill = ScoreGroup)) +
  geom_histogram(stat = "count", position = position_dodge2(preserve = "single")) +
  labs(title = "Customer Segments: Household Composition", 
       x = "Household Composition",
       y = "Count",
       fill = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# distribution of score groups by household size
ggplot(data = customer_profiles_dem, aes(x = HOUSEHOLD_SIZE_DESC, fill = ScoreGroup)) +
  geom_histogram(stat = "count", position = position_dodge2(preserve = "single")) +
  labs(title = "Customer Segments: Household Size", 
       x = "Household Size",
       y = "Count",
       fill = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# distribution of score groups by kid category
ggplot(data = customer_profiles_dem, aes(x = KID_CATEGORY_DESC, fill = ScoreGroup)) +
  geom_histogram(stat = "count", position = position_dodge2(preserve = "single")) +
  labs(title = "Customer Segments: Number of Kids", 
       x = "Kids",
       y = "Count",
       fill = "Customer Segment") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

## Highest valued customers are 35-54, married (A) or unknown (U), income between 35-74k, a homeowner, 2 adults in household, with no kids or unknown

#### CUSTOMER CLV ####

trans_26wk <- transaction_data[transaction_data$WEEK_NO >= 24 & transaction_data$WEEK_NO <= 50,]
trans_52wk <- transaction_data[transaction_data$WEEK_NO >= 51,]

# RFV for 26wk data

# Recency
max_day <- max(trans_26wk$DAY)

recency_table_26wk <- trans_26wk %>%
  group_by(household_key) %>%
  summarise(MaxPurchaseDate = max(DAY),
            Recency = max_day - max(DAY))

# recency plot
ggplot(data=recency_table_26wk, aes(x=Recency)) +
  geom_histogram()

# kmeans cluster into 4 groups
set.seed(1234)
recency_k <- kmeans(recency_table_26wk$Recency, centers = 4)
recency_table_26wk$Cluster <- recency_k$cluster
recency_table_26wk$Score <- ifelse(recency_table_26wk$Cluster==1,1,
                              ifelse(recency_table_26wk$Cluster==3,2,
                                     ifelse(recency_table_26wk$Cluster==4,3,4)))


# Frequency
frequency_table_26wk <- trans_26wk %>%
  group_by(household_key) %>%
  summarise(Frequency = length(unique(BASKET_ID)))

# frequency plot
ggplot(data=frequency_table_26wk, aes(x=Frequency)) +
  geom_histogram()

# kmeans cluster into 4 groups
set.seed(1234)
frequency_k <- kmeans(frequency_table_26wk$Frequency, centers = 4)
frequency_table_26wk$Cluster <- frequency_k$cluster
frequency_table_26wk$Score <- ifelse(frequency_table_26wk$Cluster==3,1,
                                     ifelse(frequency_table_26wk$Cluster==4,2,
                                            ifelse(frequency_table_26wk$Cluster==1,3,4)))


# Revenue
revenue_table_26wk <- trans_26wk %>%
  group_by(household_key) %>%
  summarise(Revenue = sum(REVENUE))

# revenue plot
ggplot(data=revenue_table_26wk, aes(x=Revenue)) +
  geom_histogram()

# kmeans cluster into 4 groups
set.seed(1234)
revenue_k <- kmeans(revenue_table_26wk$Revenue, centers = 4)
revenue_table_26wk$Cluster <- revenue_k$cluster
revenue_table_26wk$Score <- ifelse(revenue_table_26wk$Cluster==3,1,
                              ifelse(revenue_table_26wk$Cluster==4,2,
                                     ifelse(revenue_table_26wk$Cluster==1,3,4)))


# Customer Profiles
customer_profiles_26wk <- cbind(recency_table_26wk$household_key,
                           recency_table_26wk$Recency,
                           recency_table_26wk$Score,
                           frequency_table_26wk$Frequency,
                           frequency_table_26wk$Score,
                           revenue_table_26wk$Revenue,
                           revenue_table_26wk$Score)
customer_profiles_26wk <- as.data.frame(customer_profiles_26wk)
colnames(customer_profiles_26wk) <- c("household_key",
                                 "Recency",
                                 "RecencyCluster",
                                 "Frequency",
                                 "FrequencyCluster",
                                 "Revenue",
                                 "RevenueCluster")
customer_profiles_26wk$household_key <- as.character(customer_profiles_26wk$household_key)
customer_profiles_26wk$Recency <- as.numeric(as.character(customer_profiles_26wk$Recency))
customer_profiles_26wk$RecencyCluster <- as.numeric(as.character(customer_profiles_26wk$RecencyCluster))
customer_profiles_26wk$Frequency <- as.numeric(as.character(customer_profiles_26wk$Frequency))
customer_profiles_26wk$FrequencyCluster <- as.numeric(as.character(customer_profiles_26wk$FrequencyCluster))
customer_profiles_26wk$Revenue <- as.numeric(as.character(customer_profiles_26wk$Revenue))
customer_profiles_26wk$RevenueCluster <- as.numeric(as.character(customer_profiles_26wk$RevenueCluster))

customer_profiles_26wk$OverallScore <- customer_profiles_26wk$RecencyCluster + customer_profiles_26wk$FrequencyCluster + customer_profiles_26wk$RevenueCluster

scores <- customer_profiles_26wk %>%
  group_by(OverallScore) %>%
  summarise(Count = n(),
            Recency = mean(as.numeric(as.character(Recency))),
            Frequency = mean(as.numeric(as.character(Frequency))),
            Revenue = mean(as.numeric(as.character(Revenue))))

customer_profiles_26wk$ScoreGroup <- ifelse(
  customer_profiles_26wk$OverallScore <= 5, "Low-Value",
  ifelse(customer_profiles_26wk$OverallScore > 5 & customer_profiles_26wk$OverallScore <= 8, "Mid-Value", "High-Value")
)

# Find next 52wk customer clv (revenue)
customer_clv_52wk <- trans_52wk %>%
  group_by(household_key) %>%
  summarise(Revenue52wk = sum(REVENUE))

# Join 26wk features to 52wk clv
customer_clv_big <- left_join(customer_profiles_26wk,
                              customer_clv_52wk,
                              by = "household_key")
customer_clv_big$Revenue52wk <- ifelse(is.na(customer_clv_big$Revenue52wk),0,customer_clv_big$Revenue52wk)

# plot relationship between loyalty score and revenue
ggplot(data = customer_clv_big, aes(x = OverallScore, y = Revenue52wk)) +
  geom_point() +
  scale_x_continuous(breaks = seq(3,12,1)) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(title = "Customer Loyalty vs CLV", 
       x = "Customer Loyalty Score",
       y = "52wk Revenue") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14))

# Predict CLV with RFV score

# dummy columns for score groups
dummy_cols <- dummy(customer_clv_big$ScoreGroup)
dummy_cols <- as.data.frame(dummy_cols)
customer_clv_big <- cbind(customer_clv_big,dummy_cols)

# Define CLV segments
set.seed(1234)
clv_clusters <- kmeans(customer_clv_big$Revenue52wk, centers = 3)
customer_clv_big$clvCluster <- clv_clusters$cluster
customer_clv_big$clvCluster <- ifelse(customer_clv_big$clvCluster==3, 1,
                                      ifelse(customer_clv_big$clvCluster==1,2,3))
customer_clv_big$clvCluster <- as.factor(customer_clv_big$clvCluster)

# 1 - Low clv
# 2 - Mid clv
# 3 - High clv

# # find features correlated with 52wk revenue
# correlationMatrix <- cor(customer_clv_big[, -which(names(customer_clv_big) %in% c("household_key","ScoreGroup"))])
# correlationMatrix[,"Revenue52wk"]
# # revenue, rfv, frequency

# drop household ScoreGroup in feature set
customer_clv_feature <- customer_clv_big[, -which(names(customer_clv_big) %in% c("household_key","ScoreGroup","clvCluster"))]

set.seed(1234)
# split into test and train set
index <- createDataPartition(customer_clv_feature$Revenue52wk, p = 0.75, list = FALSE)

# training set
train_clv <- customer_clv_feature[index, ]

# test set
test_clv <- customer_clv_feature[-index, ]

train_control <- trainControl(method="repeatedcv", repeats=3)

# Train a model 
model_clv <- train(train_clv[, -which(names(train_clv) %in% c("Revenue52wk"))],
                   train_clv[, "Revenue52wk"],
                   method = 'lm',
                   family = "response",
                   trControl = train_control
                   # ,preProcess = c('scale','center') #normalization
                   # ,linout = TRUE
                   # ,skip = TRUE
                   # ,maxit = 4000
                   )
# Random Forest - RMSE: 1619.71   R2: 0.64
# Linear Regres - RMSE: 1514.14   R2: 0.67
## Neural Net - RMSE: 1520.01   R2: 0.68

# selected multiple linear regression model

predictions_clv <- predict(object = model_clv,
                           test_clv[, -which(names(test_clv) %in% c("Revenue52wk"))])
actuals_preds <- data.frame(cbind(actuals=test_clv$Revenue52wk, predicteds=predictions_clv))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy[1,2]^2 
rmse(test_clv$Revenue52wk, predictions_clv) 
# Test set - RMSE: 1482.27   R2: 0.66


# # find features correlated with 52wk revenue
# correlationMatrix <- cor(customer_clv_big[, -which(names(customer_clv_big) %in% c("household_key","ScoreGroup","Revenue52wk"))])
# correlationMatrix[,"clvCluster"]
# # revenue, RFV, frequency

# drop household ScoreGroup in feature set
customer_clv_clust_feature <- customer_clv_big[, -which(names(customer_clv_big) %in% c("household_key","ScoreGroup","Revenue52wk"))]

set.seed(1234)
# split into test and train set
index <- createDataPartition(customer_clv_clust_feature$clvCluster, p = 0.75, list = FALSE)

# training set
train_clv_clust <- customer_clv_clust_feature[index, ]

# test set
test_clv_clust <- customer_clv_clust_feature[-index, ]

train_control <- trainControl(method="repeatedcv", repeats=3)

# Train a model 
model_clv_clust <- train(train_clv_clust[, -which(names(train_clv_clust) %in% c("clvCluster"))],
                         train_clv_clust[, "clvCluster"],
                         method = 'xgbTree',
                         trControl = train_control)
# xgboost: 0.81
# Random Forest: 0.80
# knn: 0.79
## Neural Net: 0.81

# selected neural net model (performed better on test set)

predictions_clv_clust <- predict(model_clv_clust,test_clv_clust[, -which(names(test_clv_clust) %in% c("clvCluster"))])

confusionMatrix(predictions_clv_clust,test_clv_clust$clvCluster)
# Accuracy on test set: 0.80 (0.91, 0.59, 0.53)