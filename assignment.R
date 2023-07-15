#Importing libraries
library(ggplot2)
library(dplyr)

#Reading the data
data <- read.csv("FRUITS AND VEGIES.csv")
View(data)
head(data)
str(data)

#defining variables
yield = data$Yield
cup_size = data$CupEquivalentSize
cup_price = data$CupEquivalentPrice
retail_price = data$RetailPrice

count_form = data %>% count(data$Form)
count_form

#Segmentation of fruit category
canned = data$Fruit[data$Form == "Canned"]
dried = data$Fruit[data$Form == "Dried"]
fresh = data$Fruit[data$Form == "Fresh"]
frozen = data$Fruit[data$Form == "Frozen"]
juice = data$Fruit[data$Form == "Juice"]

#Descriptive Statistics
summary(data)
#yield
var(yield)
sd(yield)
range(yield)

#retail price
range(retail_price)

df1 <- as.data.frame(count_form)
df1

#Number of fruits available according to their forms
ggplot(df1, aes(x = `data$Form`, y = n)) +
  geom_bar(stat = "identity") +
  ylim(0, 30) +
  xlab("Form of fruit") +
  ylab("Count") +
  ggtitle("Count of the forms of fruit") +
  geom_text(aes(label = n), vjust = -0.5)

#Checking mean Retail price of form category
df2 <- df %>%
  group_by(`data$Form`) %>%
  summarise(price = mean(data$RetailPrice[data$Form == first(`data$Form`)]))

ggplot(df2, aes(x = `data$Form`, y = price)) +
  geom_bar(stat = "identity") +
  ylab("Price") +
  xlab("Fruit Form") +
  ggtitle("Mean Price by Fruit Form")

#Correlations
cor_price <- cor(yield, retail_price)
cor_size <- cor(retail_price, cup_size)
cor_yield <- cor(cup_size, yield)
cor_cupPrice <- cor(yield, cup_price)

#Correlation results
cor_cupPrice
cor_yield
cor_size
cor_price

#T Tests
#Independent
ind_result1 <- t.test(yield, retail_price)
ind_result2 <- t.test(cup_size, cup_price)
ind_result3 <- t.test(retail_price, cup_price)

#Results
ind_result1
ind_result2
ind_result3

#Dependent
dep_result1 <- t.test(yield, retail_price, paired=TRUE)
dep_result2 <- t.test(cup_size, cup_price, paired=TRUE)
dep_result3 <- t.test(retail_price, cup_price, paired=TRUE)

#Results
dep_result1
dep_result2
dep_result3
