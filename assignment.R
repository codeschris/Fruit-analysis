#Data visualizations
#descriptive statistics for relevant variables
#independent and paired t-tests

library(ggplot2)
library(dplyr)

#Reading the data
data <- read.csv("FRUITS AND VEGIES.csv")
View(data)
head(data)
str(data)

#defining variables
yield = data$Yield
price = data$RetailPrice
form_pkg = unique(data$Fruit)
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

barplot(count_form)
df <- as.data.frame(count_form)
df

#Number of fruits available according to their forms
ggplot(df, aes(x = `data$Form`, y = n)) +
  geom_bar(stat = "identity") +
  ylim(0, 30) +
  xlab("Form of fruit") +
  ylab("Count") +
  ggtitle("Count of the forms of fruit") +
  geom_text(aes(label = n), vjust = -0.5)

#Checking trend of retail price and yield


        