library(lubridate)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
amazon <- read.csv("Amazon Sale Report.csv")
international_sale <- read.csv("International sale Report.csv")
sale <- read.csv("Sale Report.csv")

### Data Description ###

#check for the first 5 values
head(amazon)
#check the number of columns and rows before cleaning
dim(amazon)
#check data type for each columns
str(amazon)
summary(amazon)

#International and sale dataset
str(international_sale)
str(sale)

summary(international_sale)
summary(sale)


### Data Cleaning ###

#remove Unnamed..22 column
amazon = amazon%>%select(-Unnamed..22)
dim(amazon)
#change date to Date format
amazon$Date <- parse_date_time(amazon$Date, orders = c("mdy", "m-d-y"))
amazon$Date <- as.Date(amazon$Date)

## remove null values
colSums(is.na(amazon))
amazon = na.omit(amazon)
summary(amazon)
dim(amazon)
write.csv(amazon, file = "C:/Users/User/Desktop/cleaned_amazon_data.csv", row.names = FALSE)
#check if there is any duplicated rows
any_duplicated <- any(duplicated(amazon))
print(any_duplicated)

### Data joining (International $ Sale report)###

# change the column name of SKU Code in Sale_Report to SKU
# change the column name of Design No. in Sale_Report to Style
names(sale)[names(sale) == "SKU.Code"] <- "SKU"
names(sale)[names(sale) == "Design.No."] <- "Style"

# using inner_join to combine both international_sale and sale
joined_report <- inner_join(international_sale, sale, by = c("SKU", "Size", "Style"), relationship = "many-to-many")
head(joined_report)

# check for NA values in joined dataset
colSums(is.na(joined_report))

# remove any row with NA values
clean_joined_report <- joined_report %>% na.omit()
write.csv(clean_joined_report, file = "C:/Users/User/Desktop/cleaned_joined_report.csv", row.names = FALSE)
summary(clean_joined_report)

##### Problem Statement 1#######
#get summarise of data of status
amazon %>%
  group_by(Status) %>%
  summarise(
    total_qty = sum(Qty, na.rm = TRUE),
    total_amount = sum(Amount, na.rm = TRUE)
  )%>%arrange(desc(total_qty))

#see cancelled data of order status
filtered_data <-amazon %>%
  filter(Status %in% c("Shipped - Returned to Seller", "Shipped - Rejected by Buyer", "Shipped - Returning to Seller"))


##### Problem Statement 2#######

# to remove those rows with status "Cancelled"
# create a new column Month
# remove those rows with March as we are only care about Q2 2022
filtered <- amazon %>%
  filter(Status != "Cancelled") %>%
  mutate(Month = format(Date, "%b")) %>%
  filter(Month != "Mar")
str(filtered)


# find the monthly revenue
# find the percentage difference compared to last month
monthly_revenue <- filtered %>%
  group_by(Month) %>%
  summarise(Monthly_Revenue_in_INR = sum(Amount, na.rm = TRUE)) %>%
  arrange(match(Month, c("Apr", "May", "Jun"))) %>%
  mutate(Revenue_Percentage_Change = (Monthly_Revenue_in_INR / lag(Monthly_Revenue_in_INR) - 1) * 100)


##### Problem Statement 3#######
# Convert DATE to data datatype
clean_joined_report <- clean_joined_report %>%
  mutate(DATE = as.Date(DATE, format = "%m-%d-%y"))

# drop index.y column 
clean_joined_report$index.y <- NULL

str(clean_joined_report)
summary(clean_joined_report)


#### Data visualsation 
#1) Line chart

# find the monthly revenue
# find the percentage difference compared to last month
monthly_revenue <- filtered %>%
  group_by(Month) %>%
  summarise(Monthly_Revenue_in_INR = sum(Amount, na.rm = TRUE)) %>%
  arrange(match(Month, c("Apr", "May", "Jun"))) %>%
  mutate(Revenue_Percentage_Change = (Monthly_Revenue_in_INR / lag(Monthly_Revenue_in_INR) - 1) * 100)

# Define the order of months
month_order <- c("Apr", "May", "Jun")
# Convert Month to a factor with the desired order
monthly_revenue$Month <- factor(monthly_revenue$Month, levels = month_order)
# Create a line chart
line_chart <- ggplot(monthly_revenue, aes(x = Month, y = Monthly_Revenue_in_INR, group = 1)) +
  geom_line(colour = "blue") +
  geom_point(colour = "blue") +
  labs(title = "Monthly Revenue for Each Month",
       x = "Month in Quarter 2 2022",
       y = "Monthly Revenue in INR") +  # Add labels
  theme_bw() +  # Apply a minimal theme to the plot
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  scale_y_continuous(breaks = seq(0, 27000000, by = 1000000), labels = scales::comma)
# View line chart
line_chart

#2) Pie Chart
category_cancellations <- filtered_data %>%
  group_by(Category) %>%
  summarise(
    total_qty = sum(Qty, na.rm = TRUE),
    total_orders = n()
  ) %>%
  arrange(desc(total_orders))


pie_chart <- ggplot(category_cancellations, aes(x = "", y = total_orders, fill = Category)) + 
  geom_bar(stat = "identity", 
           width = 1) +
  geom_text(aes(label = paste0(round(total_orders/sum(total_orders)*100, 1),"%")),
            position = position_stack(vjust = 0.5), size = 2.5) +
  coord_polar("y", start = 0) +
  labs(title = "Category-wise Cancellations",
       x = NULL, 
       y = NULL, 
       fill = "Category") +
  theme_void() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) # Center the title


print(pie_chart)

#3) bar chart
# Create a bar chart

#see which month has most cancellations
##remove those rows with March as we are only care about Q2 2022
monthly_cancellations <- filtered_data %>%
  mutate(month = format(as.Date(Date, "%Y-%m-%d"), "%Y-%m")) %>%
  group_by(month) %>%
  filter(month != "2022-03") %>%
  summarise(
    total_orders = n()
  ) %>%
  arrange(month)


bar_chart <- ggplot(monthly_cancellations, aes(x = month, y = total_orders)) +
  geom_bar(stat = "identity", fill = "#87CEEA", width = 0.5) +
  geom_text(aes(label= total_orders), vjust=-0.5, color="black", size=3.5)+
  labs(title = "Monthly Cancellations",
       x = "Month",
       y = "Total Orders Cancelled") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

# View bar chart
print(bar_chart)

#4)
# Bar chart
# Summarize sales by category and extract the top 3 product categories
top_categories <- clean_joined_report %>%
  group_by(Category) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) 

# Print the top 3 categories
print(top_categories)

# Create a bar chart
bar_chart2 <- ggplot(top_categories, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "orange", width = 0.5) +
  geom_text(aes(label=Count), vjust=-0.5, color="black", size=3.5)+
  labs(title = "Total Orders by Category",
       x = "Category",
       y = "Total Orders") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10))

# View bar chart
print(bar_chart2)

#5)
# Create a heat map
# Filter data to include only the top 3 categories
top3_data <- clean_joined_report %>%
  filter(Category %in% c("KURTA", "KURTA SET", "TOP"))

#head(top3_data)

# Create the heatmap
heatmap <- ggplot(top3_data, aes(x = Size, y = Color, fill = Stock)) +
  geom_tile() +
  scale_fill_gradient(low = "skyblue", high = "red") +  # Customize color gradient
  labs(title = "Stock Volume Heatmap", x = "Size", y = "Colour", fill = "Stock Volume") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotate x-axis labels for better readability
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~ Category)  # Create separate heatmaps for each category
print(heatmap)
