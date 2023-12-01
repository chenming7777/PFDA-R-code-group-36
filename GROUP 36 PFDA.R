# PFDA Group 36 
# Teh Chen Ming,TP068804
# Aidan Chang Wai Yue, TP063411
# Lim Jing Yee, TP068796
# Ten Xin Ru, TP069426

install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("reshape")
install.packages("hexbin") # for hexbin plot
install.packages("plotly") # for 3D scatter plot
install.packages("DescTools")
install.packages("plotrix")
install.packages("viridis")
install.packages("patchwork")
library(patchwork)
library(viridis)
library(plotrix)
library(DescTools)
library(plotly)
library(reshape)
library(corrplot)
library(dplyr)
library(ggplot2)
library(hexbin)
library(GGally)


# winsorizing will be only done for rent prize and size of house, BHK is still consider
# continuous, the 10 bathroom one row will be delete, as it is a extreme 
# winsorizing will be using 0.05 - 0.95
# This is to change the output limit to 5000
options(max.print = 5000)


#----------------------------------------------------
houserent_data <- read.csv("C:\\Users\\User\\Downloads\\House_Rent_Dataset.csv")
#----------------------------------------------------


# This show class of the attribute, heading name and data type of each column 
# and some data example of each columns and also the dimension of the dataset
str(houserent_data)


# This will check the general state of the data set such as median, mode, maximum
# minimum, class, first quarter, third quarter.
summary(houserent_data)




# This is to differentiate numerical and categorical data.
numeric_columns = houserent_data[, c("BHK", "Rent", "Size", "Bathroom")]

categorical_columns = houserent_data[,c("Posted.On", "Area.Type","City",
                                        "Furnishing.Status", "Tenant.Preferred", 
                                        "Point.of.Contact")]


# Firstly, we will find is there any abnormal data in categorical data
# We will check the missing data first
anyNA(houserent_data)


# Secondly we will check whether there are two identical row in the dataset
duplicate_rows <- houserent_data[duplicated(houserent_data),]
duplicate_rows


# Double check the exist of duplicate data
any(duplicated(houserent_data))


# After this we will check all the string data and the number of times they occur
table(houserent_data$Posted.On)
sorted_table <- sort(table(houserent_data$Area.Locality))
sorted_table
table(houserent_data$Area.Type)
table(houserent_data$City)
table(houserent_data$Furnishing.Status)
table(houserent_data$Tenant.Preferred)
table(houserent_data$Point.of.Contact)





# **************************
# This will be the start of the numerical data data cleaning (visualizing section)
# **************************

# explain why we will cleaning data based each city
# Firstly we find the distribution of renting house in each city using pie chart.
# This is to find the frequency of each city
table(houserent_data$City)

# Data is then save into a dataset call city_freq
city_names <- c("Bangalore", "Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai")
frequencies <- c(886, 891, 605, 868, 524, 972)

# Create a data frame with the data
city_freq <- data.frame(City = city_names, Frequency = frequencies)

# Calculate percentages for pie chart
city_freq$Percentage <- city_freq$Frequency / sum(city_freq$Frequency) * 100

# Display  pie chart for the percentage of each person
plot_ly(city_freq, labels = ~City, values = ~Frequency, type = 'pie', 
        hoverinfo = 'text', textinfo = 'label+percent', 
        text = ~paste(City, ":", Frequency, "(", Percentage, "%)"),
        insidetextfont = list(size = 12))
# Calculate total rent by city
total_rent_by_city <- houserent_data %>%
  group_by(City) %>%
  summarize(TotalRent = sum(Rent))

# Create the Bar Plot for Total House Rent Price by each city with total values on top
ggplot(total_rent_by_city, aes(x = City, y = TotalRent)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = TotalRent), vjust = -0.5, color = "black") +  # Add total value labels on top of bars
  labs(x = "City", y = "Total House Rent", title = "Total House Rent Price by each City") +
  theme_minimal()


# Calculate total Size by city
total_size_by_city <- houserent_data %>%
  group_by(City) %>%
  summarize(TotalSize = sum(Size))

# Create the Bar Plot for Total House Size by each city with total values on top
ggplot(total_size_by_city, aes(x = City, y = TotalSize)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = TotalSize), vjust = -0.5, color = "black") +  # Add total value labels on top of bars
  labs(x = "City", y = "Total House Size", title = "Total House Size by each City") +
  theme_minimal()




# Function to clean data for a city
clean_city_data <- function(city_data) {
  city_data$Rent <- Winsorize(city_data$Rent, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
  city_data$Size <- Winsorize(city_data$Size, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                              na.rm = FALSE, type = 7)
  return(city_data)
}


# Bangalore_data cleaning
Bangalore_data <- subset(houserent_data, City == "Bangalore")
Bangalore_data <- clean_city_data(Bangalore_data)
# Showing before and after from data cleaning
boxplot(Bangalore_data$Rent, main = "Bangalore Rent Price")
summary(Bangalore_data$Rent)
boxplot(Bangalore_data$Size, main = "Bangalore House Size")
summary(Bangalore_data$Size)


# Chennai_data cleaning
Chennai_data <- subset(houserent_data, City == "Chennai")
Chennai_data <- clean_city_data(Chennai_data)
# Showing before and after from data cleaning
boxplot(Chennai_data$Rent, main = "Chennai Rent Price")
summary(Chennai_data$Rent)
boxplot(Chennai_data$Size, main = "Chennai House Size")
summary(Chennai_data$Size)


# Delhi_data cleaning
Delhi_data <- subset(houserent_data, City == "Delhi")
Delhi_data <- clean_city_data(Delhi_data)
# Showing before and after from data cleaning
boxplot(Delhi_data$Rent, main = "Delhi Rent Price")
summary(Delhi_data$Rent)
boxplot(Delhi_data$Size, main = "Delhi House Size")
summary(Delhi_data$Size)


# Hyderabad_data cleaning
Hyderabad_data <- subset(houserent_data, City == "Hyderabad")
Hyderabad_data <- clean_city_data(Hyderabad_data)
# Showing before and after from data cleaning
boxplot(Hyderabad_data$Rent, main = "Hyderabad Rent Price")
summary(Hyderabad_data$Rent)
boxplot(Hyderabad_data$Size, main = "Hyderabad House Size")
summary(Hyderabad_data$Size)


# Kolkata_data cleaning
Kolkata_data <- subset(houserent_data, City == "Kolkata")
Kolkata_data <- clean_city_data(Kolkata_data)
# Showing before and after from data cleaning
boxplot(Kolkata_data$Rent, main = "Kolkata Rent Price")
summary(Kolkata_data$Rent)
boxplot(Kolkata_data$Size, main = "Kolkata House Size")
summary(Kolkata_data$Size)


# Mumbai_data
Mumbai_data <- subset(houserent_data, City == "Mumbai")
Mumbai_data <- clean_city_data(Mumbai_data)
# Showing before and after from data cleaning
boxplot(Mumbai_data$Rent, main = "Mumbai Rent Price")
summary(Mumbai_data$Rent)
boxplot(Mumbai_data$Size, main = "Mumbai House Size")
summary(Mumbai_data$Size)

# This is after winsorizing data cleaning data method
houserent_data_win <- bind_rows(Bangalore_data,Chennai_data,Delhi_data,Hyderabad_data,Kolkata_data,Mumbai_data)
View(houserent_data_win)





# Data Cleaning - manual row deletion 
# we will delete that row as its bathroom is too extreme,
# although the value is still consider meaningful data but it is too extreme and
# it will affect the further analysis

#outliner manual clear out only for Hyderabad city.
Hyderabad_data = Hyderabad_data[Hyderabad_data$Rent != 52650 & Hyderabad_data$Size != 190.7, ]

# All new data
houserent_data_win <- bind_rows(Bangalore_data,Chennai_data,Delhi_data,Hyderabad_data,Kolkata_data,Mumbai_data)
# Clear the room with 10 bathroom
houserent_data_win <- houserent_data_win[houserent_data_win$Bathroom != 10,]

numeric_columns = houserent_data_win[, c("BHK", "Rent", "Size", "Bathroom")]

#row reduction to "Area.Locality" and "Floor" columns using subset()
houserent_data_win <- subset(houserent_data_win, select = -c(Area.Locality, Floor))



# Firstly, we will look through the BHK(bedroom, hall, kitchen)
ggplot(houserent_data_win, aes(x = "", y = BHK)) +
  # Filled blue violin
  geom_violin(alpha = 0.5, fill = "blue", color="darkblue") + 
  # Pink boxplot
  geom_boxplot(width = 0.1, fill = "pink") +
  labs(title = "Box Plot and Violin Plot of BHK",
       x = "",
       y = "BHK")

hist(houserent_data_win$BHK, freq = FALSE, col = viridis(length(unique(houserent_data_win$BHK))),
     main = "Density Plot of BHK",
     xlab = "Number of BHK"
)
lines(density(houserent_data_win$BHK), col = "red",lwd = 2)
table(houserent_data_win$BHK)


# Then we will look through the Bathroom
ggplot(houserent_data_win, aes(x = "", y = Bathroom)) +
  # Filled blue violin
  geom_violin(alpha = 0.5, fill = "blue", color="darkblue") + 
  # Pink boxplot
  geom_boxplot(width = 0.1, fill = "pink") +
  labs(title = "Box Plot and Violin Plot of Bathroom",
       x = "",
       y = "Bathroom")

hist(houserent_data_win$Bathroom, freq = FALSE, col = viridis(length(unique(houserent_data_win$Bathroom))),
     main = "Density Plot of Bathroom",
     xlab = "Number of Bathroom"
)
lines(density(houserent_data$Bathroom), col = "red",lwd = 2)
table(houserent_data$Bathroom)


# Find out some extreme value only occur once and far from median
# House rent price
ggplot(houserent_data_win, aes(x = "", y = Rent)) +
  # Filled blue violin
  geom_violin(alpha = 0.5, fill = "blue", color="darkblue") + 
  # Pink boxplot
  geom_boxplot(width = 0.1, fill = "pink") +
  labs(title = "Box Plot and Violin Plot of Rent",
       x = "",
       y = "Rent")
hist(houserent_data_win$Rent, freq = FALSE,breaks = 20,
     col = viridis(20),
     main = "Density Plot of Rent",
     xlab = "Number of Rent",
)
lines(density(houserent_data_win$Rent), col = "red",lwd = 2)
summary(houserent_data_win$Rent)



# Find out some extreme value only occur once and far from median
# Color palette 
ggplot(houserent_data_win, aes(x = "", y = Size)) +
  # Filled blue violin
  geom_violin(alpha = 0.5, fill = "blue", color="darkblue") + 
  # Pink boxplot
  geom_boxplot(width = 0.1, fill = "pink") +
  labs(title = "Box Plot and Violin Plot of Size",
       x = "",
       y = "Size")
hist(houserent_data_win$Size, freq = FALSE,breaks = 20,col = viridis(24),
     main = "Density Plot of Size",
     xlab = "Number of Size"
)
lines(density(houserent_data_win$Size), col = "red",lwd = 2)
summary(houserent_data_win$Size)







# Correlation heatmap will be proceed after data cleaning the the explanation can be find in the passage
# Calculate the correlation matrix
correlation_matrix <- cor(numeric_columns)
melted_data <- melt(correlation_matrix)
ggplot(melted_data, aes(x = X1, y = X2, fill = value)) + # input the value and col name
  geom_tile() + # provide a space for the heatmap
  ggtitle("Correlation Heatmap") +
  geom_text(aes(label = round(value, 4)), size = 3) + #fill in the number of each box
  scale_fill_gradient2(low = "green", mid = "white", high = "blue", midpoint = 0) +
  # fill in color based on their number
  labs(x = "", y = "", fill = "Correlation") # cancel X, Y plot and label the fill

GGally::ggpairs(houserent_data_win, columns = c("BHK", "Size", "Rent", "Bathroom"),
                ggplot2::aes(colour=City))


# OBJECTIVE 1: To analyse the relationship between larger sized houses and rent and 
# how it affects various factors in the dataset 
# AIDAN CHANG WAI YUE (TP063411) CSDA

# filtering large houses (size > median)
larger_houses <- houserent_data_win %>% 
  filter(Size > quantile(Size, 0.5))

# Analysis 1.1: Relationship between rent and size for each city

# Create the hexbin plot with different Cities
ggplot(houserent_data_win, aes(x = Size, y = Rent)) +
  geom_hex() +
  facet_wrap(~ City) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  labs(x = "House Size", y = "Rent (in thousands)") +
  ggtitle("Hexbin Plot of House Size vs Rent Price")


# Analysis 1.2: Barplot of larger sized houses rent median and BHK

# Calculate median rent for each BHK group
median_rent <- larger_houses %>%
  group_by(BHK) %>%
  summarise(median_rent = median(Rent))

# Bar plot with numbers on bars
ggplot(larger_houses, aes(x = as.factor(BHK), y = Rent)) +
  geom_bar(stat = "summary", fun = "median", fill = "skyblue", color = "blue", alpha = 0.6) +
  geom_text(data = median_rent, aes(label = scales::comma(median_rent), y = median_rent + 5000),
            vjust = -0.5, size = 3, color = "black") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  labs(
    title = "Median Rent vs Number of Bedrooms (BHK) for Larger Houses",
    x = "Number of Bedrooms (BHK)",
    y = "Median Rent (in thousands)"
  )

# Analysis 1.3  Barplot of Median Rent of larger sized houses by Bathrooms

# Boxplot with customized y-axis range and labels
median_rent <- larger_houses %>%
  group_by(Bathroom) %>%
  summarise(median_rent = median(Rent))

# Bar plot with numbers on bars
ggplot(larger_houses, aes(x = as.factor(Bathroom), y = Rent)) +
  geom_bar(stat = "summary", fun = "median", fill = "lightgreen", color = "darkgreen", alpha = 0.6) +
  geom_text(data = median_rent, aes(label = scales::comma(median_rent), y = median_rent + 5000),
            vjust = -0.5, size = 3, color = "black") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  labs(
    title = "Median Rent vs Number of Bathrooms for Larger Houses",
    x = "Number of Bathrooms",
    y = "Median Rent (in thousands)"
  )

# Analysis 1.4: Relationship between larger sized home rent median with furnishing status
ggplot(larger_houses, aes(x = Furnishing.Status, y = Rent, fill = Furnishing.Status)) +
  geom_violin(trim = FALSE) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  labs(x = "Furnishing Status", y = "Rent (in thousands)") +
  ggtitle("Rent Median vs Furnishing Status for Larger Sized Homes") +
  theme_minimal() +
  theme(legend.position = "none")

# Analysis 1.5: Relationship between larger sized home rent in all cities

median_rent_city <- larger_houses %>%
  group_by(City) %>%summarize(Median_Rent = median(Rent))

ggplot(median_rent_city, aes(x = reorder(City, -Median_Rent), y = Median_Rent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = scales::comma(Median_Rent)), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0, 200000), labels = scales::comma_format(scale = 1e-3)) +
  labs(x = "City", y = "Median Rent (in thousands)", title = "Median Rent in Larger Sized Homes across Cities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analysis 1.6: Relationship between preferred tenant and larger sized houses

# Counting number of tenants
tenant_counts <- larger_houses %>%
  group_by(Tenant.Preferred) %>%summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Create the pie chart
pie_chart <- ggplot(tenant_counts, aes(x = "", y = percent, fill = Tenant.Preferred)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Distribution of Preferred Tenant Types in Larger Sized Houses",
    fill = "Tenant Preferred"
  ) +
  theme_void()

# Add labels with count and percentage
pie_chart_labels <- pie_chart +
  geom_text(aes(label = paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

# Print the pie chart
print(pie_chart_labels)

# Box plot with color-coded boxes and median labels
ggplot(larger_houses, aes(x = Tenant.Preferred, y = Rent, fill = Tenant.Preferred)) +
  geom_boxplot() +
  geom_text(
    data = larger_houses %>% group_by(Tenant.Preferred) %>% summarise(median_rent = median(Rent)),
    aes(label = scales::comma(median_rent), x = Tenant.Preferred, y = median_rent),
    vjust = -0.5, color = "white", size = 3
  ) +  # Add median labels
  labs(
    title = "Relationship between Larger Sized Houses Rent and Preferred Tenant",
    x = "Preferred Tenant",
    y = "Rent (in thousands)"
  ) +
  scale_fill_manual(values = c("bachelor" = "blue", "bachelor/family" = "green", "family" = "red")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3))
# Analysis 1.7: Scatterplot of how larger sized houses rent median affects area type

ggplot(larger_houses, aes(x = Area.Type, y = Rent, color = Area.Type)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  geom_text(
    data = larger_houses %>%
      group_by(Area.Type) %>%
      summarize(median_rent = median(Rent)),
    aes(x = Area.Type, y = 0, label = paste("Median: ", scales::comma(median_rent))),
    vjust = -0.5, color = "black", size = 3
  ) +  # Add median labels
  scale_y_continuous(limits = c(0, 350000), labels = scales::comma_format(scale = 1e-3)) +
  labs(x = "Area Type", y = "Rent (in thousands)", title = "Relationship between Larger Sized Houses Rent and Area Type") +
  theme_minimal() +
  theme(legend.position = "none")

# Analysis 1.8: Relationship between larger sized house rent and point of contact
ggplot(larger_houses, aes(x = `Point.of.Contact`, y = Rent, color = `Point.of.Contact`)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  geom_text(
    data = larger_houses %>%
      group_by(`Point.of.Contact`) %>%
      summarize(median_rent = median(Rent, na.rm = TRUE)),
    aes(x = `Point.of.Contact`, y = 0, label = paste("Median: ", scales::comma(median_rent))),
    vjust = 1, color = "black", size = 3
  ) +  # Add median labels at the bottom
  scale_y_continuous(limits = c(0, 350000), labels = scales::comma_format(scale = 1e-3)) +
  labs(x = "Point of Contact", y = "Rent (in thousands)", title = "Relationship between Larger Sized Houses Rent and Point of Contact") +
  theme_minimal() +
  theme(legend.position = "none")



#Objective 2: To analyze the impact of different types furnishings on the rental price of a unit in Hyderabad compared to other cities.(Lim Jing Yee TP068796)(ACS(DA))
#Analysis 2.1 Relation between the furnishing status and rental price
#To see the overall situation of impact on different furnishing status on rental price of each city
ggplot(houserent_data_win,aes(x=Furnishing.Status,y=Rent))+
  geom_jitter(width=0.4,height=0,aes(color=Furnishing.Status))+
  facet_wrap(~City)+
  labs(x="furnishing status",y="Rent (in thousand)")+
  ggtitle("Furnishing Status vs Rent Price in each city")+
  theme_minimal()+
  scale_y_continuous(labels=scales::comma_format(scale=1e-3))
#barplot that shows the relationship between the rental prices and city 
ggplot(houserent_data_win,aes(x=City,y=Rent,fill=Furnishing.Status))+
  geom_bar(stat="identity",width = 0.75)+
  labs(title="Total rent prices of each of the city",x="City",y="Rent (in thousand)",fill="City")+
  theme_minimal()+
  scale_y_continuous(labels=scales::comma_format(scale=1e-3))
#Analysis 2.2 Anova test for the impact of furnishing status on rental price
#To prove that different furnishing status has significant impact on rental price
#H0:Furnishing status do not has significant impact on rental price.
#H1:Furnishing status has significant impact on rental price.
anova_result<-aov(Rent~Furnishing.Status,data=houserent_data_win)
summary(anova_result)
#Analysis 2.3 Impact of different types of furnishings on the rental price of a unit in Hyderabad
Hyderabad_Furnishing_data<-select(Hyderabad_data_win,c("Furnishing.Status","Rent"))
Hyderabad_Furnishing_data
ggplot(Hyderabad_Furnishing_data,aes(x=Furnishing.Status,y=Rent))+
  geom_bar(stat="summary",fun="mean",fill="violet",color="purple")+
  labs(title="Average Rent Price by Furnishing Status in Hyderabad city",x="Furnishing Status",
       y="Average Rent Price")
#To find the mean of rent by furnishing status in Hyderabad
aggregate(Rent~Furnishing.Status,data=Hyderabad_Furnishing_data,FUN=mean)
#anova test on the impact of furnishing status on rental price in Hyderabad
anova_result<-aov(Rent~Furnishing.Status,data=Hyderabad_Furnishing_data)
summary(anova_result)
#Regression model to see the correlation
model<-lm(Rent~Furnishing.Status,data=Hyderabad_Furnishing_data)
summary(model)
#Analysis 2.4 Relation of rental price by furnishing status in all cities
#Compare the average rent price by furnishing status in all the cities.
ggplot(houserent_data_win,aes(x=Furnishing.Status,y=Rent,fill=City))+
  geom_bar(stat="summary",fun="mean",width = 0.75,position="dodge")+
  geom_text(data = mean_values, aes(x = Furnishing.Status, 
                                    y = Mean_Rent,
                                    label = round(Mean_Rent, 2)),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 2.5)+
  labs(title="Rent Prices by Furnishing Status in all cities",
       x="Furnishing Status",
       y="Rent Price",fill="City")+
  theme_minimal()
#imported data of other cities
Hyderabad_Furnishing_data<-select(Hyderabad_data,c("Furnishing.Status","Rent","City"))
Bangalore_Furnishing_data<-select(Bangalore_data,c("Furnishing.Status","Rent"))
Chennai_Furnishing_data<-select(Chennai_data,c("Furnishing.Status","Rent"))
Delhi_Furnishing_data<-select(Delhi_data,c("Furnishing.Status","Rent"))
Kolkata_Furnishing_data<-select(Kolkata_data,c("Furnishing.Status","Rent"))
Mumbai_Furnishing_data<-select(Mumbai_data,c("Furnishing.Status","Rent"))
#Analysis 2.5 The impact of furnishing status on rental price in Bangalore compared to Hyderabad
#Compare the average rent price by furnishing status between the Hyderabad and Bangalore.
filtered_data<-select(houserent_data_win,c("Furnishing.Status","Rent","City"))%>%
  group_by(City,Furnishing.Status)%>%filter(City%in%c("Hyderabad","Bangalore"))
ggplot(filtered_data,aes(x=Furnishing.Status,y=Rent,fill=City))+
  geom_bar(stat="summary",fun="mean",width = 0.5,position="dodge")+
  labs(title="Average Rent Prices by Furnishing Status in Bangalore and Hyderabad",
       x="Furnishing Status",
       y="Rent Price",fill="City")+
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)),
            position = position_dodge(width = 0.5), vjust = -0.5)+
  theme_minimal()+
  scale_fill_manual(values = c("Hyderabad" = "cyan3", "Bangalore" = "coral2"))
#Compute a t-test to see is there a difference of rental price between Hyderabad and Bangalore
#H0:There has no difference between mean of the rental price between Hyderabad and Bangalore
#H0:There has difference between mean of the rental price between Hyderabad and Bangalore
t.test_result<-t.test(Hyderabad_Furnishing_data$Rent,Bangalore_Furnishing_data$Rent)
print(t.test_result)
#Analysis 2.6 The impact of furnishing status on rental price in Chennai compared to Hyderabad
#Compare the average rent price by furnishing status between the Hyderabad and Chennai.
filtered_data<-select(houserent_data_win,c("Furnishing.Status","Rent","City"))%>%
  group_by(City,Furnishing.Status)%>%filter(City%in%c("Hyderabad","Chennai"))
ggplot(filtered_data,aes(x=Furnishing.Status,y=Rent,fill=City))+
  geom_bar(stat="summary",fun="mean",width = 0.5,position="dodge")+
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)),
            position = position_dodge(width = 0.5), vjust = -0.5)+
  labs(title="Average Rent Prices by Furnishing Status in Chennai and Hyderabad",
       x="Furnishing Status",
       y="Rent Price",fill="City")+
  theme_minimal()+
  scale_fill_manual(values = c("Hyderabad" = "cyan3", "Chennai" = "darkgoldenrod2"))
#Compute a t-test to see is there a difference of rental price between Hyderabad and Chennai
#H0:There has no difference between mean of the rental price between Hyderabad and Chennai
#H0:There has difference between mean of the rental price between Hyderabad and Chennai
t.test_result<-t.test(Hyderabad_Furnishing_data$Rent,Chennai_Furnishing_data$Rent)
print(t.test_result)
#Analysis 2.7 The impact of furnishing status on rental price in Delhi compared to Hyderabad
#Compare the average rent price by furnishing status between the Hyderabad and Delhi.
filtered_data<-select(houserent_data_win,c("Furnishing.Status","Rent","City"))%>%
  group_by(City,Furnishing.Status)%>%filter(City%in%c("Hyderabad","Delhi"))
ggplot(filtered_data,aes(x=Furnishing.Status,y=Rent,fill=City))+
  geom_bar(stat="summary",fun="mean",width = 0.5,position="dodge")+
  labs(title="Average Rent Prices by Furnishing Status in Delhi and Hyderabad",
       x="Furnishing Status",
       y="Rent Price",fill="City")+
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)),
            position = position_dodge(width = 0.5), vjust = -0.5)+
  theme_minimal()+
  scale_fill_manual(values = c("Hyderabad"="cyan3","Delhi" = "chartreuse3"))
#Compute a t-test to see is there a difference of rental price between Hyderabad and Delhi
#H0:There has no difference between mean of the rental price between Hyderabad and Delhi
#H0:There has difference between mean of the rental price between Hyderabad and Delhi
t.test_result<-t.test(Hyderabad_Furnishing_data$Rent,Delhi_Furnishing_data$Rent)
print(t.test_result)
#Analysis 2.8 The impact of furnishing status on rental price in Kolkata compared to Hyderabad
#Compare the average rent price by furnishing status between the Hyderabad and Kolkata.
filtered_data<-select(houserent_data_win,c("Furnishing.Status","Rent","City"))%>%
  group_by(City,Furnishing.Status)%>%filter(City%in%c("Hyderabad","Kolkata"))
ggplot(filtered_data,aes(x=Furnishing.Status,y=Rent,fill=City))+
  geom_bar(stat="summary",fun="mean",width = 0.5,position="dodge")+
  labs(title="Average Rent Prices by Furnishing Status in Kolkata and Hyderabad",
       x="Furnishing Status",
       y="Rent Price",fill="City")+
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)),
            position = position_dodge(width = 0.5), vjust = -0.5)+
  theme_minimal()+
  scale_fill_manual(values = c("Hyderabad"="cyan3","Kolkata" = "cornflowerblue"))
#Compute a t-test to see is there a difference of rental price between Hyderabad and Kolkata
#H0:There has no difference between mean of the rental price between Hyderabad and Kolkata
#H0:There has difference between mean of the rental price between Hyderabad and Kolkata
t.test_result<-t.test(Hyderabad_Furnishing_data$Rent,Kolkata_Furnishing_data$Rent)
print(t.test_result)
#Analysis 2.9 The impact of furnishing status on rental price in Mumbai compared to Hyderabad
#Compare the average rent price by furnishing status between the Hyderabad and Mumbai.
filtered_data<-select(houserent_data_win,c("Furnishing.Status","Rent","City"))%>%
  group_by(City,Furnishing.Status)%>%filter(City%in%c("Hyderabad","Mumbai"))
ggplot(filtered_data,aes(x=Furnishing.Status,y=Rent,fill=City))+
  geom_bar(stat="summary",fun="mean",width = 0.5,position="dodge")+
  labs(title="Average Rent Prices by Furnishing Status in Kolkata and Mumbai",
       x="Furnishing Status",
       y="Rent Price",fill="City")+
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 2)),
            position = position_dodge(width = 0.5), vjust = -0.5)+
  theme_minimal()+
  scale_fill_manual(values = c("Hyderabad"="cyan3","Mumbai" = "magenta1"))
#Compute a t-test to see is there a difference of rental price between Hyderabad and Mumbai
#H0:There has no difference between mean of the rental price between Hyderabad and Mumbai
#H0:There has difference between mean of the rental price between Hyderabad and Mumbai
t.test_result<-t.test(Hyderabad_Furnishing_data$Rent,Mumbai_Furnishing_data$Rent)
print(t.test_result)



#Objective 3: To determine the relationship between rent and all factors (Ten Xin Ru TP069426 APU2F2305ACS(DA))
#analysis3.1
#To change numerical data into factor
houserent_data_win <- houserent_data_win %>% mutate(BHK=as.factor(BHK))
#To check the levels for BHK
levels(houserent_data_win$BHK)
#Create a bar plot of total rent by BHK
ggplot(houserent_data_win,aes(x=Rent,y=BHK,fill=BHK))+geom_bar(stat="identity")+ggtitle("Rent by BHK")
#Create a bar plot of mean rent by BHK
ggplot(houserent_data_win,aes(x=Rent,y=BHK,fill=BHK))+geom_bar(stat="summary",fun="mean",position="dodge")+labs(x="Rent",y="BHK",title="Mean Rental by BHK")

#analysis3.2
#Create a gradient scatter plot of rent by house size
ggplot(data=houserent_data_win,aes(x=Rent,y=Size,color=Size))+geom_point(alpha=0.7,size=3)+scale_color_gradient(low="blue",high="red")+labs(title="Rent by Size")+theme_bw()

#analysis3.3
#To change the categorical data into factor
houserent_data_win <- houserent_data_win %>% mutate(Area.Type=as.factor(Area.Type))
#To check the levels for Area type to determine which graph to use
levels(houserent_data_win$Area.Type)
#Create a vertical bar plot of mean rental by area type
ggplot(houserent_data_win,aes(x=Area.Type,y=Rent))+geom_bar(stat="summary",fun="mean",fill="pink")+labs(x="Area Type",y="Mean Rental",title="Mean Rental by Area Type")
#Create a bar plot to see the analyze the rental for each area type
ggplot(houserent_data_win,aes(x=Area.Type,y=Rent,fill=Area.Type))+geom_boxplot()+labs(x="Area Type",y="Rental",title="Rental by Area Type")

#analysis3.4
#To change the categorical data into factor
houserent_data_win <- houserent_data_win %>% mutate(City=as.factor(City))
levels(houserent_data_win$City)
ggplot(houserent_data_win,aes(x=Rent,y=City,fill=City))+geom_bar(stat="identity")+ggtitle("Total Rent by City")
ggplot(houserent_data_win,aes(x=Rent,y=City))+geom_bar(stat="summary",fun="mean",fill="light blue")+labs(x="Mean Rental",y="City",title="Mean Rental by City")


#analysis3.5
#To change the categorical data into factor
houserent_data_win <- houserent_data_win %>% mutate(Furnishing.Status=as.factor(Furnishing.Status))
levels(houserent_data_win$Furnishing.Status)
#Create a jitter plot to see the distribution of rental by furnishing status
ggplot(houserent_data_win,aes(x=Furnishing.Status,y=Rent))+geom_jitter(width=0.3,height=0,aes(color=Furnishing.Status))+labs(title="Distribution of Rental by Furnishing Status")
#Create a box plot by adding mean rental into each furnishing status
ggplot(houserent_data_win,aes(x=Furnishing.Status,y=Rent,fill+Furnishing.STatus))+geom_boxplot(fill="light green")+stat_summary(fun="mean",geom="point",shape=18,size=3,color="blue")+theme_minimal()+labs(title="Boxplot of Rental by Furnishing Status")

#analysis3.6
#Change the categorical data into factor
houserent_data_win <- houserent_data_win %>% mutate(Tenant.Preferred=as.factor(Tenant.Preferred))
levels(houserent_data_win$Tenant.Preferred)
ggplot(houserent_data_win,aes(x=Tenant.Preferred,y=Rent,fill=Tenant.Preferred))+geom_bar(stat="identity")+ggtitle("Total Rent by Tenant Preferred")
ggplot(houserent_data_win,aes(x=Tenant.Preferred,y=Rent,fill=Tenant.Preferred))+geom_boxplot()+labs(x="Tenant Preferred",y="Rental",title="Boxplot of Rental by Tenant Preferred")


#analysis3.7
houserent_data_win <- houserent_data_win %>% mutate(Bathroom=as.factor(Bathroom))
levels(houserent_data_win$Bathroom)
#Create a dot plot with total rent by number of bathrooms
sum_bathroom <- houserent_data_win %>% group_by(Bathroom) %>% summarize(TotalSum=sum(Rent))
ggplot(sum_bathroom,aes(x=Bathroom,y=reorder(Bathroom,TotalSum)))+geom_point(size=5,color="violet")+labs(title="Sum Rental by Number of Bathrooms",x="Bathroom",y="Total Rental")+theme_minimal()
#Create a boxplot with mean of rental by number of bathrooms
ggplot(houserent_data_win,aes(x=Bathroom,y=Rent,fill=Bathroom))+geom_boxplot()+stat_summary(fun="mean",geom="point",shape=19,size=2,color="black")+labs(x="Bathroom",y="Rental",title="Boxplot of Rental by Bathroom")


#analysis3.8
levels(houserent_data_win$Point.of.Contact)
houserent_data_win <- houserent_data_win %>% mutate(Point.of.Contact=as.factor(Point.of.Contact))
ggplot(houserent_data_win,aes(x=Point.of.Contact,y=Rent))+geom_jitter(width=0.3,height=0,aes(color=Point.of.Contact))+labs(x="Point of Contact",y="Rent",title="Distribution of Rental by Point of Contact")




# Objective 4: To determine if a 20% increase in the size of a furnished rental house 
# in Hyderabad leads to only a 10% increase in the rent price. Teh Chen Ming (TP068804)(CSDA)
# ================================================================================
# Analysis 4.1: Correlation, density plot and scatter plot on Hyderabad with different furnished status.
GGally::ggpairs(Hyderabad_data, columns = c("BHK", "Size", "Rent", "Bathroom"),
                ggplot2::aes(colour=Furnishing.Status))





# Analysis 4.2: Relationship between house size and rent price in Hyderabad
ggplot(Hyderabad_data, aes(x = Size, y = Rent, color = Rent)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add linear regression line
  labs(x = "House Size", y = "Rent Price", title = "House Size vs. Rent Price in Hyderabad") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()
# furnished status with different size.
# Relation of size of house, rent of House with Furnishing condition in Hyderabad
fig <- plot_ly(Hyderabad_data, x = ~Size, y = ~Rent, z = ~Furnishing.Status,
               color = ~Furnishing.Status, colors = c("#FF5733", "#33FF57", "#3357FF"),
               type = "scatter3d", mode = "markers",
               marker = list(size = 5, opacity = 0.7))

# Add axis labels and plot title
fig <- fig %>% layout(scene = list(xaxis = list(title = "House Size"),
                                   yaxis = list(title = "Rent Price"),
                                   zaxis = list(title = "Furnished Condition")),
                      title = "House Size vs. Rent Price by 
                      Furnished Condition in Hyderabad")
fig


# Analysis 4.3: Relationship between house, rent size and furnishing status
ggplot(Hyderabad_data, aes(x = Size, y = Rent, size = Rent, color = Furnishing.Status)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(x = "House Size", y = "Rent Price", title = "House Size vs Rent Price in Hyderabad
       with different furnishing status") +
  theme_minimal() +
  theme(legend.position = "right")





# Analysis 4.4: Relationship between house rent and furnishing status
ggplot(Hyderabad_data, aes(x = Furnishing.Status, y = Rent)) +
  geom_boxplot(aes(fill = Furnishing.Status), width = 0.2) +
  geom_jitter(width = 0.2, height = 0, aes(color = Furnishing.Status)) +
  labs(y = "Rent Price", title = paste("Rent Price of Houses by Furnishing Condition in Hyderabad")) +
  scale_fill_manual(values = c("red", "green","blue")) +  # Set box plot fill colors
  scale_color_manual(values = c("lightgreen", "yellow", "lightpink")) +  # Set dot colors
  theme_minimal() +
  theme(legend.position = "none")

# Analysis 4.5: Relationship between house size and furnishing status
ggplot(Hyderabad_data, aes(x = Furnishing.Status, y = Size)) +
  geom_boxplot(aes(fill = Furnishing.Status), width = 0.2) +
  geom_jitter(width = 0.2, height = 0, aes(color = Furnishing.Status)) +
  labs(y = "House Size", title = paste("Size of House by Furnishing Condition in Hyderabad")) +
  scale_fill_manual(values = c("red", "green","blue")) +  # Set box plot fill colors
  scale_color_manual(values = c("blue", "red", "orange")) +  # Set dot colors
  theme_minimal() +
  theme(legend.position = "none")




# To find the most number on there
furnished_data <- Hyderabad_data[Hyderabad_data$Furnishing.Status == "Furnished", ]
semi_furnished_data <- Hyderabad_data[Hyderabad_data$Furnishing.Status == "Semi-Furnished", ]
unfurnished_data <- Hyderabad_data[Hyderabad_data$Furnishing.Status == "Unfurnished", ]
summary(furnished_data$Rent)
summary(semi_furnished_data$Rent)
summary(unfurnished_data$Rent)
summary(furnished_data$Size)
summary(semi_furnished_data$Size)
summary(unfurnished_data$Size)






# Create a data frame from the table
table_furnishing <- as.data.frame(table(Hyderabad_data$Furnishing.Status))

# Get data
values <- table_furnishing$Freq
labels <- table_furnishing$Var1

# Calculate percentages
totals <- sum(values)
percent <- round(values/totals * 100, 1) 

# Create labels
labels <- paste0(labels, "\n", percent, "%\n(n=", values, ")")

# Create the polar bar chart
ggplot(table_furnishing, aes(x="", y=Freq, fill=labels)) +
  geom_col() +
  coord_polar(theta = "y") +
  
  # Legend with new labels
  theme(legend.position="bottom") + 
  scale_fill_discrete(labels = labels) +
  labs(title="Furnishing Status",
       fill = "Category")


# 3D version
# 3D Pie Chart
library(plotrix)  
pie3D(
  values,
  labels = labels, 
  explode = 0.1,
  theta = 0.6,
  shade = 0.6,
  col = rainbow(length(values))
)

# Add title
title("Furnishing Status (3D)")








# more practical test
# This is the data with Hyderabad and furnished only
furnished_hyderabad_data <- Hyderabad_data %>%
  filter(Furnishing.Status == "Furnished")

# Here start to find the correlation of the Size and Rent in fully furnished condition
correlation <- cor(furnished_hyderabad_data$Size, furnished_hyderabad_data$Rent)
correlation
# Convert correlation to percentage using absolute value (to show magnitude)
percentage_correlation <- abs(correlation) * 100
percentage_correlation
# This will check the the rent and price
t.test(furnished_hyderabad_data$Size, furnished_hyderabad_data$Rent, paired = FALSE)
model <- lm(Rent ~ Size, data = furnished_hyderabad_data)
model


# This will find does the 20 percent of size does increase 10% of price
rent_10 = furnished_hyderabad_data$Rent * 1.1
t.test(furnished_hyderabad_data$Rent, rent_10)

nrow(furnished_hyderabad_data)

# This will show the linear regression of the plot
ggplot(data = furnished_hyderabad_data, aes(x = Size, y = Rent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se = FALSE, color="red", size=1.5) +
  labs(title="Hyderabad Rent vs Size",
       x="Size",
       y="Rent")

nrow(Hyderabad_data)
# This will show the non-linear regression of the plot
ggplot(furnished_hyderabad_data, aes(x = Size, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), se = FALSE, color = "red", size = 1.5) +
  xlab("House Size") +
  ylab("Rent") +
  ggtitle("Non-linear Regression Line Hyderabad Rent Size")



# Use linear model to predict the data
# Load data 
predict_column = furnished_hyderabad_data[, c("Rent", "Size")]
# Train linear model
model <- lm(Rent ~ Size, data = predict_column)
summary(model)
# Calculate original average rent
orig_avg_rent <- mean(predict_column$Rent)

# Increase size by 20%
predict_column$Size <- predict_column$Size * 1.2

# Make rent predictions on new size
predict_column$predicted_rent <- predict(model, newdata = predict_column) 

# Calculate new average predicted rent
new_avg_rent <- mean(predict_column$predicted_rent)

# Calculate percentage increase
pct_increase <- (new_avg_rent - orig_avg_rent) / orig_avg_rent * 100

print(paste("Original Average Rent:", orig_avg_rent)) 
print(paste("New Average Predicted Rent:", new_avg_rent))
print(paste("Percentage Rent Increase:", pct_increase))