library(tidyverse)



#---------------HURRICANE DATA---------------------------------------
#The data encompasses the years 1851 - 2023
#reading in hurricane data
url1 <-"https://raw.githubusercontent.com/WendyR20/DATA-608-Story-5/refs/heads/main/hurricane_data_1851-2023_noaa.csv"
hdata1 <- read.csv(url1)


head(hdata1)
tail(hdata1)

#what are our column names 

colnames(hdata1)

#renaming columns for neatness and readability
hdata_clean <- hdata1 %>%
  rename( Max_Winds_knots = Max.Winds..kt.,
          SS_HWS = SS..HWS,
          RMW_nm = RMW.nm,
          Central_Pressure_mb = Central.Pressure..mb. ,
          States_Affected = States.Affected,
          Storm_Names = Storm.Names,
          States_Affected_Names = States.Affected.Names,
          SS_HWS_max = SS.HWS.max,
          Central_Pressure_mb_min = Central.Pressure..mb..min,
          All_States_Affected_Names = States.Affected.Names.All
  )

colnames(hdata_clean)

#checking type of each column
sapply(hdata_clean,class)

anyNA(hdata_clean)

#converting Date column to type date
hdata_clean$Date <-  ymd_hms(hdata_clean$Date) 

#extracting years as it's own column
hdata2 <- hdata_clean %>%
  mutate(Year = year(hdata_clean$Date))


tail(hdata2)

#Since we only want to look at 25 Years
#Let's focus on 1995-2020

#subseting our data to include only the years 1995-2020

hdata_sub <- hdata2 %>%
  filter(Year >= 1995 & Year <= 2020)

yearly_h <- hdata_sub %>%   
  group_by(Year) %>%
  summarise(Num_Hurricanes = n(),
            .groups = "drop") 




ggplot(yearly_h, aes(x = Year, y = Num_Hurricanes)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point() +
  labs(title = "Number of Hurricanes per Year",
       x = "Year",
       y = "Count") +
  theme_minimal()



#exploring by category
#SSHWS stands for the Saffir-Simpson Hurricane Wind Scale  
#comparing number of hurricanes by intensity

category_year <- hdata_sub %>%   
  group_by(Year, SS_HWS) %>%
  summarise(Num_Hurricanes = n(),
            .groups = "drop") %>%
  rename(Category = SS_HWS)




#Grouping categories and calculate totals
cat_data <- category_year %>%
  mutate(Intensity = ifelse(Category >= 3, "Category 3-5", "Category 1-2")) %>%
  group_by(Year, Intensity) %>%
  summarise(Count = sum(Num_Hurricanes), .groups = "drop")

total_counts <- cat_data %>%
  group_by(Year) %>%
  summarise(Total = sum(Count))


ggplot() +
  #stacked bars for the instensity counts
  geom_bar(data = cat_data, 
           aes(x = Year, y = Count, fill = Intensity), 
           stat = "identity", alpha = 0.8) +
  
  scale_fill_manual(values = c(
    "Category 1-2" = "darkgray",  
    "Category 3-5" = "red"
  )) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 2)) +
  theme_minimal() +
  theme(
    #removing grid lines
    panel.grid.major = element_blank(), 
    #panel.grid.minor = element_blank(),
    #adding an axis line
    axis.line = element_line(size = 0.5, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 15, face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(t = 15))
  )+
  labs(
    title = "Periodic Surges in Frequency and Intensity of Hurricanes Making Landfall in the U.S.",
    subtitle = "Hurricane Activity Trends (1995–2020)",
    caption = "*Most hurricanes do not make landfall in the United States, affecting other nations or remaining in the ocean.",
    x = "Year",
    y = "Number of Hurricanes",
    fill = "Hurricane Intensity"
  ) 



#------------------------------WEATHER ANOMALY DATA----------------------------
#reading in weather anomaly data
url2 <- "https://raw.githubusercontent.com/WendyR20/DATA-608-Story-5/refs/heads/main/temp_anomaly_data.csv"
wa_data <- read_csv(url2, skip = 3)
head(wa_data)

#converting Date column to date type and creating Year column
wa_data2 <- wa_data %>%
  mutate(
    Date_ch = as.character(Date),
    Year = as.numeric(substr(Date_ch, 1, 4)),
    Month = as.numeric(substr(Date_ch, 5, 6)),
    Date = make_date(Year, Month, 1)
  )

#removing original date column column

wa_data2 <- subset(wa_data2, select = -Date_ch)
#finding year average anomaly and max yearly anomaly

wdata_year <- wa_data2 %>%
  group_by(Year) %>%
  summarise(
    Avg_Anomaly = round(mean(Anomaly, na.rm = TRUE),2),
    Max_Anomaly = max(Anomaly, na.rm = TRUE)
  )


ggplot(wdata_year, aes(x = Year, y = Avg_Anomaly)) +
  #highlighting surges
  
  annotate("rect", xmin = 1997, xmax = 1999, ymin = -Inf, ymax = Inf, 
           fill = "gray90", alpha = 0.5) +
  annotate("rect", xmin = 2005, xmax = 2007, ymin = -Inf, ymax = Inf, 
           fill = "gray90", alpha = 0.5) +
  
  annotate("rect", xmin = 2009, xmax = 2011, ymin = -Inf, ymax = Inf, 
           fill = "gray90", alpha = 0.5) +
  annotate("rect", xmin = 2011, xmax = 2013, ymin = -Inf, ymax = Inf, 
           fill = "gray90", alpha = 0.5) +
  annotate("rect", xmin = 2015, xmax = 2017, ymin = -Inf, ymax = Inf, 
           fill = "gray70", alpha = 0.5) +
  
  
  geom_line(color = "#d73027", size = 1.2) +
  geom_point(aes(color = Avg_Anomaly), size = 3) +
  
  #creating a color scale based on the anomaly value
  scale_color_gradient2(low = "#4575b4", mid = "#ffffbf", high = "#d73027", 
                        midpoint = 0) +
  
  scale_x_continuous(breaks = seq(1995, 2020, by = 2))+
  
  theme_minimal()+
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  annotate("text", x = 1995, y = 0.1, label = "20th Century Average Temp", 
           hjust = 0, color = "gray50", size = 3)+
  
  labs(title = "Accelerating Rise of U.S. Temperatures From 1995-2020",
       subtitle = "Average Annual temperature Anomalies Relative to the 1910–2000 Averages",
       caption = "Temperature anomalies are the difference between observed temperature and a long-term average, here the long term average comes from 1910-2000 temperature data",
       x = "Year",
       y = "Average Temperature Change") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 15)),
    panel.grid.major = element_blank(), 
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
    legend.position = "none"
  )



#------------------------------TORNADO DATA----------------------------

#reading in tornado data

url3 <-"https://raw.githubusercontent.com/WendyR20/DATA-608-Story-5/refs/heads/main/Yearly_Tornado_data.csv"
tdata <- read_csv(url3, skip=3)

head(tdata)
tail(tdata)

#converting date column to date type and creating year column
tdata2 <- tdata %>%
  mutate(
    Tornado_Date_ch = as.character(Date),
    Year = as.numeric(substr(Tornado_Date_ch, 1, 4)),
    Tornado_Month = as.numeric(substr(Tornado_Date_ch, 5, 6)),
    Tornado_Date = make_date(Year, Tornado_Month, 1)
  )


#removing original numerical date column and character date column
tdata2 <- subset(tdata2, select = -Tornado_Date_ch)
tdata2 <- subset(tdata2, select = -Date)

#subsetting to only include the years 1995-2020

tdata_sub <- tdata2 %>%
  filter(Year >=1995 & Year <=2020)

#grouping only the yearly tornados

sapply(tdata_sub,class)

tdata_sub$Tornadoes <-as.numeric(tdata_sub$Tornadoes)
tdata_yearly <- tdata_sub %>%
  group_by(Year) %>%
  summarise(Tornado_Yearly_Count = sum(Tornadoes,na.rm=TRUE),
            TFatalities_Yearly_Count = sum(Fatalities,na.rm=TRUE))

#------------------------JOINING ALL THREE DATAFRAMES--------------------
final_df <- yearly_h %>%
  inner_join(wdata_year, by = "Year") %>%
  inner_join(tdata_yearly, by = "Year")

#----------COMPARING MAX ANNUAL ANNOMALIES TO HURRICANE FREQUENCY-------------

ggplot(final_df, aes(x = Year)) +
  geom_col(aes(y = Num_Hurricanes), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = Avg_Anomaly * 10), color = "red", size = 1.2) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 2))+
  scale_y_continuous(
    name = "Number of Hurricanes",
    sec.axis = sec_axis(~./10, name = "Average Temperature Change")
  ) +
  labs(
    title = "Number of Hurricanes Fluctuates As Temperatures Continue to Rise in The U.S. ",
    subtitle = "Hurricane and Temperature Anomaly Data From 1995 to 2020",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 15)),
    panel.grid.major = element_blank(), 
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
    legend.position = "bottom"
  )

#----------COMPARING AVG ANNUAL ANNOMALIES TO TORNADO FREQUENCY-------------



#finding the average number of tornadoes from 1950-1990 
#finding anomaly data by comparing our 25 year stretch the one before


tdata3 <-tdata2
tdata3$Tornadoes <-as.numeric(tdata2$Tornadoes)


baseline_value <- tdata3 %>%
  filter(Year >= 1969 & Year < 1995) %>%
  group_by(Year) %>%
  summarise(Yearly_Total = sum(Tornadoes, na.rm = TRUE)) %>%
  summarise(Baseline = round(mean(Yearly_Total))) %>%
  pull(Baseline)

# 2. Prepare your Recent Data (1995-2020)
# Sum the tornadoes for these years and subtract the baseline
tdata_anomaly <- final_df %>%
  group_by(Year) %>%
  mutate(
    Tornado_Anomaly = Tornado_Yearly_Count - baseline_value,
    Direction = ifelse(Tornado_Anomaly > 0, "Above", "Below")
  )




ggplot(tdata_anomaly, aes(x = Year, y = Tornado_Anomaly)) +
  geom_col(aes(fill = Direction), alpha = 0.8, width = 0.7) +
  
  #line to represent the temperature anomalies
  geom_line(aes(y = Avg_Anomaly * 200), color = "black", size = 1) +
  
  #1969-1994 Baseline
  geom_hline(yintercept = 0, color = "gray20", size = 0.5) +
  
  scale_fill_manual(values = c("Above" = "red", "Below" = "darkblue")) +
  
  scale_y_continuous(
    name = "Tornado Count Shift (vs. 1969-1994)",
    sec.axis = sec_axis(~./200, name = "Temp Shift (°F)")
  ) +
  
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  
  labs(
    title = "Thousands More Tornadoes Each Year (1995-2020) vs Previous 25 Years As Average Temp Increases in the U.S.",
    subtitle = "Bars show shift from 1969–1994 tornado average and the line shows average annual temperature shift from previous century.",
    caption = "Baseline (0) represents the mean annual tornado count from 1950–1990.",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # The colors (Red/Blue) are intuitive
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14)
  )






