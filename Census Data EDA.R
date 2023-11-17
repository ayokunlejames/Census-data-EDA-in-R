library(tidyverse)
library(ggthemes)

#import dataset from local file directory
dataset <- read_csv("/Users/ayokunlejames/Downloads/MSc Data Science/Data Science files/Assessment/Census raw dataset.csv")

#view dataset
dataset |> View()

#Rename Columns
dataset2 <- dataset |> 
  rename(
    Income = INC,
    Gender = Female,
    Ethnicity = Eth, 
    Highest_Education = "Highest Ed",
    Marital_Status = Mar_Stat,
    Unit_Type = H8
  )

#transform Gender values to "Female" (0) & "Male"(1), Unit type and create Age_groups
dataset2 <- dataset2 |> 
  mutate (
    Gender = (if_else(Gender == 0, "Female", "Male")),
    Unit_Type = (if_else(Unit_Type == 0, "Self-Contained", "Non-Self-Contained")),
    Age_Group = cut(Age, 
                    breaks = c(0, 18, 39, 59, 79, Inf), 
                    labels = c("0-18", "19-39", "40-59", "60-79", "80+"),  
                    include.lowest = TRUE),
    .after = Age,
    Income_bracket = case_when(
      Income <=12570 ~ "<12570",
      Income > 12570 & Income < 50270 ~ "12570 - 50270",
      Income >= 50270 & Income < 150000 ~ "50270 - 150k",
      Income >= 150000 ~ "150k+")
  )

dataset2|>View()

#visualization of age distribution by ethnicity
dataset2 |>
  # filter(Age > 18) |> 
  ggplot( aes(x = Age)) + 
  geom_density(aes(color = Ethnicity), linewidth = 0.75)+
  scale_x_continuous(breaks = seq(0, 100, by = 10))


##SOCIO_ECONOMIC CONDITIONS OF ETHNIC GROUPS IN ENGLAND

#highest level of Education of the majority by ethnicity
dataset2 |>
  filter(Age >=18) |>
  group_by(Ethnicity) |>
  summarize(Education = names(which.max(table(Highest_Education))))

#highest level of Education of majority by ethnicity and gender
dataset2 |>
  filter(Age >=18) |>
  group_by(Ethnicity, Gender) |>
  summarize(Education = names(which.max(table(Highest_Education))))

#Average income by Ethnicity and Gender
eth_gen_income <- dataset2 |> 
  filter(Age >=18) |>
  group_by(Ethnicity, Gender) |>
  summarize(Average_Income = mean(Income, na.rm = TRUE)) |>
  arrange(desc(Average_Income)) |> drop_na() 

#check
eth_gen_income

#column chart to visualize
eth_gen_income |> 
  mutate(demographic = paste(Ethnicity, Gender, sep = " ")) |>
  ggplot(aes(y = Average_Income, x = reorder(demographic, -Average_Income), fill = Ethnicity)) +
  geom_col() +
  geom_text(aes(label = paste(round(Average_Income/1000, 1), 'k', sep = "")), vjust = 1.5, size = 3) +
  labs(
    title = "Average Income by Ethnicity and Gender",
    y = "Average Income in GBP",
    x = "Demographic"
    ) +
  scale_color_colorblind() +
  scale_y_continuous(breaks = seq(10000, 70000, by = 10000)) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#Income brackets by ethnicity - 1 (for intra-ethnic analysis)
 dataset2 |>
   filter(Age >= 18) |>
   count(Income_bracket, .by = Ethnicity) |> 
   rename(Ethnicity = .by, Count = n) |>
   ggplot(aes(x = Ethnicity, y = Count, fill = Income_bracket)) +
   geom_bar(position = "dodge", stat = "identity") +
   scale_y_continuous(breaks = seq(0, 8000, by = 500)) +
   labs(title = "Income Brackets of Ethnicities",
        subtitle = "Bar Chart showing the Income Distribution of Ethnicities in England",
        y = "Count",
        x = "Ethnicity",
        fill = "Income Brackets") +
   scale_color_colorblind()
 
 #boxplot visualization of income vs ethnicity (inter-ethnic analysis)
 ggplot(dataset2, aes(x = Ethnicity, y = Income)) +
   geom_boxplot() +
   scale_y_continuous(breaks = seq(0, 750000, by = 50000)) +
   labs(title = "Income distribution of Ethnicities",
        subtitle = "Boxplot showing the Income Distribution of Ethnicities in England")
        
 
##DIVORCE/SEPARATION RATES
#calculating marital success by ethnicity
mar_succ_eth <- dataset2 |>
  group_by(Ethnicity,Marital_Status) |>
  count() |>
  mutate(
    marital_success = if_else(Marital_Status %in% c("married", "Widowed"), 1, 0)
  )
#divorce/separation rate of marriages by ethnicity
div_eth <- mar_succ_eth |>
  filter(Marital_Status != "Never married" & !is.na(Marital_Status)) |>
  group_by(Ethnicity) |>
  summarize(
    successful_marriage = sum(case_when(
      marital_success == 1 ~ n,
      marital_success == 0 ~ n*0)
    ),
    unsuccessful_marriage = sum(case_when(
      marital_success == 0 ~ n,
      marital_success == 1 ~ n*0)
    ),
    success_rate = (
      (successful_marriage*100)/(unsuccessful_marriage + successful_marriage)
    ),
    divorce_rate = 100 - success_rate
  ) |>
  arrange(desc(divorce_rate)) 

#col chart of divorce rate 3
div_eth |>
  ggplot(aes(x = reorder(Ethnicity, -divorce_rate), y = divorce_rate)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  geom_text(aes(label = paste(round(divorce_rate, 1), '%', sep = "")), vjust = 1.5, size = 3) +  
  labs(title = "Divorce Rates by Ethnicity",
       subtitle = "Column Chart showing divorce rates of Ethnicities in England",
       y = "Divorce Rate",
       x = "Ethnicity")


#calculating marital success by ethnicity and gender
succ_eth_gen <- dataset2 |>
  group_by(Ethnicity,Gender,Marital_Status) |>
  count() |>
  mutate(
    marital_success = if_else(Marital_Status %in% c("married", "Widowed"), 1, 0)
  )

#divorce/separation rate of marriages by ethnicity and gender
div_eth_gen <- succ_eth_gen |>
  filter(Marital_Status != "Never married" & !is.na(Marital_Status)) |>
  group_by(Ethnicity, Gender) |>
  summarize(
    successful_marriage = sum(case_when(
      marital_success == 1 ~ n,
      marital_success == 0 ~ n*0)
    ),
    unsuccessful_marriage = sum(case_when(
      marital_success == 0 ~ n,
      marital_success == 1 ~ n*0)
    ),
    success_rate = (
      (successful_marriage*100)/(unsuccessful_marriage + successful_marriage)
    ),
    divorce_rate = 100 - success_rate
  ) |>
  arrange(desc(divorce_rate)) 

#visualize divorce rate by ethnicity and gender
div_eth_gen |>
  mutate(demographic = paste(Ethnicity, Gender, sep = " ")) |>
  ggplot(aes(x = reorder(demographic, -divorce_rate), y = divorce_rate, fill = Ethnicity)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_text(aes(label = paste(round(divorce_rate, 1), '%', sep = "")), vjust = 1.5, size = 3) +  
  labs(title = "Divorce/Separation rate by ethnicity and gender",
       subtitle = "Bar Chart showing the divorce/separation rate of marriages in England by Ethnicity and Gender",
       y = "Divorce Rate",
       x = "Demographic",
       fill = "Ethnicity") +
  scale_color_colorblind()


#HOUSING CONDITION
Household_dist <- dataset2 |> 
  group_by(ID) |> 
  summarize(
    Occupants = n_distinct(Person_ID), #no of occupants
    Total_Income = sum(Income, na.rm = TRUE), #total household income
    Ethnicity = names(which.max(table(Ethnicity))), #highest-occurring ethnicity
    no_of_children = sum(if_else(Age < 18,1,0)), #number of children
    Gender = names(which.max(table(Gender))), #higher-occurring gender
    Unit = names(which.max(table(Unit_Type)))
  ) 

#average no of occupants by ethnicity rounded to the nearest whole number
Household_dist |>
  group_by(Ethnicity) |>
  summarize(average_no_of_occupants = round(mean(Occupants))) |> 
  arrange(average_no_of_occupants)

#most popular unit type by ethnicity
dataset2 |>
  group_by(Ethnicity) |>
  summarize(Unit= names(which.max(table(Unit_Type)))) 

#Count of Observations by Ethnicity and Unit Type
dataset2 |>
  group_by(Ethnicity, Unit_Type) |>
  count() |>
  ggplot(aes(x = Ethnicity, y = Unit_Type, size = n)) +
  geom_count() +
  scale_size_continuous(range = c(3, 18)) +  
  labs(title = "Count of Observations by Ethnicity and Unit Type", x = "Ethnicity", y = "Unit Type", size = "Count")

#inspect data set to confirm visualization
dataset2 |>
  select(Ethnicity, Unit_Type) |>
  distinct()

#average number of children in a household by ethnicity 
Household_dist |> 
  group_by(Ethnicity) |>
  summarize(average_no_of_children = mean(no_of_children)) |>
  arrange(desc(average_no_of_children))


#most possible gender of kids by ethnicity
dataset2 |>
  filter(Age <18) |>
  group_by(Ethnicity) |>
  summarize(Gender = names(which.max(table(Gender))))

