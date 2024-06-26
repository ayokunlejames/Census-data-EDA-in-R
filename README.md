# Exploratory Analysis of Socio-economic factors of Demographics in England (2021)

## Introduction

This is a report of my investigation of a modified snapshot of data collected during a household census conducted in England in 2021. I performed exploratory data analysis of the dataset to derive insights about socio-economic conditions of the data subjects based on gender, income, level of education, ethnicity, marital status, and housing conditions.

### The Data

The data had 9 columns (`ID`, `Person_ID`, `Age`, `Mar_Stat`, `INC`, `Female`, `H8`, `Eth`, `Highest Ed`) and 27,410 entries with each entry representing an individual. Thanks to @tidyverse and @ggthemes, I was able to perform analysis of this dataset in RStudio using packages in the R tidyverse and ggthemes libraries.

```{r}
#| label: load-packages-import-data
#| include: false
library(tidyverse)
library(ggthemes)
dataset <- read_csv("/Users/ayokunlejames/Downloads/MSc Data Science/Data Science files/Assessment/data-1 (2).csv")
```

### Data Transformation

I performed some data transformations on the dataset including renaming columns, transforming existing columns and creating new columns because the dataset did not follow the three interrelated rules that make a dataset tidy. @wickham2014

```{r}
#| label: rename and transform columns
dataset <- dataset |> 
  rename(Income = INC,Gender = Female,Ethnicity = Eth, Highest_Education = "Highest Ed",Marital_Status = Mar_Stat,Unit_Type = H8)

dataset2 <- dataset |> 
  mutate (
    Gender = (if_else(Gender == 0, "Female", "Male")),
    Unit_Type = (if_else(Unit_Type == 0, "Self-Contained", "Non-Self-Contained")),
    Age_Group = cut(Age, 
                    breaks = c(0, 18, 39, 59, 79, Inf), 
                    labels = c("0-18", "19-39", "40-59", "60-79", "80+"),  
                    include.lowest = TRUE),
    .after = Age
  )
```

## Analysis and Insights

I performed analysis of the dataset majorly by Ethnicity and Gender demographics, on 4 metrics: Income distribution, Level of Education, Marital Status, and Housing conditions

### Income Distribution

I am interested in the distribution of income among the working class (above 18 years).I am particularly interested in the `Income` distribution by `Ethnicity` and `Gender`.

```{r, message=FALSE}
#| label: fig-eth-gen-inc
#| fig-cap: "Average Income by Ethnicity & Gender"
#| #| fig-alt: "A column Chart showing Average Income by Ethnicity & Gender "
dataset2 |> 
  filter(Age >=18) |>
  group_by(Ethnicity, Gender) |>
  summarize(Average_Income = mean(Income, na.rm = TRUE)) |>
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
```

@fig-eth-gen-inc shows the average income of demographies in England by their `Gender` and `Ethnicity`. It shows that the highest earning demographic are White females with an average annual income of 43,806 GBP while the lowest earning demographic are Asian Males with an average annual income of 12,701 GBP. It also appears that females earn more on average than Males, irrespective of their ethnicities. White Males, the highest earning male demographic earn an average of 22.1k GBP, almost 50% lower than the highest earning female demographic. Huge disparity in Income can be observed here.

### Level of Education

Let us examine the Income distribution by level of education, and the highest level of education of these demographics to see if any correlation can be observed between level of education and Income. This can help in explaining the income distribution observed by Ethnicity and Gender.

```{r, warning = FALSE, fig.height=8}
#| label: fig-ed_inc
#| fig-cap: "Income Distribution by level of Education"
#| #| fig-alt: "A Boxplot showing Income distribution of each level of Education "

dataset2 |> filter(!is.na(Highest_Education)) |> 
  ggplot(aes(x = Highest_Education, y = Income)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 750000, by = 100000)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title = "Income Distribution by level of Education",subtitle = "Boxplot showing Income distribution of each level of Education", x = "Highest Education", y = "Annual Income in GBP")
```

```{r, message=FALSE}
dataset2 |>
  filter(Age >=18) |>
  group_by(Ethnicity, Gender) |>
  summarize(Education = names(which.max(table(Highest_Education))))

```

Clearly, there appears to be some correlation between level of education and average income. The top 3 earners have majority of their demographic having some higher education ( `Some HE` ) while the bottom 3 earners have majority with `Less than Secondary School` education. `Other Female` and `Other Male` being outliers in both observations. The boxplot in @fig-ed_inc shows as expected, that individuals of working age with a `Masters or higher` earn more on average than others. Infact, the 50th percentile (top 50%) of holders of `Masters or higher` outearn every other individuals of lower levels of education, except the 75th percentile (top 25%) of holders of a `Bachelors Degree` . On the other hand, individuals with `Less than Secondary School` earn significantly low with at least 50% of them earning lower than every other individual of working age. As expected all categories of individuals have outliers who have high incomes. These outliers may skew the average, as observed in @fig-eth-gen-inc.

### Divorce and Separation Rates

I would now like to examine the marital status and divorce/separation rates of the english populace. According to @karney2021, lower socio-economic status couples are more likely to get divorced than higher-SES counterparts.

```{r, message=FALSE, warning = FALSE}
#| label: fig-div-eth-gen
#| fig-cap: "Divorce/Separation rates by ethnicity and gender"
#| fig-alt: "A Bar Chart showing the divorce/separation rate of marriages in England by Ethnicity and Gender "
#calculating marital success by ethnicity and gender
succ_eth_gen <- dataset2 |>
  group_by(Ethnicity,Gender,Marital_Status) |>
  count() |>
  mutate(marital_success = if_else(Marital_Status %in% c("married", "Widowed"), 1, 0))
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
  ) 
#visualize 
div_eth_gen |>
  mutate(demographic = paste(Ethnicity, Gender, sep = " ")) |>
  ggplot(aes(x = reorder(demographic, -divorce_rate), y = divorce_rate, fill = Ethnicity)) +
  geom_col(position = "dodge", stat = "identity") +
  geom_text(aes(label = paste(round(divorce_rate, 1), '%', sep = "")), vjust = 1.5, size = 3) +  
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title = "Divorce/Separation rate by Ethnicity and Gender",  subtitle = "Column Chart showing the divorce/separation rate of marriages in England by Ethnicity and Gender", y = "Divorce Rate (%)",
       x = "Demographic", fill = "Ethnicity") +
  scale_color_colorblind()
```

@fig-div-eth-gen supports the findings of @karney2021. Demographics of lower socio-economic status in terms of level of education and average income appear to have a higher separation rate. Black males have the highest divorce/separation rate of all demographics in England (2021) at 29.8%. They are closely followed by Black females at 24.1%. Other Males and Asian Males follow closely at 22% and 19.4% respectively. Hispanic Males and Females have the lowest separation rates at 10.5% and 3.1% respectively.

@b93f0c5a14df433bae8525da165ff21b proved from analysis that marriages of women with a college degree last longer than marriages of women with a high school diploma or less. This is supported by results from my analysis showing Hispanic Females and White females with majority having some higher education having significantly less separation rates than their counterparts with less education, with an exception of Asian females.

### Housing Condition

Lastly, I am interested in the housing and living conditions of demographics in England. The question H8 gives insights into the type of housing unit the subjects live in (i.e, whether its a `Self-Contained` Unit or a `Non-Self-Contained` Unit).

#### Housing Unit Type

Are you also wondering what type of housing units the majority of each Ethnicity live in?

```{r}
#| label: fig-eth-unit
#| fig-cap: "Count of Observations by Ethnicity and Unit Type"
#| fig-alt: "geom_count() plot showing count of ethnicity/unit observations"
dataset2 |>
  group_by(Ethnicity, Unit_Type) |>
  count() |>
ggplot(aes(x = Ethnicity, y = Unit_Type, size = n)) +
  geom_count() +
  scale_size_continuous(range = c(3, 18)) +  
  labs(title = "Count of Observations by Ethnicity and Unit Type", x = "Ethnicity", y = "Unit Type", size = "Count")
```

The circles in @fig-eth-unit represent the mapping of the count of observations at each Ethnicity – Unit type location. The area of each circle indicates the count of each observation. Asians, who happen to be the least educated demographic and arguably the least earning demographic are the only ethnicity with majority of them living in accommodation where all the rooms, including the kitchen, bathroom and toilet are NOT behind a door that only each household can use. In fact, ALL non-self-contained units in the dataset are occupied by Asians and ALL Asians live in non-self-contained units.

On the other hand, ALL Blacks live in Self-contained units, likewise Hispanics, Whites and Others. There is a huge count of whites as observed from the plot above due to a higher White population (18,929). Just as the plot area for Hispanics and Others is really small due to the small population of both demographics in the observation (936 and 853 respectively).

#### Average Number of Occupants in a household

I was also wondering which ethnicities have the highest and lowest average number of occupants within each household.

```{r}
#| label: fig-occ-eth
#| fig-cap: "Average number of household Occupants by Ethnicity"
#| fig-alt: "A Column Chart showing Average number of household Occupants by Ethnicity "
Household_dist <- dataset2 |> 
  group_by(ID) |> 
  summarize(
    Occupants = n_distinct(Person_ID), 
    Total_Income = sum(Income, na.rm = TRUE), 
    Ethnicity = names(which.max(table(Ethnicity))),
    no_of_children = sum(if_else(Age < 18,1,0)), 
    Gender = names(which.max(table(Gender))),
    Unit = names(which.max(table(Unit_Type)))
  ) 
#average no of occupants by ethnicity rounded to the nearest whole number
Household_dist |>
  group_by(Ethnicity) |>
  summarize(average_no_of_occupants = round(mean(Occupants))) |> 
  ggplot(aes(x = reorder(Ethnicity, - average_no_of_occupants), y = average_no_of_occupants)) +
  geom_col() +
  labs(x = "Ethnicity",
       y ="Average number of Occupants",
       title = "Average number of household Occupants by Ethnicity") +
  scale_color_colorblind()
```

It turns out that the Asians have the highest average number of occupants in a household, despite also having the least favorable kind of housing unit. I have now established that the Asians have the worst housing/living condition of all ethnicities in England, and the Whites have the most favorable living condition, considering that they have the least average number of occupants per household, and relatively good housing units

#### Average Number of Children per household

Additionally, I would like to know the ethnicity which has the highest average number of kids per household. The output below shows that the Asians have the highest number of children on average than all other ethnicities, with the Whites having the least average number of children per household.

```{r}
Household_dist |> 
  group_by(Ethnicity) |>
  summarize(average_no_of_children = mean(no_of_children)) |>
  arrange(desc(average_no_of_children))
```

## Conclusion

I have been able to derive some interesting insights about the socio-economic status of demographics in England from the hypothetical census dataset. It has been established that White Females earn higher on average than every other demographic in England while Asian males earn the least. On average, females earn higher than males irrespective of their ethnicities.

I have also established that Education always pays because a Master's degree or higher earns you the highest average income in England. Individuals with less than secondary education earn the least.

Divorce/Separation Rates are also higher in Blacks than other ethnicities, with black males having the highest divorce rate. Hispanics on the other hand have the lowest divorce rates with the hispanic females having the lowest divorce rate of any demographic in England.

Lastly, Asians have the worst housing and living conditions, because they live in non-self-contained units, have the highest average number of occupants, and highest average number of children in each household.

