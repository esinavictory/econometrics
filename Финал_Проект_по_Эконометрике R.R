install.packages('marginaleffects')
install.packages('modelsummary')
install.packages('tidyverse')
install.packages('fixest')
install.packages("readxl")
install.packages('gplots')
install.packages('plm')
install.packages('gridExtra')
install.packages('ggplot2')
install.packages("gt")
install.packages("pandoc")
install.packages("broom.mixed") 
library(readxl)
library(modelsummary)
library(tidyverse)
library(fixest)
library(marginaleffects)
library(dplyr)
library(tidyr)
library(gplots)
library(plm)
library(gridExtra)
library(ggplot2)
library(gt)
library(broom.mixed)
library(fixest)
library(broom)
update.packages()

###ОБРАБОТКА ДАТАСЕТА

#Скачиваем и переименовываем датасеты 
Wave7 <- read_csv('WVS_Cross-National_Wave_7_csv_v6_0.csv')
glimpse(Wave7)

Wave6 <- read_csv2('WV6_Data_csv_v20201117.csv')
glimpse(Wave6)

Wave5 <- read_csv2('WV5_Data_csv_v20180912.csv')
glimpse(Wave5)

#Выбираем нужные переменные
Chosen_values_7 <- Wave7[,c('A_YEAR', 'B_COUNTRY', 'Q49', 'Q110','Q48')]
Chosen_values_6 <- Wave6[,c('V262', 'V2', 'V23', 'V100','V55' )]
Chosen_values_5 <- Wave5[,c('V260', 'V2', 'V22', 'V120', 'V46')]

#Убираем отрацательные значения 
Chosen_values_7 <- Chosen_values_7 %>%
  filter(if_all(everything(), ~ .x >= 0 | is.na(.x)))

Chosen_values_6 <- Chosen_values_6 %>%
  filter(if_all(everything(), ~ .x >= 0 | is.na(.x)))

Chosen_values_5 <- Chosen_values_5 %>%
  filter(if_all(everything(), ~ .x >= 0 | is.na(.x)))

#Выбираем нужные переменные

result6 <- Chosen_values_6 %>%
  group_by(V2) %>%                                    # группировка по стране
  summarise(across(V23:V55, ~ mean(.x, na.rm = TRUE)))  # средние по переменным

result6

result5 <- Chosen_values_5 %>%
  group_by(V2) %>%                                    # группировка по стране
  summarise(across(V22:V46, ~ mean(.x, na.rm = TRUE)))  # средние по переменным

result5

result7 <- Chosen_values_7 %>%
  group_by(B_COUNTRY) %>%                                    # группировка по стране
  summarise(across(Q49:Q48, ~ mean(.x, na.rm = TRUE)))  # средние по переменным

result7

#Переименовываем название столбца со страной
result7 <- result7 %>% rename(COUNTRY = B_COUNTRY)
result6 <- result6 %>% rename(COUNTRY = V2)
result5 <- result5 %>% rename(COUNTRY = V2)

#Соединяю таблицы
Merged <- result7 %>%
  full_join(result6, by = "COUNTRY") %>%
  full_join(result5, by = "COUNTRY") 
  
#Отчищаем таблицу от ошибок, которые встречаются в двух волнах
Merged_clean1 <- Merged %>%
  filter(!if_all(V23:V46, is.na)) 
Merged_clean2 <- Merged_clean1 %>%
  filter(!if_all(Q49:V55, is.na))  

Merged_clean <- Merged_clean2 %>%
  filter(!(is.na(Q49) & is.na(Q110) & is.na(Q48) & 
             is.na(V46) & is.na(V120) & is.na(V22)))
         
# Создаем список правил для замены ячеек с NA
replacement_rules <- list(
  list(col = "V23", val1 = "Q49", val2 = "V22"),
  list(col = "V100", val1 = "Q110", val2 = "V120"),
  list(col = "V55", val1 = "Q48", val2 = "V46"),
  list(col = "V22", val1 = "Q49", val2 = "V23"),
  list(col = "V120", val1 = "Q110", val2 = "V100"),
  list(col = "V46", val1 = "Q48", val2 = "V55"),
  list(col = "Q49", val1 = "V23", val2 = "V22"),
  list(col = "Q110", val1 = "V120", val2 = "V100"),
  list(col = "Q48", val1 = "V46", val2 = "V55")
)

# Применяем правила замены
for(rule in replacement_rules) {
  na_indices <- is.na(Merged_clean[[rule$col]])
  Merged_clean[[rule$col]][na_indices] <- (Merged_clean[[rule$val1]][na_indices] + Merged_clean[[rule$val2]][na_indices]) / 2
}

#Переименовываем столбцы с переменными
names(Merged_clean)[names(Merged_clean) == "V22"] <- "satisfaction5"
names(Merged_clean)[names(Merged_clean) == "V23"] <- "satisfaction6"
names(Merged_clean)[names(Merged_clean) == "Q49"] <- "satisfaction7"
names(Merged_clean)[names(Merged_clean) == "Q110"] <- "hardwork7"
names(Merged_clean)[names(Merged_clean) == "V100"] <- "hardwork6"
names(Merged_clean)[names(Merged_clean) == "V120"] <- "hardwork5"
names(Merged_clean)[names(Merged_clean) == "V46"] <- "control5"
names(Merged_clean)[names(Merged_clean) == "V55"] <- "control6"
names(Merged_clean)[names(Merged_clean) == "Q48"] <- "control7"

#Создаем справочник соответствия кодов и названий стран
country_codes <- c(
  "8" = "Albania", "12" = "Algeria", "16" = "American Samoa", "20" = "Andorra",
  "24" = "Angola", "28" = "Antigua and Barbuda", "32" = "Argentina", 
  "51" = "Armenia", "36" = "Australia", "40" = "Austria", "31" = "Azerbaijan",
  "50" = "Bangladesh", "52" = "Barbados", "56" = "Belgium", "60" = "Bermuda",
  "64" = "Bhutan", "68" = "Bolivia", "70" = "Bosnia", "72" = "Botswana",
  "76" = "Brazil", "84" = "Belize", "100" = "Bulgaria", "854" = "Burkina Faso",
  "108" = "Burundi", "112" = "Belarus", "116" = "Cambodia", "120" = "Cameroon",
  "124" = "Canada", "148" = "Chad", "152" = "Chile", "156" = "China",
  "170" = "Colombia", "384" = "Côte d'Ivoire", "184" = "Cook Islands",
  "188" = "Costa Rica", "191" = "Croatia", "192" = "Cuba", "196" = "Cyprus",
  "203" = "Czech Republic", "180" = "Dem. Rep. of Congo", "208" = "Denmark",
  "214" = "Dominican Republic", "818" = "Egypt", "218" = "Ecuador",
  "222" = "El Salvador", "226" = "Equatorial Guinea", "231" = "Ethiopia",
  "232" = "Eritrea", "233" = "Estonia", "246" = "Finland", "250" = "France",
  "268" = "Georgia", "270" = "Gambia", "624" = "Guinea-Bissau", "276" = "Germany",
  "288" = "Ghana", "292" = "Gibraltar", "300" = "Greece", "320" = "Guatemala",
  "324" = "Guinea", "328" = "Guyana", "826" = "Great Britain", "332" = "Haiti",
  "340" = "Honduras", "344" = "Hong Kong", "348" = "Hungary", "352" = "Iceland",
  "356" = "India", "360" = "Indonesia", "364" = "Iran", "368" = "Iraq",
  "372" = "Ireland", "376" = "Israel", "380" = "Italy", "400" = "Jordan",
  "388" = "Jamaica", "392" = "Japan", "398" = "Kazakhstan", "404" = "Kenya",
  "414" = "Kuwait", "417" = "Kyrgyzstan", "418" = "Laos", "422" = "Lebanon",
  "426" = "Lesotho", "428" = "Latvia", "430" = "Liberia", "434" = "Libya",
  "450" = "Madagascar", "454" = "Malawi", "458" = "Malaysia", "466" = "Mali",
  "470" = "Malta", "474" = "Martinique", "478" = "Mauritania", "480" = "Mauritius",
  "484" = "Mexico", "492" = "Monaco", "496" = "Mongolia", "498" = "Moldova",
  "504" = "Morocco", "508" = "Mozambique", "104" = "Myanmar", "807" = "Macedonia",
  "516" = "Namibia", "524" = "Nepal", "528" = "Netherlands", "554" = "New Zealand",
  "558" = "Nicaragua", "562" = "Niger", "566" = "Nigeria", "578" = "Norway",
  "408" = "North Korea", "512" = "Oman", "586" = "Pakistan", "591" = "Panama",
  "598" = "Papua New Guinea", "600" = "Paraguay", "604" = "Peru", "608" = "Philippines",
  "616" = "Poland", "620" = "Portugal", "275" = "Palestine", "630" = "Puerto Rico",
  "634" = "Qatar", "642" = "Romania", "643" = "Russia", "646" = "Rwanda",
  "682" = "Saudi Arabia", "144" = "Sri Lanka", "686" = "Senegal", "690" = "Seychelles",
  "694" = "Sierra Leone", "702" = "Singapore", "703" = "Slovakia", "705" = "Slovenia",
  "706" = "Somalia", "710" = "South Africa", "724" = "Spain", "736" = "Sudan",
  "740" = "Suriname", "752" = "Sweden", "756" = "Switzerland", "760" = "Syria",
  "410" = "South Korea", "891" = "Serbia and Montenegro", "911" = "Serbia",
  "912" = "Montenegro", "626" = "Timor-Leste", "762" = "Tajikistan", "764" = "Thailand",
  "768" = "Togo", "780" = "Trinidad", "788" = "Tunisia", "158" = "Taiwan",
  "792" = "Turkey", "795" = "Turkmenistan", "834" = "Tanzania", "784" = "United Arab Emirates",
  "800" = "Uganda", "804" = "Ukraine", "840" = "United States", "850" = "U.S. Virgin Islands",
  "858" = "Uruguay", "860" = "Uzbekistan", "704" = "Viet Nam", "862" = "Venezuela",
  "887" = "Yemen", "894" = "Zambia", "716" = "Zimbabwe"
)

# Создадим вектор с нашими странами, которые находятся в таблице
country_numbers <- c(20, 32, 36, 51, 76, 124, 152, 156, 158, 170, 
                     196, 218, 231, 276, 344, 356, 360, 364, 368, 392,
                     398, 400, 410, 417, 422, 434, 458, 484, 504, 528,
                     554, 566, 586, 604, 608, 642, 643, 688, 702, 704,
                     716, 764, 788, 792, 804, 818, 826, 840, 858, 860,
                     268, 288, 616, 646, 705, 710, 724, 752, 780)
# Преобразуем коды в названия стран
country_names <- country_codes[as.character(country_numbers)]

# Заменим значения кода стран в таблице на названия
Merged_clean$COUNTRY <- country_codes[as.character(Merged_clean$COUNTRY)]

# Удалим страны, которых нет в справочнике
Merged_clean_clean <- Merged_clean[!is.na(Merged_clean$COUNTRY), ]

#Работа с файлом WBD эксель =======================================================================================
# Загрузим дополнительные данные из WBD
data_WBD <- read_excel("P_Data_Extract_From_World_Development_Indicators2.xlsx", range = "A1:J4789")

# Переименование столбцов для удобства
colnames(data_WBD) <- c("Time", "Time_Code", "COUNTRY", "Country_Code","Employment_in_industry", "Educational_attainment",
                    "GDP_per_capita", "Gini_index", "Unemployment", "Health_expenditure")

# Преобразование типов данных. Заменяем пропуски на значения Na
data_WBD$Time <- as.numeric(data_WBD$Time)
data_WBD$Employment_in_industry <- as.numeric(data_WBD$Employment_in_industry)
data_WBD$Educational_attainment <- as.numeric(data_WBD$Educational_attainment)
data_WBD$GDP_per_capita <- as.numeric(data_WBD$GDP_per_capita)
data_WBD$Gini_index <- as.numeric(data_WBD$Gini_index)
data_WBD$Unemployment <- as.numeric(data_WBD$Unemployment)
data_WBD$Health_expenditure <- as.numeric(data_WBD$Health_expenditure)

# Создание периодов (волн)
data_WBD <- data_WBD %>%
  mutate(Wave = case_when(
    Time >= 2005 & Time <= 2009 ~ "5",
    Time >= 2010 & Time <= 2014 ~ "6",
    Time >= 2017 & Time <= 2022 ~ "7",
    TRUE ~ "Other"
  ))

# Фильтрация только нужных периодов/ убираем года 2015 и 2016
filtered_data <- data_WBD %>%
  filter(Wave != "Other")

#Считаем среднее по странам
summary_stats <- filtered_data %>%
  group_by(COUNTRY, Country_Code, Wave) %>%
  summarise(
    Mean_Employment_in_industry = ifelse(all(is.na(Employment_in_industry)), 
                                 NA, 
                                 mean(Employment_in_industry, na.rm = TRUE)),
    Mean_Educational_attainment = ifelse(all(is.na(Educational_attainment)), 
                                 NA, 
                                 mean(Educational_attainment, na.rm = TRUE)),
    Mean_GDP_per_capita = ifelse(all(is.na(GDP_per_capita)), 
                                 NA, 
                                 mean(GDP_per_capita, na.rm = TRUE)),
    Mean_Gini_index = ifelse(all(is.na(Gini_index)), 
                             NA, 
                             mean(Gini_index, na.rm = TRUE)),
    Mean_Unemployment = ifelse(all(is.na(Unemployment)), 
                               NA, 
                               mean(Unemployment, na.rm = TRUE)),
    Mean_Health_expenditure = ifelse(all(is.na(Health_expenditure)), 
                                     NA, 
                                     mean(Health_expenditure, na.rm = TRUE)),
    .groups = 'drop'
  )

# Преобразование в широкий формат для лучшей читаемости
wide_format <- summary_stats %>%
  pivot_wider(
    names_from = Wave,
    values_from = c(Mean_Employment_in_industry, Mean_Educational_attainment, Mean_GDP_per_capita, Mean_Gini_index, 
                    Mean_Unemployment, Mean_Health_expenditure),
    names_glue = "{Wave}_{.value}"
  )

wide_format <- wide_format %>%
  mutate(COUNTRY = case_when(
    COUNTRY == "Taiwan, China" ~ "Taiwan",
    COUNTRY == "Hong Kong SAR, China" ~ "Hong Kong",
    COUNTRY == "Iran, Islamic Rep." ~ "Iran",
    COUNTRY == "Korea, Rep." ~ "South Korea",
    COUNTRY == "Kyrgyz Republic" ~ "Kyrgyzstan",
    COUNTRY == "Russian Federation" ~ "Russia",
    COUNTRY == "Turkiye" ~ "Turkey",
    COUNTRY == "Egypt, Arab Rep." ~ "Egypt",
    COUNTRY == "United Kingdom" ~ "Great Britain",
    TRUE ~ COUNTRY
  ))


#Соединяю таблицы
Merged_WVS_WBD <- Merged_clean_clean %>%
  left_join(wide_format, by = "COUNTRY") 

# Переназовем столбцы
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "5_Mean_Employment_in_industry"] <- "industry5"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "6_Mean_Employment_in_industry"] <- "industry6"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "7_Mean_Employment_in_industry"] <- "industry7"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "5_Mean_Educational_attainment"] <- "Education5"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "6_Mean_Educational_attainment"] <- "Education6"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "7_Mean_Educational_attainment"] <- "Education7"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "5_Mean_GDP_per_capita"] <- "GDP5"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "6_Mean_GDP_per_capita"] <- "GDP6"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "7_Mean_GDP_per_capita"] <- "GDP7"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "5_Mean_Gini_index"] <- "Gini5"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "6_Mean_Gini_index"] <- "Gini6"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "7_Mean_Gini_index"] <- "Gini7"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "5_Mean_Unemployment"] <- "Unemployment5"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "6_Mean_Unemployment"] <- "Unemployment6"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "7_Mean_Unemployment"] <- "Unemployment7"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "5_Mean_Health_expenditure"] <- "Health5"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "6_Mean_Health_expenditure"] <- "Health6"
names(Merged_WVS_WBD)[names(Merged_WVS_WBD) == "7_Mean_Health_expenditure"] <- "Health7"
  
#Отчищаем таблицу от ошибок, которые встречаются в WBD
Merged_WVS_WBD <- Merged_WVS_WBD %>%
  filter(!if_all(Country_Code:Health7, is.na))

# Создаем список правил для замены ячеек с NA
replacement_rules_Merged_WVS_WBD <- list(
  list(col = "industry5", val1 = "industry6", val2 = "industry7"),
  list(col = "industry6", val1 = "industry5", val2 = "industry7"),
  list(col = "industry7", val1 = "industry5", val2 = "industry6"),
  list(col = "Education5", val1 = "Education6", val2 = "Education7"),
  list(col = "Education6", val1 = "Education5", val2 = "Education7"),
  list(col = "Education7", val1 = "Education5", val2 = "Education6"),
  list(col = "GDP5", val1 = "GDP6", val2 = "GDP7"),
  list(col = "GDP6", val1 = "GDP5", val2 = "GDP7"),
  list(col = "GDP7", val1 = "GDP5", val2 = "GDP6"),
  list(col = "Gini5", val1 = "Gini6", val2 = "Gini7"),
  list(col = "Gini6", val1 = "Gini5", val2 = "Gini7"),
  list(col = "Gini7", val1 = "Gini5", val2 = "Gini6"),
  list(col = "Unemployment5", val1 = "Unemployment6", val2 = "Unemployment7"),
  list(col = "Unemployment6", val1 = "Unemployment5", val2 = "Unemployment7"),
  list(col = "Unemployment7", val1 = "Unemployment5", val2 = "Unemployment6"),
  list(col = "Health5", val1 = "Health6", val2 = "Health7"),
  list(col = "Health6", val1 = "Health5", val2 = "Health7"),
  list(col = "Health7", val1 = "Health5", val2 = "Health6")
)

for(rule in replacement_rules_Merged_WVS_WBD) {
     na_indices <- is.na(Merged_WVS_WBD[[rule$col]])
     Merged_WVS_WBD[[rule$col]][na_indices] <- (Merged_WVS_WBD[[rule$val1]][na_indices] + Merged_WVS_WBD[[rule$val2]][na_indices]) / 2
   }

# Удаляем ошибки, которые встречаются в двух и трех волнах в WBD
Merged_WVS_WBD <- Merged_WVS_WBD %>% 
  drop_na()

#Удалим лишний столбец Country_code
FINAL_Merged_WVS_WBD <- Merged_WVS_WBD %>% select(-c(Country_Code))

#Указать правильный порядок столбцов
FINAL_Merged_WVS_WBD_balanced <- FINAL_Merged_WVS_WBD %>% select(COUNTRY,	'satisfaction5',	'hardwork5', 'control5', "industry5", "Education5", 'GDP5', 'Gini5', 'Unemployment5', 'Health5', 'satisfaction6',	'hardwork6',	'control6', "industry6", "Education6",'GDP6', 'Gini6', 'Unemployment6', 'Health6', 'satisfaction7',	'hardwork7',	'control7', "industry7", "Education7", 'GDP7', 'Gini7', 'Unemployment7', 'Health7')


### Unbalanced data========================================================================
#Отчищаем таблицу от ошибок, которые встречаются в двух волнах
Merged_unbalanced1 <- Merged %>%
  filter(!if_all(V23:V46, is.na)) 
Merged_unbalanced2 <- Merged_unbalanced1 %>%
  filter(!if_all(Q49:V55, is.na))  

Merged_unbalanced <- Merged_unbalanced2 %>%
  filter(!(is.na(Q49) & is.na(Q110) & is.na(Q48) & 
             is.na(V46) & is.na(V120) & is.na(V22)))

#Переименовываем столбцы с переменными
names(Merged_unbalanced)[names(Merged_unbalanced) == "V22"] <- "satisfaction5"
names(Merged_unbalanced)[names(Merged_unbalanced) == "V23"] <- "satisfaction6"
names(Merged_unbalanced)[names(Merged_unbalanced) == "Q49"] <- "satisfaction7"
names(Merged_unbalanced)[names(Merged_unbalanced) == "Q110"] <- "hardwork7"
names(Merged_unbalanced)[names(Merged_unbalanced) == "V100"] <- "hardwork6"
names(Merged_unbalanced)[names(Merged_unbalanced) == "V120"] <- "hardwork5"
names(Merged_unbalanced)[names(Merged_unbalanced) == "V46"] <- "control5"
names(Merged_unbalanced)[names(Merged_unbalanced) == "V55"] <- "control6"
names(Merged_unbalanced)[names(Merged_unbalanced) == "Q48"] <- "control7"

#Создаем справочник соответствия кодов и названий стран
country_codes <- c(
  "8" = "Albania", "12" = "Algeria", "16" = "American Samoa", "20" = "Andorra",
  "24" = "Angola", "28" = "Antigua and Barbuda", "32" = "Argentina", 
  "51" = "Armenia", "36" = "Australia", "40" = "Austria", "31" = "Azerbaijan",
  "50" = "Bangladesh", "52" = "Barbados", "56" = "Belgium", "60" = "Bermuda",
  "64" = "Bhutan", "68" = "Bolivia", "70" = "Bosnia", "72" = "Botswana",
  "76" = "Brazil", "84" = "Belize", "100" = "Bulgaria", "854" = "Burkina Faso",
  "108" = "Burundi", "112" = "Belarus", "116" = "Cambodia", "120" = "Cameroon",
  "124" = "Canada", "148" = "Chad", "152" = "Chile", "156" = "China",
  "170" = "Colombia", "384" = "Côte d'Ivoire", "184" = "Cook Islands",
  "188" = "Costa Rica", "191" = "Croatia", "192" = "Cuba", "196" = "Cyprus",
  "203" = "Czech Republic", "180" = "Dem. Rep. of Congo", "208" = "Denmark",
  "214" = "Dominican Republic", "818" = "Egypt", "218" = "Ecuador",
  "222" = "El Salvador", "226" = "Equatorial Guinea", "231" = "Ethiopia",
  "232" = "Eritrea", "233" = "Estonia", "246" = "Finland", "250" = "France",
  "268" = "Georgia", "270" = "Gambia", "624" = "Guinea-Bissau", "276" = "Germany",
  "288" = "Ghana", "292" = "Gibraltar", "300" = "Greece", "320" = "Guatemala",
  "324" = "Guinea", "328" = "Guyana", "826" = "Great Britain", "332" = "Haiti",
  "340" = "Honduras", "344" = "Hong Kong", "348" = "Hungary", "352" = "Iceland",
  "356" = "India", "360" = "Indonesia", "364" = "Iran", "368" = "Iraq",
  "372" = "Ireland", "376" = "Israel", "380" = "Italy", "400" = "Jordan",
  "388" = "Jamaica", "392" = "Japan", "398" = "Kazakhstan", "404" = "Kenya",
  "414" = "Kuwait", "417" = "Kyrgyzstan", "418" = "Laos", "422" = "Lebanon",
  "426" = "Lesotho", "428" = "Latvia", "430" = "Liberia", "434" = "Libya",
  "450" = "Madagascar", "454" = "Malawi", "458" = "Malaysia", "466" = "Mali",
  "470" = "Malta", "474" = "Martinique", "478" = "Mauritania", "480" = "Mauritius",
  "484" = "Mexico", "492" = "Monaco", "496" = "Mongolia", "498" = "Moldova",
  "504" = "Morocco", "508" = "Mozambique", "104" = "Myanmar", "807" = "Macedonia",
  "516" = "Namibia", "524" = "Nepal", "528" = "Netherlands", "554" = "New Zealand",
  "558" = "Nicaragua", "562" = "Niger", "566" = "Nigeria", "578" = "Norway",
  "408" = "North Korea", "512" = "Oman", "586" = "Pakistan", "591" = "Panama",
  "598" = "Papua New Guinea", "600" = "Paraguay", "604" = "Peru", "608" = "Philippines",
  "616" = "Poland", "620" = "Portugal", "275" = "Palestine", "630" = "Puerto Rico",
  "634" = "Qatar", "642" = "Romania", "643" = "Russia", "646" = "Rwanda",
  "682" = "Saudi Arabia", "144" = "Sri Lanka", "686" = "Senegal", "690" = "Seychelles",
  "694" = "Sierra Leone", "702" = "Singapore", "703" = "Slovakia", "705" = "Slovenia",
  "706" = "Somalia", "710" = "South Africa", "724" = "Spain", "736" = "Sudan",
  "740" = "Suriname", "752" = "Sweden", "756" = "Switzerland", "760" = "Syria",
  "410" = "South Korea", "891" = "Serbia and Montenegro", "911" = "Serbia",
  "912" = "Montenegro", "626" = "Timor-Leste", "762" = "Tajikistan", "764" = "Thailand",
  "768" = "Togo", "780" = "Trinidad", "788" = "Tunisia", "158" = "Taiwan",
  "792" = "Turkey", "795" = "Turkmenistan", "834" = "Tanzania", "784" = "United Arab Emirates",
  "800" = "Uganda", "804" = "Ukraine", "840" = "United States", "850" = "U.S. Virgin Islands",
  "858" = "Uruguay", "860" = "Uzbekistan", "704" = "Viet Nam", "862" = "Venezuela",
  "887" = "Yemen", "894" = "Zambia", "716" = "Zimbabwe"
)

# Создадим вектор с нашими странами, которые находятся в таблице
country_numbers <- c(20, 32, 36, 51, 76, 124, 152, 156, 158, 170, 
                     196, 218, 231, 276, 344, 356, 360, 364, 368, 392,
                     398, 400, 410, 417, 422, 434, 458, 484, 504, 528,
                     554, 566, 586, 604, 608, 642, 643, 688, 702, 704,
                     716, 764, 788, 792, 804, 818, 826, 840, 858, 860,
                     268, 288, 616, 646, 705, 710, 724, 752, 780)
# Преобразуем коды в названия стран
country_names <- country_codes[as.character(country_numbers)]

# Заменим значения кода стран в таблице на названия
Merged_unbalanced$COUNTRY <- country_codes[as.character(Merged_unbalanced$COUNTRY)]

# Удалим страны, которых нет в справочнике
Merged_unbalanced_clean <- Merged_unbalanced[!is.na(Merged_unbalanced$COUNTRY), ]

##WBD
#Соединяю таблицы
Merged_WVS_WBD_unbalanced <- Merged_unbalanced_clean %>%
  left_join(wide_format, by = "COUNTRY") 

# Переназовем столбцы
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "5_Mean_Employment_in_industry"] <- "industry5"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "6_Mean_Employment_in_industry"] <- "industry6"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "7_Mean_Employment_in_industry"] <- "industry7"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "5_Mean_Educational_attainment"] <- "Education5"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "6_Mean_Educational_attainment"] <- "Education6"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "7_Mean_Educational_attainment"] <- "Education7"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "5_Mean_GDP_per_capita"] <- "GDP5"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "6_Mean_GDP_per_capita"] <- "GDP6"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "7_Mean_GDP_per_capita"] <- "GDP7"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "5_Mean_Gini_index"] <- "Gini5"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "6_Mean_Gini_index"] <- "Gini6"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "7_Mean_Gini_index"] <- "Gini7"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "5_Mean_Unemployment"] <- "Unemployment5"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "6_Mean_Unemployment"] <- "Unemployment6"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "7_Mean_Unemployment"] <- "Unemployment7"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "5_Mean_Health_expenditure"] <- "Health5"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "6_Mean_Health_expenditure"] <- "Health6"
names(Merged_WVS_WBD_unbalanced)[names(Merged_WVS_WBD_unbalanced) == "7_Mean_Health_expenditure"] <- "Health7"


#Отчищаем таблицу от ошибок, которые встречаются в WBD
Merged_WVS_WBD_unbalanced <- Merged_WVS_WBD_unbalanced %>%
  filter(!if_all(Country_Code:Health7, is.na))

Merged_WVS_WBD_unbalanced1<- Merged_WVS_WBD_unbalanced %>%
  filter(rowSums(is.na(select(., industry5, industry6, industry7))) < 2)
Merged_WVS_WBD_unbalanced1<- Merged_WVS_WBD_unbalanced1 %>%
  filter(rowSums(is.na(select(., Education5, Education6, Education7))) < 2)
Merged_WVS_WBD_unbalanced1<- Merged_WVS_WBD_unbalanced1 %>%
  filter(rowSums(is.na(select(., GDP5, GDP6, GDP7))) < 2)
Merged_WVS_WBD_unbalanced1<- Merged_WVS_WBD_unbalanced1 %>%
  filter(rowSums(is.na(select(., Gini5, Gini6, Gini7))) < 2)
Merged_WVS_WBD_unbalanced1<- Merged_WVS_WBD_unbalanced1 %>%
  filter(rowSums(is.na(select(., Unemployment5, Unemployment6, Unemployment7))) < 2)
Merged_WVS_WBD_unbalanced1<- Merged_WVS_WBD_unbalanced1 %>%
  filter(rowSums(is.na(select(., Health5, Health6, Health7))) < 2)

#Удалим лишний столбец Country_code
FINAL_Merged_WVS_WBD_unbalanced <- Merged_WVS_WBD_unbalanced1 %>% select(-c(Country_Code))

#Указать правильный порядок столбцов
FINAL_Merged_WVS_WBD_unbalanced <- FINAL_Merged_WVS_WBD_unbalanced %>% select(COUNTRY,	'satisfaction5',	'hardwork5', 'control5', "industry5", "Education5", 'GDP5', 'Gini5', 'Unemployment5', 'Health5', 'satisfaction6',	'hardwork6',	'control6', "industry6", "Education6",'GDP6', 'Gini6', 'Unemployment6', 'Health6', 'satisfaction7',	'hardwork7',	'control7', "industry7", "Education7", 'GDP7', 'Gini7', 'Unemployment7', 'Health7')

#работа с balanced data
FINAL_balanced_Merged_WVS_WBD_long <- FINAL_Merged_WVS_WBD_balanced %>% pivot_longer(
  cols = -COUNTRY,
  names_to = c(".value", "wave"),
  names_pattern = "(\\w+)(\\d+)"
) %>%
  rename(
    life_satisfaction = satisfaction,
    belief_hardwork = hardwork,
    personal_control = control, 
    gdp = GDP,
    gini = Gini,
    unemployment = Unemployment,
    health = Health
  ) %>%
  mutate(wave = as.numeric(wave))

#ГРАФИКИ (balanced data)
#Графики распределения
plot_belief <- ggplot(FINAL_balanced_Merged_WVS_WBD_long, 
                      aes(x = belief_hardwork)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", 
                 color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  labs(x = "Belief in Hardwork", 
       y = "Плотность") +
  theme_minimal()
plot_belief

plot_satisfaction <- ggplot(FINAL_balanced_Merged_WVS_WBD_long, 
                            aes(x = life_satisfaction)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", 
                 color = "black", alpha = 0.7) +
  geom_density(color = "darkgreen", size = 1) +
  labs(x = "Life Satisfaction", 
       y = "Плотность") +
  theme_minimal()
plot_satisfaction

# Размещаем оба графика рядом
grid.arrange(plot_belief, plot_satisfaction, ncol = 2)


#Точечная диаграмма рассеяния (life_satisfaction и belief_hardwork)
plot1 <- qplot(belief_hardwork, life_satisfaction, data = FINAL_balanced_Merged_WVS_WBD_long, geom = "point") + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Общий график",
       x = "belief_hardwork", 
       y = "life_satisfaction") +
  theme_minimal()
plot1

plot2 <- ggplot(FINAL_balanced_Merged_WVS_WBD_long, 
                aes(x = belief_hardwork, y = life_satisfaction, color = factor(wave))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("5" = "red", "6" = "blue", "7" = "green"),
                     name = "Wave") +
  labs(title = "График по волнам",
       x = "belief_hardwork", 
       y = "life_satisfaction") +
  theme_minimal()
plot2 

# Размещаем оба графика рядом
grid.arrange(plot2, plot1, ncol = 2)

#Графики средних значений по волнам
par(mfrow = c(2, 3))

# Life satisfaction по волнам
plotmeans(life_satisfaction ~ wave, FINAL_balanced_Merged_WVS_WBD_long,
          main = "Life Satisfaction by Wave",
          xlab = "Wave", ylab = "Life Satisfaction")

# Belief in hardwork по волнам
plotmeans(belief_hardwork ~ wave, FINAL_balanced_Merged_WVS_WBD_long,
          main = "Belief in Hardwork by Wave",
          xlab = "Wave", ylab = "Belief in Hardwork")

# Personal control по волнам
plotmeans(personal_control ~ wave, FINAL_balanced_Merged_WVS_WBD_long,
          main = "Personal Control by Wave",
          xlab = "Wave", ylab = "Personal Control")

# GDP по волнам
plotmeans(gdp ~ wave, FINAL_balanced_Merged_WVS_WBD_long,
          main = "GDP by Wave",
          xlab = "Wave", ylab = "GDP")

# Gini по волнам
plotmeans(gini ~ wave, FINAL_balanced_Merged_WVS_WBD_long,
          main = "Gini Coefficient by Wave",
          xlab = "Wave", ylab = "Gini Coefficient")

# Unemployment по волнам
plotmeans(unemployment ~ wave, FINAL_balanced_Merged_WVS_WBD_long,
          main = "Unemployment by Wave",
          xlab = "Wave", ylab = "Unemployment Rate")

par(mfrow = c(1, 1))

###Models=====================================================================================
#pooled ols без учета personal_control
model1 <- feols(
  life_satisfaction ~ belief_hardwork + gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY,
)
summary(model1)

# Create and display the table
table<-tidy(model1) |> 
  mutate(
    Variable = case_when(
      term == "belief_hardwork" ~ "Belief in hard work",
      term == "gdp" ~ "GDP per capita", 
      term == "gini" ~ "Gini coefficient",
      term == "unemployment" ~ "Unemployment rate",
      term == "health" ~ "Health expenditure"
    ),
    Significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) |>
  select(Variable, Coefficient = estimate, "Std. Error" = std.error, "p-value" = p.value, Significance) |>
  gt() |>
  tab_header(
    title = "Regression Results: Life Satisfaction",
    subtitle = "Fixed effects model (country + wave)"
  )
table
gtsave(table, "regression_results.docx")

#Hausman test
fe_model1 <- plm(
  life_satisfaction ~ belief_hardwork + gdp + gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "within"
)

re_model1  <- plm(
  life_satisfaction ~ belief_hardwork + gdp + gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "random"
)

hausman_test <- phtest(fe_model1, re_model1)
print(hausman_test)

#pooled ols c учетом personal_control
model2 <- feols(
  life_satisfaction ~ belief_hardwork + personal_control + 
    gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)
summary(model2)

gtsave(model2, "regression_result.docx")

#Hausman test
fe_model2 <- plm(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "within",
  effect = "twoways"
)

# RE модель для model2
re_model2 <- plm(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "random"
)

# Тест Хаусмана для model2
hausman_test2 <- phtest(fe_model2, re_model2)
print("Hausman Test for Model2:")
print(hausman_test2)

#Квадратичный эффект веры в тяжелый труд
model3 <- feols(
  life_satisfaction ~ belief_hardwork + I(belief_hardwork^2) + 
    personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)
summary(model3)

# FE модель для model3
fe_model3 <- plm(
  life_satisfaction ~ belief_hardwork + I(belief_hardwork^2) + personal_control + 
    gdp + gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "within",
  effect = "twoways"
)

# RE модель для model3
re_model3 <- plm(
  life_satisfaction ~ belief_hardwork + I(belief_hardwork^2) + personal_control + 
    gdp + gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "random"
)

# Тест Хаусмана для model3
hausman_test3 <- phtest(fe_model3, re_model3)
print(hausman_test3)

#Interection term c экономическим развитием
model4 <- feols(
  life_satisfaction ~ belief_hardwork * gdp + 
    personal_control + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)
summary(model4)

# FE модель для model4
fe_model4 <- plm(
  life_satisfaction ~ belief_hardwork * gdp + personal_control + 
    gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "within",
  effect = "twoways"
)

# RE модель для model4
re_model4 <- plm(
  life_satisfaction ~ belief_hardwork * gdp + personal_control + 
    gini + unemployment + health,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  model = "random"
)

# Тест Хаусмана для model4
hausman_test4 <- phtest(fe_model4, re_model4)
print(hausman_test4)

#Только фиксированные эффекты стран (Entity Fixed Effects)
model_fe_country <- feols(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health | COUNTRY,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)
summary(model_fe_country)

#Двухсторонние фиксированные эффекты (Country + Time)
model_fe_twoway <- feols(
  life_satisfaction ~ belief_hardwork + gdp + personal_control + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)
summary(model_fe_twoway)

model_time_fe <- feols(
  life_satisfaction ~ belief_hardwork + gdp + personal_control + gini + unemployment + health + factor(wave) | COUNTRY,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)
summary(model_time_fe)


#работа с unbalanced data
FINAL_Merged_WVS_WBD_unbalanced_long <- FINAL_Merged_WVS_WBD_unbalanced %>% pivot_longer(
  cols = -COUNTRY,
  names_to = c(".value", "wave"),
  names_pattern = "(\\w+)(\\d+)"
) %>%
  rename(
    life_satisfaction = satisfaction ,
    belief_hardwork = hardwork, 
    personal_control = control, 
    gdp = GDP,
    gini = Gini,
    unemployment = Unemployment,
    health = Health
  ) %>%
  mutate(wave = as.numeric(wave))

#ГРАФИКИ (unbalanced data)
#Графики распределения

plot_belief <- ggplot(FINAL_Merged_WVS_WBD_unbalanced_long, 
                      aes(x = belief_hardwork)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", 
                 color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  labs(x = "Belief in Hardwork", 
       y = "Плотность") +
  theme_minimal()

plot_satisfaction <- ggplot(FINAL_Merged_WVS_WBD_unbalanced_long, 
                            aes(x = life_satisfaction)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", 
                 color = "black", alpha = 0.7) +
  geom_density(color = "darkgreen", size = 1) +
  labs(x = "Life Satisfaction", 
       y = "Плотность") +
  theme_minimal()

# Размещаем оба графика рядом
grid.arrange(plot_belief, plot_satisfaction, ncol = 2)


#Точечная диаграмма рассеяния (life_satisfaction и belief_hardwork)
plot1 <- qplot(belief_hardwork, life_satisfaction, data = FINAL_Merged_WVS_WBD_unbalanced_long, geom = "point") + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Общий график",
       x = "belief_hardwork", 
       y = "life_satisfaction") +
  theme_minimal()

plot2 <- ggplot(FINAL_Merged_WVS_WBD_unbalanced_long, 
                aes(x = belief_hardwork, y = life_satisfaction, color = factor(wave))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("5" = "red", "6" = "blue", "7" = "green"),
                     name = "Wave") +
  labs(title = "График по волнам",
       x = "belief_hardwork", 
       y = "life_satisfaction") +
  theme_minimal()

# Размещаем оба графика рядом
grid.arrange(plot2, plot1, ncol = 2)

#Графики средних значений по волнам
par(mfrow = c(2, 3))

# Life satisfaction по волнам
plotmeans(life_satisfaction ~ wave, FINAL_Merged_WVS_WBD_unbalanced_long,
          main = "Life Satisfaction by Wave",
          xlab = "Wave", ylab = "Life Satisfaction")

# Belief in hardwork по волнам
plotmeans(belief_hardwork ~ wave, FINAL_Merged_WVS_WBD_unbalanced_long,
          main = "Belief in Hardwork by Wave",
          xlab = "Wave", ylab = "Belief in Hardwork")

# Personal control по волнам
plotmeans(personal_control ~ wave, FINAL_Merged_WVS_WBD_unbalanced_long,
          main = "Personal Control by Wave",
          xlab = "Wave", ylab = "Personal Control")

# GDP по волнам
plotmeans(gdp ~ wave, FINAL_Merged_WVS_WBD_unbalanced_long,
          main = "GDP by Wave",
          xlab = "Wave", ylab = "GDP")

# Gini по волнам
plotmeans(gini ~ wave, FINAL_Merged_WVS_WBD_unbalanced_long,
          main = "Gini Coefficient by Wave",
          xlab = "Wave", ylab = "Gini Coefficient")

# Unemployment по волнам
plotmeans(unemployment ~ wave, FINAL_Merged_WVS_WBD_unbalanced_long,
          main = "Unemployment by Wave",
          xlab = "Wave", ylab = "Unemployment Rate")

par(mfrow = c(1, 1))

#pooled ols без учета personal_control
model1 <- feols(
  life_satisfaction ~ belief_hardwork + gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  cluster = ~ COUNTRY
)
summary(model1)


#Hausman test
fe_model1 <- plm(
  life_satisfaction ~ belief_hardwork + gdp + gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "within"
)

re_model1  <- plm(
  life_satisfaction ~ belief_hardwork + gdp + gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "random"
)

hausman_test <- phtest(fe_model1, re_model1)
print(hausman_test)

#pooled ols c учетом personal_control
model2 <- feols(
  life_satisfaction ~ belief_hardwork + personal_control + 
    gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  cluster = ~ COUNTRY
)
summary(model2)

#Hausman test
fe_model2 <- plm(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "within",
  effect = "twoways"
)

# RE модель для model2
re_model2 <- plm(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "random"
)

# Тест Хаусмана для model2
hausman_test2 <- phtest(fe_model2, re_model2)
print("Hausman Test for Model2:")
print(hausman_test2)

#Квадратичный эффект веры в тяжелый труд
model3 <- feols(
  life_satisfaction ~ belief_hardwork + I(belief_hardwork^2) + 
    personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_Merged_WVS_WBD_unbalanced_long ,
  cluster = ~ COUNTRY
)
summary(model3)

# Comparison Table for Model 3 (Quadratic Effect)
model3_comparison <- data.frame(
  Aspect = c("Observations", "Countries", "belief_hardwork (linear)", 
             "belief_hardwork² (quadratic)", "personal_control", 
             "unemployment", "Within R²", "RMSE"),
  Balanced_Data = c("150", "50", "1.019*", "-0.133***", "0.613***", 
                    "-0.028*", "0.454", "0.212"),
  Unbalanced_Data = c("115", "46", "0.677", "-0.074", "0.660***", 
                      "-0.046**", "0.436", "0.214")
)

print(model3_comparison)
# FE модель для model3
fe_model3 <- plm(
  life_satisfaction ~ belief_hardwork + I(belief_hardwork^2) + personal_control + 
    gdp + gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "within",
  effect = "twoways"
)

# RE модель для model3
re_model3 <- plm(
  life_satisfaction ~ belief_hardwork + I(belief_hardwork^2) + personal_control + 
    gdp + gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "random"
)

# Тест Хаусмана для model3
hausman_test3 <- phtest(fe_model3, re_model3)
print(hausman_test3)

#Interection term c экономическим развитием
model4 <- feols(
  life_satisfaction ~ belief_hardwork * gdp + 
    personal_control + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  cluster = ~ COUNTRY
)
summary(model4)

# FE модель для model4
fe_model4 <- plm(
  life_satisfaction ~ belief_hardwork * gdp + personal_control + 
    gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "within",
  effect = "twoways"
)

# RE модель для model4
re_model4 <- plm(
  life_satisfaction ~ belief_hardwork * gdp + personal_control + 
    gini + unemployment + health,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  model = "random"
)

# Тест Хаусмана для model4
hausman_test4 <- phtest(fe_model4, re_model4)
print(hausman_test4)

#Только фиксированные эффекты стран (Entity Fixed Effects)
model_fe_country <- feols(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health | COUNTRY,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  cluster = ~ COUNTRY
)
summary(model_fe_country)

#Двухсторонние фиксированные эффекты (Country + Time)
model_fe_twoway <- feols(
  life_satisfaction ~ belief_hardwork + gdp + personal_control + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  cluster = ~ COUNTRY
)
summary(model_fe_twoway)

model_time_fe <- feols(
  life_satisfaction ~ belief_hardwork + gdp + personal_control + gini + unemployment + health + factor(wave) | COUNTRY,
  data = FINAL_Merged_WVS_WBD_unbalanced_long,
  cluster = ~ COUNTRY
)
summary(model_time_fe)




#описательная
#Делаем длинный формат данных
Final_data_long_balanced <- FINAL_Merged_WVS_WBD_balanced %>%
  pivot_longer(
    cols = -COUNTRY,  
    names_to = c(".value", "wave"),  
    names_sep = "(?<=[a-zA-Z])(?=[0-9])"  
  ) %>%
  mutate(wave = as.numeric(wave))  

#Задаем панельную структуру данных 
p_Final_data_long_balanced <- plm::pdata.frame(Final_data_long_balanced, index = c('COUNTRY','wave'))
#Определяем количество стран в каждой волне 
table (Final_data_long_balanced$wave)
#Проверяем сбалансированность 
is.pbalanced(p_Final_data_long_balanced)
# Попробуйте установить один из этих пакетов
install.packages("panelr")
# ИЛИ
install.packages("pmd")
library(panelr)
library(modelsummary)
library(dplyr)
##Descriptive statistics 
datasummary(factor(wave)* All(p_Final_data_long_balanced)~ Mean +SD + Min + Max, data=p_Final_data_long_balanced)
datasummary(factor(wave)* (satisfaction + hardwork)~ Mean +SD + Min + Max, data=p_Final_data_long_balanced)
xtsum(p_Final_data_long_balanced, 'hardwork', id = 'COUNTRY', t = 'wave', na.rm = T, dec = 6)

#Plots
gplots::plotmeans(satisfaction ~ wave,p_Final_data_long_balanced)
gplots::plotmeans(hardwork ~ wave,p_Final_data_long_balanced)
gplots::plotmeans(control ~ wave,p_Final_data_long_balanced)



















# ПОДГОТОВКА ДАННЫХ ДЛЯ АНАЛИЗА ПОДВЫБОРОК И АЛЬТЕРНАТИВНЫХ СПЕЦИФИКАЦИЙ

# 1. СОЗДАЕМ ПЕРЕМЕННУЮ ДЛЯ РАЗВИТЫХ/РАЗВИВАЮЩИХСЯ СТРАН
FINAL_balanced_Merged_WVS_WBD_long <- FINAL_balanced_Merged_WVS_WBD_long %>%
  group_by(wave) %>%
  mutate(
    gdp_median = median(gdp, na.rm = TRUE),
    developed = ifelse(gdp > gdp_median, "developed", "developing"),
    developed_dummy = ifelse(gdp > gdp_median, 1, 0)
  ) %>%
  ungroup()

# 2. СОЗДАЕМ АЛЬТЕРНАТИВНЫЕ СПЕЦИФИКАЦИИ ДЛЯ BELIEF_HARDWORK
FINAL_balanced_Merged_WVS_WBD_long <- FINAL_balanced_Merged_WVS_WBD_long %>%
  mutate(
    # А. Дихотомическая переменная (бинарная)
    belief_binary = ifelse(belief_hardwork <= 5, "high_belief", "low_belief"),
    
    # Б. Тертили (низкая/средняя/высокая вера)
    belief_tertile = ntile(belief_hardwork, 3),
    belief_categorical = case_when(
      belief_tertile == 1 ~ "low",
      belief_tertile == 2 ~ "medium", 
      belief_tertile == 3 ~ "high"
    ),
    
    # В. Стандартизированная переменная
    belief_standardized = as.numeric(scale(belief_hardwork)),
    
    # Г. Квантили (более детальная категоризация)
    belief_quantile = ntile(belief_hardwork, 4),
    belief_quantile_cat = case_when(
      belief_quantile == 1 ~ "very_low",
      belief_quantile == 2 ~ "low",
      belief_quantile == 3 ~ "high",
      belief_quantile == 4 ~ "very_high"
    )
  )

# 3. БАЗОВАЯ МОДЕЛЬ ДЛЯ СРАВНЕНИЯ (ORIGINAL SPECIFICATION)
model_original <- feols(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)

# 4. АНАЛИЗ ПОДВЫБОРОК: РАЗВИТЫЕ VS РАЗВИВАЮЩИЕСЯ СТРАНЫ

# Модель для развитых стран
model_developed <- feols(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = filter(FINAL_balanced_Merged_WVS_WBD_long, developed == "developed"),
  cluster = ~ COUNTRY
)

# Модель для развивающихся стран  
model_developing <- feols(
  life_satisfaction ~ belief_hardwork + personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = filter(FINAL_balanced_Merged_WVS_WBD_long, developed == "developing"),
  cluster = ~ COUNTRY
)

# Модель с взаимодействием (альтернативный подход)
model_interaction <- feols(
  life_satisfaction ~ belief_hardwork * developed + personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)

# 5. АЛЬТЕРНАТИВНЫЕ СПЕЦИФИКАЦИИ BELIEF_HARDWORK

# Модель с бинарной переменной
model_binary <- feols(
  life_satisfaction ~ i(belief_binary) + personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = FINAL_balanced_Merged_WVS_WBD_long,
  cluster = ~ COUNTRY
)


# 6. ВЫВОД РЕЗУЛЬТАТОВ

# Сравнение моделей для подвыборок
cat("=== АНАЛИЗ ПОДВЫБОРОК: РАЗВИТЫЕ VS РАЗВИВАЮЩИЕСЯ СТРАНЫ ===\n")
etable(model_developed, model_developing, model_interaction,
       title = "Сравнение моделей по группам стран",
       keep = c("belief_hardwork", "developed", "personal_control", "unemployment"),
       se.below = TRUE,
       fitstat = c("n", "r2", "aic"))

# Сравнение альтернативных спецификаций
cat("\n=== АЛЬТЕРНАТИВНЫЕ СПЕЦИФИКАЦИИ BELIEF_HARDWORK ===\n")
etable(model_original, model_binary, 
       title = "Сравнение альтернативных спецификаций веры в труд",
       keep = c("belief", "personal_control", "unemployment"),
       se.below = TRUE,
       fitstat = c("n", "r2", "aic"))



# 7. ДОПОЛНИТЕЛЬНАЯ ВИЗУАЛИЗАЦИЯ

# График для анализа подвыборок
library(ggplot2)

ggplot(FINAL_balanced_Merged_WVS_WBD_long, aes(x = belief_hardwork, y = life_satisfaction, color = developed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "The relationship between faith in work and life satisfaction by country group",
       x = "Faith in hard work", y = "Life satisfaction") +
  theme_minimal()

#self-control
ggplot(FINAL_balanced_Merged_WVS_WBD_long, aes(x = personal_control, y = life_satisfaction, color = developed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "The relationship between believe in personal control and life satisfaction by country group",
       x = "Faith in personal control", y = "Life satisfaction") +
  theme_minimal()


# 8. СТАТИСТИЧЕСКИЕ ТЕСТЫ РАЗЛИЧИЙ

# Тест равенства коэффициентов между группами стран
cat("\n=== ТЕСТЫ СТАТИСТИЧЕСКИХ РАЗЛИЧИЙ ===\n")

# Создаем данные для комбинированной модели
data_combined <- FINAL_balanced_Merged_WVS_WBD_long %>%
  mutate(developed_num = ifelse(developed == "developed", 1, 0))

# Модель с взаимодействиями для тестирования
model_test <- feols(
  life_satisfaction ~ belief_hardwork * developed_num + personal_control + gdp + gini + unemployment + health | COUNTRY + wave,
  data = data_combined,
  cluster = ~ COUNTRY
)

# Выводим тест взаимодействия
cat("Тест взаимодействия belief_hardwork × developed:\n")
print(coeftest(model_test)["belief_hardwork:developed_num",])

# Альтернативный способ - смотрим всю таблицу коэффициентов
cat("\nПолная таблица коэффициентов модели с взаимодействием:\n")
etable(model_test, keep = c("belief_hardwork", "developed_num", "belief_hardwork:developed_num"))