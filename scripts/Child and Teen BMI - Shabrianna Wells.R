
library(vegan)
library(ggplot2)
library(maps)
library(sf)
library(raster)
library(leaflet)
library(dplyr)
library(tidyr)


child_BMI_data <- read.csv('./data/Child_Adolescent_BMI.csv')


#' Ages represented: 5-19
#' Years represented: 1990-2020
#' Sexes represented: Boy and girl
#' Land use represented: urban and rural
#' Number of countries/territories represented: ~200
#' Income groups represented: Low, lower-middle, upper-middle, high


#' Change "C“te d'Ivoire" to Cote d'Ivoire?

child_BMI_data <- child_BMI_data %>% 
  mutate(Country = if_else(Country== "C“te d'Ivoire", "Cote d'Ivoire", Country))


#' Change names of columns

names(child_BMI_data) <- c('country', 'sex', 'year', 'age', 'BMI_urban', 'BMI_rural')

head(child_BMI_data)

#' names look good but need to convert to long format to 
#' have a single BMI column
?pivot_longer
dat <- pivot_longer(child_BMI_data, cols = starts_with('BMI'),
                    names_to='landuse')
head(dat)
names(dat) <- c('country', 'sex', 'year', 'age', 'landuse', 'BMI')
dat$landuse <- sub(x = dat$landuse, pattern = 'BMI_', replacement = '')
head(dat)


#' New column for income groups

dat <- dat %>%
  mutate(incomegroup = case_when(
    country %in% c("Afghanistan", "Burundi",
                   "Burkina Faso", "Central African Republic", "DR Congo",
                   "Eritrea", "Ethiopia", "Gambia", "Guinea Bissau", "Liberia",
                   "Madagascar", "Mali", "Mozambique", "Malawi", "Niger",
                   "North Korea", "Rwanada", "Sudan", "Sierra Leone", "Somalia",
                   "South Sudan", "Syrian Arab Republic", "Chad", "Togo",
                   "Uganda", "Yemen") ~ "Low",
    country %in% c("Angola", "Benin", "Bangladesh", "Bolivia", "Bhutan", "Cote d’Ivoire", "Cameroon", 
                   "Congo", "Comoros", "Cabo Verde", "Djibouti", "Algeria", "Egypt", "Micronesia (Federated States of)",
                   "Ghana", "Guinea", "Honduras", "Haiti", "India", "Iran", "Jordan", "Kenya", "Kyrgyzstan",
                   "Cambodia", "Kiribati", "Lao PDR", "Lebanon", "Sri Lanka", "Lesotho", "Morocco", 
                   "Myanmar", "Mongolia", "Mauritania", "Nigeria", "Nicaragua", "Nepal", "Pakistan", 
                   "Papua New Guinea", "Senegal", "Solomon Islands", "Sao Tome and Principe", 
                   "Tajikistan", "Timor-Leste", "Tunisia", "Tanzania", "Ukraine", "Uzbekistan", 
                   "Vietnam", "Eswatini", "Philippines", "Vanuatu", "Samoa", "Zambia", "Zimbabwe"
                   ) ~ "Lower-Middle",
    country %in% c("Albania", "Argentina", "Armenia", "Azerbaijan", "Bulgaria", 
                   "Bosnia and Herzegovina", "Cuba", "Dominica", "Grenada", 
                   "Belarus", "Belize", "Brazil", "Botswana", "China", "Colombia", "Costa Rica", 
                   "Dominican Republic", "Ecuador", "Fiji", "Gabon", "Georgia", "Equatorial Guinea", 
                   "Guatemala", "Indonesia", "Iraq", "Jamaica", "Kazakhstan", "Libya",
                   "St. Lucia", "Moldova", "Mauritius", "Venezuela","Occupied Palestinian Territory", 
                   "Maldives", "Mexico", "Marshall Islands", "North Macedonia", "Montenegro",
                   "Malaysia", "Namibia", "Peru", "Palau", "Paraguay", 
                   "Russian Federation", "El Salvador", "Serbia", "Suriname", "Thailand", "Turkmenistan", 
                   "Tonga", "Turkey", "Tuvalu", "St. Vincent and the Grenadines", "South Africa"
                   ) ~ "Upper-Middle",
    country %in% c("Aruba", "Andorra", "United Arab Emirates", "American Samoa", "Antigua and Barbuda", 
                   "Australia", "Austria", "Belgium", "Bahrain", "Bahamas", "Bermuda", "Barbados", 
                   "Brunei Darussalam", "Canada", "Switzerland", "Chile", "Cyprus", "Czechia", 
                   "Germany", "Greenland", "Italy", "Japan", "Latvia", "Poland", 
                   "Denmark", "Spain", "Estonia", "Finland", "France", "United Kingdom", "Greece", 
                   "Guam", "Guyana", "Croatia", "Hungary", "Ireland", "Iceland", "Israel", 
                   "St. Kitts and Nevis", "South Korea", "Kuwait", "Lithuania", "Luxembourg", 
                   "Malta", "Netherlands", "Norway", "Nauru", "New Zealand", "Oman", "Panama", 
                   "Puerto Rico", "Portugal", "French Polynesia", "Qatar", "Romania", "Saudi Arabia", 
                   "Singapore", "Slovakia", "Slovenia", "Sweden", "Seychelles", "Trinidad and Tobago", 
                   "Taiwan", "Uruguay", "United States of America") ~ "High",
    TRUE ~ NA_character_  
  ))






#' Shape file
regions <- st_read("data/world-administrative-boundaries.shp")
ggplot() + 
  geom_sf(data = regions, size = 1.5, color = "black", fill = "cyan1") +
  ggtitle("Geopolitical Regions") + 
  coord_sf()

dat <- merge(dat, regions, by.x = 'country', by.y = 'name',
             all.x = TRUE, all.y = FALSE)

#' List of regions
world_r <- unique(dat$region)
world_r

#' List of countries/territories
world_c <- unique(dat$country)
world_c

#' Box plots for data

boxplot(BMI ~ year, data = dat)
ggplot(dat, aes(year, BMI, group = year)) +
  geom_boxplot()
#' The median BMI increases each year

boxplot(BMI ~ sex, data = dat)
ggplot(dat, aes(sex, BMI, group = sex)) +
  geom_boxplot()
#' The median BMI for girls looks larger than BMI for boys

boxplot(BMI ~ age, data = dat)
ggplot(dat, aes(age, BMI, group = age)) +
  geom_boxplot()
#' The median BMI increases with age

boxplot(BMI ~ landuse, data = dat) 
ggplot(dat, aes(landuse, BMI, group = landuse)) +
  geom_boxplot()
#' The median BMI for urban looks larger than BMI for rural

boxplot(BMI ~ incomegroup, data = dat) 
ggplot(dat, aes(age, incomegroup, group = incomegroup)) +
  geom_boxplot()
#' The median BMI for high income countries is larger than the median BMI for low incomes countries

boxplot(BMI ~ country, data = dat) 
ggplot(dat, aes(country, BMI, group = country)) +
  geom_boxplot()
#' Since there are over 100 countries used in this data set, it's difficult to compare all the box plots



#' Linear Models
#' Country, Region, and Income group are all based on info from the first column
#' so I will analyze these variables separately

#' Linear model 1
mod_country <- lm (BMI ~ country + year + sex + age + landuse,
            data = dat)
car::Anova(mod_country, type =3)
summary(mod_country)
#' For mod_country, age and year have significant differences (largest F values). 
#' Country is the least important variable (smallest F value).
#' According to the anova, the intercept F value is 106784.4.
#' F value for country = 6720.6
#' F value for year = 139751.7
#' F value for sex =  25547.2
#' F value for age = 4720319.1
#' F value for landuse = 26889.4

termplot(mod_country, se = TRUE, residuals = TRUE)
plot(mod_country)
#' For Residuals vs. Fitted the points are equally spread out.
#' This indicates a linear relationship.
#' For Q-Q Residuals, the points generally follow the reference line.
#' The indicates a normal distribution.
#' For Scale Location the points generally follow the horizontal line.
#' This indicates that there is homoscedasticity.
#' For Residuals vs Leverage, most of the points are contained on one side of the plot.
#' This indicated that there are few, if any, outliers for this model.


#'Linear model 2
mod_region <- lm (BMI ~ region + year + sex + age + landuse,
            data = dat)
car::Anova(mod_region, type =3)
summary(mod_region)
#' For mod_region, age and year have significant differences. 
#' Sex is the least important variable.
#' According to the anova, the intercept F value is 52355.
#' F value for region = 27071
#' F value for year =  72428
#' F value for sex =  13129
#' F value for age = 13486
#' F value for landuse = 13486

termplot(mod_region, se = TRUE, residuals = TRUE)
plot(mod_region)
#' For Residuals vs. Fitted, the points are equally spread out.
#' This indicates a linear relationship.
#' For Q-Q Residuals, the points generally follow the reference line.
#' The indicates a normal distribution.
#' For Scale Location, the points generally follow the horizontal line.
#' This indicates that there is homoscedasticity.
#' For Residuals vs Leverage, there are a lot of point towards the right of the plot.
#' This indicates that this model contains a lot of outliers.


#'Linear model 3
mod_income <- lm(BMI ~ incomegroup + year + sex + age + landuse, data = dat)
car::Anova(mod_income, type=3)
summary(mod_income)
#' For mod_income; age, year, and income group have significant differences. 
#' Sex is the least important variable.
#' According to the anova, the intercept F value is 31007.3.
#' F value for income group = 39499.6
#' F value for year =  42593.7
#' F value for sex =  8061.2
#' F value for age = 1483996.0
#' F value for landuse = 8270.2

termplot(mod_income, se = TRUE, residuals = TRUE)
plot(mod_income)
#' For Residuals vs. Fitted, the points are equally spread out.
#' This indicates a linear relationship.
#' For Q-Q Residuals, the points start to deviate from the reference line around x=1.
#' This indicates a skewed distribution. 
#' For Scale Location,the points generally follow the horizontal line.
#' This indicates that there is homoscedasticity.
#' For Residuals vs Leverage, most of the points are contained on one side of the plot.
#' This indicated that there are few, if any, outliers for this model.


#' So the most important variables in this data set are age, year, and income group



#' Box plots based on significant variables

boxplot(BMI ~ year + incomegroup, data = dat)
abline(h=18, col= "red")
#' The first cluster of box plots represents BMI by year among high income countries
#' The second cluster of box plots represents BMI by year among low income countries
#' The third cluster of box plots represents BMI by year among lower-middle income countries
#' The fourth cluster of box plots represents BMI by year among upper-middle income countries
#' It looks like high income countries have the highest median BMI within a 30-year period
#' It looks like upper-middle income countries have the lowest median BMI within a 30-year period

boxplot(BMI ~ age + incomegroup, data = dat)
abline(h=20, col= "red")
#' The first cluster of box plots represents BMI by age among high income countries
#' The second cluster of box plots represents BMI by age among low income countries
#' The third cluster of box plots represents BMI by age among lower-middle income countries
#' The fourth cluster of box plots represents BMI by age among upper-middle income countries
#' It looks like high income countries have the highest median BMI among all ages
#' It looks like low income countries have the lowest median BMI among all ages


boxplot(BMI ~ age + year, data = dat)
abline(h=20, col= "red")
#' It looks like median BMI increases every year
#' It's difficult to see the box plots ordered by age


