# Project: Data Wrangling Exercise 1: Basic Data Manipulation

# Load libraries and setting work directory
library(tidyr)
library(dplyr)
setwd("/Users/Tortosae/Desktop/Data science course")


# 0: Load the data in RStudio
refine <- read.csv(file="refine_original.csv",header=TRUE)
levels(refine$company)
View(refine)

#1: Clean up brand names
#Transform the values in the column to be: 
#philips, akzo, van houten and unilever (all lowercase)
levels(refine$company) <-  list (philips = c("fillips", "phillips", "phillipS", "Phillips", "phillps", "phlips", "phllips"),
                                akzo = c("ak zo", "akz0", "Akzo"),
                                van_houten = c("van Houten", "Van Houten"),
                                unilever = c("Unilever","unilver"))
levels(refine$company)

#2: Separate product code and number
#Add two new columns called product_code and product_number.
refine <-refine %>%
  separate(Product.code...number, c("product_code", "product_number"))
levels(refine$company)

#3: Add product categories
#Add a column with the product category for each record.
refine <- refine %>% mutate(product_category = case_when(product_code == "p" ~"Smartphone",
                                                         product_code == "v" ~ "TV",
                                                         product_code == "x" ~ "Laptop",
                                                         product_code == "q" ~ "Tablet"))

#4: Add full address for geocoding
#Create a new column full_address that concatenates the three address fields (address, city, country), separated by commas
refine <-refine %>% 
         mutate(full_address = paste(address, city, country, sep = ', '))


#5: Create dummy variables for company and product category
#Create dummy binary variables for each of them with the prefix company_ and product_.
#Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever.
#Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet.
#install.packages("fastDummies")

#library(fastDummies)
#refine <- dummy_cols(refine, select_columns = "company")
#refine <- dummy_cols(refine, select_columns = "product_category")

refine <- refine %>% 
  mutate(company_philips = ifelse(company == "philips", 1, 0)) %>% 
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>% 
  mutate(company_van_houten = ifelse(company == "van_houten", 1, 0)) %>% 
  mutate(company_unilever = ifelse(company == "unilever", 1, 0)) %>% 
  mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>%
  mutate(product_tv = ifelse(product_category == "TV", 1, 0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>%
  mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0)) 

#6: Submit the project on Github
write.table(refine, file = "refine_clean.csv", sep = ",")
