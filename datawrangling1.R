setwd(~/Springboard/data wrangling1)## Setworking Directory
library(dplyr)
library(tidyr)
library(stringr)
library(caret)
refd<- read.csv("~/Springboard/data wrangling1/refine_original.csv")
str(refd)##Read file structure
## Clean up brand names
refd$company = str_to_lower(refd$company)
glimpse(refd)
## Correct misspellings of the brand names to standardized 
refd$company <- str_replace(refd$company,"phillips|phllips|phillps|phlips|fillips","philips")
refd$company <- str_replace(refd$company,"k\\sz","kz")
refd$company <- str_replace(refd$company,"z[0-9]","zo")
refd$company <- str_replace(refd$company,"unilever|unilver","unilever")
View(refd)

## Separate product code and number
refd <- refd %>%
  separate(Product.code...number, c( "product_code" , "product_number"), sep = '-', remove = TRUE) 
glimpse(refd$product_code)

## Add product categories (p = Smartphone, v = TV, x = Laptop, q = Tablet)
make_product_category <-function(productCode){
  if( productCode == "p"){
    return("SmartPhone")
  }else if(productCode == "v"){
    return("TV")
  }
  else if(productCode == "x"){
    return("Laptop")
  }else if(productCode == "q"){
    return("Tablet")
  }
}
refd <- refd %>% 
  mutate( product_category = sapply(product_code, make_product_category)  )
glimpse(refd)

## Geocode Cities
refd <- refd %>% 
  mutate( full_address = paste(address ,city, country, sep= ", " ))
glimpse(refd)

## Create dummy variables (with caret)for company and product category columns
dmy <-dummyVars((~company + ~product_category), data = refd, fullRank=T)
#Use spread command to add 1 and 0
trsf <- data.frame(predict(dmy, newdata = refd))
print(trsf)
#Keeping copies of original columns for verification
refd1 <-c(refd,trsf) 
View(refd1)
##Write refd1 to refine_clean.csv
write.csv(refd1, file = "~/Springboard/data wrangling1/refine_clean_jk.csv", row.names = FALSE)

     