## --------------
#
#Author : Naga Bandarupalli
#Date   : 2016-09-22
#Purpose: Spring Board Data Science - Data Wrangling Exercise
#
## --------------

#Loadingrequired libraries
library("dplyr")
library("tidyr")

#read original data set
toys_original <- read.table(file = "Data/refine_original.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#convert to dplyr table data frame
toys <- tbl_df(toys_original)

#below statement used to derive regex patterns
# toys %>% distinct(company)

#use regular expression to clean company names and standardize them
toys$company[grepl("(^[p|f])*(s$)", toys$company, ignore.case = TRUE)] <- "philips"
toys$company[grepl("(^a)*([o|0]$)", toys$company, ignore.case = TRUE)] <- "akzo"
toys$company[grepl("(^v)*(n$)", toys$company, ignore.case = TRUE)] <- "van houten"
toys$company[grepl("(^u)*(r$)", toys$company, ignore.case = TRUE)] <- "unilever"

#split the product code.. number column into product_code and product_number
product_split <- strsplit(toys$Product.code...number, "-")

#add product_code, product_number to toys
toys <- mutate(toys, product_code = tolower(unlist(lapply(product_split,`[[`,1))), 
                        product_number = tolower(unlist(lapply(product_split,`[[`,2))))

#add product category column
toys <- toys %>% rowwise() %>% mutate(product_category = (  if(product_code == 'p'){'Smartphone'} 
                                                    else if(product_code == 'v'){'TV'} 
                                                    else if(product_code == 'x'){'Laptop'} 
                                                    else if(product_code == 'q'){'Tablet'} 
                                                    else{''}
                                                )
                              )

#add full address column by combining address, city, country 
toys <- toys %>% rowwise() %>% mutate(full_address = paste(address,",",city,",",country))

#add biary columns for company
toys <- toys %>% rowwise() %>% mutate(company_philips = ifelse(company == 'philips',1,0),
                                      company_akzo = ifelse(company == 'akzo',1,0),
                                      company_van_houten = ifelse(company == 'van houten',1,0),
                                      company_unilever = ifelse(company == 'unilever',1,0)
                                      )

#add biary columns for product type
toys <- toys %>% rowwise() %>% mutate(product_smartphone = ifelse(product_code == 'p',1,0),
                                      product_tv = ifelse(product_code == 'v',1,0),
                                      product_laptop= ifelse(product_code == 'x',1,0),
                                      product_tablet = ifelse(product_code == 'q',1,0)
)

#write the updated data set into seperate file after cleaning
write.csv(toys, "Data/refine_clean.csv", row.names = FALSE)