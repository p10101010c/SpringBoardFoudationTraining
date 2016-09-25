## --------------
#
#Author : Naga Bandarupalli
#Date   : 2016-09-22
#Purpose: Spring Board Data Science - Data Wrangling Exercise
#
## --------------

library("dplyr")
library("tidyr")

toys_original <- read.table(file = "Data/refine_original.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
toys <- tbl_df(toys_original)
toys$company <- tolower(toys$company)
toys %>% distinct(company)
toys$company[grepl("(^[p|f])*(s$)", toys$company, ignore.case = TRUE)] <- "philips"
toys$company[grepl("(^a)*([o|0]$)", toys$company, ignore.case = TRUE)] <- "akzo"
toys$company[grepl("(^v)*(n$)", toys$company, ignore.case = TRUE)] <- "van houten"
toys$company[grepl("(^u)*(r$)", toys$company, ignore.case = TRUE)] <- "unilever"
product_split <- strsplit(toys$Product.code...number, "-")

toys <- mutate(toys, product_code = tolower(unlist(lapply(product_split,`[[`,1))), 
                        product_number = tolower(unlist(lapply(product_split,`[[`,2))))

toys <- toys %>% rowwise() %>% mutate(product_category = (  if(product_code == 'p'){'Smartphone'} 
                                                    else if(product_code == 'v'){'TV'} 
                                                    else if(product_code == 'x'){'Laptop'} 
                                                    else if(product_code == 'q'){'Tablet'} 
                                                    else{''}
                                                )
                              )
toys
