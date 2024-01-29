library(tidyverse)

raw <- read_csv('https://www.dropbox.com/scl/fi/ug8tbxsdd2qtsfqwnnox1/dollar_store.csv?rlkey=fu36g6uhfpx8u644d1rpsq11i&dl=1')

ds <- janitor::clean_names(raw)

#to know
  #str_detect()
#str_remove()
#str_replace()
#str_extract()

ds %>% 
  select(unit_size) %>% 
  mutate(unit_size_c = parse_number(unit_size)) %>% 
  #filter(str_detect(unit_size, 'each'))
  mutate(unit_type = if_else(str_detect(unit_size, 'each'), 'each',
                                  if_else(str_detect(unit_size, 'ounce'), 'ounce', 'nothing'))) %>% 
  filter(unit_type == 'nothing')


ds %>% 
  select(unit_size) %>% 
  mutate(unit_size_c = parse_number(unit_size)) %>% 
  mutate(unit_type = str_extract(unit_size, 'each|ounce|pound'))

ds %>% 
  select(unit_size) %>% 
  mutate(unit_size_c = parse_number(unit_size)) %>% 
  mutate(unit_type = str_extract(unit_size, '[A-Za-z].*')) %>% 
  count(unit_type)

ds %>% 
  select(product_info) %>% 
  mutate(product_info_c = str_remove_all(product_info, 'Brand:|Product:')) %>% 
  mutate(product_info_c = str_remove(product_info, 'Fl. Oz.'))
  
ds %>% 
  select(description) %>% 
  mutate(description = str_remove_all(description, '\\[|\\]')) %>% 
  mutate(description = na_if(description, ''))
 

# Convert to number formats: price, star_rating, review_count, stock_status, unit_size
#   Goals:   (1) Don't lose information
#            (2) Missing where appropriate, but only where appropriate



# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column

separate_wider_delim()

ds %>% 
  select(product_info) %>% 
  separate_wider_delim(product_info,
                       delim = ' - ',
                       names = c('brand','product'),
                       too_many = 'merge')

# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Convert date columns to date and/or date-time columns, then calculate how many
# days each product sat on the shelf before its first sale.

library(lubridate)


ds %>% 
  select(date_added, first_sold_day, first_sold_time) %>% 
  mutate(date_added_c = dmy(date_added)) %>% 
  mutate(new_date_c = str_c(first_sold_day,first_sold_time, sep = '~$#!')) %>% 
  # mutate(new_date_c = str_remove(new_date_c, '@')) %>%     <----- not necessary, lubridate is very smart
  mutate(first_dt = mdy_hms(new_date_c))




