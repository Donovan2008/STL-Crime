
#############################3#######################
##  Select Carjack records
numbers.cjack <- stl.city.Group2018 %>%
    select(Complaint, CodedMonth, DateOccur, Crime, 
           Description, Neighborhood, 
           CADAddress, CADStreet) %>%
  filter(Crime == 38111) 
numbers.cjack %>% 
  head()
########################################################

## Group by month and count
numbers.cjack %>% 
  group_by(CodedMonth) %>%
  count(CodedMonth) %>%
  view()
###################################
## Plot the count by month
numbers.cjack %>% 
  group_by(CodedMonth) %>%
  count(CodedMonth) %>% 
  ggplot(aes(x = CodedMonth, y = n)) +
  geom_col(color = "cornflowerblue") +
  theme(axis.text.x = element_text(angle = 90)) # change tex to verticle
#################### (need to change legends)
str(numbers.cjack)

###########################################
## Group by Neighborhood and count
numbers.cjack %>% 
  group_by(Neighborhood) %>%
  count(Neighborhood) %>%
  view()
#################################################



## Homicide
#############################3#######################
##  Select Homicide records
numbers.homicide <- stl.city.Group2018 %>%
  select(Complaint, CodedMonth, DateOccur, Crime, 
         Description, Neighborhood, 
         CADAddress, CADStreet) %>%
  filter(Crime == 10000) 
numbers.homicide %>% 
  head()
########################################################

## Group by month and count
numbers.homicide %>% 
  group_by(CodedMonth) %>%
  count(CodedMonth) %>%
  view()
###################################
## Plot the count by month
numbers.homicide %>% 
  group_by(CodedMonth) %>%
  count(CodedMonth) %>% 
  ggplot(aes(x = CodedMonth, y = n)) +
  geom_col(color = "cornflowerblue") +
  theme(axis.text.x = element_text(angle = 90)) # change tex to verticle
#################### (need to change legends)
str(numbers.homicide)

###########################################
## Group by Neighborhood and count
numbers.homicide %>% 
  group_by(Neighborhood) %>%
  count(Neighborhood) %>%
  view()
#################################################











# get neighborhood
company_ipo_sector %>% 
  group_by(ipo.year) %>% 
  filter(ipo.year > 2003 & !is.na(sector)) %>%
  mutate(sector = str_remove(sector, "Consumer")) %>% 
  count(sector) %>% 
  ggplot(aes(x = sector, y = n, fill = sector)) +
  geom_col() +
  facet_wrap(~ipo.year) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "")



