
# Question 1 &2 --------------------------------------------------------------

library(tidyverse)

ls(forbes)

forbes <- forbes %>%
  mutate(
    net_worth_number = parse_number(net_worth),
    rank_number = parse_number(rank)) %>%
  filter(net_worth_number < 100) %>%
 

# Question 3 --------------------------------------------------------------
##Answer: definitely we're seeing clearer pattern in the second plot. These seems to be an almost curvilinear relationship between age and net worth.
##Note: I tried to add the geom_smooth function but not sure why I got the error message!

forbes %>%
  filter(!is.na(age)) %>%
  ggplot(aes(x = age, y = net_worth_number)) +
  geom_point()
  
forbes %>%
  filter(!is.na(age)) %>%
  ggplot(aes(x = age, y = log(net_worth_number))) +
  geom_point()


# Question 4 --------------------------------------------------------------

networth_difference <- forbes %>%
  group_by(country) %>%
  mutate(difference = max(net_worth_number) - min(net_worth_number)) %>%
  arrange(desc(-difference))


##Note: I thought of adding the following codes for filtering countries with less than 6 people out - but I only ended up with the two variables - why?
  summarise(count = n()) %>%
  filter(count >= 6) 


# Question 5 --------------------------------------------------------------

ggplot(networth_difference, aes(x=country,y = difference)) + 
  geom_bar(stat = "identity") +
  xlab('Country') +
  ylab('Net worth difference ') +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
    
##Note: I'm not sure why there's an outlier in the graph, I couldn't see that in the table. 
  

# Question 6 --------------------------------------------------------------

ggplot(networth_difference, aes(x = reorder(country, -difference), y = difference)) + 
  geom_bar(stat = "identity") +
  xlab('Country') +
  ylab('Net worth difference ') +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))

# Question 7 --------------------------------------------------------------
##Answer: Yes! The ranks that are repeated can be seen in the table.
  
forbes %>% 
  group_by(rank_number) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  View
  

# Question 8 --------------------------------------------------------------
##Note:I'm at my wit's end on this one. The best guess is the following, but obviously it didn't achieve the task:-)
  
new_ranking <- forbes %>%
  group_by(country) %>%
  summarise(avg_rank = mean(rank_number)) 
  

##Note:(1) Just a general question of how to delete the dataframe?
##(2): obviously, I still have a long way to go before mastering R. I look forward to your feedback!
             