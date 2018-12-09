library(tidyverse)
library(stringr)

readRDS("austen_word_freqs.Rds")


# Question 1 ------------------------------------------------------------------------------------------------------


tidy_df <- function(data, column_prefix = "var"){
  
}
##I'm not sure how to adapt to this given function, but from what I know from 
##tidying chapter, this is how I would do the conversion, using gather()

tidy_df <- gather(data, column_prefix = "var", value = "value", var1:var3, na.rm = TRUE)

# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

# extract_possible_names 

tb <- tibble(
  name = str_extract_all(austen_text, "\\b[A-Z]\\w+")[[2]]
)

#add unique ID
tb$ID <- seq.int(nrow(tb))


##note: for this question, I couldn't figure out how to extract
##the names with the text ID column - how do I do this???



# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names

tb2 <- tb %>% 
  mutate(name = tolower(name)) %>%
  group_by(name) %>%
  summarize(count = n())

colnames(tb2)[2] <- "count_cap"

tb3 <-
  tb2 %>%
  left_join(austen_word_freqs, by = c("name" = "word")) %>%
  mutate(frac = count_cap/count) %>% 
  filter(frac > 0.75)

# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book

##note: so here I came across the same problem as question 1 - that I couldn't figure out how to 
## extract the unique names withe its book (title) identifier column
## but to demonstrate my effort, let's assume that I was able to produce a dataframe (df) with
##three columns - (1)title (2)name (3)occurrences. I'd use the following codes to answer your questions

##to answer which book contain highest number of unique names

df1 <- df %>%
  aggregate(df$name, list(df$book), paste, collapse=",") 

df1 %>% 
  separate_rows(name) %>% 
  filter(name != '') %>% 
  group_by(title) %>% 
  summarise(number = n_distinct(name)) %>% 
  left_join(df, .) %>%
  arrange(desc(number))

##to answer which book contains the most occurences of names

df2 <- df %>% 
  group_by(title) %>% 
  summarise(occurrences = n_distinct(occurrences)) %>% 
  arrange(desc(occurrences))

##not sure if this would work, but it would be my best theoretical guess!:)