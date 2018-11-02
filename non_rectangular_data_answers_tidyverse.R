library('tidyverse')

multivac = 'asimov_the_last_question.txt'
lines = readLines(multivac)

#####################
# 3. build a dataframe with the following columns (and data types)

#Line	is_dialogue	is_question	word_count	text
#int	Bool		Bool		int			string

# alternative version to Cam's code
# using vectorized functions more directly

data.frame(is.dialogue = grepl("\"", lines),
           is.question = grepl("\\?", lines),
           word.count = sapply(strsplit(lines, "\\ "), length),
           text = lines)


last_answer = readLines('asimov_the_last_answer.txt')
is.vector(last_answer)

# to read in the second file, copy-paste the code
# and replace all the "lines" with "last_answer"
# And I added the summarize statement as an example to find the mean

data.frame(is.dialogue = grepl("\"", last_answer),
           is.question = grepl("\\?", last_answer),
           word.count = sapply(strsplit(last_answer, "\\ "), length),
           text = last_answer) %>% 
  summarise_all(mean)

# The above code is a mix of base and tidyverse code
# I converted it to a more tidyverse-style of coding

lines %>% 
  as.tibble() %>% 
  mutate(is.dialogue = grepl("\"", value), 
         # the as.tibble statement automatically creates the value column name
         is.question = grepl("\\?", value),
         word.count = sapply(strsplit(value, "\\ "), length)) %>% 
  summarise_if(is.numeric, mean) # summarise_if does it only for numeric variables
  
# The advantage of the tidyverse approach:
# If you want to repeat the analysis with the second data set
# you only have to replace the "lines" with "last_answer" once
# thus easier to re-use code sections

last_answer %>% 
  as.tibble() %>% 
  mutate(is.dialogue = grepl("\"", value),
         is.question = grepl("\\?", value),
         word.count = sapply(strsplit(value, "\\ "), length)) %>% 
  summarise_if(is.numeric, mean)
