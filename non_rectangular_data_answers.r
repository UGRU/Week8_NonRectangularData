
multivac = 'asimov_the_last_question.txt'
# this is a file with the text from a short story by Isaac Asimov:
# 'The last question'

csv_try = read.csv(multivac)
#it gets confused because we told it to read a csv, but there are just random
#commas interspersed throughout the lines...
csv_try

lines = readLines(multivac)

#note:
#library('tidyverse')
#lines = read_lines(multivac)
#is also a viable play

summary(lines)
is.vector(lines)

lines[1:2]

lines[length(lines)] 

#Questions to answer:

#####################
# 1. how many lines of dialogue are there?:
# hint: look at lines 27, 29, 36, 90
# efficiency hint: can you write a generic function to answer this and question 2?

dialouge = 0
for(i in 1:length(lines)){
	if(grepl("\"" , lines[i])){
		dialouge = dialouge + 1	
	}
}

dialouge

#####
# OR - this method is GENERIC and therefore more efficient
#####
IsIn = function(subject, query){
	# == index position
	return(	grepl(subject , query))
}

dialouge_list = lapply(lines, function(x){IsIn("\"", x)})

length(dialouge_list[dialouge_list == TRUE])


#####################
# 2. a. what is the first question in the text? 

# The advantage of the second method for question 1 is that we get to reuse the funciton.
# Looking for the last question is the same pattern as looking for dialouge

question_bool = lapply(lines, function(x){IsIn("\\?", x)})

question_list = lines[question_bool==TRUE]

question_list[1]

# b. what is the last question?

question_list[length(question_list)]



#####################
# 3. build a dataframe with the following columns (and data types)

#Line	is_dialogue	is_question	word_count	text
#int	Bool		Bool		int			string

WordCount = function(str){
	words = strsplit(str, "\\s+")[[1]]
	return(length(words))#int
}

wc = lapply(lines, function(x){WordCount(x)})
unlist(wc)


# limitations of R
# we cant have a function that makes two outputs... 
# which is a pretty standard thing :(

# in a perfect world this would work:

# WordCount = function(str){
#	words = strsplit(str, "\\s+")[[1]]
#	return(len(words), words)#int
# }

# wc_list, words = lapply(lines, function(x){WordCount(x)})



question_df = data.frame(line= 1:length(lines),
						is_dialouge = unlist(dialouge_list),
						is_question = unlist(question_bool),
						word_count = unlist(wc),
						text = lines)

head(question_df)


#####################
# 4. finally answer the following:
# The Hemingway-Kafka index is a measure of how long an author's sentences are. 
# It is a simply the average number of words in a sentence from a given text.
	# For the purpose of today's exercise, lines of dialogue are counted as a single sentence.

# a. What is the HK-index for 'The Last Question'?
mean(question_df$word_count)

# 24 years after writing 'The Last Question', our boy Isaac wrote another short story
# titled 'The Last Answer' which is found in the following text file:
last_answer = readLines('asimov_the_last_answer.txt')

ans_dialouge_list = lapply(last_answer, function(x){IsIn("\"", x)})
ans_question_bool = lapply(last_answer, function(x){IsIn("\\?", x)})
ans_wc = lapply(last_answer, function(x){WordCount(x)})


answer_df = data.frame(line= 1:length(last_answer),
						is_dialouge = unlist(ans_dialouge_list),
						is_question = unlist(ans_question_bool),
						word_count = unlist(ans_wc),
						text = last_answer)

mean(answer_df$word_count)


# Given the HK-index of the two texts, is there statistical evidence of Isaac Asimov getting more long winded with age?
head(question_df)
head(answer_df)

hk_test = t.test(question_df$word_count, answer_df$word_count,
					alternative="less", var.equal=TRUE)

hk_test
#looks like he did get more long winded with age!



#closing remarks on good practices for using functions:

#move them all to the top of the file
#include a brief comment that describes the input and output and what the function accomplishes

# if you're working across multiple files, you don't need to copy and paste functions over
# use the source function to import functions from other script files!
source('./path_to_file/import_test.r')
?source
#then any functions defined in 'import_test.r' would be avaliable in the current script



##Karls tidyverse solution #####

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
