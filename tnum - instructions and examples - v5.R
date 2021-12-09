## ----setup, echo=FALSE, warning=FALSE, message=FALSE-------------------------------------------------

library(knitr)
library(kableExtra)
library(magrittr)
library(gutenbergr)

library(tidyverse)
library(tnum)



## ----server_test, echo=FALSE, results='asis'---------------------------------------------------------

cat(system("ping mssp1.bu.edu", intern = TRUE), sep="<br>") %>%  kable()



## ----echo=TRUE---------------------------------------------------------------------------------------

tnum.authorize("mssp1.bu.edu")



## ----eval=FALSE--------------------------------------------------------------------------------------
## 
## tnum.getDBPathList(taxonomy = "subject", levels=1)
## 


## ----austen_example, echo=TRUE, warning=FALSE, message=FALSE-----------------------------------------

 q111 <- tnum.query(query = "austen# has ordinal", max=500)   ## everything
 df111 <- tnum.objectsToDf(q111)

## show ordered objects in document
q112 <- tnum.query("austen# has ordinal")   ## show ordered objects in document
df112 <- tnum.objectsToDf(q112)

## focus on one paragraph -- note the word count for each sentence
q3 <- tnum.query("austen/persuasion/chapter-1/paragraph-7# has count#")  # just 1 para
df3 <- tnum.objectsToDf(q3)
df3


## and now look at the text in a sentence
q1 <- tnum.query("austen/persuasion/chapter-1/paragraph-7/sentence-3# has text")
df1 <- tnum.objectsToDf(q1)
df1

## To extract a paragraph of text
q4 <- tnum.query("austen/persuasion/chapter-1/paragraph-7/sentence# has text", max = 15)
df4 <- tnum.objectsToDf(q4)
para_text4 <- df4 %>% pull(string.value) %>% 
                      str_replace_all("\"","") %>% 
                      str_flatten(collapse = " ")




## steps to understand
# a <- para_text4[4]
# a
# 
# b <- str_replace_all(a,"\"","")
# b
# 
# c <- para_text4
# c
# 
# c <- str_replace_all(c,"\"","")





## ----setup_test2, echo=TRUE, warning=FALSE, message=FALSE--------------------------------------------
  


tnum.setSpace("test2")



## ----guten_down, warning=FALSE, message=FALSE, eval=FALSE--------------------------------------------
## 
## time_machine22 <- gutenberg_download(gutenberg_id = 35)  ## download The Time Machine
## 


## ----ingest, eval=FALSE------------------------------------------------------------------------------
## time_mach_txt <- readLines("tm10m.txt")
## 
## source("Book2TN-v6A-1.R")
## 
## 
## ## ingest time_mach_text into a fresh TN name
## 
## tnBooksFromLines(time_mach_txt, "wells12/hw12")  ###  new version v6A-1
## 
## 


## ----eval=FALSE--------------------------------------------------------------------------------------
##  tnum.getDBPathList(taxonomy="subject", levels=2)
## 


## ----hgwells1----------------------------------------------------------------------------------------

## use query to check TNs

###  w    queries
###  wdf  dataframe
###
###  starting with 10 -- in 3s
###

##  The ordinal numbers for the entire book 
##  show the sequence of objects in order of their appearance.
w10 <- tnum.query("wells12/hw12# has ordinal", max=1800)
wdf10 <- tnum.objectsToDf((w10))


## Examing the first 50 TNs  makes it easy to see the Table of Contents
## and to see that object 22 is the heading at the start of Chapter 1


## This shows the Table of Contents
w11 <- tnum.query("wells12/hw12# has ordinal", start = 3 ,max=37)
wdf11 <- tnum.objectsToDf(w11)


table_of_contents <- wdf11 %>% select(string.value) 

w12 <- tnum.query("wells12/hw12# has text", start = 3 ,max=18)
wdf12 <- tnum.objectsToDf((w11))

## Look at just the headings shows the structure of the book
w13 <- tnum.query("wells12/hw12/heading# has text", max=40)
wdf13 <- tnum.objectsToDf(w13)


## It may look like the table of contents is repeated twice,
## but examing the ordinals produces chapter list that includes the 
## ordinal location for the heading of each chapter
w14 <- tnum.query("wells12/hw12/heading# has ordinal", max=40)
wdf14 <- tnum.objectsToDf(w14)

chapter_locations <- left_join(select(wdf13, subject, string.value), 
                               select(wdf14, subject, numeric.value)) %>% 
                     slice(22:38)
## add column for chapter number
chapter_locations %<>% mutate(chapter=1:17)

w15 <- tnum.query("wells12/hw12/section:0022# has ordinal")
wdf15 <- tnum.objectsToDf(w15)



a <- chapter_locations %>% filter(chapter==2) %>% 
                           select(numeric.value) %>% 
                           unlist()

a <- str_pad(as.character(a),4,side="left",pad="0")

b <- paste0("wells12/hw12/section:",a,"#", " has ordinal")

b

w16 <- tnum.query("wells12/hw12/section:0022# has ordinal")



## chapter 1 para 1, word counts for the 3 sentences in para 1
q20 <- tnum.query("wells12/hw12# has *", max=3)
df20 <- tnum.objectsToDf(q20)



#  chapter locations  ordinal numbers
ord_ch1 <- unlist(tnum.query("wells12/hw12/heading:0022# has ordinal"))
ord_ch2 <- unlist(tnum.query("wells12/hw12/heading:0023# has ordinal"))


ch1_txt <- tnum.query("wells12/hw12/section:0022/paragraph:0002/# has text", max=30)

ch1_txt_df <- tnum.objectsToDf(ch1_txt)
ch1_txt_df$string.value



ch2_txt <- tnum.query("wells12/hw12/section:0022/paragraph:0002/sentence:# has *", max=30)
ch2_txt_df <- tnum.objectsToDf(ch2_txt)

ch2_txt_df$string.value

length(ch2_txt_df$string.value)


q21 <- tnum.query("wells12/hw12/section:0022/paragraph:0001/# has *", max = 30)
df21 <- tnum.objectsToDf(q21)

w20 <- tnum.query()


######
## character tagging

qr1 <- tnum.query("wells12/hw12/section:# has * = REGEXP(\" Richardson\")")
qr_df_1 <- tnum.objectsToDf(qr1)

tnum.tagByQuery("wells12/hw12/section:# has * = REGEXP(\" Richardson\")", adds=("Richardson"))

 # now the query for the tag gives you the same references

 qr2 <- tnum.query("wells12/hw12/section:0037/# has ordinal")
 qd_df_2 <- tnum.objectsToDf(qr2)

 




## ----character_tags----------------------------------------------------------------------------------


## Time Traveller
## Weena
## Morlock
## Eloi

## From chapter IV through Chap XV, the time traveller is telling his story
## in the first person.  Tagging " I " throughout the book and and handling the 
## restriction to chatpters 4 through 15 after conversion to df sounds like
## the easiest way to go.

q30 <- tnum.query("wells12/hw12/section:# has * = REGEXP(\" Time Traveller\")", max = 100)
qdf30 <- tnum.objectsToDf(q30)
tnum.tagByQuery("wells12/hw12/section:# has * = REGEXP(\" Time Traveller\")", adds=("Time_Traveller"))


q31 <- tnum.query("wells12/hw12/section:# has * = REGEXP(\" I \")", max=700)
qdf31 <- tnum.objectsToDf(q31)
tnum.tagByQuery("wells12/hw12/section:# has * = REGEXP(\" I \")", adds=("I"))


q32 <- tnum.query("wells12/hw12/section:# has * = REGEXP(\" Weena\")", max=50)
qdf32 <- tnum.objectsToDf(q32)
tnum.tagByQuery("wells12/hw12/section:# has * = REGEXP(\" Weena\")", adds=("Weena"))


q33 <- tnum.query("wells12/hw12/section:# has * = REGEXP(\" Eloi\")", max=15)
qdf33 <- tnum.objectsToDf(q33)
tnum.tagByQuery("wells12/hw12/section:# has * = REGEXP(\" Eloi\")", adds=("Eloi"))

q34 <- tnum.query("wells12/hw12/section:# has * = REGEXP(\" Morlock\")", max=70)
qdf34 <- tnum.objectsToDf(q34)
tnum.tagByQuery("wells12/hw12/section:# has * = REGEXP(\" Morlock\")", adds=("Morlock"))



 



## ----para_forms--------------------------------------------------------------------------------------



pq1 <- tnum.query("wells12/hw12/section:* has ordinal", max = 1800)
pqdf1 <- tnum.objectsToDf(pq1)

## Convert pqdf1 into a dataframe to reformat it for para-level analysis
## start with ordinals -- the gaps in the orginal numbering show where the headings go


bk_df <- pqdf1 %>% separate(col=subject, sep="/para", into = c("section", "para")) 

bk_df %<>% separate(col=section, sep=":", into= c("out","section"))

bk_df %<>% separate(col=para, sep="/", into=c("pars", "sent"))

bk_df %<>% separate(col=pars, sep=":", into=c("out1", "para"))

bk_df %<>% separate(col=sent, sep=":", into=c("out2", "sent"))

bk_df %<>% rename(ordinal=numeric.value)

bk_df %<>% select(section, para, sent, ordinal)

## Now the word counts

pq2 <- tnum.query("wells12/hw12/section:* has count:#", max = 1800)
pqdf2 <- tnum.objectsToDf(pq2)

bk_w_df <- pqdf2 %>% separate(col=subject, sep="e:", into=c("out", "sent1"))

bk_w_df %<>% rename(word_count = numeric.value)

bk_w_df %<>% select(sent1, word_count)

bk_df <- cbind2(bk_df, bk_w_df)

## check for anomalies
a <- filter(bk_df_1, sent==sent1)

bk_df %<>% select(section, para, sent, ordinal, word_count) 

## now add the text

pq3 <- tnum.query("wells12/hw12/section:* has text", max = 1800)
pqdf3 <- tnum.objectsToDf(pq3)

bk_t_df <- pqdf3 %>% separate(col=subject, sep="e:", into=c("out", "sent1"))

bk_t_df %<>% rename(s_text = string.value)

bk_t_df %<>% select(s_text)
  
bk_df <- cbind2(bk_df, bk_t_df)


pq4 <- tnum.query("wells12/hw12/section:* has text", max = 1800)
pqdf4 <- tnum.objectsToDf(pq4)

bk_tag_df <- pqdf4 %>% select(tags)


## form paragraphs


  
  




## ----sentr1------------------------------------------------------------------------------------------

library(sentimentr)

jane_1 <- get_sentences(para_text4)

## to get sentiment scores by sentence
sentiment(jane_1)

## to get sentiment scores aggregated by paragraph
sentiment_by(jane_1)





## ----------------------------------------------------------------------------------------------------


#tnum.query("wells12/hw12/section:0022/paragraph:0002/sentence:# has *")

q31 <- tnum.query("wells12/hw12/section:# has ordinal", max=500)
qdf31 <- tnum.objectsToDf(q31)


q30 <- tnum.query("wells12/hw12/section:0022# has * = REGEXP(\" Filby\")")

qdf30 <- tnum.objectsToDf(q30)

tnum.tagByQuery("wells12/hw12/section:0022# has * = REGEXP(\" Filby\")", adds=("reference:Filby"))


 # now the query for the tag gives you the same references

 q31 <- tnum.query("@reference:Filby")
 qdf31 <- tnum.objectsToDf(q31)

 # graph4 <- tnum.makeTnumPhraseGraph(query2)


