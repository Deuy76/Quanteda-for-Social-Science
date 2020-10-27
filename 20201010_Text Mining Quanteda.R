#======================================================#
# Title: 'Text Mining with Quanteda for Social Science #
# Author: "CHOI, Doo Young Wicks"                      #
# Date: 2020-10-01                                     #
#======================================================#

# This code is to do co-occurrence analysis with "R" quanteda-package. 
# The code used to show links between words with the semantic perspectives. 


#====================================#
# Adding basic Library Environment   #
#====================================#

# Loading the libraries for the 'Text Mining' Process 
# 1) loading Text or Excel files
library(readtext)
library(readxl)
# 2) LSE Mining Tool
library(quanteda)
# 3) To perform "part-of-speech tagging", "named entity recognition's", dependency relationship analysis.
library(spacyr)
# 4) Stopwords library 
library(stopwords)
# 5) plot library
library(ggplot2)
# 6) Topic Model library
library(topicmodels)
library(seededlda)
# 7) Sampling Library
library (rsample)
# 8) Extra Fonts
library(extrafont)
# 9) Managing dates
library(lubridate)
# 10) Sentiment Analysis
library(sentometrics)
# Table Manipulation
library(plyr)
#====================================
# File Loading 
#====================================
# Choose a file to process
fileToLoad <- file.choose()
# In cases of Excel file
Text.source <- read_excel(fileToLoad)
# In cases of a CSV or a TXT file
# Text.source <- readtext(fileToLoad)

# Chage Cell Name, if needs
# X <- X %>% rename(text=texts)
#Text.source <- Uganda_New_Vision

#====================================
# Preprocessing Data
#====================================

# Preparing Corpus Target
Mining.text.corpus <- Text.source

# Manipulating date 
Mining.text.corpus$date <- as.Date(Mining.text.corpus$date)
# Mining.text.corpus$date <- dmy(Mining.text.corpus$date)
Mining.text.corpus
# Set specific date 
Mining.text.corpus <- Mining.text.corpus[which(Mining.text.corpus$date >= as.Date("2020-01-01") & Mining.text.corpus$date <= as.Date("2020-12-31")),]

Mining.text.corpus

# Processing Corpus
Mining.text.corpus <- quanteda::corpus(Mining.text.corpus$text,
                                       # docnames = Mining.text.corpus$title,
                                       docvars = data.frame (year=substr(Mining.text.corpus$date, 0, 4)))

# Check Corpus Summary

summary(Mining.text.corpus)

#============================================#
# Pre data clensing for the Text analysis    #
# Setting personal stopwords                 #
#============================================#

mystopwords <- c ("0o", "0s", "3a", "3b", "3d", "6b", "6o", "a", "a1", "a2", "a3", "a4", "ab",
                  "able", "about", "above", "abst", "ac", "accordance", "according", "accordingly", 
                  "across", "act", "actually", "ad", "added", "adj", "ae", "af", "affected",
                  "affecting", "affects", "after", "afterwards", "ag", "again", "against",
                  "ah", "ain", "ain't", "aj", "al", "all", "allow", "allows", "almost", "alone",
                  "along", "already", "also", "although", "always", "am", "among", "amongst",
                  "amoungst", "amount", "an", "and", "announce", "another", "any", "anybody",
                  "anyhow", "anymore", "anyone", "anything", "anyway", "anyways", "anywhere",
                  "ao", "ap", "apart", "apparently", "appear", "appreciate", "appropriate",
                  "approximately", "ar", "are", "aren", "arent", "aren't", "arise", "around",
                  "as", "a's", "aside", "ask", "asking", "associated", "at", "au", "auth", "av",
                  "available", "aw", "away", "awfully", "ax", "ay", "az", "b", "b1", "b2", "b3",
                  "ba", "back", "bc", "bd", "be", "became", "because", "become", "becomes",
                  "becoming", "been", "before", "beforehand", "begin", "beginning", "beginnings", 
                  "begins", "behind", "being", "believe", "below", "beside", "besides", "best", 
                  "better", "between", "beyond", "bi", "bill", "biol", "bj", "bk", "bl", "bn",
                  "both", "bottom", "bp", "br", "brief", "briefly", "bs", "bt", "bu", "but",
                  "bx", "by", "c", "c1", "c2", "c3", "ca", "call", "came", "can", "cannot",
                  "cant", "can't", "cause", "causes", "cc", "cd", "ce", "certain", "certainly",
                  "cf", "cg", "ch", "changes", "ci", "cit", "cj", "cl", "clearly", "cm", "c'mon", 
                  "cn", "co", "com", "come", "comes", "con", "concerning", "consequently", "consider",
                  "considering", "contain", "containing", "contains", "corresponding", "could", "couldn",
                  "couldnt", "couldn't", "course", "cp", "cq", "cr", "cry", "cs", "c's", "ct", "cu",
                  "currently", "cv", "cx", "cy", "cz", "d", "d2", "da", "date", "dc", "dd", "de",
                  "definitely", "describe", "described", "despite", "detail", "df", "di", "did",
                  "didn", "didn't", "different", "dj", "dk", "dl", "do", "does", "doesn", "doesn't",
                  "doing", "don", "done", "don't", "down", "downwards", "dp", "dr", "ds", "dt", 
                  "du", "due", "during", "dx", "dy", "e", "e2", "e3", "ea", "each", "ec", "ed", 
                  "edu", "ee", "ef", "effect", "eg", "ei", "eight", "eighty", "either", "ej", 
                  "el", "eleven", "else", "elsewhere", "em", "empty", "en", "end", "ending", 
                  "enough", "entirely", "eo", "ep", "eq", "er", "es", "especially", "est", "et", 
                  "et-al", "etc", "eu", "ev", "even", "ever", "every", "everybody", "everyone", 
                  "everything", "everywhere", "ex", "exactly", "example", "except", "ey", "f", 
                  "f2", "fa", "far", "fc", "few", "ff", "fi", "fifteen", "fifth", "fify", "fill", 
                  "find", "fire", "first", "five", "fix", "fj", "fl", "fn", "fo", "followed", 
                  "following", "follows", "for", "former", "formerly", "forth", "forty", "found", 
                  "four", "fr", "from", "front", "fs", "ft", "fu", "full", "further", "furthermore", 
                  "fy", "g", "ga", "gave", "ge", "get", "gets", "getting", "gi", "give", "given", 
                  "gives", "giving", "gj", "gl", "go", "goes", "going", "gone", "got", "gotten", 
                  "gr", "greetings", "gs", "gy", "h", "h2", "h3", "had", "hadn", "hadn't", "happens",
                  "hardly", "has", "hasn", "hasnt", "hasn't", "have", "haven", "haven't", "having", 
                  "he", "hed", "he'd", "he'll", "hello", "help", "hence", "her", "here", "hereafter",
                  "hereby", "herein", "heres", "here's", "hereupon", "hers", "herself", "hes", "he's", 
                  "hh", "hi", "hid", "him", "himself", "his", "hither", "hj", "ho", "home", "hopefully",
                  "how", "howbeit", "however", "how's", "hr", "hs", "http", "hu", "hundred", 
                  "hy", "i", "i2", "i3", "i4", "i6", "i7", "i8", "ia", "ib", "ibid", "ic", "id", "i'd", 
                  "ie", "if", "ig", "ignored", "ih", "ii", "ij", "il", "i'll", "im", "i'm", 
                  "immediate", "immediately", "importance", "important", "in", "inasmuch", "inc", 
                  "indeed", "index", "indicate", "indicated", "indicates", "information", "inner", 
                  "insofar", "instead", "interest", "into", "invention", "inward", "io", "ip", "iq",
                  "ir", "is", "isn", "isn't", "it", "itd", "it'd", "it'll", "its", "it's", "itself", 
                  "iv", "i've", "ix", "iy", "iz", "j", "jj", "jr", "js", "jt", "ju", "just", "k", 
                  "ke", "keep", "keeps", "kept", "kg", "kj", "km", "know", "known", "knows", "ko", "l", 
                  "l2", "la", "largely", "last", "lately", "later", "latter", "latterly", "lb", "lc", "le",
                  "least", "les", "less", "lest", "let", "lets", "let's", "lf", "like", "liked", "likely",
                  "line", "little", "lj", "ll", "ll", "ln", "lo", "look", "looking", "looks", "los", 
                  "lr", "ls", "lt", "ltd", "m", "m2", "ma", "made", "mainly", "make", "makes", "many", 
                  "may", "maybe", "me", "mean", "means", "meantime", "meanwhile", "merely", "mg", "might",
                  "mightn", "mightn't", "mill", "million", "mine", "miss", "ml", "mn", "mo", "more", 
                  "moreover", "most", "mostly", "move", "mr", "mrs", "ms", "mt", "mu", "much", "mug", 
                  "must", "mustn", "mustn't", "my", "myself", "n", "n2", "na", "name", "namely", "nay", 
                  "nc", "nd", "ne", "near", "nearly", "necessarily", "necessary", "need", "needn", "needn't", 
                  "needs", "neither", "never", "nevertheless", "new", "next", "ng", "ni", "nine", "ninety", 
                  "nj", "nl", "nn", "no", "nobody", "non", "none", "nonetheless", "noone", "nor", "normally", 
                  "nos", "not", "noted", "nothing", "novel", "now", "nowhere", "nr", "ns", "nt", "ny", "o", 
                  "oa", "ob", "obtain", "obtained", "obviously", "oc", "od", "of", "off", "often", "og", 
                  "oh", "oi", "oj", "ok", "okay", "ol", "old", "om", "omitted", "on", "once", "one", "ones", 
                  "only", "onto", "oo", "op", "oq", "or", "ord", "os", "ot", "other", "others", "otherwise", 
                  "ou", "ought", "our", "ours", "ourselves", "out", "outside", "over", "overall", "ow", "owing", 
                  "own", "ox", "oz", "p", "p1", "p2", "p3", "page", "pagecount", "pages", "par", "part", 
                  "particular", "particularly", "pas", "past", "pc", "pd", "pe", "per", "perhaps", "pf", 
                  "ph", "pi", "pj", "pk", "pl", "placed", "please", "plus", "pm", "pn", "po", "poorly", 
                  "possible", "possibly", "potentially", "pp", "pq", "pr", "predominantly", "present", 
                  "presumably", "previously", "primarily", "probably", "promptly", "proud", "provides", 
                  "ps", "pt", "pu", "put", "py", "q", "qj", "qu", "que", "quickly", "quite", "qv", "r", "r2", 
                  "ra", "ran", "rather", "rc", "rd", "re", "readily", "really", "reasonably", "recent", 
                  "recently", "ref", "refs", "regarding", "regardless", "regards", "related", "relatively", 
                  "research", "research-articl", "respectively", "resulted", "resulting", "results", "rf", 
                  "rh", "ri", "right", "rj", "rl", "rm", "rn", "ro", "rq", "rr", "rs", "rt", "ru", "run", 
                  "rv", "ry", "s", "s2", "sa", "said", "same", "saw", "say", "saying", "says", "sc", "sd", 
                  "se", "sec", "second", "secondly", "section", "see", "seeing", "seem", "seemed", "seeming", 
                  "seems", "seen", "self", "selves", "sensible", "sent", "serious", "seriously", "seven", 
                  "several", "sf", "shall", "shan", "shan't", "she", "shed", "she'd", "she'll", "shes", "she's", 
                  "should", "shouldn", "shouldn't", "should've", "show", "showed", "shown", "showns", "shows", 
                  "si", "side", "significant", "significantly", "similar", "similarly", "since", "sincere", 
                  "six", "sixty", "sj", "sl", "slightly", "sm", "sn", "so", "some", "somebody", "somehow", 
                  "someone", "somethan", "something", "sometime", "sometimes", "somewhat", "somewhere", "soon", 
                  "sorry", "sp", "specifically", "specified", "specify", "specifying", "sq", "sr", "ss", "st", 
                  "still", "stop", "strongly", "sub", "substantially", "successfully", "such", "sufficiently", 
                  "suggest", "sup", "sure", "sy", "system", "sz", "t", "t1", "t2", "t3", "take", "taken", "taking",
                  "tb", "tc", "td", "te", "tell", "ten", "tends", "tf", "th", "than", "thank", "thanks", "thanx", 
                  "that", "that'll", "thats", "that's", "that've", "the", "their", "theirs", "them", "themselves",
                  "then", "thence", "there", "thereafter", "thereby", "thered", "therefore", "therein", "there'll",
                  "thereof", "therere", "theres", "there's", "thereto", "thereupon", "there've", "these", "they", 
                  "theyd", "they'd", "they'll", "theyre", "they're", "they've", "thickv", "thin", "think", "third", 
                  "this", "thorough", "thoroughly", "those", "thou", "though", "thoughh", "thousand", "three", "throug", 
                  "through", "throughout", "thru", "thus", "ti", "til", "tip", "tj", "tl", "tm", "tn", "to", "together", 
                  "too", "took", "top", "toward", "towards", "tp", "tq", "tr", "tried", "tries", "truly", "try", 
                  "trying", "ts", "t's", "tt", "tv", "twelve", "twenty", "twice", "two", "tx", "u", "u201d", "ue", "ui", 
                  "uj", "uk", "um", "un", "under", "unfortunately", "unless", "unlike", "unlikely", "until", "unto", "uo", 
                  "up", "upon", "ups", "ur", "us", "use", "used", "useful", "usefully", "usefulness", "uses", "using", 
                  "usually", "ut", "v", "va", "value", "various", "vd", "ve", "ve", "very", "via", "viz", "vj", "vo", 
                  "wasn't", "way", "we", "wed", "we'd", "welcome", "well", "we'll", "well-b", "went", "were", "we're", 
                  "weren", "werent", "weren't", "we've", "what", "whatever", "what'll", "whats", "what's", "when", "whence",
                  "whenever", "when's", "where", "whereafter", "whereas", "whereby", "wherein", "wheres", "where's",
                  "whereupon", "wherever", "whether", "which", "while", "whim", "whither", "who", "whod", "whoever",
                  "whole", "who'll", "whom", "whomever", "whos", "who's", "whose", "why", "why's", "wi", "widely", 
                  "will", "willing", "wish", "with", "within", "without", "wo", "won", "wonder", "wont", "won't", 
                  "words", "world", "would", "wouldn", "wouldnt", "wouldn't", "www", "x", "x1", "x2", "x3", "xf", 
                  "xi", "xj", "xk", "xl", "xn", "xo", "xs", "xt", "xv", "xx", "y", "y2", "yes", "yet", "yj", "yl", 
                  "you", "youd", "you'd", "you'll", "your", "youre", "you're", "yours", "yourself", "yourselves", 
                  "you've", "yr", "ys", "yt", "z", "zero", "zi", "zz", "GMT", "cases","number", "people", "country", 
                  "toll","told", "V1", "day", "today","yesterday", "year","years", "google","ways","stories","time",
                  "nation", "Nation", "week", "cent","month", "note", "author","NMG","billion","million","thousand",
                  "sh100,000","sh348m","2.5pc", "mp's", "MPsNew", "Monday","Tuesday","Wednesday","Thursday","Friday",
                  "Saturday","Sunday","January", "February", "March", "April", "May","June", "July","August","September",
                  "October", "November", "December","sh1bn","Photo","PHOTO","photo","writer", "EABW","eabw","EditorComment",
                  "21st","22nd","201622nd", "7th")

# Tokenizing
Mining.TXT.token <- tokens(Mining.text.corpus, what = "word", 
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE, split_hyphens = TRUE,
                           remove_url=TRUE) %>%
                           # Merge two words                         
                           tokens_compound(list(c("Dar", "es", "Salaam")))%>%
                           tokens_remove(pattern = mystopwords,
                           valuetype = 'fixed', padding = T)%>%
                           tokens_remove(stopwords("english"))%>%
                           tokens_tolower(keep_acronyms = FALSE)%>%
                           tokens_wordstem("english")


# Remove e-mails
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", 
                                  valuetype = 'regex', padding = TRUE)
# remove web sites
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", 
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[com]+", 
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9-]+\\.[com]+", 
                                  valuetype = 'regex', padding = TRUE)

# Merge two words
# Mining.TXT.token <- tokens_compound(Mining.TXT.token, dictionary(list(key1 = "United states",
#                                                                      key2 = "United States")))



# View
View(Mining.TXT.token)

# document feature matrix (dfm) with the  bag-of-words
Mining.TXT.token.dfm <- dfm(Mining.TXT.token, tolower = FALSE)
# remove the term of frequency lesser than 5 times
Mining.TXT.token.dfm <- dfm_trim(Mining.TXT.token.dfm, min_termfreq = 5) %>%
dfm_weight("boolean") 

nfeat(Mining.TXT.token.dfm)

# Confirm data matrix
Mining.TXT.token.matrix <- as.matrix (Mining.TXT.token.dfm)
View (Mining.TXT.token.matrix)
dim (Mining.TXT.token.matrix)

# Term frequency (TF)
term.frequency <- function(row) {
  row / sum (row)
}

# Inverse Document Frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col >0))
  
  log10(corpus.size / doc.count)
}

# TF-IDF 계산
tf.idf <- function(x, idf) {
  x * idf
}

# 1st step, normalize all documents via TF.
Mining.TXT.token.df <- apply(Mining.TXT.token.matrix, 1, term.frequency)
dim(Mining.TXT.token.df)
View(Mining.TXT.token.df)

# 2nd step,IDF culculate vector
# for training data and for test data!
Mining.TXT.token.idf <- apply(Mining.TXT.token.matrix, 2, inverse.doc.freq)
str(Mining.TXT.token.idf)


# Traning TF-IDF to get final copus traning
Mining.TXT.token.tfidf <-  apply(Mining.TXT.token.df, 2, tf.idf, idf = Mining.TXT.token.idf)
dim(Mining.TXT.token.tfidf)
View(Mining.TXT.token.tfidf)


# Convert to matrix
Mining.TXT.token.tfidf <- t(Mining.TXT.token.tfidf)
dim(Mining.TXT.token.tfidf)
View(Mining.TXT.token.tfidf)


# Check imcomplete cases
incomplete.cases <- which(!complete.cases(Mining.TXT.token.tfidf))
Mining.TXT.token$text1[incomplete.cases]


# Touch imcompleted cases
Mining.TXT.token.tfidf[incomplete.cases,] <- rep(0.0, ncol(Mining.TXT.token.tfidf))
dim(Mining.TXT.token.tfidf)
sum (which(!complete.cases (Mining.TXT.token.tfidf)))


# Make a clean data frame using the same process as before.
Mining.TXT.token.df <- cbind(data.frame(Mining.TXT.token.tfidf))
names(Mining.TXT.token.df) <- make.names(names(Mining.TXT.token.df))

Mining.TXT.token.dfm<-dfm_select(Mining.TXT.token.dfm, min_nchar=3)


# =======================================
# Mutual Frequency Analysys
# =======================================
#key <- textstat_keyness(Mining.TXT.token.dfm)
#head(key, 10) %>% knitr::kable()


# =======================================
# Frequency Plot
# =======================================


Mining.TXT.token.dfm.inaug <- textstat_frequency(Mining.TXT.token.dfm, n = 100)

# Sort by reverse frequency order
Mining.TXT.token.dfm.inaug$feature <- with(Mining.TXT.token.dfm.inaug, reorder(feature, -frequency))

ggplot(Mining.TXT.token.dfm.inaug, aes(x =feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# =======================================
# Create Co-occurrences Network 
# =======================================

# Using prior dfm
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm)
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm, tri = FALSE) # Create Co-occurance Network
feat <- names(topfeatures(Mining.TXT.textplot.Network, 50)) # Setting a top features for the Co-occurance
set.seed(100)
Mining.TXT.textplot.Network <- fcm_select(Mining.TXT.textplot.Network, feat, verbose = FALSE)

# Plot CON

textplot_network(Mining.TXT.textplot.Network,
                 vertex_labelsize = 2.5 * rowSums(Mining.TXT.textplot.Network)/
                 min(rowSums(Mining.TXT.textplot.Network)),
                 min_freq = 0.90,
                 vertex_labelfont = "Tahoma" , edge_size = 1)




# =======================================
# Create Topic Model
# =======================================

Topic.Model <- textmodel_lda(Mining.TXT.token.dfm, k = 5)
Topic.Model <- data.frame(terms(Topic.Model, 10))
Topic.Model

# =======================================
# Create Word Cloud
# =======================================

# Word Cloud
#wordcloud.results <- textplot_wordcloud(Mining.TXT.token.dfm, min_count = 10, random_order = FALSE,
#                                        rotation = .25, color = RColorBrewer::brewer.pal(8,"Dark2"))

# =======================================
# Sentiment Analycis
# =======================================
#====================================
# File Loading 
# 파일 읽어드리기
#====================================

# Choose a file
# 파일 읽기
fileToLoad <- file.choose()
# In cases of Excel file
# Excel파일의 경우
Text.source <- readtext(fileToLoad)
# In cases of a CSV or a TXT file
# CSV 또는 TXT 파일의 경우
# CSV又はTXTファイルの場合
#Text.source <- readtext(fileToLoad)

# Summary
summary(Text.source)

#====================================#
# Sentiment Analysis Process         #
#====================================#
#====================================
# File Loading 

#====================================

# Summary
summary(Text.source)

Text.source <- rename(Text.source,c("title" = "id", "text" = "texts"))


# construct a object with sentiment measures
Corpus <- sento_corpus(Text.source)
class(Corpus)

tail(quanteda::docvars(Corpus))
cat("\n")
# list of lexicons:  "HENRY_en", "GI_en"
corpusSample <- corpus_sample(Corpus)
l <- sento_lexicons(list_lexicons[c("LM_en")], list_valence_shifters[["en"]])
ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "day", lag = 2)
sentomeasures <- sento_measures(corpusSample, l, ctr)

#=====================================
# plot sentiment measures
#=====================================

plot(sentomeasures, group = "lexicons")
View(sentomeasures)
Article.sentiment <-data.frame(sentomeasures$sentiment)

