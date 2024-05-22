library(htm2txt)
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(Matrix)

# data import
url <- 'http://bos.zrc-sazu.si/sbsj.html'
text <- gettxt(url)

# data procesing
## list of words
words <- text %>% str_split("\n") %>% 
  as.data.frame() %>% .[-c(1:5), ] %>%
  as.data.frame() %>% slice(., 1:(n() - 5)) 

colnames(words) <- c("besede")

words <- words %>% mutate(
  besede = besede %>% 
    str_replace(" \\s*\\([^\\)]+\\)", "") %>%
    str_replace_all("\xc2\xa0", " ")%>%
    str_replace_all("-"," ") %>%
    str_trim("right") %>%
    str_replace_all(" ",".") %>%
    paste0(".")
)

## matrix for weights
letter <- c("a", "b", "c", "č", "d", "e", "f", "g", "h", "i", "j", "k", "l", 
            "m", "n", "o", "p", "r", "s", "š", "t", "u", "v", "z", "ž",".")

weight_mat <- matrix(0, nrow = 26, ncol = 26)
colnames(weight_mat) <- letter
rownames(weight_mat) <- toupper(letter)

# check if string is composed with letter
if_letter <- function(str){
  strsplit(str,"") %>%
    unlist() %in% letter %>%
    all()
}

# weight +1 for next character 
# ex. abc => [a,b] +1 & [b,c] +1
add_weights <- function(word){
  if(word == "."){
    if(nchar(word) > 1){
      add_weights(substring(word,2))
    }
  }else if(if_letter(word)){
    l1 = substring(toupper(word), 1, 1)
    l2 = substring(tolower(word), 2, 2)
    weight_mat[l1,l2] <<- weight_mat[l1,l2] + 1
    cut_word = substring(word, 2)
    add_weights(cut_word)
  }else{}
}


# calculate weights for words
apply(words, 1, function(x) add_weights(x))

# visualization
## heatmap
heatmap(weight_mat)

# letter co-occurence
weight_mat %>% as.dfm() %>%
  fcm() %>% textplot_network()
