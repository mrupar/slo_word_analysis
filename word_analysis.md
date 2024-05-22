Word Analysis
================
Miha Rupar

- [Data import](#data-import)
- [Data procesing](#data-procesing)
- [Calculate weight](#calculate-weight)
- [Visualization](#visualization)
  - [heatmap](#heatmap)
  - [letter co-occurence](#letter-co-occurence)

## Data import

``` r
url <- 'http://bos.zrc-sazu.si/sbsj.html'
text <- gettxt(url)
```

## Data procesing

Transforming data to get desired outcome for later use

``` r
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
```

## Calculate weight

Defining weight matrix

``` r
letter <- c("a", "b", "c", "č", "d", "e", "f", "g", "h", "i", "j", "k", "l", 
            "m", "n", "o", "p", "r", "s", "š", "t", "u", "v", "z", "ž",".")

weight_mat <- matrix(0, nrow = 26, ncol = 26)
colnames(weight_mat) <- letter
rownames(weight_mat) <- toupper(letter)
```

Function for checking if input string is composed with characters from
variable letter

``` r
if_letter <- function(str){
  strsplit(str,"") %>%
    unlist() %in% letter %>%
    all()
}
```

Function for adding weights to matrix, based on next character in
string. Ex. “abc” =\> \[a,b\] +1 & \[b,c\] +1

``` r
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
```

Application of weights function on data

``` r
apply(words, 1, function(x) add_weights(x))
```

## Visualization

### heatmap

``` r
heatmap(weight_mat)
```

![](word_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### letter co-occurence

``` r
weight_mat %>% as.dfm() %>%
  fcm() %>% textplot_network()
```

![](word_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
