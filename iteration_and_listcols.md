Iterations and listcols
================
Jasmine Niu (jn2855)
2022-11-01

Let’s look at a list.

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000)))

l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.092555 -0.688742 -0.007055  0.002093  0.702122  3.252225

Accessing list items

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[3]]
```

    ## [1]  TRUE FALSE

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

normal distribution…

``` r
list_norm =
  list(
    a = rnorm(20, 5, 4),
    b = rnorm(20, -12, 3),
    c = rnorm(20, 17, 4),
    d = rnorm(20, 100, 1)
  )

is.list(list_norm)
```

    ## [1] TRUE

``` r
mean_and_sd = function(x) {
  
  
  if(!is.numeric(x)) {
    stop("Z scores only work for numbers")

  }
  
  if(length(x) < 3) {
    stop("Z scores really only work if you have three or more numbers")
  }  
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Let’s try to make this work.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.04  3.05

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.5  2.98

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.2  3.38

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  100. 0.988

Let’s a `for` loop instead.

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])

}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.04  3.05
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.5  2.98
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.2  3.38
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  100. 0.988

## can we map??

we can map!!

``` r
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.04  3.05
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.5  2.98
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.2  3.38
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  100. 0.988

``` r
#first argument: can be a list, vector or data frame

output = map(.x = list_norm, ~ mean_and_sd(.x))
```

so…what about other functions?

``` r
map(list_norm, var)
```

    ## $a
    ## [1] 9.313008
    ## 
    ## $b
    ## [1] 8.890641
    ## 
    ## $c
    ## [1] 11.40292
    ## 
    ## $d
    ## [1] 0.9761833

``` r
map(list_norm, summary)
```

    ## $a
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.7666  4.0404  6.1784  6.0376  8.5306 10.8587 
    ## 
    ## $b
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -17.15  -14.75  -13.24  -12.53  -10.33   -6.81 
    ## 
    ## $c
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   10.69   14.94   17.72   17.24   19.15   23.52 
    ## 
    ## $d
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   98.02   99.31   99.96   99.96  100.65  101.50

map variants…

``` r
map_dbl(list_norm, median)
```

    ##          a          b          c          d 
    ##   6.178392 -13.240974  17.724819  99.959209

``` r
map_df(list_norm, mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1   6.04 3.05 
    ## 2 -12.5  2.98 
    ## 3  17.2  3.38 
    ## 4 100.   0.988

``` r
map_dbl(list_norm, median, .id = "input")
```

    ##          a          b          c          d 
    ##   6.178392 -13.240974  17.724819  99.959209

``` r
#.id argument keeps the names of the elements in the input list

# output = map2(.x = input_1, .y = input_2, ~func(arg_1 = .x, arg_2 = .y))
```

## list columns…

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    norm = list_norm
  )

listcol_df[["norm"]]
```

    ## $a
    ##  [1] 10.1872608  8.1523617  9.3297583  0.7665836  5.3908156  4.1447854
    ##  [7]  7.3454539  0.7745837  3.6406745  6.9983227  6.5116280 10.8586885
    ## [13]  8.8571764  3.7274368  1.1684764  8.4440171  4.6467504  5.1710121
    ## [19]  8.7904269  5.8451568
    ## 
    ## $b
    ##  [1] -13.800105 -13.356118 -16.314337  -6.809686 -14.704560 -12.235973
    ##  [7] -15.358633  -8.353759 -12.736303 -10.439124 -15.122299 -10.009056
    ## [13] -13.276882 -14.872484 -13.935374  -7.720288 -17.145722  -8.212451
    ## [19] -13.205066 -13.060538
    ## 
    ## $c
    ##  [1] 18.49277 16.40996 18.12330 19.08154 20.86801 11.66128 15.12354 17.32634
    ##  [9] 13.26803 15.55240 19.92850 19.12651 14.39872 23.52500 19.03619 14.28596
    ## [17] 19.21317 16.35748 22.23452 10.69027
    ## 
    ## $d
    ##  [1]  99.94467 101.36042  99.20077  99.34515 100.60259 100.62570 101.50381
    ##  [8]  99.64640  99.97375 100.92660  99.09016 100.73139 100.13819 101.26877
    ## [15]  98.01720  98.21218  99.46900 100.43536  99.69797  99.01238

``` r
output = map(listcol_df[["norm"]], mean_and_sd)
```

can we add list columns and then what…

``` r
listcol_df %>% 
  mutate(
    m_sd = map(norm, mean_and_sd) #map_df()
  ) %>% 
  select(-norm)
```

    ## # A tibble: 4 × 2
    ##   name  m_sd            
    ##   <chr> <named list>    
    ## 1 a     <tibble [1 × 2]>
    ## 2 b     <tibble [1 × 2]>
    ## 3 c     <tibble [1 × 2]>
    ## 4 d     <tibble [1 × 2]>

## What about something more realistic…

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\finij\AppData\Local/Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-09-29 10:35:09 (8.418)

    ## file min/max dates: 1869-01-01 / 2022-09-30

    ## using cached file: C:\Users\finij\AppData\Local/Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-09-29 10:35:23 (1.703)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: C:\Users\finij\AppData\Local/Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-09-29 10:35:29 (0.952)

    ## file min/max dates: 1999-09-01 / 2022-09-30

let’s nest within weather stations…

``` r
weather_nest_df = 
  weather_df %>% 
  nest(data = date:tmin)
```

Really is a list column!

``` r
weather_nest_df[["data"]]
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest_df %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest_df[["data"]][[1]]
```

    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

``` r
lm(tmax ~ tmin, data = weather_nest_df[["data"]][[1]]) #y~x
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
lm(tmax ~ tmin, data = weather_nest_df[["data"]][[2]]) 
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509

``` r
lm(tmax ~ tmin, data = weather_nest_df[["data"]][[3]]) 
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

Let’s write a short lil ol function

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}

weather_lm(weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nest_df[["data"]], weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

Can i do all this in a tidy way

``` r
weather_nest_df %>% 
  mutate(
    model = map(data, weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model 
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

unnesting

``` r
weather_nest_df %>% 
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

## Napoleon!!

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What we did last time

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

map(vec_urls, read_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                      stars text                        
    ##    <chr>                                      <dbl> <chr>                       
    ##  1 Best                                           5 "One of the funniest movies…
    ##  2 Classic Movie                                  5 "Love this movie. Everyone …
    ##  3 Goofy movie                                    5 "I used this movie for a mi…
    ##  4 Lol hey it’s Napoleon. What’s not to love…     5 "Vote for Pedro"            
    ##  5 Still the best                                 5 "Completely stupid, absolut…
    ##  6 70’s and 80’s Schtick Comedy                   5 "…especially funny if you h…
    ##  7 Amazon Censorship                              5 "I hope Amazon does not cen…
    ##  8 Watch to say you did                           3 "I know it's supposed to be…
    ##  9 Best Movie Ever!                               5 "We just love this movie an…
    ## 10 Quirky                                         5 "Good family film"          
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                                         stars text                     
    ##    <chr>                                         <dbl> <chr>                    
    ##  1 Funny movie - can't play it !                     1 Sony 4k player won't eve…
    ##  2 A brilliant story about teenage life              5 Napoleon Dynamite delive…
    ##  3 HUHYAH                                            5 Spicy                    
    ##  4 Cult Classic                                      4 Takes a time or two to f…
    ##  5 Sweet                                             5 Timeless Movie. My Grand…
    ##  6 Cute                                              4 Fun                      
    ##  7 great collectible                                 5 one of the greatest movi…
    ##  8 Iconic, hilarious flick ! About friend ship .     5 Who doesn’t love this mo…
    ##  9 Funny                                             5 Me and my dad watched th…
    ## 10 Low budget but okay                               3 This has been a classic …
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 "Disappointing"                                                       2 "We …
    ##  2 "Favorite movie \U0001f37f"                                           5 "Thi…
    ##  3 "none"                                                                5 "thi…
    ##  4 "Great movie"                                                         5 "Vot…
    ##  5 "Get this to improve your nunchuck and bowstaff skills. Dancing …     5 "Got…
    ##  6 "Incredible Movie"                                                    5 "Fun…
    ##  7 "Always loved this movie!"                                            5 "I h…
    ##  8 "Great movie"                                                         5 "Bou…
    ##  9 "The case was damaged"                                                3 "It …
    ## 10 "It’s classic"                                                        5 "Cle…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 Irreverent comedy                                                     5 "If …
    ##  2 Great classic!                                                        5 "Fun…
    ##  3 Most Awesomsomest Movie EVER!!!                                       5 "Thi…
    ##  4 Always a favorite                                                     5 "I r…
    ##  5 It’s not working the disc keeps showing error when I tried other…     1 "It’…
    ##  6 Gosh!                                                                 5 "Eve…
    ##  7 An Acquired Taste                                                     1 "Thi…
    ##  8 What is this ?                                                        4 "Nic…
    ##  9 Napoleon Dynamite                                                     2 "I w…
    ## 10 Great movie                                                           5 "Gre…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                                                           stars text   
    ##    <chr>                                                           <dbl> <chr>  
    ##  1 Good movie                                                          5 "Good …
    ##  2 Came as Described                                                   5 "Came …
    ##  3 Oddly on my list of keepers.                                        5 "Good …
    ##  4 Low budget fun                                                      5 "Oddba…
    ##  5 On a scale of 1 to 10 this rates a minus                            1 "This …
    ##  6 I always wondered...                                                5 "what …
    ##  7 Audio/video not synced                                              1 "I tho…
    ##  8 Kind of feels like only a bully would actually laugh at this...     1 "...as…
    ##  9 movie                                                               5 "good …
    ## 10 An Overdose of Comical Cringe                                       5 "Excel…

``` r
napoleon_reviews = 
  tibble(
    page = 1:5,
    page_url = str_c(url_base, page)
  ) %>% 
  mutate(
    reviews = map(page_url, read_page_reviews)
  )

napoleon_reviews %>% 
  select(-page_url) %>% 
  unnest(reviews)
```

    ## # A tibble: 50 × 4
    ##     page title                                      stars text                  
    ##    <int> <chr>                                      <dbl> <chr>                 
    ##  1     1 Best                                           5 "One of the funniest …
    ##  2     1 Classic Movie                                  5 "Love this movie. Eve…
    ##  3     1 Goofy movie                                    5 "I used this movie fo…
    ##  4     1 Lol hey it’s Napoleon. What’s not to love…     5 "Vote for Pedro"      
    ##  5     1 Still the best                                 5 "Completely stupid, a…
    ##  6     1 70’s and 80’s Schtick Comedy                   5 "…especially funny if…
    ##  7     1 Amazon Censorship                              5 "I hope Amazon does n…
    ##  8     1 Watch to say you did                           3 "I know it's supposed…
    ##  9     1 Best Movie Ever!                               5 "We just love this mo…
    ## 10     1 Quirky                                         5 "Good family film"    
    ## # … with 40 more rows
