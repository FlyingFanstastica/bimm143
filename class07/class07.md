Class 7: R functions ad packages
================
Xiao
2019/4/23

# function revisited

We will source a file from online with our functions from last day

``` r
source("http://tinyurl.com/rescale-R")
```

Try out the last day’s rescale()
    fucntion

``` r
rescale(1: 10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

Try the rescale2() functions that catches string inputs

``` r
#rescale2(c(1: 10, "string"))
```

# Find missing NA values in two vectors

Start with a simple example of he larger problem I am tring to solve

``` r
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)
```

``` r
is.na(x)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
is.na(y)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE

Try putting these together with an AND

``` r
is.na(x) & is.na(y)
```

    ## [1] FALSE FALSE  TRUE FALSE FALSE

Take the sum() to find out how many TRUE values we have and thus how
many NAs we had in both x and y

``` r
sum(is.na(x) & is.na(y))
```

    ## [1] 1

Now I can make this into our first function…

``` r
both_na <- function(x, y) {
  sum(is.na(x) & is.na(y))
}
```

``` r
both_na(x, y)
```

    ## [1] 1

Test, test, test

``` r
x <- c(NA, NA, NA)
y1 <- c( 1, NA, NA)
y2 <- c( 1, NA, NA, NA)
```

``` r
both_na(x, y2)
```

    ## Warning in is.na(x) & is.na(y): 挿偄僆僽僕僃僋僩偺挿偝偑抁偄僆僽僕僃僋僩偺
    ## 挿偝偺攞悢偵側偭偰偄傑偣傫

    ## [1] 3

``` r
# gonna be 3 because the system will automatically recycle the first few elements in shorter vector the match their lengths.
```

Now lets try the new both\_na2 function on our different length input
vectors

``` r
#both_na2(x, y2)
```

``` r
which(is.na(c(1, 2, NA, 4)))
```

    ## [1] 3

``` r
df1
```

    ##     IDs exp
    ## 1 gene1   2
    ## 2 gene2   1
    ## 3 gene3   1

``` r
df2
```

    ##     IDs exp
    ## 1 gene2  -2
    ## 2 gene4  NA
    ## 3 gene3   1
    ## 4 gene5   2

Make things simple

``` r
x <- df1$IDs
y <- df2$IDs
```

``` r
intersect(x, y)
```

    ## [1] "gene2" "gene3"

``` r
x %in% y
```

    ## [1] FALSE  TRUE  TRUE

``` r
# Code -> Extract function -> function name
gene_intersect <- function(x, y) {
  cbind( df1[ df1$IDs %in% df2$IDs, ],
         df2[ df2$IDs %in% df1$IDs, "exp"]  )
}
```

``` r
gene_intersect(df1, df2)
```

    ##     IDs exp df2[df2$IDs %in% df1$IDs, "exp"]
    ## 2 gene2   1                               -2
    ## 3 gene3   1                                1

``` r
dropLow <- function(x) {
  i <- (sum(x) - min(x))/(length(x) - 1)
  message("The final average grade is ", i)
}
```

``` r
x <- c(100, 100, 100, 100, 100, 100, 100, 90)
y <- c(100, 90, 90, 90, 90, 90, 97, 80)
dropLow(x)
```

    ## The final average grade is 100

``` r
dropLow(y)
```

    ## The final average grade is 92.4285714285714
