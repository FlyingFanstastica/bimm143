class13
================
Xiao
2019/5/14

Childhood asthma Q1. What are those 4 candidate SNPs? rs12936231,
rs8067378, rs9303277, and rs7216389

Q5. What proportion of the Mexican Ancestry in Los Angeles sample
population (MXL) are homozygous for the asthma associated SNP (G|G)?

``` r
data <- read.csv("MXL.csv")
geno <- data$Genotype..forward.strand.
table(geno) / sum(table(geno)) * 100
```

    ## geno
    ##     A|A     A|G     G|A     G|G 
    ## 34.3750 32.8125 18.7500 14.0625

``` r
library(seqinr)
library(gtools)
phred <- asc( s2c("DDDDCDEDCDDDDBBDDDCC@") ) - 33
phred
```

    ##  D  D  D  D  C  D  E  D  C  D  D  D  D  B  B  D  D  D  C  C  @ 
    ## 35 35 35 35 34 35 36 35 34 35 35 35 35 33 33 35 35 35 34 34 31

``` r
prob <- 10**(-phred / 10)
```

My IP: 149.165.171.19

\!\!Tools -\> Search“FASTQC” -\> FASTQC

``` r
expr <- read.table("rs8067378_ENSG00000172057.6.txt")
head(expr)
```

    ##    sample geno      exp
    ## 1 HG00367  A/G 28.96038
    ## 2 NA20768  A/G 20.24449
    ## 3 HG00361  A/A 31.32628
    ## 4 HG00135  A/A 34.11169
    ## 5 NA18870  G/G 18.25141
    ## 6 NA11993  A/A 32.89721

``` r
summary(expr)
```

    ##      sample     geno          exp        
    ##  HG00096:  1   A/A:108   Min.   : 6.675  
    ##  HG00097:  1   A/G:233   1st Qu.:20.004  
    ##  HG00099:  1   G/G:121   Median :25.116  
    ##  HG00100:  1             Mean   :25.640  
    ##  HG00101:  1             3rd Qu.:30.779  
    ##  HG00102:  1             Max.   :51.518  
    ##  (Other):456

``` r
inds <- expr$geno == "G/G"
summary(expr[inds, "exp"])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   6.675  16.903  20.074  20.594  24.457  33.956

``` r
boxplot(exp ~ geno, data = expr)
```

![](class13_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
