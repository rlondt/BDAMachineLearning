Opdracht Week2 (Onderdeel R)
========================================================
author: Romano
date: 20-02-2019
autosize: true

Inlezen van het bestand 
========================================================


```r
# load file
df <- read_delim(file="../datafiles/gbs.csv", delim = ",")
```
Factorize sensible columns
========================================================

```r
df$City <- as.factor(df$City)
df$SalesOrg <- as.factor(df$SalesOrg)
df$Product <- as.factor(df$Product)
df$ProdCat <- as.factor(df$ProdCat)
df$Division <- as.factor(df$Division)
```
Show stucture and some summary(1)
========================================================

```r
str(df)
```

```
Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame':	25171 obs. of  14 variables:
 $ YEAR      : num  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
 $ MONTH     : num  1 1 1 1 1 1 1 1 1 1 ...
 $ Customer  : num  16000 16000 16000 16000 16000 16000 16000 16000 16000 16000 ...
 $ CustDescr : chr  "Capital Bikes" "Capital Bikes" "Capital Bikes" "Capital Bikes" ...
 $ City      : Factor w/ 5 levels "Berlin","Frankfurt",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ SalesOrg  : Factor w/ 2 levels "DN00","DS00": 1 1 1 1 1 1 1 1 1 1 ...
 $ Product   : Factor w/ 29 levels "BOTL1000","CAGE1000",..: 9 29 20 26 16 23 26 6 26 26 ...
 $ ProdDescr : chr  "E-Bike Tailwind" "T-shirt" "Road Bike Carbon Shimano" "Air Pump" ...
 $ ProdCat   : Factor w/ 6 levels "ACC","EBI","ORB",..: 2 1 4 1 3 5 1 5 1 1 ...
 $ CatDescr  : chr  "E Bike" "Accessoire" "Roadbike" "Accessoire" ...
 $ Division  : Factor w/ 2 levels "AS","BI": 2 1 2 1 2 2 1 2 1 1 ...
 $ SalesQuant: num  1 6 2 3 3 11 1 5 7 8 ...
 $ Revenue   : num  3247.8 146.2 6495.6 68.2 3897.4 ...
 $ Discount  : num  97.44 4.38 194.87 2.04 116.93 ...
 - attr(*, "spec")=
  .. cols(
  ..   YEAR = col_double(),
  ..   MONTH = col_double(),
  ..   Customer = col_double(),
  ..   CustDescr = col_character(),
  ..   City = col_character(),
  ..   SalesOrg = col_character(),
  ..   Product = col_character(),
  ..   ProdDescr = col_character(),
  ..   ProdCat = col_character(),
  ..   CatDescr = col_character(),
  ..   Division = col_character(),
  ..   SalesQuant = col_double(),
  ..   Revenue = col_double(),
  ..   Discount = col_double()
  .. )
```
Show stucture and some summary(2)
========================================================

```r
summary(df$YEAR)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2010    2011    2012    2012    2014    2015 
```

```r
summary(df[c("Revenue", "Discount")])
```

```
    Revenue            Discount      
 Min.   :   14.61   Min.   :   0.43  
 1st Qu.:  157.11   1st Qu.:   5.46  
 Median : 4487.02   Median : 143.02  
 Mean   : 8644.64   Mean   : 313.36  
 3rd Qu.:12991.30   3rd Qu.: 429.08  
 Max.   :90260.87   Max.   :4513.04  
```
Show quantile, range & IQR for some features
========================================================

```r
quantile(df$Revenue)
```

```
      0%      25%      50%      75%     100% 
   14.61   157.11  4487.02 12991.30 90260.87 
```

```r
range(df$Revenue)
```

```
[1]    14.61 90260.87
```

```r
range(df$YEAR)
```

```
[1] 2010 2015
```

```r
diff(range(df$Revenue))
```

```
[1] 90246.26
```

```r
IQR(df$Revenue)
```

```
[1] 12834.19
```
Mean, variance and sd of Revenue
========================================================

```r
mean(df$Revenue)
```

```
[1] 8644.637
```

```r
var(df$Revenue)
```

```
[1] 125974290
```

```r
sd(df$Revenue)
```

```
[1] 11223.83
```
Visualization graphs (dataprep)
========================================================

```r
df$year_as_factor = as.factor(df$YEAR)

df_sum <- df %>%
  group_by(YEAR) %>%
  summarize(
    salescount = n(),
    sum_revenue = sum(Revenue),
    sum_discount = sum(Discount)
  )

df_sum_salesorg <- df %>%
  group_by(YEAR, SalesOrg) %>%
  summarize(
    salescount = n(),
    sum_revenue = sum(Revenue),
    sum_discount = sum(Discount)
  )

df_sum_prodcat <- df %>%
  group_by(YEAR, ProdCat) %>%
  summarize(
    salescount = n(),
    sum_revenue = sum(Revenue),
    sum_discount = sum(Discount)
  )
```
Visualization graphs (revenue by year)
========================================================

```r
ggplot(df_sum, aes(y=sum_revenue, x=YEAR))+
  geom_bar(stat="identity") 
```

![plot of chunk unnamed-chunk-9](Romano-W2.2-V01-figure/unnamed-chunk-9-1.png)
Visualization graphs (revenue by salesorg)
========================================================

```r
ggplot(df_sum_salesorg, aes(y=sum_revenue, x=YEAR))+
  geom_bar(stat="identity",aes( fill=SalesOrg ), position = position_fill() )

ggplot(df_sum_salesorg, aes(y=sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=SalesOrg ), position = position_fill() )

ggplot(df_sum_salesorg, aes(y=sum_revenue-sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=SalesOrg ), position = position_fill() )
```

![plot of chunk unnamed-chunk-10](Romano-W2.2-V01-figure/unnamed-chunk-10-1.png)![plot of chunk unnamed-chunk-10](Romano-W2.2-V01-figure/unnamed-chunk-10-2.png)![plot of chunk unnamed-chunk-10](Romano-W2.2-V01-figure/unnamed-chunk-10-3.png)
Visualization graphs (revenue by productcategory)
========================================================

```r
ggplot(df_sum_prodcat, aes(y=sum_revenue, x=YEAR))+
  geom_bar(stat="identity",aes( fill=ProdCat ), position = position_dodge() )

ggplot(df_sum_prodcat, aes(y=sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=ProdCat ), position = position_dodge() )

ggplot(df_sum_prodcat, aes(y=sum_revenue-sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=ProdCat ), position = position_dodge() )
```

![plot of chunk unnamed-chunk-11](Romano-W2.2-V01-figure/unnamed-chunk-11-1.png)![plot of chunk unnamed-chunk-11](Romano-W2.2-V01-figure/unnamed-chunk-11-2.png)![plot of chunk unnamed-chunk-11](Romano-W2.2-V01-figure/unnamed-chunk-11-3.png)
