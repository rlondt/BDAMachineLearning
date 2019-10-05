library(tidyverse)

# load file
df <- read_delim(file="./datafiles/gbs.csv", delim = ",")

# factorize sensible columns
df$City <- as.factor(df$City)
df$SalesOrg <- as.factor(df$SalesOrg)
df$Product <- as.factor(df$Product)
df$ProdCat <- as.factor(df$ProdCat)
df$Division <- as.factor(df$Division)

# show stucture and some summary
str(df)

summary(df$YEAR)
summary(df[c("Revenue", "Discount")])

# show quantile, range & IQR for some features
quantile(df$Revenue)
range(df$Revenue)

range(df$YEAR)

diff(range(df$Revenue))

IQR(df$Revenue)

# mean, variance and sd of Revenue
mean(df$Revenue)
var(df$Revenue)
sd(df$Revenue)


# three visualization graphs
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


ggplot(df_sum, aes(y=sum_revenue, x=YEAR))+
  geom_bar(stat="identity")

ggplot(df_sum_salesorg, aes(y=sum_revenue, x=YEAR))+
  geom_bar(stat="identity",aes( fill=SalesOrg ), position = position_fill() )

ggplot(df_sum_salesorg, aes(y=sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=SalesOrg ), position = position_fill() )

ggplot(df_sum_salesorg, aes(y=sum_revenue-sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=SalesOrg ), position = position_fill() )

ggplot(df_sum_prodcat, aes(y=sum_revenue, x=YEAR))+
  geom_bar(stat="identity",aes( fill=ProdCat ), position = position_dodge() )

ggplot(df_sum_prodcat, aes(y=sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=ProdCat ), position = position_dodge() )

ggplot(df_sum_prodcat, aes(y=sum_revenue-sum_discount, x=YEAR))+
  geom_bar(stat="identity",aes( fill=ProdCat ), position = position_dodge() )

