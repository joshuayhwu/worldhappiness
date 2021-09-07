In Economics, there is a theory called “Easterlin Paradox”. Where the
country’s GDP growth doesn’t improve happiness in the long-run.

To test this theory, my goal was to use the same data the World
Happiness Report uses from 2015 - 2019, originated from GallUp Polls, to
generate a model to find significant factors contributing to a country’s
happiness index.

Loading libraries
-----------------

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(corrplot)

    ## corrplot 0.89 loaded

    library(ggplot2)
    library(ggpubr)

    ## Warning: package 'ggpubr' was built under R version 4.1.1

    library(aod)

    ## Warning: package 'aod' was built under R version 4.1.1

    library(MASS)

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    library(stargazer)

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

Loading World Happiness Data from 2015 - 2019
---------------------------------------------

    whr.raw.2015 = read.csv("2015.csv")
    whr.raw.2016 = read.csv("2016.csv")
    whr.raw.2017 = read.csv("2017.csv")
    whr.raw.2018 = read.csv("2018.csv")
    whr.raw.2019 = read.csv("2019.csv")

First check if the dimensions of the data sets match

    # Checking Dimensions
    paste("WHR 2015 Data has", dim(whr.raw.2015)[1], "rows and", dim(whr.raw.2015)[2], "columns", sep=" ")

    ## [1] "WHR 2015 Data has 158 rows and 12 columns"

    paste("WHR 2016 Data has", dim(whr.raw.2016)[1], "rows and", dim(whr.raw.2016)[2], "columns", sep=" ")

    ## [1] "WHR 2016 Data has 157 rows and 13 columns"

    paste("WHR 2017 Data has", dim(whr.raw.2017)[1], "rows and", dim(whr.raw.2017)[2], "columns", sep=" ")

    ## [1] "WHR 2017 Data has 155 rows and 12 columns"

    paste("WHR 2018 Data has", dim(whr.raw.2018)[1], "rows and", dim(whr.raw.2018)[2], "columns", sep=" ")

    ## [1] "WHR 2018 Data has 156 rows and 9 columns"

    paste("WHR 2019 Data has", dim(whr.raw.2019)[1], "rows and", dim(whr.raw.2019)[2], "columns", sep=" ")

    ## [1] "WHR 2019 Data has 156 rows and 9 columns"

It appears some of the countries are not reported in the subsequent
reports. Furthermore, columns like happiness rank, standard error and
dystopian residual is missing from subsequent years of report.

Lets check for potential missing values in the data set as well.

    # Checking Missing Values
    sapply(whr.raw.2015, function(x) sum(is.na(x)))

    ##                       Country                        Region 
    ##                             0                             0 
    ##                Happiness.Rank               Happiness.Score 
    ##                             0                             0 
    ##                Standard.Error      Economy..GDP.per.Capita. 
    ##                             0                             0 
    ##                        Family      Health..Life.Expectancy. 
    ##                             0                             0 
    ##                       Freedom Trust..Government.Corruption. 
    ##                             0                             0 
    ##                    Generosity             Dystopia.Residual 
    ##                             0                             0

    sapply(whr.raw.2016, function(x) sum(is.na(x)))

    ##                       Country                        Region 
    ##                             0                             0 
    ##                Happiness.Rank               Happiness.Score 
    ##                             0                             0 
    ##     Lower.Confidence.Interval     Upper.Confidence.Interval 
    ##                             0                             0 
    ##      Economy..GDP.per.Capita.                        Family 
    ##                             0                             0 
    ##      Health..Life.Expectancy.                       Freedom 
    ##                             0                             0 
    ## Trust..Government.Corruption.                    Generosity 
    ##                             0                             0 
    ##             Dystopia.Residual 
    ##                             0

    sapply(whr.raw.2017, function(x) sum(is.na(x)))

    ##                       Country                Happiness.Rank 
    ##                             0                             0 
    ##               Happiness.Score                  Whisker.high 
    ##                             0                             0 
    ##                   Whisker.low      Economy..GDP.per.Capita. 
    ##                             0                             0 
    ##                        Family      Health..Life.Expectancy. 
    ##                             0                             0 
    ##                       Freedom                    Generosity 
    ##                             0                             0 
    ## Trust..Government.Corruption.             Dystopia.Residual 
    ##                             0                             0

    sapply(whr.raw.2018, function(x) sum(is.na(x)))

    ##                 Overall.rank            Country.or.region 
    ##                            0                            0 
    ##                        Score               GDP.per.capita 
    ##                            0                            0 
    ##               Social.support      Healthy.life.expectancy 
    ##                            0                            0 
    ## Freedom.to.make.life.choices                   Generosity 
    ##                            0                            0 
    ##    Perceptions.of.corruption 
    ##                            0

    sapply(whr.raw.2019, function(x) sum(is.na(x)))

    ##                 Overall.rank            Country.or.region 
    ##                            0                            0 
    ##                        Score               GDP.per.capita 
    ##                            0                            0 
    ##               Social.support      Healthy.life.expectancy 
    ##                            0                            0 
    ## Freedom.to.make.life.choices                   Generosity 
    ##                            0                            0 
    ##    Perceptions.of.corruption 
    ##                            0

It seems like there aren’t any missing values across all 5 data sets

Data Cleaning
-------------

First drop the columns that are only present in some iterations of whr

    # Dropping columns that are not present across all data sets
    whr.raw.2015 = subset(whr.raw.2015, select = -c(Dystopia.Residual, Standard.Error))
    whr.raw.2016 = subset(whr.raw.2016, select = -c(Lower.Confidence.Interval, 
                                                    Upper.Confidence.Interval,
                                                    Dystopia.Residual))
    whr.raw.2017 = subset(whr.raw.2017, select = -c(Whisker.high, 
                                                    Whisker.low,
                                                    Dystopia.Residual))

Standardizing the columns across all 5 data set

    # Standardized column names to "Country", "Region", "Happiness.Rank", "Happiness.Score", "GDP.per.Capita", "Freedom", "Trust..Government.Corruption", "Generosity", "Health..Life.Expectancy.", "Social Support"

    # 2015 Data - Family is rename social support in later data sets
    whr.raw.2015 = whr.raw.2015 %>%
      rename(
        GDP.per.Capita = Economy..GDP.per.Capita.,
        Social.support = Family
      )

    # 2016 Data
    whr.raw.2016 = whr.raw.2016 %>%
      rename(
        GDP.per.Capita = Economy..GDP.per.Capita.,
        Social.support = Family
      )

    # 2017 Data
    whr.raw.2017 = whr.raw.2017 %>%
      rename(
        GDP.per.Capita = Economy..GDP.per.Capita.,
        Social.support = Family
      )

    # 2018 Data
    whr.raw.2018 = whr.raw.2018 %>%
      rename(
        Trust..Government.Corruption. = Perceptions.of.corruption,
        Happiness.Rank = Overall.rank,
        Happiness.Score = Score,
        Country = Country.or.region,
        Freedom = Freedom.to.make.life.choices,
        Health..Life.Expectancy. = Healthy.life.expectancy,
        GDP.per.Capita = GDP.per.capita
      )

    # 2019 Data
    whr.raw.2019 = whr.raw.2019 %>%
      rename(
        Trust..Government.Corruption. = Perceptions.of.corruption,
        Happiness.Rank = Overall.rank,
        Happiness.Score = Score,
        Country = Country.or.region,
        Freedom = Freedom.to.make.life.choices,
        Health..Life.Expectancy. = Healthy.life.expectancy,
        GDP.per.Capita = GDP.per.capita
      )

Add the Region column back to 2017 to 2019 data and add missing values

    # Join
    regions = whr.raw.2015[,c("Country", "Region")]
    whr.raw.2017 = merge(x=whr.raw.2017, y = regions, by = "Country", all.x=TRUE)
    whr.raw.2018 = merge(x=whr.raw.2018, y = regions, by = "Country", all.x=TRUE)
    whr.raw.2019 = merge(x=whr.raw.2019, y = regions, by = "Country", all.x=TRUE)

    # Manually fillling up Region Values - 2017
    whr.raw.2017[14, "Region"] = 'Latin America and Caribbean'
    whr.raw.2017[56, "Region"] = 'Eastern Asia'
    whr.raw.2017[97, "Region"]= 'Sub-Saharan Africa'
    whr.raw.2017[125, "Region"]= 'Sub-Saharan Africa'
    whr.raw.2017[128, "Region"]= 'Sub-Saharan Africa'
    whr.raw.2017[135, "Region"] = 'Eastern Asia'

    # Manually filling in data - 2018
    whr.raw.2018[14, "Region"] = 'Latin America and Caribbean'
    whr.raw.2018[98, "Region"] = 'Sub-Saharan Africa'
    whr.raw.2018[105, "Region"]= "Western Europe"
    whr.raw.2018[126, "Region"]= 'Sub-Saharan Africa'
    whr.raw.2018[129, "Region"]= 'Sub-Saharan Africa'
    whr.raw.2018[141, "Region"] = "Latin America and Caribbean"

    # Manually filling in data - 2019
    whr.raw.2019[47, "Region"] = 'Sub-Saharan Africa'
    whr.raw.2019[97, "Region"] = 'Sub-Saharan Africa'
    whr.raw.2019[104, "Region"]= "Western Europe"
    whr.raw.2019[105, "Region"]= "Western Europe"
    whr.raw.2019[126, "Region"]= 'Sub-Saharan Africa'
    whr.raw.2019[129, "Region"]= 'Sub-Saharan Africa'
    whr.raw.2019[141, "Region"] = "Latin America and Caribbean"


    # Impute trust score based on average of UAE Trust score in other years
    whr.raw.2018[147, "Trust..Government.Corruption."] = "0.31"

Concatenate the data frames

    whr.raw.2015["Year"] = 2015
    whr.raw.2016["Year"] = 2016
    whr.raw.2017["Year"] = 2017
    whr.raw.2018["Year"] = 2018
    whr.raw.2019["Year"] = 2019

    whr.all = rbind(whr.raw.2015, whr.raw.2016, whr.raw.2017, whr.raw.2018, whr.raw.2019)
    whr.all$Trust..Government.Corruption. = as.numeric(whr.all$Trust..Government.Corruption.)
    head(whr.all)

    ##       Country         Region Happiness.Rank Happiness.Score GDP.per.Capita
    ## 1 Switzerland Western Europe              1           7.587        1.39651
    ## 2     Iceland Western Europe              2           7.561        1.30232
    ## 3     Denmark Western Europe              3           7.527        1.32548
    ## 4      Norway Western Europe              4           7.522        1.45900
    ## 5      Canada  North America              5           7.427        1.32629
    ## 6     Finland Western Europe              6           7.406        1.29025
    ##   Social.support Health..Life.Expectancy. Freedom Trust..Government.Corruption.
    ## 1        1.34951                  0.94143 0.66557                       0.41978
    ## 2        1.40223                  0.94784 0.62877                       0.14145
    ## 3        1.36058                  0.87464 0.64938                       0.48357
    ## 4        1.33095                  0.88521 0.66973                       0.36503
    ## 5        1.32261                  0.90563 0.63297                       0.32957
    ## 6        1.31826                  0.88911 0.64169                       0.41372
    ##   Generosity Year
    ## 1    0.29678 2015
    ## 2    0.43630 2015
    ## 3    0.34139 2015
    ## 4    0.34699 2015
    ## 5    0.45811 2015
    ## 6    0.23351 2015

After we are done with the combined dataset, we can work on some
visualizations!

Exploratory Data Analysis
-------------------------

Spearman Rank Correlation
-------------------------

Since the rank is an ordinal variable, we need to use Spearman’s rank
correlation the explore the crucial variables for our model. We will
focus on 2015.

    cor.test(x=whr.raw.2015$GDP.per.Capita, y=whr.raw.2015$Happiness.Rank, method = 'spearman')

    ## Warning in cor.test.default(x = whr.raw.2015$GDP.per.Capita, y =
    ## whr.raw.2015$Happiness.Rank, : Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  whr.raw.2015$GDP.per.Capita and whr.raw.2015$Happiness.Rank
    ## S = 1180421, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.7957028

    cor.test(x=whr.raw.2015$Social.support, y=whr.raw.2015$Happiness.Rank, method = 'spearman')

    ## Warning in cor.test.default(x = whr.raw.2015$Social.support, y =
    ## whr.raw.2015$Happiness.Rank, : Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  whr.raw.2015$Social.support and whr.raw.2015$Happiness.Rank
    ## S = 1163550, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## -0.770038

    cor.test(x=whr.raw.2015$Health..Life.Expectancy., y=whr.raw.2015$Happiness.Rank, method = 'spearman')

    ## Warning in cor.test.default(x = whr.raw.2015$Health..Life.Expectancy., y =
    ## whr.raw.2015$Happiness.Rank, : Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  whr.raw.2015$Health..Life.Expectancy. and whr.raw.2015$Happiness.Rank
    ## S = 1154954, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.7569604

    cor.test(x=whr.raw.2015$Freedom, y=whr.raw.2015$Happiness.Rank, method = 'spearman')

    ## Warning in cor.test.default(x = whr.raw.2015$Freedom, y =
    ## whr.raw.2015$Happiness.Rank, : Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  whr.raw.2015$Freedom and whr.raw.2015$Happiness.Rank
    ## S = 1036229, p-value = 2.301e-15
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.5763522

    cor.test(x=whr.raw.2015$Trust..Government.Corruption., y=whr.raw.2015$Happiness.Rank, method = 'spearman')

    ## Warning in cor.test.default(x = whr.raw.2015$Trust..Government.Corruption., :
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  whr.raw.2015$Trust..Government.Corruption. and whr.raw.2015$Happiness.Rank
    ## S = 856142, p-value = 0.0001126
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.3023969

    cor.test(x=whr.raw.2015$Generosity, y=whr.raw.2015$Happiness.Rank, method = 'spearman')

    ## Warning in cor.test.default(x = whr.raw.2015$Generosity, y =
    ## whr.raw.2015$Happiness.Rank, : Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  whr.raw.2015$Generosity and whr.raw.2015$Happiness.Rank
    ## S = 770729, p-value = 0.03024
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## -0.172463

It appears that the rank is negatively correlated with the variables in
2015. In other words, higher measures of coefficient lowers the
country’s happiness rank.

### Pairplot

    whr.all.num = whr.all[, -c(1,2,3, 11)]
    pairs(whr.all.num, pch=19, lower.panel=NULL)

![](WHA_files/figure-markdown_strict/unnamed-chunk-10-1.png)

The pairplot shows the general trend between each variables.
Unfortunately, there seems to be significant overplotting due to the
number of countries involved. We will have to used correlation plot
instead.

### Correlation Plot

    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(cor(whr.all.num), col=col(200),  type="upper", 
             addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE )

![](WHA_files/figure-markdown_strict/unnamed-chunk-11-1.png)

The correlation plot shows the correlation between each variable with 1
illustrating strong positive correlation and -1 being strong negative
correlation.

It appears that average Happiness score is strongly correlated with GDP,
Life expectancy, and Freedom, while generosity has a weaker
relationship.

It is interesting that the rank version and the average version shows
comptely opposite stories.

### Top 10 Countries for all average variables

Lets take the average of the measures across the years and see which
country is the best in each category.

    top10 = whr.all %>% 
      group_by(Country) %>%
      summarise(avg_happy = mean(Happiness.Score), avg_gdp = mean(GDP.per.Capita),
                avg_social = mean(Social.support), avg_life = mean(Health..Life.Expectancy.),
                avg_free = mean(Freedom), avg_trust = mean(Trust..Government.Corruption.))

    top10happy = ggplot(data.frame(top10[order(top10$avg_happy, decreasing = TRUE),][c(1:10),]),aes(x=reorder(as.factor(Country), -avg_happy), y=avg_happy)) + geom_point() + 
      theme(axis.text.x = element_text(angle=45)) +
        xlab("Country") +
      ylab("Happiness") +
      ggtitle("Top 10 Countries with highest average Happiness")

    top10gdp = ggplot(data.frame(top10[order(top10$avg_happy, decreasing = TRUE),][c(1:10),]),aes(x=reorder(as.factor(Country), -avg_gdp), y=avg_gdp)) + geom_point() + 
      theme(axis.text.x = element_text(angle=45)) +
        xlab("Country") +
      ylab("GDP Per Capita") +
      ggtitle("Top 10 Countries with highest average GDP per Capita Countries")

    top10life = ggplot(data.frame(top10[order(top10$avg_happy, decreasing = TRUE),][c(1:10),]),aes(x=reorder(as.factor(Country), -avg_life), y=avg_life)) + geom_point() + 
      theme(axis.text.x = element_text(angle=45)) +
      xlab("Country") +
      ylab("Health Life Expectancy") +
      ggtitle("Top 10 Countries with highest average Health Life Expectancy")

    top10free = ggplot(data.frame(top10[order(top10$avg_happy, decreasing = TRUE),][c(1:10),]),aes(x=reorder(as.factor(Country), -avg_free), y=avg_free)) + geom_point() + 
      theme(axis.text.x = element_text(angle=45)) +
      xlab("Country") +
      ylab("Freedom") +
      ggtitle("Top 10 Countries with highest average Freedom")

    top10trust = ggplot(data.frame(top10[order(top10$avg_happy, decreasing = TRUE),][c(1:10),]),aes(x=reorder(as.factor(Country), -avg_trust), y=avg_trust)) + geom_point() + 
      theme(axis.text.x = element_text(angle=45)) +
        xlab("Country") +
      ylab("Trust for Government") +
      ggtitle("Top 10 Countries with highest with highest average trust for government")

    top10social = ggplot(data.frame(top10[order(top10$avg_happy, decreasing = TRUE),][c(1:10),]),aes(x=reorder(as.factor(Country), -avg_social), y=avg_social)) + geom_point() +
      theme(axis.text.x = element_text(angle=45)) +
        xlab("Country") +
      ylab("Social Support") +
      ggtitle("Top 10 Countries with highest average Social Support")

    ggarrange(top10happy, top10gdp, top10life,
              top10free, top10trust, top10social,
              ncol = 1, nrow = 2)

    ## $`1`

![](WHA_files/figure-markdown_strict/unnamed-chunk-19-1.png)

    ## 
    ## $`2`

![](WHA_files/figure-markdown_strict/unnamed-chunk-19-2.png)

    ## 
    ## $`3`

![](WHA_files/figure-markdown_strict/unnamed-chunk-19-3.png)

    ## 
    ## attr(,"class")
    ## [1] "list"      "ggarrange"

Denmark, Sweden, and Norway are high up across all average measures,
being in every top 10 graph. It seems like the Nordic countries have a
secret to their happiness. Likewise for Australia and  
New Zealand, the Oceania countries are right behind the Nordic countries
in the average measures.

### Happiness by Region

    regionhappy = whr.all %>% 
      group_by(Region) %>%
      summarise(avg_happy = mean(Happiness.Score))

    ggplot(regionhappy, aes(x=reorder(as.factor(Region), -avg_happy), y=avg_happy)) + geom_point() +
      theme(axis.text.x = element_text(angle=45)) +
        xlab("Country") +
      ylab("Happiness") +
      ggtitle(" Average Happiness by Region")

![](WHA_files/figure-markdown_strict/unnamed-chunk-20-1.png)

Obviously Australia and New Zealand have the highest happiness given
only two countries were involved, pulling the average up. North America
and Western Europe closely follow, while Sub-Saharan Africa has the
lowest average by region. There seemed to be preliminary evidence
against the Easterlin paradox.

Modeling
--------

To start off we will consider an ordinal logistic regression based on
the rank. The choice of the model is due to the happiness score being a
linear combination of the other measures - an automatic positive
correlation. To mitigate this biased, I will regress the happiness
country each year to the other numeric variables. I will also run a
linear regression on the average score on the other average measures

    whr.raw.2018$Trust..Government.Corruption. = as.numeric(whr.raw.2018$Trust..Government.Corruption.)

    olr.2015 = polr(as.factor(Happiness.Rank) ~ GDP.per.Capita + Social.support + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, data = whr.raw.2015, Hess=TRUE)

    olr.2016 = polr(as.factor(Happiness.Rank) ~ GDP.per.Capita + Social.support + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, data = whr.raw.2016, Hess=TRUE)

    olr.2017 = polr(as.factor(Happiness.Rank) ~ GDP.per.Capita + Social.support + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, data = whr.raw.2017, Hess=TRUE)

    olr.2018 = polr(as.factor(Happiness.Rank) ~ GDP.per.Capita + Social.support + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, data = whr.raw.2018, Hess=TRUE)

    olr.2019 = polr(as.factor(Happiness.Rank) ~ GDP.per.Capita + Social.support + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, data = whr.raw.2015, Hess=TRUE)


    stargazer(olr.2015, olr.2016, olr.2017, olr.2018, olr.2019, title="Happiness",
              align = TRUE, dep.var.labels = "Happiness.Rank", type = "html")

    ## 
    ## <table style="text-align:center"><caption><strong>Happiness</strong></caption>
    ## <tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="5"><em>Dependent variable:</em></td></tr>
    ## <tr><td></td><td colspan="5" style="border-bottom: 1px solid black"></td></tr>
    ## <tr><td style="text-align:left"></td><td colspan="5">Happiness.Rank</td></tr>
    ## <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td></tr>
    ## <tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">GDP.per.Capita</td><td>-2.443<sup>***</sup></td><td>-2.598<sup>***</sup></td><td>-3.039<sup>***</sup></td><td>-3.615<sup>***</sup></td><td>-2.443<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.723)</td><td>(0.734)</td><td>(0.747)</td><td>(0.792)</td><td>(0.723)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Social.support</td><td>-5.212<sup>***</sup></td><td>-4.572<sup>***</sup></td><td>-4.799<sup>***</sup></td><td>-3.919<sup>***</sup></td><td>-5.212<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(0.851)</td><td>(0.829)</td><td>(0.777)</td><td>(0.732)</td><td>(0.851)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Health..Life.Expectancy.</td><td>-3.643<sup>***</sup></td><td>-4.811<sup>***</sup></td><td>-4.623<sup>***</sup></td><td>-2.729<sup>**</sup></td><td>-3.643<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(1.091)</td><td>(1.226)</td><td>(1.196)</td><td>(1.188)</td><td>(1.091)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Freedom</td><td>-4.111<sup>***</sup></td><td>-5.171<sup>***</sup></td><td>-5.920<sup>***</sup></td><td>-5.486<sup>***</sup></td><td>-4.111<sup>***</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(1.265)</td><td>(1.358)</td><td>(1.260)</td><td>(1.136)</td><td>(1.265)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Trust..Government.Corruption.</td><td>-3.006<sup>**</sup></td><td>-2.840<sup>*</sup></td><td>-1.861</td><td>-3.134</td><td>-3.006<sup>**</sup></td></tr>
    ## <tr><td style="text-align:left"></td><td>(1.484)</td><td>(1.693)</td><td>(1.955)</td><td>(2.174)</td><td>(1.484)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td style="text-align:left">Generosity</td><td>-1.164</td><td>-0.991</td><td>-0.987</td><td>-0.498</td><td>-1.164</td></tr>
    ## <tr><td style="text-align:left"></td><td>(1.231)</td><td>(1.199)</td><td>(1.156)</td><td>(1.709)</td><td>(1.231)</td></tr>
    ## <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td></tr>
    ## <tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>158</td><td>157</td><td>155</td><td>156</td><td>158</td></tr>
    ## <tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="5" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
    ## </table>

Across all years, the rank is negatively correlated to all variables. In
other words, a unit increase in the measure has an expected reduction of
the <coefficient> unit of rank. This is perhaps a result of the ~150
countries involved in ranking relative to the small values in each
average measure.

    lm.happy = lm(Happiness.Score ~  GDP.per.Capita + Social.support + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, data = whr.all.num)
    summary(lm.happy)

    ## 
    ## Call:
    ## lm(formula = Happiness.Score ~ GDP.per.Capita + Social.support + 
    ##     Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + 
    ##     Generosity, data = whr.all.num)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.92978 -0.32560  0.01677  0.35611  1.66219 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    2.17669    0.07978  27.284  < 2e-16 ***
    ## GDP.per.Capita                 1.14193    0.08296  13.764  < 2e-16 ***
    ## Social.support                 0.64346    0.08068   7.975 5.45e-15 ***
    ## Health..Life.Expectancy.       1.00742    0.13137   7.669 5.22e-14 ***
    ## Freedom                        1.47811    0.16321   9.057  < 2e-16 ***
    ## Trust..Government.Corruption.  0.86255    0.22310   3.866 0.000120 ***
    ## Generosity                     0.59189    0.17561   3.371 0.000787 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5497 on 775 degrees of freedom
    ## Multiple R-squared:  0.7641, Adjusted R-squared:  0.7623 
    ## F-statistic: 418.5 on 6 and 775 DF,  p-value: < 2.2e-16

As seen from the average regression, GDP, Life Expectancy, and Freedom
are indeed positively correlated with the average happiness score.

Overall, there is somewhat ambiguous evidence regarding the Easterlin
paradox. However, it seems like GDP, Life Expectancy, and Freedom helps
a country’s happiness on an aggregate level. For immigrants with a
preference in happiness, the Nordic countries and Oceania is the optimal
choice.

Refernece
---------

-   Source of Data:
    <a href="https://www.kaggle.com/unsdsn/world-happiness" class="uri">https://www.kaggle.com/unsdsn/world-happiness</a>
-   Spearman Correlation:
    <a href="https://www.kaggle.com/unsdsn/world-happiness" class="uri">https://www.kaggle.com/unsdsn/world-happiness</a>
-   Ordinal Logistic Regression:
    <a href="https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/" class="uri">https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/</a>
