CASE STUDY: CHIS (California Health Study)

Exploring Data
Code:
> # Explore the dataset with summary and str
> summary(adult)
      RBMI           BMI_P          RACEHPR2         SRSEX      
 Min.   :1.000   Min.   :12.65   Min.   :1.000   Min.   :1.000  
 1st Qu.:2.000   1st Qu.:22.77   1st Qu.:5.000   1st Qu.:1.000  
 Median :3.000   Median :25.72   Median :6.000   Median :2.000  
 Mean   :2.748   Mean   :26.64   Mean   :5.088   Mean   :1.591  
 3rd Qu.:3.000   3rd Qu.:29.32   3rd Qu.:6.000   3rd Qu.:2.000  
 Max.   :4.000   Max.   :93.72   Max.   :6.000   Max.   :2.000  
    ///
      AB51             POVLL      
 Min.   :-1.0000   Min.   :1.000  
 1st Qu.:-1.0000   1st Qu.:2.000  
 Median :-1.0000   Median :4.000  
 Mean   :-0.7108   Mean   :3.196  
 3rd Qu.:-1.0000   3rd Qu.:4.000  
 Max.   : 3.0000   Max.   :4.000  

> str(adult)
'data.frame':	44346 obs. of  10 variables:
 $ RBMI    : num  3 3 3 2 3 4 3 2 3 3 ...
 ////
  $ POVLL   : num  4 4 4 4 4 4 4 3 4 4 ...
> 
> # Age histogram
> ggplot(adult, aes (x = SRAGE_P)) + 
  geom_histogram()
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
> 
> # BMI histogram
> ggplot(adult, aes (x = BMI_P)) + 
  geom_histogram()
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
> 
> # Age colored by BMI, default binwidth
> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1)


Unusual Values
•	 BMI is symmetrical around 25, the border between healthy and overweight.
•	 There is an unexpectedly large number of very old people.
•	 The proportion of each BMI category is consistent throughout ages.
•	 The dataset indicates that California is a very young population.



Default Binwidths
•	 1.00
•	 1.66
•	 2.00
•	 2.23



Data Cleaning
Code:

> # Remove individual aboves 84
> adult <- adult[adult$SRAGE_P <= 84, ] 
> 
> # Remove individuals with a BMI below 16 and above or equal to 52
> adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]
> 
> # Relabel the race variable
> adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino",
                                                    "Asian",
                                                    "African American",
                                                    "White"))
> 
> # Relabel the BMI categories variable
> adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight",
                                            "Normal-weight",
                                            "Over-weight",
                                            "Obese"))
>

                                                                                  
 
Multiple Histograms
Code: Post-fix
> # The dataset adult is available
> 
> # The color scale used in the plot
> BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")
> 
> # Theme to fix category display in faceted plot
> fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(), 
                    legend.position = "none")
> 
> # Histogram, add BMI_fill and customizations
> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .) +
  theme_classic() +
  fix_strips
>

 
Alternatives
Code:
> # Plot 1 - Count histogram
> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  BMI_fill
> 
> # Plot 2 - Density histogram
> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill
> 
> # Plot 3 - Faceted count histogram
> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  BMI_fill + 
  facet_grid(RBMI ~ .)
> 
> # Plot 4 - Faceted density histogram
> ggplot(adult, aes(x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill + 
  facet_grid(RBMI ~ .)
> 
> # Plot 5 - Density histogram with position = "fill"
> ggplot(adult, aes(x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, position = "fill") +
  BMI_fill
> 
> # Plot 6 - The accurate histogram
> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill
>

Do Things Manually

Code:

> # An attempt to facet the accurate frequency histogram from before (failed)
> ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill +
  facet_grid(RBMI ~ .)
> 
> # Create DF with table()
> DF <- table(adult$RBMI, adult$SRAGE_P)
> 
> # Use apply on DF to get frequency of each group: DF_freq
> DF_freq <- apply(DF, 2, function(x) x/sum(x))
> 
> # Load reshape2 and use melt() on DF_freq to create DF_melted
> library(reshape2)
> DF_melted <- melt(DF_freq)
> 
> # Change names of DF_melted
> names(DF_melted) <- c("FILL", "X", "value")
> 
> # Add code to make this a faceted plot
> ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_bar(stat = "identity", position = "stack") +
  BMI_fill + 
  facet_grid(FILL ~ .)
>



Merimeko/Mosaic Plot
Code: > # The initial contingency table
> DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))
> 
> # Add the columns groupsSum, xmax and xmin. Remove groupSum again.
> DF$groupSum <- rowSums(DF)
> DF$xmax <- cumsum(DF$groupSum)
> DF$xmin <- DF$xmax - DF$groupSum
> # The groupSum column needs to be removed, don't remove this line
> DF$groupSum <- NULL
> 
> # Copy row names to variable X
> DF$X <- row.names(DF)
> 
> # Melt the dataset
> library(reshape2)
> DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")
> 
> # dplyr call to calculate ymin and ymax - don't change
> library(dplyr)

Attaching package: 'dplyr'
The following objects are masked from 'package:stats':

    filter, lag
The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union
> DF_melted <- DF_melted %>% 
  group_by(X) %>% 
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))
> 
> # Plot rectangles - don't change.
> library(ggthemes)
> ggplot(DF_melted, aes(ymin = ymin, 
                 ymax = ymax,
                 xmin = xmin, 
                 xmax = xmax, 
                 fill = FILL)) + 
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()
>



Adding statistics
Code:

> # Perform chi.sq test (RBMI and SRAGE_P)
> results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))
> 
> # Melt results$residuals and store as resid
> resid <- melt(results$residuals)
> 
> # Change names of resid
> names(resid) <- c("FILL", "X", "residual")
> 
> # merge the two datasets:
> DF_all <- merge(DF_melted, resid)
> 
> # Update plot command
> library(ggthemes)
> ggplot(DF_all, aes(ymin = ymin, 
                   ymax = ymax,
                   xmin = xmin, 
                   xmax = xmax, 
                   fill = residual)) + 
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()
>



Adding text

Code:
> # Position for labels on x axis
> DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
> 
> # Position for labels on y axis (don't change)
> index <- DF_all$xmax == max(DF_all$xmax)
> DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
> 
> # Plot
> ggplot(DF_all, aes(ymin = ymin, ymax = ymax, xmin = xmin, 
                   xmax = xmax, fill = residual)) + 
  geom_rect(col = "white") +
  # geom_text for ages (i.e. the x axis)
  geom_text(aes(x = xtext, 
                label = X),
            y = 1,
            size = 3,
            angle = 90,
            hjust = 1,
            show.legend = FALSE) +
  # geom_text for BMI (i.e. the fill axis)
  geom_text(aes(x = max(xmax), 
                y = ytext,
                label = FILL),
            size = 3,
            hjust = 1,
            show.legend = FALSE) +
  scale_fill_gradient2() +
  theme_tufte() +
  theme(legend.position = "bottom")
>






Generalizations
Code: >## Heavy Lifting##

 # Load all packages
> library(ggplot2)
> library(reshape2)
> library(dplyr)
> library(ggthemes)
> 
> # Script generalized into a function
> mosaicGG <- function(data, X, FILL) {
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  DF_melted <- DF_melted %>% 
    group_by(X) %>% 
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")

  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin, 
                          xmax = xmax, fill = residual)) + 
  geom_rect(col = "white") +
  geom_text(aes(x = xtext, label = X),
            y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
  geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
            size = 3, hjust = 1, show.legend = FALSE) +
  scale_fill_gradient2("Residuals") +
  scale_x_continuous("Individuals", expand = c(0,0)) +
  scale_y_continuous("Proportion", expand = c(0,0)) +
  theme_tufte() +
  theme(legend.position = "bottom")
  print(g)}
> 
> # BMI described by age
> mosaicGG(adult, X = "SRAGE_P", FILL = "RBMI")
> 
> # Poverty described by age
> mosaicGG(adult, X = "SRAGE_P", FILL = "POVLL")
> 
> # mtcars: am described by cyl
> mosaicGG(mtcars, X = "cyl", FILL = "am")
Warning message: Chi-squared approximation may be incorrect
> 
> # Vocab: vocabulary described by education
> library(car)
> mosaicGG(Vocab, X = "education", FILL = "vocabulary")
Warning message: Chi-squared approximation may be incorrect
>


