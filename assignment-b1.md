Making a Function
================
Nicola Rammell

- [Introduction](#introduction)
  - [Load libraries](#load-libraries)
- [Exercise 1: Make a Function (25
  points)](#exercise-1-make-a-function-25-points)
- [Exercise 2: Document your Function (20
  points)](#exercise-2-document-your-function-20-points)
- [Exercise 3: Include Examples (15
  points)](#exercise-3-include-examples-15-points)
- [Exercise 4: Test the Function (25
  points)](#exercise-4-test-the-function-25-points)

## Introduction

This STAT 545B Assignment B1 covers how to make, fortify, and test a
function.

### Load libraries

First, load the packages we will use in this assignment.

``` r
# load libraries
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rlang)  
```

    ## 
    ## Attaching package: 'rlang'
    ## 
    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl,
    ##     flatten_raw, invoke, splice

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following objects are masked from 'package:rlang':
    ## 
    ##     is_false, is_null, is_true
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
# load datasets
library(datateachr)
library(palmerpenguins)
```

## Exercise 1: Make a Function (25 points)

**In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.**

For this assignment, I will be making a plot function. This function
will be based closely off my STAT 545A Mini Data Analysis, which I
completed using the “Vancouver Street Tree Dataset” from the
`datateachr` package.

In particular, in the Mini Data Analysis Deliverable 1, I generated a
plot using data for a specific tree genus of interest. The plot showed
the distribution of tree diameter values for each species within a
genus. In the Mini Data Analysis, I discussed that exploring this
pattern for only genus was a major limitation, and I am interested in
being able to quickly generate the same plot for multiple genera of
interest.

In this exercise, I will explore how to make a function that can easily
repeat a similar plot for any tree genus (or other data!). I would like
to make this function because I am not only interested in plotting data
for one genus; I would like the ability to quickly explore a plot unique
to any genus of interest.

This would be a useful function because otherwise, a large code chunk
would need to be repeated to produce each plot. If the data analysis
requires a separate plot for several genera of interest, this would make
the script long and it would be easy to make errors in the copy, paste,
and edit workflow.

The function that I would like to make will take in a dataframe,
manipulate the data, and then plot it. Specifically, the function will
reorder at categorical variable by a numeric variable, and plot the
results in the form of a boxplot that shows categories arranged from
values of highest to lowest. This allows us to quickly visualize the
overall pattern and helps us identify which category has the largest
values! In addition, this function includes a built-in title feature so
you will be required to keep track of exactly what data you are
plotting.

First, we’ll create the basic function. Simply, the function generates a
label with the title of your plot, orders your numerical variable by
your categorical variable, and creates a boxplot with the results. The
function is named as a verb: `order_boxplot`.

``` r
# create function to order and plot a numerical variable by a categorical one

order_boxplot <- function(df, x, y, title) {
  label <- rlang::englue("{{ title }} {{ y }}")         # label = title + y-variable
  
  df %>%
    mutate({{ x }} := fct_reorder({{ x }}, {{ y }})) %>% # order categorical variable
    ggplot(aes(x = {{ x }}, y = {{ y }})) +              # take ordered variables
    geom_boxplot() +                                     # plot the results
    coord_flip() +                                       # then flip the axes
    xlab("") + ylab("") +                                # remove axes labels 
    labs(title = label)                                  # add title + y-variable
}
```

Now, we specify two more features: (1) how to deal with missing values;
and (2) how to ensure the input variables are of the correct class.

This function specifically plots a *numeric* variable, `y`, by a
*categorical* variable, `x`, so we will add an error trap with the input
variables are not of the correct class. As for missing values, we will
specify that NA values should be removed – but no siliently! We want
missing values to be removed *with* a warning that this has occured –
telling us exactly how many rows were removed in making the plot so that
we can check that makes sense. This will be specified in the
`fct_reorder` part of the function as .na_rm = NULL (this removes NAs
but generates a warning). We are doing this because we are just plotting
one variable by a categorical one, so rows that are missing either piece
of information should always be removed.

``` r
# improve the function by adding two features

order_boxplot <- function(df, x, y, title) {              
  
    if(!is.character(df %>% pull({{ x }}))) {           # check x is categorical
    stop('Please provide a categorical input!\n',
         'You have provided an object of class:', class(df %>% pull({{ x }}))[1])
  }  
  
    if(!is.numeric(df %>% pull({{ y }}))) {             # check y is numerical      
    stop('Please provide a numeric input!\n',
         'You have provided an object of class:', class(df %>% pull({{ y }}))[1])
  }
  
  label <- rlang::englue("{{ title }} {{ y }}")         # label = title + variable 
  
  df %>%
    mutate({{ x }} := fct_reorder({{ x }}, {{ y }},      # orders categorical variable
                                  .na_rm = NULL)) %>%    # removes NAs with a warning
    ggplot(aes(x = {{ x }}, y = {{ y }})) +              # takes ordered variables
    geom_boxplot() +                                     # and plots the results
    coord_flip() +                                       # flips the axes
    xlab("") + ylab("") +                                # default no axes labels 
    labs(title = label)                                  # adds title + variable
}
```

## Exercise 2: Document your Function (20 points)

**In the same code chunk where you made your function, document the
function using roxygen2 tags.**

Next, I will add the documentation to the function.

``` r
#' @title
#' Order boxplot 
#' 
#' @description
#' `order_boxplot` returns a figure with boxplots ordered from greatest to least
#'
#' @details
#' This is a generic function that takes any categorical x-variable and any 
#' numerical y-variable as input. The function will order the categorical variable
#' by the numeric one, and then generate a figure with boxplots ordered from values 
#' of greatest to least. The function also takes a 'title' as input, which is 
#' partially specified by the user and partially generated automatically based on
#' the variable name from the dataframe.  
#' 
#' @param df     provide a dataframe (e.g. tibble)
#' @param x      specify the categorical variable to be plotted
#' @param y      specify the numeric variable to be plotted
#' @param title  provide a title that describes your categorical variable  
#' 
#' @return 
#' This function will return a plot (an S3 gg object). 
#' 
#' @examples
#' diamonds %>% 
#'   order_boxplot(cut, price, Diamond)  # e.g. plot title will be Diamond price
#' 
#' penguins %>% 
#'   order_boxplot(species, body_mass_g, Penguin)
#'   
order_boxplot <- function(df, x, y, title) {              
  
    if(!is.character(df %>% pull({{ x }}))) {           # check x is categorical
    stop('Please provide a categorical input!\n',
         'You have provided an object of class:', class(df %>% pull({{ x }}))[1])
  }  
  
    if(!is.numeric(df %>% pull({{ y }}))) {             # check y is numerical      
    stop('Please provide a numeric input!\n',
         'You have provided an object of class:', class(df %>% pull({{ y }}))[1])
  }
  
  label <- rlang::englue("{{ title }} {{ y }}")         # label = title + variable 
  
  df %>%
    mutate({{ x }} := fct_reorder({{ x }}, {{ y }},      # orders categorical variable
                                  .na_rm = NULL)) %>%    # removes NAs with a warning
    ggplot(aes(x = {{ x }}, y = {{ y }})) +              # takes ordered variables
    geom_boxplot() +                                     # and plots the results
    coord_flip() +                                       # flips the axes
    xlab("") + ylab("") +                                # default no axes labels 
    labs(title = label)                                  # adds title + variable
}
```

## Exercise 3: Include Examples (15 points)

**Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.**

Now, let’s try some examples. I will demonstrate using the function on 4
different datasets.

First, I will demonstrate using the function on the `vancouver_trees`
dataset from the `datateachr` package, which is what I originally
designed the function for. Specifically, I would like to see how the
function works to explore patterns in a number of different tree genera!

``` r
# try the function on the Birch genus subset from the vancouver_trees data
vancouver_trees %>%
  filter(genus_name == "BETULA") %>%
  order_boxplot(common_name, diameter, Birch)   # specify "Birch" title
```

![](assignment-b1_files/figure-gfm/Fig%201-1.png)<!-- -->

``` r
# or, customize it (using a preferred theme, add axis label)
vancouver_trees %>%
  filter(genus_name == "BETULA") %>%
  order_boxplot(common_name, diameter, Birch) + # specify "Birch" title
  theme_classic() + 
  ylab("DBH")
```

![](assignment-b1_files/figure-gfm/Fig%202-1.png)<!-- -->

``` r
# try the function on the Ash genus subset from the vancouver_trees data
vancouver_trees %>%
  filter(genus_name == "FRAXINUS") %>%
  order_boxplot(common_name, diameter, Ash)     # specifiy "Ash" title
```

![](assignment-b1_files/figure-gfm/Fig%203-1.png)<!-- -->

``` r
# or, customize it (remove those 3 extreme outliers from the plot)
vancouver_trees %>%
  filter(genus_name == "FRAXINUS") %>%
  order_boxplot(common_name, diameter, Ash) +   # specify "Ash" title
  ylim(0, 45) +                                 # removes 3 outliers from plot
  theme_classic() + 
  ylab("DBH")
```

    ## Warning: Removed 3 rows containing non-finite values (`stat_boxplot()`).

![](assignment-b1_files/figure-gfm/Fig%204-1.png)<!-- -->

``` r
# try the function on the Oak genus subset from the vancouver_trees data
vancouver_trees %>%
  filter(genus_name == "QUERCUS") %>%
  order_boxplot(common_name, diameter, Oak) # specify "Oak" title
```

![](assignment-b1_files/figure-gfm/Fig%205-1.png)<!-- -->

``` r
# or, customize it (remove those 2 extreme outliers from the plot)
vancouver_trees %>%
  filter(genus_name == "QUERCUS") %>%
  order_boxplot(common_name, diameter, Oak) +   # specify "Oak" title
  ylim(0, 60) +                                 # removes 2 outliers from plot
  theme_classic() + 
  ylab("DBH")
```

    ## Warning: Removed 2 rows containing non-finite values (`stat_boxplot()`).

![](assignment-b1_files/figure-gfm/Fig%206-1.png)<!-- -->

Second, I will demonstrate using the function on the `diamonds` dataset
from the `ggplot2` package. This will show that the function can be used
on any dataset (that has both categorical and numeric data).
Additionally, I will demonstrate the use of error trapping (and
correcting) the incorrect variable class type using this dataset.

``` r
# try the function on the diamonds data
diamonds %>%
  order_boxplot(cut, price, Diamond) # specify "Diamond" title
```

    ## Error in order_boxplot(., cut, price, Diamond): Please provide a categorical input!
    ## You have provided an object of class:ordered

``` r
# check the data structure
str(diamonds)
```

    ## tibble [53,940 × 10] (S3: tbl_df/tbl/data.frame)
    ##  $ carat  : num [1:53940] 0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
    ##  $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
    ##  $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
    ##  $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
    ##  $ depth  : num [1:53940] 61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
    ##  $ table  : num [1:53940] 55 61 65 58 58 57 57 55 61 61 ...
    ##  $ price  : int [1:53940] 326 326 327 334 335 336 336 337 337 338 ...
    ##  $ x      : num [1:53940] 3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
    ##  $ y      : num [1:53940] 3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
    ##  $ z      : num [1:53940] 2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

As seen in the above example, the diamonds data has the `clarity`
variable coded as an ordered factor. The function is expecting a
categorical x-variable. So, let’s fix this problem, and then try the
function again:

``` r
# make the ordered factor variable a character variable and then run the function
diamonds %>%
  mutate(cut = as.character(cut)) %>%  # fix variable class
  order_boxplot(cut, price, Diamond)   # and then run the function 
```

![](assignment-b1_files/figure-gfm/Fig%207-1.png)<!-- -->

``` r
# or, customize the plot with a theme and labels 
diamonds %>%
  mutate(cut = as.character(cut)) %>%  
  order_boxplot(cut, price, Diamond) +  
  theme_classic() +                          # add theme        
  xlab("Cut quality") + ylab("Price ($ US)") # add labels 
```

![](assignment-b1_files/figure-gfm/Fig%208-1.png)<!-- -->

As seen above, this corrected the problem, as expected! And, by the
looks of the plot… there is no significant difference in the price of
diamonds based on the quality of the cut.

Next, I will demonstrate the use of the function on the `mpg` dataset
from the `ggplot2` package.

``` r
# first, confirm desired x-variable is categorical and y-variable is numeric
str(mpg)
```

    ## tibble [234 × 11] (S3: tbl_df/tbl/data.frame)
    ##  $ manufacturer: chr [1:234] "audi" "audi" "audi" "audi" ...
    ##  $ model       : chr [1:234] "a4" "a4" "a4" "a4" ...
    ##  $ displ       : num [1:234] 1.8 1.8 2 2 2.8 2.8 3.1 1.8 1.8 2 ...
    ##  $ year        : int [1:234] 1999 1999 2008 2008 1999 1999 2008 1999 1999 2008 ...
    ##  $ cyl         : int [1:234] 4 4 4 4 6 6 6 4 4 4 ...
    ##  $ trans       : chr [1:234] "auto(l5)" "manual(m5)" "manual(m6)" "auto(av)" ...
    ##  $ drv         : chr [1:234] "f" "f" "f" "f" ...
    ##  $ cty         : int [1:234] 18 21 20 21 16 18 18 18 16 20 ...
    ##  $ hwy         : int [1:234] 29 29 31 30 26 26 27 26 25 28 ...
    ##  $ fl          : chr [1:234] "p" "p" "p" "p" ...
    ##  $ class       : chr [1:234] "compact" "compact" "compact" "compact" ...

``` r
# then, try the function on the mpg data, customizing as you like!
mpg %>%
  mutate("displacement" = displ) %>%           # rename displ to displacement
  order_boxplot(model, displacement, Model) +  # run the function
  ylab("Engine displacement (litres)") +       # add y-lab
  theme_classic()                              # add classic theme
```

![](assignment-b1_files/figure-gfm/Fig%209-1.png)<!-- -->

Lastly, I will try the function on the `penguins` data from the
`palmerpenguins` package.

``` r
# try the function on the penguins data
penguins %>%
  order_boxplot(species, body_mass_g, Penguin) # specify "Penguin" title
```

    ## Error in order_boxplot(., species, body_mass_g, Penguin): Please provide a categorical input!
    ## You have provided an object of class:factor

Again, we receive the error that our categorical variable was not coded
as categorical, but this time, as a factor. Correct this error and try
again:

``` r
# correct variable class and customize as you like!
penguins %>%
  mutate(species = as.character(species)) %>%  # correct variable class
  mutate(mass = body_mass_g) %>%               # rename y-variable
  order_boxplot(species, mass, Penguin) +      # run the function
  ylab("Body mass (g)") +                      # customize y-lab
  theme_classic()                              # customize theme
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `species = fct_reorder(species, mass, .na_rm = NULL)`.
    ## Caused by warning:
    ## ! `fct_reorder()` removing 2 missing values.
    ## ℹ Use `.na_rm = TRUE` to silence this message.
    ## ℹ Use `.na_rm = FALSE` to preserve NAs.

    ## Warning: Removed 2 rows containing non-finite values (`stat_boxplot()`).

![](assignment-b1_files/figure-gfm/Fig%2010-1.png)<!-- -->

Here, we get the warning that two NA rows were removed in the making of
this plot. Let’s ensure this makes sense:

``` r
# check structore of body_mass_g variable
summary(penguins$body_mass_g)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    2700    3550    4050    4202    4750    6300       2

``` r
View(penguins) # confirms there are 2 NA values 
```

The `body_mass_g` variable has two missing values, so we are happy with
the warning that let us know two rows were removed in the making of this
figure.

The above boxplot was created by customizing the labels and theme, but
the code is still much more concise using the function than if we were
to write out the full code required to make the plot! And, much more
difficult to make a mistake, or not complete the re-ordering in the same
way each time. Also notice, 2 NA rows were removed to make this plot.

## Exercise 4: Test the Function (25 points)

**Write formal tests for your function. You should use at least three
non-redundant uses of an expect\_() function from the testthat package,
and they should be contained in a test_that() function (or more than
one). They should all pass.**

In the last exercise, we’ll use the `testthat` package to test the
function. I will first test that there is no error when everything is
input correctly using `expect_no_error`:

``` r
# test there is no error when everything is input correctly 
test_that("Test 1: working order_boxplot function", {
  expect_no_error(order_boxplot(vancouver_trees, common_name, diameter, title))
})
```

    ## Test passed 🥇

The above preliminary test shows that there’s no error with these
inputs: they meet the function’s requirements.

Next, I will test that an error *is* thrown when the input variables are
not of the correct class using `expect_error`.

``` r
# make variables of the wrong class to use in the next test
x <- as.factor(vancouver_trees$common_name) 
str(x) # function is expecting the x-variable to be a character, not a factor
```

    ##  Factor w/ 634 levels "ACCOLADE CHERRY",..: 66 296 292 33 259 89 111 111 111 33 ...

``` r
y <- as.character(vancouver_trees$diameter)
str(y) # function is expecting the y-variable to be numeric, not a character
```

    ##  chr [1:146611] "10" "10" "4" "18" "9" "5" "15" "14" "16" "7.5" "7.75" "16" ...

``` r
# now, run the test using variables of the wrong class and check you get an error 
test_that("Test 2: incorrect variable class in order_boxplot function", {
  expect_error(order_boxplot(vancouver_trees, x, diameter, Tree))
  expect_error(order_boxplot(vancouver_trees, common_name, y, Tree))
})
```

    ## Test passed 🌈

Now, we can see that the function *does* throw an error when the
variables are input in the wrong class. This first test, using
`expect_error`, shows that when either the `x` or `y` variables are not
stored as the correct class, we get an error, as expected.

Next, I will test that a particular error message is supplied if the
user forgets to specify a title using `expect_error`. This is important
because as seen above in the `vancouver_trees` examples, we may have
many plots and want each one labelled according to which data went into
the plot.

``` r
test_that("Test 3: missing title in order_boxplot function", {
  expect_error(order_boxplot(vancouver_trees, 
                             common_name, 
                             diameter), "`title` is absent but must be supplied")
})
```

    ## Test passed 😸

Next, I want to test that when the function is run on data that has NA
values, we get a warning, using `expect_warning`. This was seen in the
above example, when the penguin data (that has 2 NA values) generated a
warning letting us know. Let’s test that again here:

``` r
# correct the variable class so we don't get an error
penguins2 <- penguins %>%
  mutate(species = as.character(species))
```

``` r
# now, run the test using df with NA values and check you get a warning 
test_that("Test 4: NA values in order_boxplot function", {
  expect_warning(order_boxplot(penguins2, species, body_mass_g, Penguins))
})
```

    ## Test passed 😸

Next, using `expect_is`, test that the function does indeed create an
S3: gg object!

``` r
test_that("Test 5: return class type of order_boxplot function",{
  myplot <- order_boxplot(vancouver_trees, common_name, diameter, Tree)
  expect_is(myplot, "gg")
})
```

    ## Test passed 😸

Finally, you could also achieve a similar result using the
`expect_equal` function.

``` r
test_that("Test 6: return class type of order_boxplot function", {
  expect_equal(class(order_boxplot(vancouver_trees, common_name, diameter, Tree))[1], "gg")
})
```

    ## Test passed 🎊
