### Install "readxl", a package which enables us to directly import .xls and .xlsx in R

install.packages("readxl")                             # imports excel spreadsheet # X41598_2017_5114_MOESM3_ESM <- read_excel("C:/data/41598_2017_5114_MOESM3_ESM.xls")#X41598_2017_5114_MOESM3_ESM <- read_excel("C:/data/41598_2017_5114_MOESM3_ESM.xls")
install.packages("vegan")                              # VEGetation ANalysis (Oksanen et al. 2020)
install.packages("DescTools")                          # gtest fo it

### Load required packages, N.B. while you only need to install a package once (install.packages function)
### You need to load a library every time you start a new R session! # As you buy a book only once, whereas you need to take it from the bookshelf every time you want to read it

library(readxl)
library(vegan)
library(DescTools)

### Import the dataset

tab <- read_excel("C:/data/41598_2017_5114_MOESM3_ESM.xls", sheet = 2, .name_repair = "minimal")

# Exercise 1, what does the "sheet" and ".name_repair" arguments mean?

?read_excel                                           #Read xls and xlsx files

### Let's take a look at the data structure

dim(tab)                                              #1845   35 the first number is the number of rows, the second number is the number of columns

                                                      # N.B. remember this order as it's consistent throughout R, first come rows and then come columns!
str(tab)                                              # structure
head(tab)                                             # head of the matrix

### Let's repair the table, we don't want empty rows or columns and we want MEANINGFUL column names

headers <- c("Life_Form", "Alien/Native", "Species")                                  # here we reshape ex novo the first three column names
islands <- paste(names(tab)[4:ncol(tab)], tab[1, 4:ncol(tab)], sep = " ")             # here we combine island name and time period
headers <- c(headers, islands) # finally, we paste everything together

head(tab)
dim(tab)

tab <- tab[4:nrow(tab), ]                                                   # shape again tab
names(tab) <- headers                                                       # give a different name

### Let's give a check to the current table

head(tab)
dim(tab)
str(tab)                                                                   # It seems the occurence matrix is wrongly encoded as "character", instead of being "integer"

### Let's convert the occurence matrix to "integer", a particular case of "numeric" type

species <- sapply(tab[, 4:ncol(tab)], as.integer)
tab <- cbind.data.frame(tab[, 1:3], species) # and bind everything back together

### Exercise 2, check the current data structure

dim(tab)
head(tab)
str(tab)

### The dataset is almost ready but there could be some errors, let's check if there's any NAs

anyNA(tab[, 1:3])                                     #FALSE
anyNA(tab[,])                                         #TRUE
anyNA(species)                                        #TRUE
which(is.na(species), arr.ind = T)                    #[1,]  32  13
species[which(is.na(species), arr.ind = T)] <- 0      # give an integer value

### Let's build the "final" dataset

tab <- cbind.data.frame(tab[, 1:3], species)
anyNA(tab)                                           #FALSE    

? cbind                                              #Combine R Objects by Rows or Columns
? cbind.data.frame                                   # ||   Take a sequence of vector, matrix or data-frame arguments and combine by columns or rows, respectively. These are generic functions with methods for other R classes.








