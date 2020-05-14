install.packages(c("vegan", "tidyverse", "sars", "sf", "raster", "ggthemes", "ggmap"))
library (vegan)
library (tidyverse)

#-- Conflicts ------------------------------------------ tidyverse_conflicts() --
#x tidyr::extract() masks raster::extract()
#x dplyr::filter()  masks stats::filter()
#x dplyr::lag()     masks stats::lag()
#x dplyr::select()  masks raster::select()

library (sars)
library (sf)
library (raster)                                  #installation of libraries packages
library (ggthemes)
library (ggmap)

print("Hello World!")                             # print function ### First command

                                                  ### Few mathematical operations
23
1+1
2-1
3*2
9/2
9%/%2                                             # ?"%/%" = the time that 9 can be divided by 2
9%%2                                              # ?"%%" = scarto intero della divisione 9/2 #https://stackoverflow.com/questions/30257819/what-does-the-double-percentage-sign-mean
3^2
9^(1/2)

### Boolean operators

1 == 1                                           # ?"==" i.g is true? or false that 1==1 ?
1 == 2
1 < 2
1:3 > 2
1:3 >= 2
1:3 <= 2
2 + 2 == 5
2 + 2 != 5
9/2 == (9%/%2 + (9%%2)/2)
                                                 #atomic elements are . - !
1:10                                             ### Sequencies and repetitions
seq(1, 10)                                       # seq function
?seq                                             #sequence generation
seq(1, 11, 2)                                    # sequenza da 1 a 11 solo numeri primi "2"
rep(1, 10)                                       # rep function
rep("Hi!", 3)
rep(T, 7)
help("rep")                                      # help function the previewsly #Replicate Elements of Vectors and Lists
rep(c("A", "B"), each = 5)                       #[1] "A" "A" "A" "A" "A" "B" "B" "B" "B" "B"
rep(c("A", "B"), length = 9)                     #[1] "A" "B" "A" "B" "A" "B" "A" "B" "A"
rep(1:3, each = 5)                               #[1] 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3
rep(1:3, length = 8)

### Vectors

v <- 1:3                                         #built a vettor
v                                                #[1] 1 2 3
class(v)                                         # class function = "integer

v <- c(1.5, pi, 3.66)                            # c function
v                                                #1.500000 3.141593 3.660000
class(v)                                         # "numeric"

v <- c("Pinus", "Salix", "Populus")              # vector built
v
class(v)                                         # "character"

v <- c(TRUE, FALSE, TRUE)
v
class(v)                                        #"logical"

LETTERS                                         #"A" "B" "C" "D" "E" "F" "G" "H"......"Z"

### Exercise 1, which type of vector is "v <- 1:10"?

v <- 1:10                                       
class(v)                                       # "integer"

### Exercise 1b, create a vector of integers from 10 to 1 (length = 10)

10:1
#or
v <- 10:1

### Subsetting and modifying vectors

v <- 1:10
v
v[5]
v[10]                                           #[1] 10
v[10] <- 1
v                                               #[1] 1 2 3 4 5 6 7 8 9 1
v[c(1, 3)]
v[c(3, 1)]                                      #[1] 3 1

v <- 1:10
length(v)                                       # length function = [1] 10 lunga 10 num
v[length(v)]                                    # the same but in other way
v[length(v) - 1]                                # less last value = 9
v[v >= 5]                                       # only value major or ugual then 5
a <- c(1, 10)
v[v == a]                                       #1 10
LETTERS[1:10]                                   #[1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
v <- 1:3
v[c(TRUE, FALSE, TRUE)]                         #[1] 1 3
v[c(TRUE, TRUE, TRUE)]                          # 1 2 3

### Exercise 2, extract the second to last(penultimo) and the last elements from the following vector
### you should extract both by using length() and ...

v <- c(1:4, LETTERS[1:10], 23.5, 67, 91, 21, 7:-3)
v                                                        #see vector
v[c(length(v) - 1, length(v))]                           #extrapolate second to last and last variable of vector

### Exercise 2b, determine the type of the previous vector

class(v)                                                 # note that the vector was "coerced" to character!

### Operations with vectors

v <- 1:10
v
sum(v)                                                  # sum function = 55
v + 1                                                   # 2  3  4  5  6  7  8  9 10 11
v * 2                                                   # 2  4  6  8 10 12 14 16 18 20
v + v                                                   # 2  4  6  8 10 12 14 16 18 20

# more on vectors https://data-flair.training/blogs/r-vector/#
