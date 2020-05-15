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


### How many occurences?

occ <- sum(tab[, 4:ncol(tab)])
occ                                                                   # occurences = 10893

### How many species in total?

sp <- sum(rowSums(tab[, 4:ncol(tab)]) > 0)
sp                                                                    # 1831

### How many species in 1830 - 1950

sp_1900 <- sum(rowSums(tab[, grep("1830-1950", names(tab))]) > 0)    # 1601
sp_1900

### How many species in 1951 - 2015

sp_2000 <- sum(rowSums(tab[, grep("1951-2015", names(tab))]) > 0)    # 1541
sp_2000

### Exercise 3, which is the difference among 1951 - 2015 and 1830 - 1950 in percentage?

diff_sp <- ((sp_2000 - sp_1900)/sp_1900)*100
diff_sp                                                              # -3.74......
round(diff_sp, 1)                                                    # -3.7
paste(round(diff_sp, 1), "%", sep = "")                              # "-3.7%"

### How many alien species were found in 1830 - 1950?

al_1900 <- sum(rowSums(tab[tab$`Alien/Native` == "A", grep("1830-1950", names(tab))]) > 0)
al_1900                                                             # [1] 78

### How many alien species were found in 1951 - 2015?

al_2000 <- sum(rowSums(tab[tab$`Alien/Native` == "A", grep("1951-2015", names(tab))]) > 0)
al_2000                                                             # 181

### Exercise 4, which is the percentage of alien species found in 1830 - 1950?

perc_al_1900 <- (al_1900/sp_1900) *100                               #[1] 4.871955
perc_al_1900
round(perc_al_1900, 1)                                                    # 4.9%
paste(round(perc_al_1900, 1), "%", sep = "")

### Exercise 5, which is the percentage of alien species found in 1951 - 2015?

perc_al_2000 <- (al_2000/sp_2000) *100                               #[1] 11.745
perc_al_2000
round(perc_al_2000, 1)                                               # 11.7%
paste(round(perc_al_2000, 1), "%", sep = "")

### Exercise 6, how many native species were found in 1830 - 1950?

nat_1900 <- sp_1900 - al_1900                                        # 1523

### Exercise 7, how many native species were found in 1951 - 2015?

nat_2000 <- sp_2000 - al_2000                                        # 1360

### How much similar are the floras of the two periods?

floras_time <- data.frame(Species = tab$Species,
                          old_flora = as.numeric(rowSums(tab[, grep("1830-1950", names(tab))]) > 0),
                          new_flora = as.numeric(rowSums(tab[, grep("1951-2015", names(tab))]) > 0),
                          Life_Form = tab$Life_Form,
                          `Alien/Native` = tab$`Alien/Native`)
head(floras_time)
dist_jacc <- vegdist(floras_time[, 2:3], method = "jaccard")           ##In vegdist(floras_time[, 2:3], method = "jaccard") :  you have empty rows: their dissimilarities may be meaningless in method “jaccard” 2: In vegdist(floras_time[, 2:3], method = "jaccard") : missing values in results
floras_time[which(rowSums(floras_time[, 2:3]) == 0), 1]                ## build the rows..!
floras_time <- floras_time[-which(rowSums(floras_time[, 2:3]) == 0), ]
dist_jacc <- vegdist(t(floras_time[, 2:3]), method = "jaccard")
sim_jacc <- 1 - dist_jacc
round(sim_jacc, 2)                                                     # old_flora/ new_flora      0.72

### Let's make a barplot of native and alien species in 1830 - 1950 vs 1951 - 2015

mat_floras <- matrix(data = c(nat_1900, al_1900, nat_2000, al_2000),        # matrix preparation
                     nrow = 2)
barplot(mat_floras,                                                         # matrix
        ylim = c(0, 2400),                                                  # y numeric bar, 2400 for correction of the space to insert the an leggend al sp/nat sp.
        space = 1,                                                          # space
        ylab = "Number of species",                                         # ylabel
        names.arg = c("1900", "2000"),                                      # name of the barsplots 1 and 2, nrow=2
        legend.text = c("native species", "alien species"),                 # leggend
        args.legend = list(x = "topright"))

### Let's calculate the contingency tables for Alien/Native species and Lifeforms across the two periods
### and test if proportions differed significantly across the two periods

choros_tab <- matrix(c(sum(floras_time$old_flora[floras_time$Alien.Native != "A"]),
                       sum(floras_time$old_flora[floras_time$Alien.Native == "A"]),
                       sum(floras_time$new_flora[floras_time$Alien.Native != "A"]),
                       sum(floras_time$new_flora[floras_time$Alien.Native == "A"])),
                       ncol = 2)
choros_tab                                                            #[,1] [,2]
                                                                      #[1,] 1523 1360     contingency values for allien and native species
                                                                      #[2,]   78  181     
GTest(choros_tab, correct = "none")                                   # G test= choros_tab= G = 50.191, X-squared df = 1, p-value = 1.395e-12

choros_tab_2 <- round(prop.table(choros_tab, 2) * 100, 1)             # Rounding of Numbers
choros_tab_2 <- paste(choros_tab_2, "%", sep = "")                    #?paste= Concatenate Strings
choros_tab_2 <- matrix(choros_tab_2, ncol = 2)
colnames(choros_tab_2) <- c("1830-1950", "1951-2015")                 # matrix!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
rownames(choros_tab_2) <- c("Native Plants", "Alien Plants")

choros_tab_2                                          #              1830-1950 1951-2015
                                                      # Native Plants "95.1%"   "88.3%"  
                                                      # Alien Plants  "4.9%"    "11.7%"



floras_time$Life_Form_Simpl <- ifelse(floras_time$Life_Form == "T", "AH",
                                      ifelse(floras_time$Life_Form %in% c("Ch", "P"), "W", "PH"))

lf_tab <- matrix(c(sum(floras_time$old_flora[floras_time$Life_Form_Simpl == "AH"]),
                   sum(floras_time$old_flora[floras_time$Life_Form_Simpl == "PH"]),
                   sum(floras_time$old_flora[floras_time$Life_Form_Simpl == "W"]),
                   sum(floras_time$new_flora[floras_time$Life_Form_Simpl == "AH"]),
                   sum(floras_time$new_flora[floras_time$Life_Form_Simpl == "PH"]),
                   sum(floras_time$new_flora[floras_time$Life_Form_Simpl == "W"])),
                   ncol = 2)
lf_tab                                                       #      [,1] [,2]
                                                             #[1,]  636  588   annual= Therophyte
                                                             #[2,]  690  633   perennial= Camephyte
                                                             #[3,]  275  320   woody= Phanerophyte
GTest(lf_tab, correct = "none")                              #G = 6.6002, X-squared df = 2, p-value = 0.03688


#### Make table of percentage and matrix of our value on life forms

lf_tab_2 <- round(prop.table(lf_tab, 2) * 100, 1)
lf_tab_2 <- paste(lf_tab_2, "%", sep = "")
lf_tab_2 <- matrix(lf_tab_2, ncol = 2)
colnames(lf_tab_2) <- c("1830-1950", "1951-2015")
rownames(lf_tab_2) <- c("Annual Plants", "Perennial Herbaceous Plants", "Woody Plants")
lf_tab_2                                                                         #                            1830-1950
                                                                                 # Annual Plants               "39.7%%" 
                                                                                 # Perennial Herbaceous Plants "43.1%%" 
                                                                                 # Woody Plants                "17.2%%" 
                                                                                 #                            1951-2015
                                                                                 # Annual Plants               "38.2%%" 
                                                                                 # Perennial Herbaceous Plants "41.1%%" 
                                                                                 # Woody Plants                "20.8%%" 
# Make a barplot of life forms
AH19 <- 636
PH19 <- 690
W19 <- 275
AH20 <- 588
PH20 <- 633
W20 <- 320

mat_lf <- matrix(data = c(AH19, PH19, W19, AH20, PH20, W20),
                     nrow = 3)
barplot(mat_lf, 
        ylim = c(0, 2400), 
        space = 0.5,
        ylab = "number of species", 
        names.arg = c("1900", "2000"),
        legend.text = c("AH species", "PH species", "W species"),
        args.legend = list(x = "topright"))

## Are native and alien species of the two periods correlated???

### Let's check the native species and the alien spiecies

sr_natives <- specnumber(tab[tab$`Alien/Native` == "N", 3:ncol(tab)], MARGIN = 2)
sr_aliens <- specnumber(tab[tab$`Alien/Native` == "A", 3:ncol(tab)], MARGIN = 2)
sr_natives_1900 <- sr_natives[grep("1830-1950", names(sr_natives))]
sr_natives_2000 <- sr_natives[grep("1951-2015", names(sr_natives))]
sr_aliens_1900 <- sr_aliens[grep("1830-1950", names(sr_aliens))]
sr_aliens_2000 <- sr_aliens[grep("1951-2015", names(sr_aliens))]

                                                                                          # build our data frame of species relationship and sp. area relationship
sr_tab <- data.frame(sr_natives_1900 = sr_natives_1900,
                     sr_natives_2000 = sr_natives_2000,
                     sr_aliens_1900 = sr_aliens_1900,
                     sr_aliens_2000 = sr_aliens_2000)
sr_tab                                                                                    # new matx sr nat and al
rownames(sr_tab) <- c("Argentario", "Argentarola", "Capraia", "Cerboli", "Elba",          #name of the rows= island name
                      "Fobu", "Formica Grande", "Giannutri", "Giglio", "Gorgona",
                      "Isolotto Porto Ercole", "Montecristo", "Palmaiola", "Pianosa",
                      "Scarpa", "Scola")

sr_tab$area <- c(60.23, 0.012, 19.32, 0.089,
                 224.09, 0.0072, 0.099, 2.39,                                             # let's linkage the spiecies richness tab at the islands area
                 21.54, 2.26, 0.063, 10.43,
                 0.0832, 10.28, 0.005, 0.015)
sr_tab                                                                                    # new matrix sr nat, sr al, area

mod_nat <- lm(sr_natives_2000 ~ sr_natives_1900, sr_tab)                                  # Fitting Linear Models
mod_nat                                                                                   # Coefficients: (Intercept)  sr_natives_1900  
                                                                                                         # 17.5830           0.7965 
summary(mod_nat)                                                                          # Estimate Std. Error t value Pr(>|t|)

plot(sr_natives_1900, sr_natives_2000,
     xlab = "Number of native species (1900)",
     ylab = "Number of native species (2000)")                                            # plot species richness
abline(a = mod_nat$coefficients[1],                                                       # ? abline = Add Straight Lines to a Plot
       b = mod_nat$coefficients[2])
text(x = 240, y = 820,
     labels = paste("y = ", 
                    round(mod_nat$coefficients[2], 3), 
                    "x + ",
                    round(mod_nat$coefficients[1], 2),
                    "\n",
                    "R^2 = ",
                    round(summary(mod_nat)$adj.r.squared, 3),
                    ", p < 0.0001 ", sep = ""))                                          # G test



### Exercise 8 Calculate a similar model for alien species in the two periods,
### Plot the data and model statistics

mod_aliens <- lm(sr_aliens_2000 ~ sr_aliens_1900, sr_tab)                                 # Fitting Linear Models
mod_aliens                                                                                # Coefficients: (Intercept)  sr_natives_1900   
summary(mod_aliens)                                                                       # Estimate Std. Error t value Pr(>|t|)

plot(sr_aliens_1900, sr_aliens_2000,
     xlab = "Number of aliens species (1900)",
     ylab = "Number of aliens species (2000)")                                            # plot species richness
abline(a = mod_aliens$coefficients[1],                                                    # ? abline = Add Straight Lines to a Plot
       b = mod_aliens$coefficients[2])
text(x = 10, y = 100,
     labels = paste("y = ", 
                    round(mod_aliens$coefficients[2], 3), 
                    "x + ",
                    round(mod_aliens$coefficients[1], 2),
                    "\n",
                    "R^2 = ",
                    round(summary(mod_aliens)$adj.r.squared, 3),
                    ", p < 0.0001 ", sep = ""))                                           # G test

### And now the relative difference in species richness (natives and aliens) among the two periods
## first start we calculate the relative difference in native spiecies richness among the two period

sr_tab$diff_nat <- (sr_tab$sr_natives_1900 - sr_tab$sr_natives_2000) / sr_tab$sr_natives_2000 * 100   # link rs_tab$diff_nat and the percentage 

plot(sr_tab$area, sr_tab$diff_nat,
     xlab = expression(paste("Island Area (", km^2, ")", sep = "")),
     ylab = "Relative difference native species richness")
abline(a = 0, b = 0, lty = 2)

### Exercise 9, calculate the Relative difference in alien species richness among the two periods and plot it

sr_tab$diff_aliens <- (sr_tab$sr_aliens_1900 - sr_tab$sr_aliens_2000) / sr_tab$sr_aliens_2000 * 100

plot(sr_tab$area, sr_tab$diff_aliens,
     xlab = expression(paste("Island Area (", km^2, ")", sep = "")),
     ylab = "Relative difference aliens species richness")
abline(a = 0, b = 0, lty = 2)

### Plot them together

par(mfrow = c(1, 2))

plot(sr_tab$area, sr_tab$diff_nat,
     xlab = expression(paste("Island Area (", km^2, ")", sep = "")),
     ylab = "Relative difference native species richness")
abline(a = 0, b = 0, lty = 2)

# Requires ex 9 solution
plot(sr_tab$area, sr_tab$diff_aliens,
     xlab = expression(paste("Island Area (", km^2, ")", sep = "")),
     ylab = "Relative difference aliens species richness")
abline(a = 0, b = 0, lty = 2)

par(mfrow = c(1, 1)) # alternatively, you can just close the graphic device with "dev.off()"
dev.off()


### How many species were gained, lost and remained stable across time?

tab_al <- tab[tab$`Alien/Native` == "A", 4:ncol(tab)]

mat_change_al <- matrix(ncol = 16, nrow = nrow(tab_al))                                            #data frame
colnames(mat_change_al) <- c("Argentario", "Argentarola", "Capraia", "Cerboli", "Elba",
                          "Fobu", "Formica Grande", "Giannutri", "Giglio", "Gorgona",
                          "Isolotto Porto Ercole", "Montecristo", "Palmaiola", "Pianosa",
                          "Scarpa", "Scola")                                                       #column names
for (i in 1:16) {                                                                                  # for function, to make a model/algorithm
        tmp <- tab_al[, c(i, 16+i)]
        tmp2 <- ifelse(tmp[, 1] == 1 & tmp[, 2] == 1, "Stable",
                       ifelse(tmp[, 1] == 1 & tmp[, 2] == 0, "Loss",
                              ifelse(tmp[, 1] == 0 & tmp[, 2] == 1, "Gain", "Not Present")))       # matrix/ model through excel stable species, loss species, gain species(absent) 
        mat_change_al[, i] <- tmp2
}

df_change_al <- as.data.frame(mat_change_al)                                                       #data.frame to build matrix of changes in aliens sp

install.packages ("tidyverse")                                                                     #install.packages "tidyverse

library(tidyverse)

df_change_al <- pivot_longer(df_change_al, cols = 1:16)                                            #Pivot data from wide to long

df_change_al <- df_change_al %>% 
        filter(value != "Not Present") %>% 
        group_by(name, value) %>% 
        tally() %>% 
        mutate(n = log1p(n),
               set = "Alien")

tab_nat <- tab[tab$`Alien/Native` != "A", 4:ncol(tab)]

mat_change_nat <- matrix(ncol = 16, nrow = nrow(tab_nat))
colnames(mat_change_nat) <- c("Argentario", "Argentarola", "Capraia", "Cerboli", "Elba",
                             "Fobu", "Formica Grande", "Giannutri", "Giglio", "Gorgona",
                             "Isolotto Porto Ercole", "Montecristo", "Palmaiola", "Pianosa",
                             "Scarpa", "Scola")
for (i in 1:16) {
        tmp <- tab_nat[, c(i, 16+i)]
        tmp2 <- ifelse(tmp[, 1] == 1 & tmp[, 2] == 1, "Stable",
                       ifelse(tmp[, 1] == 1 & tmp[, 2] == 0, "Loss",
                              ifelse(tmp[, 1] == 0 & tmp[, 2] == 1, "Gain", "Not Present")))
        mat_change_nat[, i] <- tmp2
}

df_change_nat <- as.data.frame(mat_change_nat)

df_change_nat <- pivot_longer(df_change_nat, cols = 1:16)

df_change_nat <- df_change_nat %>% 
        filter(value != "Not Present") %>% 
        group_by(name, value) %>% 
        tally() %>% 
        mutate(n = log1p(n),
               set = "Native")

df_change <- bind_rows(df_change_al, df_change_nat)

ggplot(df_change, aes(x = name, colour = value, y = n)) +
        geom_point() +
        scale_y_continuous(trans = scales::log1p_trans()) +
        scale_colour_viridis_d() +
        ylim(c(0, 6)) +
        xlab("") +
        ylab("log(S+1)") +
        labs(colour = "") +
        coord_flip() +
        facet_wrap(~ set) +
        theme_bw() +
        theme(legend.position = "top",
              axis.text = element_text(colour = "black"))                                   #Error Scale for 'y' is already present. Adding another #scale for 'y', which will replace the existing #scale. #Warning message:#Removed 5 rows containing missing values #(geom_point).

##### make a par plot of the 4 graphts

par(mfrow = c(2,2))                                                                        #par plot 2x2


### ISARs native 1900

sar_nat_1900 <- nls(sr_natives_1900 ~ SSarrhenius(area, k, z), data = sr_tab)              #Nonlinear regression model model: sr_natives_1900 ~ SSarrhenius(area, k, z) data: sr_tab
sar_nat_1900                                                                               #  k        z 
                                                                                           # 252.0522   0.2954      residual sum-of-squares: 89305
summary(sar_nat_1900)                                                                      # all the parameters

plot(sr_tab$area, sr_tab$sr_natives_1900,                                                  # plot ISARs for natives 1900
     xlab = expression(paste("Island area (", km^2, ")", sep = "")),                                                                                      # in this way I have a label for column and row... so 4 instead 8 
     ylab = "Number of native species",
     main = "1830-1950",
     ylim = c(0, 1500))
curve(coef(sar_nat_1900)[1]*x^coef(sar_nat_1900)[2],
      add = T, from = 0, to = 250, col = 'green', lwd = 2)                                  # curve line related at the angular coefficient

### Exercise 10, calculate ISARs for native species in 1951-2015 
### and for alien species in both periods, then compose the multipanel image (2x2)
### you can see on page 11 (top) of BioMac_Lecture_09_2020_04_16_IBT_case_study.odp /.pdf
### N.B. use the correct labels on the correct panels by xlab, ylab and main arguments

##########ISARs of native 2000

sar_nat_2000 <- nls(sr_natives_2000 ~ SSarrhenius(area, k, z), data = sr_tab)              
sar_nat_2000                                                                               
                                                                                           
summary(sar_nat_2000)                                                                      

plot(sr_tab$area, sr_tab$sr_natives_2000,                                                 
     xlab = expression(paste("Island area (", km^2, ")", sep = "")),
     ylab = "Number of native species",
     main = "1951-2015",
     ylim = c(0, 1500))
curve(coef(sar_nat_2000)[1]*x^coef(sar_nat_2000)[2],
      add = T, from = 0, to = 250, col = 'green', lwd = 2)  

########## ISARs for alien 1900


sar_aliens_1900 <- nls(sr_aliens_1900 ~ SSarrhenius(area, k, z), data = sr_tab)              #Nonlinear regression model model: sr_natives_1900 ~ SSarrhenius(area, k, z) data: sr_tab
sar_aliens_1900                                                                               
                                                                                         
summary(sar_aliens_1900)                                                                      

plot(sr_tab$area, sr_tab$sr_aliens_1900,                                                 
     xlab = expression(paste("Island area (", km^2, ")", sep = "")),
     ylab = "Number of aliens species",
     #main = "1830-1950",
     ylim = c(0, 150))
curve(coef(sar_aliens_1900)[1]*x^coef(sar_aliens_1900)[2],
      add = T, from = 0, to = 250, col = 'red', lwd = 2)


########## ISARs for alien 2000

sar_aliens_2000 <- nls(sr_aliens_2000 ~ SSarrhenius(area, k, z), data = sr_tab)              
sar_aliens_2000                                                                               
                                                                                           
summary(sar_aliens_2000)                                                                      

plot(sr_tab$area, sr_tab$sr_aliens_2000,                                                 
     xlab = expression(paste("Island area (", km^2, ")", sep = "")),
     ylab = "Number of aliens species",
     #main = "1951-2015",
     ylim = c(0, 150))
curve(coef(sar_aliens_2000)[1]*x^coef(sar_aliens_2000)[2],
      add = T, from = 0, to = 250, col = 'red', lwd = 2) 





### ISARs according to growth form
### In order to calculate these ISARs we first need to calculate species richness 
### of growth forms across islands among the two periods

sr_annual <- specnumber(tab[tab$Life_Form == "T", 3:ncol(tab)], MARGIN = 2)
sr_peren_herb <- specnumber(tab[tab$Life_Form %in% c("H", "G", "I"), 3:ncol(tab)], MARGIN = 2)
sr_tab$sr_annual_1900 <- sr_annual[grep("1830-1950", names(sr_annual))]
sr_tab$sr_annual_2000 <- sr_annual[grep("1951-2015", names(sr_annual))]
sr_tab$sr_peren_herb_1900 <- sr_peren_herb[grep("1830-1950", names(sr_peren_herb))]
sr_tab$sr_peren_herb_2000 <- sr_peren_herb[grep("1951-2015", names(sr_peren_herb))]

### Exercise 11, calculate SR of woody plants in the two periods and add it to sr_tab
### Woody plants can have two different life forms according to Raunkiaer, "P" (= Phanerophytes)
### and "Ch" (= Chamaephytes)





### Exercise 12, reproduce the multipanel plot of page 11 (bottom) 
### of BioMac_Lecture_09_2020_04_16_IBT_case_study.odp /.pdf








### 





