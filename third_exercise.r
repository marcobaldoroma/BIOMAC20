data(sipoo)
data(sipoo.map)

 

sr <- specnumber(sipoo)  
dat <- data.frame(sr = sr, area = sipoo.map$area)

 

mod <- nls(sr ~ SSarrhenius(area, k, z), data = dat)        # non linear  ~ which is the response variable or more variables k=log of c, where c is the parameter of Arrhenius power f., that is relate at the vegetation analysis model 

 

plot(dat$area, dat$sr,
     xlab = "Island area (sq.km)",
     ylab = "Bird species richness",
     ylim = c(0, 40),
     las = 1,
     main = "ISAR (Arrhenius) \nSimberloff & Martin (1991)")
curve(coef(mod)[1]*x^coef(mod)[2],
      add = T, lwd = 2, from = 0, to = 260)
      
#Formula: sr ~ SSarrhenius(area, k, z)

#Parameters:
#  Estimate Std. Error t value Pr(>|t|)    
#k  3.40619    0.40790   8.351 3.16e-07 ***
#z  0.43644    0.02743  15.910 3.15e-11 ***
#---
#Signif. codes:  
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1     

#Residual standard error: 2.209 on 16 degrees of freedom

#Number of iterations to convergence: 5 
#Achieved convergence tolerance: 1.056e-06


summary(sars::sar_power(dat))


###### andrea basella(berancidoli, bettles)

install.packages("betapart")
install.packages("vegan")
library(betapart)
library(vegan)


data(BCI)       # load   barrioo colorado island, artificial arcipelago in south america, marine researve

?(BCI)


sr <- specnumber(BCI)     # calculate SR (default is by rows)
sr
names(sr) <- NULL         # remove names
sr
hist(sr)                  # plotting  

hist(sr,
     breaks = 20,                         # how many categories/parts is divided my matrix, 20! 
     xlab = "Species Richness",
     ylab = "Number of plots",
     main = "")                           # remove the title..
     
     # distant matrix is a matrix that mesure the distance between pairs of plots. i.g metric distance

###calculate distance and similitarity matrix among pairs of plots and multiple plots

#transform the abundance matrix into a presence/absence matrix #pa is present or absence of our occurrence matrix method

BCI <- decostand(BCI, "pa") # transform the abundance matrix into a presence/absence matrix

str(BCI)

##I am showing you an example of distance matrix you can be familiar with: city distance EU
# 50 cities how many pairs i could have in my matrix ?? 50*49/2!! n*n-1/2(2 because is a triangle matrix)


vegdist(BCI[1:2, ], method = "jaccard")           # calculate distance      # distance in species composition in triangle matrix. if every plot rappresent all the species pool I will got 1

vegdist(BCI[1:2, ], method = "jaccard")       # calculate similarity    

vegdist(BCI[2:3, ], method = "jaccard")
vegdist(BCI[1:3, ], method = "jaccard")
vegdist(BCI, method = "jaccard")
hist(vegdist(BCI, method = "jaccard"),
     xlab = "Jaccard's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")

hist(vegdist(BCI, method = "bray"),
     xlab = "Sorensen's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")                                    ## histogram of similarity  

par(mfrow = c(1, 2))
hist(vegdist(BCI, method = "jaccard"),
     xlab = "Jaccard's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")
     
hist(vegdist(BCI, method = "bray"),
     xlab = "Sorensen's dissimilarity metric",
     ylab = "Number of pairs",
     main = "")
     
par(mfrow = c(1, 1))
d1 <- vegdist(BCI[1:25, ])       # distance among the first 25 plots
d2 <- vegdist(BCI[26:50, ])      # distance among the last 25 plots
par(mfrow = c(1, 2))
hist(d1)
hist(d2)
t.test(d1, d2)                             # testing for difference in means (any direction) x= d1=0.0076  y=d2= 0.022


t.test(d1, d2, alternative = "greater")    # testing for difference in means (first one is greater than the second one) x= 0.336 y mean=0.321

length (as.numeric(d1))     #300 that is the 


### Exercise 1, use the given vector v to split the dataset into two subsets, calculate and plot SR for both
### and test for significant differences in their mean value, do the same for jaccard abd sorensen distances
### do the plots as 1X2 multiframe plots

set.seed(42)
v <- sample(1:50, 25)            #simple sample


d<-BCI[v,]
f<-BCI[-v,]


sr_d <- specnumber(d)
sr_f <- specnumber(f)


par(mfrow=c(1,2))
hist(sr_d, xlim = c(75,110))
hist(sr_f, xlim = c(75,110))


t.test(sr_d,sr_f)


jac_d <- vegdist(d, method = "jaccard")
jac_f <- vegdist(f, method = "jaccard")


#par(mfrow=c(1,2))
hist(jac_d)
hist(jac_f)


t.test(jac_d, jac_f)


sor_d <- vegdist(d, method = "bray")
sor_f <- vegdist(f, method = "bray")


#par(mfrow=c(1,2))
hist(sor_d)
hist(sor_f)


t.test(sor_d,sor_f)

### Exercise 1b, use "Stream" from BCI.env to split the dataset and repeat the calculations, plots and tests
### you did in the previous exercise  lunedi 18/05


data("BCI.env")

head(BCI.env)
BCI_no_stream <- BCI[BCI.env$Stream=="No",]
BCI_stream <- BCI[BCI.env$Stream=="Yes",]

dim(BCI_no_stream )
dim(BCI_stream)

sr_ns <-  specnumber(BCI_no_stream)
  
sr_s <-  specnumber(BCI_stream)

par(mfrow = c(1,2))
hist(sr_ns,
     main = "")
hist(sr_s,
     main "")
t.test(sr_ns, sr_s)

jac_ns <- vegdist(BCI_no_stream, "jaccard")
jac_s <- vegdist(BCI_stream, "jaccard")

par(mfrow = c(1,2))
hist(sr_ns,
     main = "")
hist(sr_s,
     main "")

t.test(sr_ns, sr_s)

sor_ns <- vegdist(BCI_no_stream, "bray")
sor_s <- vegdist(BCI_stream, "bray")

par(mfrow = c(1,2))
hist(sr_ns,
     main = "")
hist(sr_s,
     main "")
t.test(sr_ns, sr_s)



(((((((((((((data("BCI.env")

 

head(BCI.env)

 

BCI_no_stream <- BCI[BCI.env$Stream == "No", ]
BCI_stream <- BCI[BCI.env$Stream == "Yes", ]

 

dim(BCI_no_stream)
dim(BCI_stream)

 

sr_ns <- specnumber(BCI_no_stream)
sr_s <- specnumber(BCI_stream)

 

par(mfrow = c(1, 2))
hist(sr_ns,
     main = "")
hist(sr_s,
     main = "")
t.test(sr_ns, sr_s)

 

jac_ns <- vegdist(BCI_no_stream, "jaccard")
jac_s <- vegdist(BCI_stream, "jaccard")

 

par(mfrow = c(1, 2))
hist(jac_ns,
     main = "")
hist(jac_s,
     main = "")
t.test(jac_ns, jac_s)

 

sor_ns <-vegdist(BCI_no_stream, "bray")
sor_s <- vegdist(BCI_stream, "bray")

 

par(mfrow = c(1, 2))
hist(sor_ns,
     main = "")
hist(sor_s,
     main = "")
t.test(sor_ns, sor_s)
             )))))))))))))))))))))))))))))))))))))))))))))))))))










#stream or river
#stream, or any stress, if it 



????????????????????????????????????????????????????????????????????

### Partitioning beta diversity

library(betapart)

beta.multi(BCI)

#$beta.SIM        Simpson dissimilarity= turnover
#[1] 0.8840073
#$beta.SNE        Nestedness
#[1] 0.01347489
#$beta.SOR        Sorensen 
#[1] 0.8974822

beta.multi(BCI[1:2, ])
beta.multi(BCI[1:3, ])

beta.pair(BCI[1:3, ])
beta.pair(BCI)

boxplot(beta.pair(BCI))
par(nfrow = c(1,1))
boxplot(beta.pair(BCI))

boxplot(beta.pair(BCI),
        names = c("Turnover", "Nestedness", "Total beta"))

par(mfrow = c(1, 2))
boxplot(beta.pair(BCI),
        y= "dissimilarity",
        names = c("Turnover", "Nestedness", "Total beta"),
        main = "Sorensen")
boxplot(beta.pair(BCI, "jaccard"),
        y= "dissimilarity",
        names = c("Turnover", "Nestedness", "Total beta"),
        main = "Jaccard")
par(mfrow = c(1, 1))





### Exercise 2, calculate and plot beta components (for pairs) according to Jaccard for ceram.s and ceram.n in a multi-panel plot,
### before doing so inspect the two datasets and calculate overall beta components

library(betapart)

# ceram.s Cerambycidae from Southern and northern European Countries
data(ceram.s)  
data(ceram.n)

head(ceram.n[1:10])
head(ceram.s[1:10])

beta.multi(ceram.n)
beta.multi(ceram.s)

beta.multi(ceram.n[1:10])
beta.multi(ceram.s[1:10])

beta.pair(ceram.n[1:10])
beta.pair(ceram.n)

beta.pair(ceram.s[1:10])
beta.pair(ceram.s)

par(mfrow = c(2, 2))
boxplot(beta.pair(ceram.n),
        ylab= "Sor dissimilarity",
        names = c("Turnover", "Nestedness", "Total beta"),
        main = " Cerambycidae N-EU Sorensen")
boxplot(beta.pair(ceram.n, "jaccard"),
        ylab = "Jac dissimilarity",
        names = c("Turnover", "Nestedness", "Total beta"),
        main = " Cerambycidae N-EU Jaccard")

#par(mfrow = c(1, 2))
boxplot(beta.pair(ceram.s),
        ylab= "Sor dissimilarity",
        names = c("Turnover", "Nestedness", "Total beta"),
        main = "Cerambycidae S-EU Sorensen")
boxplot(beta.pair(ceram.s, "jaccard"),
        ylab = "Jac dissimilarity",
        names = c("Turnover", "Nestedness", "Total beta"),
        main = " Cerambycidae S-EU Jaccard")

dev.off()

    
### Exercise 2b, do the same you did in exercise B for the following simulated dataset "m"
### by using f to split the dataset into two subsets


set.seed(42)
m <- matrix(data = sample(0:1, 1000, replace = T), ncol = 20)
f <- sample(c("A", "B"), 50, replace = T)

m_A <- m[f=="A",]
m_B <- m[f=="B",]

par(mfrow = c(1, 2))
boxplot(beta.pair(m_A, "jaccard"),
        main = "m_A",
        ylab = "Jaccard dissimilarity index",
        ylim = c(0, 0.8),
        names = c("Turnover", "Nestedness", "Total beta"))
boxplot(beta.pair(m_B, "jaccard"),
        main = "m_B",
        ylim = c(0, 0.8),
        names = c("Turnover", "Nestedness", "Total beta"))

###################### set.seed(42)
set.seed(42)
m <- matrix(data = sample(0:1, 1000, replace = T), ncol = 20)
f <- sample(c("A", "B"), 50, replace = T)

m_a <- m[f == "A", ]
m_b <- m[f == "B", ]

par(mfrow = c(1, 2))
boxplot(beta.pair(m_a, "jaccard"),
        ylim = c(0, 1),
        names = c("Turnover", "Nestedness", "Overall Beta"))
boxplot(beta.pair(m_b, "jaccard"),
        ylim = c(0, 1),
        names = c("Turnover", "Nestedness", "Overall Beta"))
par(mfrow = c(1, 1))
beta.multi(m_a)
beta.multi(m_b)
 #######################################################

### Distance decay
# possiamo usare il concetto e modello della distanza anche per esempio per la distanza tra i pattern di riflettanza della vegetazione!!

data(BCI.env)
head(BCI.env)

spat.dist <- dist(BCI.env[, 1:2])    # packages SPAT statistical default pack. R. distance euclidea!!!

dissim.BCI <- beta.pair(BCI)$beta.sim   # we have to use one at time variable on beta.pair we decide sim=turnover

plot(spat.dist, dissim.BCI, ylim = c(0.1, 0.6), xlim = c(0, max(spat.dist)))   # plot the distance decay matrix, first we use spat.dist= distanza in X axis and dissim.BCI in Y axis, at the end we specify the limit of axis
# rapper or other model you can use
BCI.decay.exp <- decay.model(dissim.BCI, spat.dist, model.type = "exp", perm = 100)  # perm = permutated
BCI.decay.exp

BCI.decay.pow <- decay.model(dissim.BCI, spat.dist, model.type = "pow", perm = 100)  # we specify the dissimilarity, after the spatial distamce, the model.type, in the end permutation= in order to see significats patter
BCI.decay.pow

# important information 
# $model  Call:  glm(formula = y ~ x, family = gaussian(link = "log"))
#$pseudo.r.squared [1] 0.06687897  = AIC: -3843

plot.decay(BCI.decay.exp, col = "red", remove.dots = TRUE, add = TRUE)
plot.decay(BCI.decay.pow, col = "blue", remove.dots = TRUE, add = TRUE)

## we fit different model to check the best model for our analysis, the model the we can use to better fit the relation on my informations
# akaike acaico information criterio

BCI.decay.exp$pseudo.r.squared
BCI.decay.pow$pseudo.r.squared
BCI.decay.exp$model
BCI.decay.pow$model

### Exercise 3, display distance decay patterns for the "mite" dataset (from vegan), fit the power
### and the exponential model for the overall beta diversity according to 
### Sorensen ($beta.sor component of beta.pair()) and evaluate the two models.
### Plot location can be found in "mite.xy", use the function data() to load the two matrices.
### Remember to transform the abundance matrix into a presence/absence matrix
### before calculating beta diversity

?mite


BCI.decay.exp <- decay.model(dissim.BCI, spat.dist, model.type = "exp", perm = 100)
BCI.decay.exp

BCI.decay.exp <- decay.model(dissim.BCI, spat.dist, model.type = "exp", perm = 100)
BCI.decay.exp

 

BCI.decay.pow <- decay.model(dissim.BCI, spat.dist, model.type = "pow", perm = 100)
BCI.decay.pow

 

plot.decay(BCI.decay.exp, col = "red", remove.dots = TRUE, add = TRUE)
plot.decay(BCI.decay.pow, col = "blue", remove.dots = TRUE, add = TRUE)

R^2 the greater the better
AIC the smaller the better

### Exercise 3, display distance decay patterns for the "mite" dataset (from vegan), fit the power
### and the exponential model for the overall beta diversity according to 
### Sorensen ($beta.sor component of beta.pair()) and evaluate the two models.
### Plot location can be found in "mite.xy", use the function data() to load the two matrices.
### Remember to transform the abundance matrix into a presence/absence matrix
### before calculating beta diversity

 

library(vegan)
?mite

remember to load vegan package before loading the dataset



### Rarefaction

data(dune)
data(dune.env)

summary(dune.env$A1)

dune_la <- dune[dune.env$A1 <= 4.2, ]
dune_ha <- dune[dune.env$A1 > 4.2, ]

raremax <- min(rowSums(dune))
raremax

rarecurve(dune_la, sample = raremax)
rarecurve(dune_ha, sample = raremax)

dune_la_agg <- colSums(dune_la)
dune_ha_agg <- colSums(dune_ha)

dune_agg <- rbind(dune_la_agg, dune_ha_agg)
row.names(dune_agg) <- c("Low altitude", "High altitude")

rarecurve(dune_agg)

### Rarefaction curves show the expected number of species found in each plot 
### given an increasing number of sampled individuals

### Analogously, accumulation curves show the expected number of species 
### given an increasing number of sampled forest fragments, you can check "specaccum" function from vegan

### You can also estimate the number of species found in each plot
### by using abundance-based species richness estimators, like Chao1 and ACE, you can check "estimateR" function from vegan

### Finally, you can also estimate the number of species in the dataset, or in groups of plots,
### according to sample-based diversity estimators like Jackknife, you can check "specpool" function from vegan

### BEWARE, these techniques are used for different, although similar, purposes and they hold
### DIFFERENT degrees of robustness!
    




