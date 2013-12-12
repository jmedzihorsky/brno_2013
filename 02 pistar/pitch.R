#	pistar @ PolBeRG	
#	II.	
#	Juraj Medzihorsky
#	11 October


#	Installing pistar:
#	------------------

#	from github, the latest public version

library(devtools)	# needed for installing from github

install_github("pistar", "jmedzihorsky")

library(pistar)

citation("pistar")


#	Other needed libraries:
#	-----------------------

library(ADGofTest)	#	for Anderson-Darling test


#	=========================
#	1	Univariate Continuous
#	=========================


#	Prepare the birth wieght data
#	-----------------------------

a <- readLines("d:/medzihorskyj/Nat2011psPub_2013_07_22.txt")

gw <- function(x) as.numeric(substr(x, 463, 466))
gs <- function(x) substr(x, 436, 436)
gp <- function(x) as.numeric(substr(x, 423, 423))

w <- sapply(a, gw, USE.NAMES=F)

sex <- sapply(a, gs, USE.NAMES=F)[-which(w==9999)]	#	w/o NA weight
plu <- sapply(a, gp, USE.NAMES=F)[-which(w==9999)]	#	w/o NA weight

y <- w[-which(w==9999)]				#	remove NAs		


#	Split by birth type:
#	--------------------

y_f <- y[sex=="F"]					#	all female 
y_m <- y[sex=="M"]					#	all male

s_f <- y[ (sex=="F") & (plu==1) ]	#	single female
s_m <- y[ (sex=="M") & (plu==1) ]	#	single male

t_f <- y[ (sex=="F") & (plu==2) ]	#	twin female
t_m <- y[ (sex=="M") & (plu==2) ]	#	twin male



#	Let us look on all births first:
#	--------------------------------

hist(y, col='gray', main="Birth Weight in US territories in 2011")


plot(density(y), main="Birth Weight in US territories in 2011")

	#	now what is the MLE for Normal?
	curve(dnorm(x, mean(y), sd(y)), add=T, col='red')


#	pistar

p <- pistar.uv(y, dnorm, inits=c(mean(y), sd(y)))

plot(p)

p

summary(p)

#	parameters of the Normal are different:

round(data.frame('standard'=c(mean(y), sd(y)), 'pistar'=p@param$est))


#	-----------------------------
#	pi* vs. standard tests of fit
#	-----------------------------


#	Densities:
#	----------

ad <- function(y, m) plot(density(y, n=512), main=m)	
ac <- function(y) curve(dnorm(x, mean(y), sd(y)), col="red", add=T)


par(mfrow=c(2,2))
	ad(s_f, m="Female Single")	;	ac(s_f) 
	ad(s_m, m="Male Single")	;	ac(s_m)
	ad(t_f, m="Female Twin")	;	ac(t_f)
	ad(t_m, m="Male Twin")   	;	ac(t_m)


#	Conventional tests:
#	-------------------

#	Kolmogorov-Smirnov test:


k_s_f <- ks.test(s_f, pnorm, mean(s_f), sd(s_f))$p.value
k_s_m <- ks.test(s_m, pnorm, mean(s_m), sd(s_m))$p.value
k_t_f <- ks.test(t_f, pnorm, mean(t_f), sd(t_f))$p.value
k_t_m <- ks.test(t_m, pnorm, mean(t_m), sd(t_m))$p.value


#	save the results:

R <- data.frame(plur=c('single', 'single', 'twin', 'twin'),
				sex=c('F', 'M', 'F', 'M'), 	
				n=c(length(s_f), length(s_m), 
					length(t_f), length(t_m)), 
				p_ks=round(c(k_s_f, k_s_m, k_t_f, k_t_m), 3))

#	inspect it:
R


#	Anderson-Darling test:

a_s_f <- ad.test(s_f, pnorm, mean(s_f), sd(s_f))$p.value
a_s_m <- ad.test(s_m, pnorm, mean(s_m), sd(s_m))$p.value
a_t_f <- ad.test(t_f, pnorm, mean(t_f), sd(t_f))$p.value
a_t_m <- ad.test(t_m, pnorm, mean(t_m), sd(t_m))$p.value

#	Update the results:

R$p_ad <- round(c(a_s_f, a_s_m, a_t_f, a_t_m), 3)

R

#	pi* using pistar:
#	-----------------

#	Unlike KS & AD tests, pi* uses PDF and not CDF

p_s_f <- pistar.uv(s_f, dnorm, inits=list(mu=mean(s_f), sigma=sd(s_f)))
p_s_m <- pistar.uv(s_m, dnorm, inits=list(mu=mean(s_m), sigma=sd(s_m)))
p_t_f <- pistar.uv(t_f, dnorm, inits=list(mu=mean(t_f), sigma=sd(t_f)))
p_t_m <- pistar.uv(t_m, dnorm, inits=list(mu=mean(t_m), sigma=sd(t_m)))


R$pistar <- round(c(p_s_f@pistar$est, p_s_m@pistar$est, 
					p_t_f@pistar$est, p_t_m@pistar$est), 2)

R


#	===================
#	Contingency tables:
#	===================


#	2 by 2 can be done by hand ....


data(Fienberg1980a)

Fienberg1980a

#	this time we also want a jackknife:

f <- pistar.ll(Fienberg1980a, jack=TRUE)

f

summary(f)

plot(f)


#	another example

data(HairEyeColor)

#       check if the data is an "array"
is(HairEyeColor, "array")

#       it is not, so it first needs to be converted:
HEC <- array(HairEyeColor, 
			 dim=dim(HairEyeColor),
			 dimnames=dimnames(HairEyeColor))


#       find pi* for independence in a 3-way table

h <- pistar(proc="ll", data=HEC, margin=list(1, 2, 3), jack=F)

h

summary(h)

str(h)

round(h@pred$unres)


#	and using pistar.ct this can be done for any model fit
#	to data that can be represented as a contingency table with
#	non-negative cell values


#	And what is wrong with the Chi^2 test?


H <- matrix(1:4, 2, 2)

H

chisq.test(H*10)

chisq.test(H*100)
