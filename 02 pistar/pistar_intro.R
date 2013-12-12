#	Uvod k pouzivaniu balika pistar
#	Juraj Medzihorsky
#	10 December 2013



#	Co je balik 'pistar'?
#	---------------------
#                                                                              
#	Balik 'pistar' obsahuje subor funkcii pre zistovanie hodnot 
#	Rudas-Clogg-Lindsayovho zmesoveho indexu dopasovania (mixture index of fit), 
#	znamy aj ako 'pi*', pre siroku skalu modelov. 
#
#	Klasicke mierky dopasovania predpokladaju, ze model popisuje celu populaciu.  
#	Tento prepoklad je casto porusovany.  Pi* namiesto toho predpoklada, ze 
#	model popisuje iba cast populacie, ktorej velkost ale dopredu nepozname. 
#	Tato cast moze byt nulova, ale na druhu stranu moze zahrnat celu populaciu.  
#	Tento predpoklad je vzdy pravdivy a umoznuje merat dopasovanie modelu novym 
#	sposobom:  ako velkost najmensej mozne casti populacie, ktoru nemozno 
#	popisat modelom. Cim mensia je tato cast, tym lepsie model popisuje data.

#	Instalacia
#	----------
#
#	Pre nainstalovanie balika 'pistar' treba balik 'devtools' ktory umoznuje 
#	instalovanie balikov zo serveru github.com.  'devtools' si mozeme 
#	nainstalovat nasledovne (vyber serveru z menu ma vplyv len na rychlost 
#	operacie)

install.packages('devtools')

#	Ak sa 'devtools' uspesne nainstaloval, mozeme ho spustit pomocou

library(devtools)

#	So spustenym 'devtools' mozeme nainstalovat 'pistar' (chvilu to potrva, 
#	kedze balik sa instaluje zo zdrojoveho kodu)

install_github("pistar", username="jmedzihorsky")

#	Ak sa 'pistar' nainstaloval, mozeme ho spustit

library(pistar)

#	Zakladne informacie o baliku si mozeme zobrazit pomocou

citation("pistar")

#	A

help('pistar-package')


#	Pouzitie
#	--------

#	(1)
#                                                                              #
#	Pozrime sa na najednoduchsie pouztie pi*. Tym je zistit, ako dobre nejake 
#	rozdelenie popisuje pozorovane hodnoty nejakej premennej.
#
#	Najprv potrebujeme data.  Na ukazku si ich vytvorime tahom z dvoch 
#	Poissonovych rozdeleni. 1000 pozorovani je z jedneho rozdelenia a 200 z
#	druheho.
#
#	Pri tvorbe pseudonahodnych cisel v R je vhodne ulozit si tzv 'seed', ktory 
#	je vstupnou hodnotou pre generator pseudonahodnych cisel.  Pokial tvorime 
#	pseudonahodne cisla pomocou toho isteho prikazu, a s tym istym 'seed' 
#	cislom, vytvorime vzdy tie iste cisla. 

set.seed(1989)

e <- c(rpois(1e3, 2), rpois(2e2, 5))

#	Vytvorene cisla su ulozene vo vektore 'e'. Mozeme si ich zhrnut:

summary(e)

#	Alebo zobrazit ich rozdelenie na histograme:

hist(e)

#	Inym sposobom zhrnutia je tabulka s frekvenciami jednotlivych hodnot:

te <- freq.table(e)

#	Tabulku si mozeme zobrazit:

te

#	S tabulkou budeme dalej pracovat, kedze nam zjednodusi pracu.
#
#	Tabulku si mozeme zobrazit aj graficky:

plot(te)

#                                                                              
#	Mame teda data a potrebujeme model, ktoreho dopasovanie datam chceme zistit.  
#	V tomto pripade nim bude 'utate' Poissonovo rozdelenie.  Toto rozdelenie je 
#	rovnake ako proste Poissonovo rozdelenie, s tym rozdielom, ze hodnoty mensie 
#	ako 0 a vacsie ako 5 nie su pod nim mozne. Tento model si zadefinujeme ako 
#	funkciu:

md <- 
	function(x, l, lo=0, up=5)
	{
        z <- dpois(x, l)
        z[x<lo] <- 0
        z[x>up] <- 0
        z <- z/sum(z)
        return(z)
	}

#	Nasa funkcia ma 4 argumenty:
#		x	...	pozorovane data
#		l	...	parameter lambda Poissonovho rozdelenia
#		lo 	... najnizsia povolena hodnota premennej
#		hi	... najvyssia --||--

#	A mozeme hladat hodnot pi* tohoto rozdelenia pre 
#	nase data pomocou funkcie 'pistar' z balika 'pistar'.
#	V tomto pripade musime zadat nasledovne argumenty:
#		proc	...	procedura, v tomto pripade "uv"	ako "UniVariate"
#		data	...	pouzijeme tabulku frekvencii te
#		dfn		...	model (ako funkcia)
#		n_par	...	pocet pohyblivych parametrov modelu
#					v nasom pripade 1 (lambda)
#		discrete ..	je pozorovana premenna diskretna?
#					v nasom pripade ano, cize 'TRUE'
#		freq	...	su data frekvencna tabulka?
#					v nasom pripade ano, cize 'TRUE'
#		jack	...	pouzit jackknife?
#					v nasom pripade anie, cize 'FALSE'


pe <- pistar(proc="uv", data=te, dfn=md, n_par=1,
			 discrete=TRUE, freq=TRUE, jack=FALSE)

#	Uz pocas behu funkcie sme mohli vidiet, ze hodnota pi* pre nas model je 
#	0.18, cize nase 'utate' rozdelenie nedokaze popisat najmenej 18% nasej 
#	vzorky.

#	Mozeme sa na objekt pozriet, ale cely sa nam nezobrazi, lebo by to bolo 
#	neprakticke.  Namiesto toho uvidime iba jeho zhrnutie, ktore obsahuje 
#	prikaz, ktorym sme objekt vytvorili (Call), hodnotu pi*, a tiez parameter
#	nasho utateho rozdelenia, pri ktorom sa dosahuje hodnota pi*.

pe

#	Elegantnejsie zhruntie je:

summary(pe)

#	Vysledok si mozeme zobrazit aj na grafe:

plot(pe)

#	Sere stlpce su pozorovane rozdelenie a modre su najvacsia cast dat, ktorym 
#	model pasuje.  Vidime, ze nas model presne popisuje pocet hodnot 2 a 4, ale 
#	uz nie 0, 1, 3, a 5, a samozrejme nie hodnot vyssich ako 5.

#	Znamena hodnota pi*=0.18, ze nas model dobre popisuje data?  Odpoved zavisi 
#	od toho, na co chceme model pouzit.  pi* nespokytuje automaticky "ano"/"nie" 
#	odpovede.  Funguje podobne ako R^2:  poskytuje nam jednociselne zhrnutie 
#	dopasovania modelu, ktore sa lahko vyklada.  Ci je v tomto pripade  18% 
#	uspokojiva hodnota, alebo nie, zavisi od toho, preco nas tento model 
#	zaujima.  Kazdym padom, kedze data sami vytvorili, vieme, ze 83% pochadza 
#	z jedneho, a 17% z druheho rozdelenia.  Vypocitana hodnota pi* sa tomu 
#	velmi blizi.

#	(2)
#                                                                              #
#	Podobne mozeme postupovat pri jednorozmernom modeli spojitej premennej.  
#	Opat si najprv vytvorme data, tak, ze 100 pozorovani je z normalneho 
#	rozdelenia so stredom = 0 a odchylkou = 2 a 20 pozorovani z rovnomerneho 
#	rozdelenia od -1 do 1.  Vieme teda, ze 83% je z jedneho rozdelenia a 17% z 
#	druheho.

set.seed(1989)
y <- c(rnorm(1e2, 0, 2), runif(2e1, -1, 1))

#	V tomto pripade nas frekvencna tabulka samozrejme nezuajima, kedze nasa 
#	premenna je spojita. Mozeme si ju zhrnut pomocou

summary(y)

#	alebo graficky pomocou histogramu

hist(y)

#	Na zaklade histogramu mozeme usudit, ze normalne rozdelenie je dobrym 
#	popisom (modelom) dat.  Nemusime vytvarat novu funkciu, kedze R funkciu pre
#	normalne rozdelnie uz obsahuje. Na jej manual sa mozeme pozriet:

?dnorm

#   Na zistenie hodnoty pi* opat pouzijeme funckiu 'pistar'.
#	Tentokrat s mierne odlisnymi argumentami:
#		dfn		...	'dnorm', R funkcia pre hustotu normalneho rozdelenia
#		discrete ..	FALSE, nase data su spojite
#		n_par	...	2, nase rozdelenia ma stred a odchylku
	

py <- pistar(proc="uv", data=y, dfn=dnorm, 
			 n_par=2, discrete=FALSE, jack=FALSE)

#	Zhrnutia

py

summary(py)

#	Hodnota pi* = 0.21 pre normalne rozdelenie, pri strede -0.2
#	a odchylke 1.5.
#
#	Pozrime sa na vysledok graficky:

plot(py)        

#	Vidime tri farebne odlisene rozdelenia:  
#		cierne	...	pozorovane rozdelenie
#		modre	...	model, teda normalne rozdelenie
#		sede	...	zvyskove rozdelenie, teda
#					21% ktore nie su popisane modelom
#                                                                              
#	Z grafu vidime, ze normalne rozdelenie velmi dobre popisuje tu cast vzorky, 
#	ktora ma hodnoty blizke jeho stredu, ale toto sa zhorsuje smerom vyssie a
#	nizsie od jeho stredu.


#	(3)
#                                                                              
#	Pre spolocenskovedny vyskum su ovela zaujimavejsie pouzitia na viacrozmerne 
#	problemy.  Jednym z takychto pouziti je zistovanie, ci su dve diskretne 
#	premenne nezavisle.  Taketo data su zvycajne ulozene v kontingencnych
#	tabulkach.  Mozeme pouzit zname data z clanku 'How the Cases You Choose
#	Affect the Answers You Get' Barbary Geddes (1990), ktorymi ilustruje 
#	svoj argument na priklade testovania teorie Thedy Skocpol (1979) o 
#	revoluciach.  Tieto data vlozime do kontingencnej tabylky s 2 riadkami a 2 
#	stlpcami.  Puzijeme typ dat 'array'

Geddes1990 <- array(dim=c(2, 2),
					 data=c(1, 2, 7, 67))

#	V bunkach su pocty krajin. Riadky rozdeluju na krajiny, ktore utrpeli 
#	porazku vo vojne a stlpce krajiny, ktore prezili revoluciu. Tabulku si 
#	mozeme zobrazit:

Geddes1990


#	Pridame nazvy riadkov a stlpcov

dimnames(Geddes1990) <- list('war'=c('defeated','undefeated'), 
							  'rev'=c('revolution', 'norevolution'))

Geddes1990

#	Nezavislost premennych v 2x2 tabulke sa zvycajne meria pomerom skrizenych 
#	nasobkov (cross-product ratio). CPR = 1 sa povazuje za nezavislost, hodnoty 
#	nizsie alebo vyssie ako 1 za vzdialene od nezavislosti. CPR si mozeme 
#	vyratat polahky 
#
#	Zadefinujme si funkciu:

cpr <-
	function(x)
	{
		(x[1]*x[2])/(x[2]*x[3])	
	}

#	A pouzime ju:	

cpr(Geddes1990)

#	Hodnota je pomerne vzdialena od 1.  Znamena to teda, ze tieto dve premenne 
#	nie su v nasej vzorke/populaci nezavisle?  Konvencnou metodou zodpovedania 
#	tejto otazky je Chi^2 test.  Ten poskytuje podoved typu "ano"/"nie".
#	Otestujme teda nezavislost nasich data pomocu tohto testu:

chisq.test(Geddes1990)

#	Hodnota p je vyssia nez zvycajne hladiny statistickej vyznamnosti, a teda 
#	odpoved testu je, ze na tychto hladinach mozeme povazovat premenne za 
#	nezavisle.
#
#	Velkym problemom tohoto testu ale je, ze odpovede, ktore dava, zavisia od 
#	velkosti vzorky. Vytvorme si novu tabulku s rovnakym CPR, ale stonasovne 
#	viac pozorovaniami:

G2 <- Geddes1990*100

#	Pozrime sa na nove data

G2

#	Overme ich CPR:


cpr(G2)

#	Hodnota CPR je rovnaka ako pri prvej tabulke.  Avsak Chi^2 test dava 
#	inu odpoved:

chisq.test(G2)

#	Data v druhej tabulke nemozno povazovat za nezavisle a standardnych 
#	hladinach statistickej vyznamnosti. Navysse, Chi^2 test predpoklada, ze
#		1)	Data su nahodnou vzorkou.
#		2)	Testovany model (teda nezavislost) popisuje
#			vsetky pozorovania.
#
#	O prepoklade 1) vieme, ze je nepravdivy.  Nase data su populaciu, a nie 
#	vyberovou vzorkou. Predpoklad 2 je tiez problematicky.  Je mozne, ze 
#	porazka vo vojne nesuvusi s revoluciou v pripade niektorych krajin, ale pre 
#	ine suvisiet moze.  
#
#	Otestujme si teda, ako dobre popisuje nezavislost (definovana ako CPR=1) 
#	nase data.  Opat pouzijeme funkciu pistar.  Argumenty su:
#		proc	... procedura, v tomto pripade '2by2'
#					kedze sa jedna o 2x2 tabulku
#		data	...	nase data
#	Dalsie argumenty nie su potrebne, kedze defaultny model pre 2by2 tabulky 
#	je nezavislost.  Pripadne ine modely mozno specifikovat rucne.

s1 <- pistar(proc='2by2', data=Geddes1990)

#	pi* je 0.01, teda nezavislost nedokaze popisat len 1% zo vzorky.  Mozeme si 
#	vysledky zobrazit graficky:

plot(s1)

#	Vidime, ze rozdelenie pod modelom je takmer rovnaka ako pozorovane 
#	rozdelenie. Kde je teda to nepopisane percento?  Mozeme si objekt rozobrat.  
#	Pozrime sa najprv na jeho zlozenie:

str(s1, max.level=2)

#	cast @pred obsahuje tri rozdelenia, ktore sme uz videli
#	na grafe:
#		1)	model	...	rozdelenie pod modelom
#		2)	unres	...	zvyskove rozdelenie
#		3)	combi	...	pozorvane rozdelenie

str(s1@pred, max.level=2)

#	pozrime sa teda kde su nepopisane zvysky:

round(s1@pred$unres)

#	Teda pokial vyberieme zo vzorky jeden pripad  prvej bunky, pozorovane data 
#	budu nezavisle podla CPR.
#
#	A co ak pouzijeme druhu, 'stonasobnu', tabulku?

s2 <- pistar("2by2", G2)

#	Hodnota pi* je opat 0.01.  Na rozdiel od chi^2 testu pi* nezavisi od 
#	velkosti vzorky.  Jediny rozdiel je, ze teraz mame 100 nasobne viac 
#	pripadov, ktore nie su popisane modelom CPR=1:

round(s2@pred$unres)

#	A co ak chceme zohladnit velkost vzorky? Intuitivne mozeme ocakavat, ze 
#	velkost vzorky ma vplyv na to, nakokolko sa mozeme na vypocitanu hodnotu 
#	pi* spolahnut.  Procedura jackknife umoznuje vypocitat interval 
#	spolahlivosti pi*.

#	Pozrime sa na "stonasobnu" vzorku

i2 <- pistar('2by2', G2, jack=TRUE)

summary(i2)

#	Interval je velmi uzky: spodna aj horna hranica sa de facto rovnaju hodnote 
#	pi*.  A co v pripade povodnej vzorky?  Jackknife spociva na odoberani
#	pripadov z buniek a ziadna bunka nemoze nadobudat nulovu hodnotu.  Ak mame
#	v tabulke bunky s 1 alebo menej pripadmi, mozeme zvysit
#	hodnotu kazdej bunky o velmi male nenulove cislo (0.01)


i1 <- pistar('2by2', Geddes1990+0.01, jack=TRUE)

summary(i1)

#	Vysledny interval je vyrazne sirsi. 

#	Zlozitejsie viacrozmene modely si vyskusame na workshope.
