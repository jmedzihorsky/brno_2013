#	Uvod do R
#	Juraj Medzihorsky
#	08 December 2013
#	
#
#	Riadky, ktore zacinaju znakom # su komentar.  Ak tento 
#	subor otvorite v R, a spustite riadky s komentarom, R
#	ich rozpozna ako komentar, a preskoci ich.
#	Riadky, ktore nezacinaju # su prikazy v R.  Mozeme ich
#	alebo
#		1)	Prepisat alebo vlepit do R konzoly.
#		2)	V R GUI pre Windows otvorit tento subor pomocou
#			prikazu 'open script' v menu.  Nasledne mozeme
#			prikazu posielat z okna skriptu do okna konzoly
#			tak, ze ich vysvietime a stlacime ctrl+R.

#	Zacnime zakladnymi operaciami.
#	Scitanie:

16 + 4

#	Odcitanie:

2 - 10

#	Nasobenie:

23 * 17

#	Delenie:

20 / 4

#	Prikaz pre umocnovanie je trochu menej zjavny.  Pouziva
#	sa znak ^.  Napriklad dva na tretiu je:

2 ^ 3

#	Dalsia bezna operacia je tvorba radov.  Pouziva sa dvojbodka (:).
#	Takto vytvorime rad celych cisel od 2 do 5:

2 : 5

#	Rad moze posutpovat aj od vyssieho cisla k nizsiemu:

2 : -3

#	V pripade, ze nas nezaujima cely rad, ale len dve cisla z neho,
#	mozeme ich spojit prikazom 'c':

c( 2 , 4 )

#	'c' je skratka pre 'combine' a cisla, ktore spajame su oddelene
#	ciarkou (,).  'c' je funkcia, a jej argumenty su v zatvorkach.
#	Tento system vyuzivaju mnohe funkcie v R.

#	Udaje mozeme ulozit do pamate R ako objekty.  Tato operacia
#	spociva na pomenovani objektu a naslednom pripadeni hodnot
#	objektu pomocou prikazu '<-' .  Takto mozeme vytvorit objekt
#	A, ktory obsahuje rad celuch cisel od 0 do 4"

A <- 0:4

#	Obsah tohto objektu sa nam pri vytvoreni nezobrazil.  Ak ho
# 	chceme vidiet, napiseme meno objektu:

A

#	Nas objekt A ma 5 hodnot, ktore su zoradene. Kazda z hodnot je
#	jedno cele cislo.  Rozne typy objektov mozu mat na jendotlivych
#	miestach rozne ine objekty, ako cisla, pismena a ine.  Ked 
#	chceme vidiet v konzole len jednu z hodnot v objekte, napiseme
#	meno objektu nasledovane poradovym cislom hodnoty v hranatych
#	zatvorkach:


A[ 3 ]

#	Tretia hodnota objektu A je cislo 2.  Mozeme si zobrazit aj 
#	viacero vybranych hodnot v A.  Napriklad prve tri pomocou 
#	vytvorenia radu od 1 do 3:

A[ 1 : 3 ]


#	Albo iba druhu a stvrtu hodnotu pomocou combine:

A[ c(2, 4) ]


#	S objektami mozeme prevadzat operacie.  Napriklad mozeme 
#	zvysit kazdu s hodnot v A o 1:

A + 1

#	Zvysila tato operacia hodnoty ulozene v A? Pozrime sa:

A

#	Nie. Pre zmenu hodnot ulozenych v objekte sa pouziva operator
#	<-.  Ak chceme zvysit o 1 kazdu hodnotu v objekte A, postupujeme
#	napriklad takto:

A <- A + 1

#	Vysledok operacie mozeme skontrolovat:

A

#	Vytvorme teraz dalsi objekt, nazvyme ho B, a vlozme do neho
#	cele cisla od 5 do 1.

B <- 5:1

#	Skontrolujme objekt:

B

#	Teraz mame v pamati R dva objekty.  Nazvy objketov v pamati R
#	mozeme skontrolovat pomocou:

ls()

#	Vidime, ze v pamati mame dva objekty: A a B.  Kedze sme tieto
#	objekty vytvorili, vieme co obsahuju.  Su to vektory.  Vektor
#	v R je nieco ako vlak s vagonmi.  V kazdom vagone moze byt nieco
#	ine a kazdy ma poradove cislo.  Ale existuju rozne typy 'vlakov'.
#	Typ objektu mozeme skontrolovat:

typeof(A)


#	Su aj ine typy objektov.  Napr. znakovy -- 'character':

typeof( "nejake pismena a cisla 01234" )

#	alebo booleovsky:

typeof( c(TRUE, FALSE) )

#	Dlzku vektora mozeme skontrolovat:

length( A )

length(B)

#	Nase dva objekty maju rovnaku dlzku a mozeme s nimi jednoducho
#	vykonat niektore operacie.  Napriklad mozeme scitat prvu hodnotu
#	v A s prvou hodnotou v B a tak dalej pomocou:	

A + B

#	Alebo vynasobit prvy hodnotu v s prvou v B atd pomocou:

A * B


#	S objektami mozeme vykonavat aj ine operacie.  Napriklad mozeme
#	scitat vsetky hodnoty v ciselnom objekte:

sum( A )

#	Alebo ich vynasobit:

prod( B )


#	Ak nas zaujima napriklad priemerna hodnota v A, mozeme:

sum(A) / length(A)

#	alebo jednoducho:

mean(A)

#	Objekty mozeme spajat pomocou combine do jedneho vektora:

c( A , B )


#	Alebo do dvojrozmerneho objektu, kde je kazdy z objektov
#	stlpcom:

cbind( A , B )

#	'cbind' je skratka pre 'bind as columns'.
#
#	Objekty si mozeme zobrazit aj pomocou grafov.  Napriklad 
#	nase dva objekty si mozeme zobrazit pomocou

plot( x = A , y = B )

#	Otvorilo sa dalsie okno.

#	Suradnice bodov su pary hodnot v A a B:  prva hodnota z A
#	je suradnicou na osi x a prva hodnota z B na osi y atd.
#	Aj pri takomto jednoduchom grafe mame v R k dispozicii
#	vela moznosti.  Mozeme napriklad zmenit vyzor bodov:

plot( x = A , y = B , pch = 19 )

#	Alebo ich farbu:

plot( x = A , y = B , col = 'blue' )

#	Vsetky moznosti zistime pomocou otvorenia manualu pre
#	funkciu 'plot'. Heslo otvorime pomocou

?plot

#	Takto mozno otvorit dokumentaciu ku vsetkym zdokumentovanym
#	funkciam.  V pripade, ze si presny nazov funkcie nepamatame
#	mozeme si zobrazit zoznam vestkych hesiel blizkych zadanemu
#	vyrazu tak, ze pred vyraz vlozime ??:

??scatter




