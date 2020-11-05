# 1
 log((640320**3)+744)/(sqrt(163))
  #[1] 3.141593
 pi
  #[1] 3.141593

#2
 notes<-c(18,1.5,9.5,15.5,15,15.5,0.5,14.5,10)

#3
 notes<-c(notes, 4)

#4
 notes10<-notes/2
 choix<-notes10>6
 length(notes10[choix])

#5
 notes10bis<-c(notes10[1],notes10[3],notes10[length(notes10)])
 notes10bis
  #[1] 9.00 4.75 2.00
 mean(notes10bis)
  #[1] 5.25

#6
 notes10sup<-notes10>10
 notes10sup
  # [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 length(notes[notes10sup])
# [1] 5

#7
 min(notes[notes==floor(notes)])
  # [1] 4

#8
 notes2<-notes-2

#9 
 length(notes2[notes2<0])
 notes2[notes2<0]<-0
 notes2
  #[1] 16.0  0.0  7.5 13.5 13.0 13.5  0.0 12.5  8.0  2.0

#10
 adn<-c("A","C","A","A","G","A","T","G","C","C","A","T","T","G","T","C")
 adn2<-factor(adn)
 adn2
  #[1] A C A A G A T G C C A T T G T C
    #Levels: A C G T
 nlevels(adn2)
  #[1] 4
 levels(adn2)
  #[1] "A" "C" "G" "T"

#11
 length(adn2[adn2=="A"])
 #[1] 5
 length(adn2[adn2=="C"])
 # [1] 4
 length(adn2[adn2=="G"])
 # [1] 3
 length(adn2[adn2=="T"])
 # [1] 4
 
#12
 setwd("C:/Users/camil/Documents/UTC/SY02/TP1_Prise_en_main/data")
 X<-read.csv("sy02.data")
 length(X)
 #[1] 6
 ncol(X)
 #[1] 6
 nrow(X)
 #[1] 297
 names(X)
 #[1] "correcteur.median" "median"            "correcteur.final"  "final"            
 #[5] "moyenne"           "resultat"

#13
 head(X)
 #correcteur.median median correcteur.final final moyenne resultat
 #1                BR   11.0              ALC  17.5    14.9        C
 #2                EN   14.0               BR  16.0    15.2        C
 #3               ALC   10.5              ALC  13.0    12.0        D
 #4                BR   17.0               BR  13.0    14.6        C
 #5                EG   14.5               EN  14.0    14.2        C
 #6                EG   12.0               EN  19.5    16.5        B
 
 # quantitatives = 3
 # qualitatives = 3

#14
 X[,c(2,ncol(X))]
  # 
  #median resultat
  #1     11.0        C
  #2     14.0        C
  #...
  #296    9.5        C
  #297   10.5        F

#15
 mean(X[X$correcteur.median=="EG",'median'])
 #[1] 12.63208

#16
 (  nrow( X[X$median < X$final ,] )  ) / nrow(X)
 #[1] 0.6801347

#17
 mean(X[,'final'])
 sd(X[,'final'])
 var(X[,'final'])
 median(X[,'final'])
 max(X[,'final'])
 min(X[,'final'])

#18
 summary(X[,'final'])
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
  #   0.00   12.50   16.00   14.76   18.00   20.00 

#19
 Q1 <- quantile(X[,'median'],0.25)
 Q3 <- quantile(X[,'median'],0.75)
 Q3 - Q1
 IQR(X[,'median'])
 # [1] 6

#20
 nms <- sort(X[,'median'])
 leng <- length(X[,'median'])
 mean ( nms [11 : (leng-10)] )

#21
 table(X[,'correcteur.median'])
 # ALC  BR CFG  DH  EG  EN  HP 
 # 73  49  25  48  53  25  24 
 barplot(table(X[,'correcteur.median']))
 
#22
 boxplot(X[,'final'])

#23
 moustache <- quantile(X[,'final'], 0.25) - 1.5*IQR(X[,'final'])
 X[,'final']<moustache
 sum(X[,'final']<moustache)
 #[1] 12
 
#24
 stem(X[,'final'])
 #The decimal point is at the |
 #0 | 0000005
 #1 | 
 #2 | 055
 #...
 #19 | 000000000000000055555555555555555555
 #20 | 00000000000000000000000
 
#25
 hist(X[,'final'])
 # ou bien
 # final <- X$final
 # hist(final)
 
#26
 final <- X$final
 hist(final, breaks = c(0, 15, 20))
 
#27
 length(final[final < 15])/length(final)/15
 length(final[final >= 15])/length(final)/5
 
#28
 colonnes <- c(0,3,7,10,14,17,20)
 h <- hist(final, breaks=colonnes, plot=FALSE)
 sum( diff(colonnes) * h$density )
 
#29
 plot(X[,'final'],X[,'median'])
 # => ceux qui réussissent le médian <=> final
 
#30
 boxplot(final ~ correcteur.final, data=X)
 
#31
 #boxplot(final ~ correcteur.final='DH', data=X)
 
#32
 stripchart(final ~ correcteur.final, data=X)
 
#33
 stripchart(final ~ correcteur.final, data=X)
 # donne la quantité d'élémenst présents pour chaque variable
 # ici : le nb de personnes ayant eu cette note