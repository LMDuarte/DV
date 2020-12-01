#demographics//////////////////////////////////////
summary(da37404.0001$RAGE)

age= da37404.0001$RAGE
sd(age)

summary(da37404.0001$RTYP)



#/////////////////////////////////////////////
print(da37404.0001$STRFINAN)

sf=(da37404.0001$STRFINAN)

sf
#Combining scale--taking care of spouse
print(da37404.0001$PEVSPSR_A)
carea=(da37404.0001$PEVSPSR_A)


#recoding variable into numeric
library(plyr)

da37404.0001$carea <- revalue(da37404.0001$PEVSPSR_A, c("(1) Never"="1", "(2) Rarely"="2","(3) Sometimes"="3","(4) Often"="4"))

print(da37404.0001$carea)

by(da37404.0001$carea,da37404.0001$RTYP,summary)

is.factor(da37404.0001$carea)
da37404.0001$carea= as.numeric(da37404.0001$carea)
is.numeric(da37404.0001$carea)
by(da37404.0001$carea,da37404.0001$RTYP,summary)
#recoding spouse help get better perspective

da37404.0001$carec <- revalue(da37404.0001$PEVSPSR_C, c("(1) Never"="1", "(2) Rarely"="2","(3) Sometimes"="3","(4) Often"="4"))

print(da37404.0001$carec)
da37404.0001$carec= as.numeric(da37404.0001$carec)
by(da37404.0001$carec,da37404.0001$RTYP,summary)

##testing adding variables

da37404.0001$carepos<-da37404.0001$carec + da37404.0001$carea
print(da37404.0001$carepos)
by(da37404.0001$carepos,da37404.0001$RTYP,summary)


#test ANOVA

summary(da37404.0001$RTYP)
one.way<- aov(carepos~ RTYP, data=da37404.0001 )
summary(one.way)

###prove///////////////////////////////////////////////////////////////////////////////////////////////
> is.factor(da37404.0001$carea)
[1] TRUE
> da37404.0001$carea= as.numeric(da37404.0001$carea)
> is.factor(da37404.0001$carea)
[1] FALSE
####///////////////////////////////////////////////////////////////////////////////////////////////////





##recode spouse took care of my task when sick

da37404.0001$caref <- revalue(da37404.0001$PEVSPSR_F, c("(1) Never"="1", 
                                                        "(2) Rarely"="2",
                                                        "(3) Sometimes"="3",
                                                        "(4) Often"="4"))
print(da37404.0001$PEVSPSR_F)

print(da37404.0001$caref)
da37404.0001$caref= as.numeric(da37404.0001$caref)
by(da37404.0001$caref,da37404.0001$RTYP,summary)

##recode spouse dealt with doctors
da37404.0001$careg <- revalue(da37404.0001$PEVSPSR_G, c("(1) Never"="1", "(2) Rarely"="2","(3) Sometimes"="3","(4) Often"="4"))

print(da37404.0001$careg)
da37404.0001$careg= as.numeric(da37404.0001$careg)
by(da37404.0001$careg,da37404.0001$RTYP,summary)
##recode spouse provided time and space
da37404.0001$careh <- revalue(da37404.0001$PEVSPSR_H, c("(1) Never"="1", "(2) Rarely"="2","(3) Sometimes"="3","(4) Often"="4"))

print(da37404.0001$careh)
da37404.0001$careh= as.numeric(da37404.0001$careh)
by(da37404.0001$careh,da37404.0001$RTYP,summary)
##adding variables
da37404.0001$carepos<-da37404.0001$carec + da37404.0001$carea + da37404.0001$careh + da37404.0001$careg + da37404.0001$caref
print(da37404.0001$carepos)
by(da37404.0001$carepos,da37404.0001$RTYP,summary)
s
var(da37404.0001$carepos)
mean (da37404.0001$carepos)

library(psych)
describe(s[["(1) Gay"]])
describe(s[["(2) Lesbian"]])
describe(s[["(3) Straight"]])
describe(da37404.0001$carepos)
s=by(da37404.0001$carepos,da37404.0001$RTYP,summary)

#ANOVA positive care

one.way<- aov(carepos~ RTYP, data=da37404.0001 )
summary(one.way)

#visualise it
library(ggplot2)

ggplot(da37404.0001, aes(x=RTYP, y=carepos, fill=RTYP)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="PRGn")+
scale_x_discrete() + xlab("Relationship type") +
  ylab("Positive Partner Care")

##//////Power in relationship//////////////////////////////////////////////////////////////////////////////////////////////////////////////
print(da37404.0001$GENEGAL)
by(da37404.0001$GENEGAL,da37404.0001$RTYP,summary)
summary(da37404.0001$GENEGAL)
library(ggplot2)
library(waffle)
#248G


pwrg <- c(`Strongly Disagree (2.02%)`=5, `Disagree (10%)`= 25, `Neither Agree nor Disagree (7.66%)`=19, 
          `Agree (35.5%)`=88,`Strongly agree (44.8%)`=111)
waffle(pwrg/5, rows=5, size=0.6,  
       colors=c("darkseagreen1", "darkseagreen3", "aquamarine3",  
                "#3A9ABD","skyblue4"),  
       title="Gay",  
       xlab="1 square = 5 participants")




#342L
pwrl <- c(`Strongly Disagree (1.8%)`=6, `Disagree (7.6%)`= 26, `Neither Agree nor Disagree (11.11%)`=38, 
          `Agree (29.8%)`=102,`Strongly agree (49.7%)`=170)

pwrl

waffle(pwrl/5, rows=5, size=0.6,  
       colors=c("#D62900", "#FF9B55", "salmon",  
                "#D461A6","#A50062"),  
       title=" Lesbian:",  
       xlab="1 square = 5 participants")

#248S
pwrs <- c(`Strongly Disagree (2.01%)`=5, `Disagree (10.5%)`= 26, `Neither Agree nor Disagree (10.08%)`=25, 
          `Agree (40.32%)`=100,`Strongly agree (37.1%)`=92)

pwrs

waffle(pwrs/5, rows=5, size=0.6,  
       colors=c("beige", "bisque2", "bisque4",  
                "salmon1","salmon3"),  
       title="Straight",  
       xlab="1 square = 5 participant",)
       
       
       
       

#//////////////////proportion test///////////////////////////////////////////////////////////////////////
res <- prop.test(x = c(111, 92), n = c(248, 248))

res
#Pie Charts////////////////////////////////////////////////////////////////////////////////////////////////////
#////Lesbian
library(scales)
pwl <- data.frame(
  Agreement = c("Strongly Disagree (1.8%) ",
                "Disagree (7.6%)", 
                "Neither Agree nor Disagree (11.11%)", 
                "Agree (29.8%)", 
                "Strongly agree (49.7%"),
  Lesbian = c(6,26,38,102,170)
)

head(pwl)
p<-ggplot(pwl,aes(x="",y= Lesbian,fill= Agreement))+
  geom_bar(width = 1, stat = "identity")
p

pie <- p +coord_polar("y", start = 0)


pie+ scale_fill_manual(values=c("#D461A6", "#FF9B55", "salmon",  
                                "#A50062","#D62900")) 


#/////straight pie
library(scales)
pwl <- data.frame(
  Agreement = c("Strongly Disagree (2.01%) ",
                "Disagree (10.5%)", 
                "Neither Agree nor Disagree (10.08%)", 
                "Agree (40.32%)", 
                "Strongly agree (37.1%)"),
  Straight = c(5,26,25,100,92)
)
head(pwl)
p<-ggplot(pwl,aes(x="",y= Straight,fill= Agreement))+
  geom_bar(width = 1, stat = "identity")
p

pie <- p +coord_polar("y", start = 0)


pie+ scale_fill_manual(values=c("#D461A6", "#FF9B55", "salmon",  
                                "#A50062","#D62900")) 




#Gender A//////////////////

print(da37404.0001$RGENIDPH)

da37404.0001$gendera <- revalue(da37404.0001$RGENIDPH, c("(1) Strongly Disagree"="1", "(2) Disagree"="2",
                                                        "(3) Neither Agree nor Disagree"="3","(4) Agree"="4",
                                                        "(5) Strongly agree"="5"))

print(da37404.0001$gendera)
da37404.0001$gendera= as.numeric(da37404.0001$gendera)
by(da37404.0001$gendera,da37404.0001$RTYP,summary)

#GenderB

RGENIDHO

summary(da37404.0001$RGENIDHO)
da37404.0001$genderb <- revalue(da37404.0001$RGENIDHO, c("(1) Strongly Disagree"="1", "(2) Disagree"="2",
                                                         "(3) Neither Agree nor Disagree"="3","(4) Agree"="4",
                                                         "(5) Strongly agree"="5"))
da37404.0001$genderb= as.numeric(da37404.0001$genderb)
print(da37404.0001$genderb)
summary(da37404.0001$genderb)
by(da37404.0001$genderb,da37404.0001$RTYP,summary)

#Adding gender a+b
da37404.0001$genderid<-da37404.0001$gendera + da37404.0001$genderb
print(da37404.0001$genderid)
by(da37404.0001$genderid,da37404.0001$RTYP,summary)
#ANOVA

one.way<- aov(genderid~ RTYP, data=da37404.0001 )
summary(one.way)

#visual//////////////////////////////////////////

ggplot(da37404.0001, aes(x=RTYP, y=genderid, fill=RTYP)) + 
  geom_boxplot(alpha=.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")+
  scale_x_discrete() + xlab("Relationship type") +
  ylab("Gender ID:Paricipant")

#////Correlation
cor(da37404.0001$genderid, da37404.0001$carepos)


x <- da37404.0001$genderid
y <- da37404.0001$carepos
cor(x, y)
"reg.line"

library("ggpubr")

ggscatter(da37404.0001, x = "carepos", y = "genderid", 
          color = "#00AFBB", 
          shape = 21,
          size = 3,
          add = "reg.line", add.params = list(color = "black", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gender ID", ylab = "Taking Care of Partner when sick")


res <- cor.test(da37404.0001$genderid, da37404.0001$carepos, 
                method = "spearman")
res
p <- ggplot(da37404.0001, aes(x=genderid, y=carepos))
p + geom_bin2d(bins=8)


###No correlation between gender ID and Taking care of partner



#diadic coping////////////////////////////////////////////////////////////////////////////////////////////////////////
#STRESS A
print(da37404.0001$SPTORDC1)

da37404.0001$stressa <- revalue(da37404.0001$SPTORDC1, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stressa)
da37404.0001$stressa= as.numeric(da37404.0001$stressa)
by(da37404.0001$stressa,da37404.0001$RTYP,summary)
#STRESS C
print(da37404.0001$SPTORDC3)

da37404.0001$stressc <- revalue(da37404.0001$SPTORDC3, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stressc)
da37404.0001$stressc= as.numeric(da37404.0001$stressc)
by(da37404.0001$stressc,da37404.0001$RTYP,summary)

#STRESS D
print(da37404.0001$SPTORDC4)

da37404.0001$stressd <- revalue(da37404.0001$SPTORDC4, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stressd)
da37404.0001$stressd= as.numeric(da37404.0001$stressd)
by(da37404.0001$stressd,da37404.0001$RTYP,summary)
#STRESS F
print(da37404.0001$SPTORDC6)

da37404.0001$stressf <- revalue(da37404.0001$SPTORDC6, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stressf)
da37404.0001$stressf= as.numeric(da37404.0001$stressf)
by(da37404.0001$stressf,da37404.0001$RTYP,summary)
#adding a c d f
da37404.0001$poscope<-da37404.0001$stressf + da37404.0001$stressd + da37404.0001$stressc + da37404.0001$stressa
print(da37404.0001$poscope)
by(da37404.0001$poscope,da37404.0001$RTYP,summary)

library(psych)
s=by(da37404.0001$poscope,da37404.0001$RTYP,summary)
describe(s[["(1) Gay"]])
describe(s[["(2) Lesbian"]])
describe(s[["(3) Straight"]])
describe(da37404.0001$poscope)



#ANOVA
one.way<- aov(poscope~ RTYP, data=da37404.0001 )
summary(one.way)
#visual
ggplot(da37404.0001, aes(x=RTYP, y=poscope, fill=RTYP)) + 
  geom_boxplot(alpha=.5) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="PuRd")+
  scale_x_discrete() + xlab("Relationship type") +
  ylab("Partner positive coping ")





ggscatter(da37404.0001, x = "genderid", y = "poscope", 
          color = "#00AFBB", 
          shape = 21,
          size = 3,
          add = "reg.line", add.params = list(color = "black", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Gender ID", ylab = "Taking Care of Partner when sick")


res <- cor.test(da37404.0001$genderid, da37404.0001$poscope, 
                method = "spearman")
res

p <- ggplot(da37404.0001, aes(x=genderid, y=poscope))
p + geom_bin2d(bins=7)

#Negative Coping b e g h

print(da37404.0001$SPTORDC2)

da37404.0001$stressb <- revalue(da37404.0001$SPTORDC2, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stressb)
da37404.0001$stressb= as.numeric(da37404.0001$stressb)
by(da37404.0001$stressb,da37404.0001$RTYP,summary)

## neg cope e
print(da37404.0001$SPTORDC5)

da37404.0001$stresse <- revalue(da37404.0001$SPTORDC5, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stresse)
da37404.0001$stresse= as.numeric(da37404.0001$stresse)
by(da37404.0001$stresse,da37404.0001$RTYP,summary)
##neg cope g
print(da37404.0001$SPTORDC7)

da37404.0001$stressg <- revalue(da37404.0001$SPTORDC7, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stressg)
da37404.0001$stressg= as.numeric(da37404.0001$stressg)
by(da37404.0001$stressg,da37404.0001$RTYP,summary)
#neg cope h
print(da37404.0001$SPTORDC8)

da37404.0001$stressh <- revalue(da37404.0001$SPTORDC8, c("(1) Very Rarely"="1",
                                                         "(3) Sometimes"="3","(4) Often"="4",
                                                         "(5) Very Often"="5","(2) Rarely"="2"))

print(da37404.0001$stressh)
da37404.0001$stressh= as.numeric(da37404.0001$stressh)
by(da37404.0001$stressh,da37404.0001$RTYP,summary)

#adding b e g h
da37404.0001$negcope<-da37404.0001$stressb + da37404.0001$stresse + da37404.0001$stressg + da37404.0001$stressh
print(da37404.0001$negcope)
summary(da37404.0001$negcope)
by(da37404.0001$negcope,da37404.0001$RTYP,summary)
#ANOVA
one.way<- aov(negcope~ RTYP, data=da37404.0001 )
summary(one.way)
#visual
ggplot(da37404.0001, aes(x=RTYP, y=negcope, fill=RTYP)) + 
  geom_boxplot(alpha=.5) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Purples")+
  scale_x_discrete() + xlab("Relationship type") +
  ylab("Partner negative coping ")
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#happy 
RELHAPPY
print(da37404.0001$RELHAPPY)

da37404.0001$happy <- revalue(da37404.0001$RELHAPPY, c("(1) Extremely Unhappy"="1",
                                                         "(3) A Little Unhappy"="3","(4) Happy"="4",
                                                         "(5) Very Happy"="5","(2) Fairly Unhappy"="2",
                                                         "(6) Extremely Happy"="6","(7) Perfect"="7" ))

print(da37404.0001$happy)
da37404.0001$happy= as.numeric(da37404.0001$happy)
by(da37404.0001$happy,da37404.0001$RTYP,summary)

library(psych)
s=by(da37404.0001$happy,da37404.0001$RTYP,summary)
describe(s[["(1) Gay"]])
describe(s[["(2) Lesbian"]])
describe(s[["(3) Straight"]])
describe(da37404.0001$happy)

#ANOVA
one.way<- aov(happy~ RTYP, data=da37404.0001 )
summary(one.way)
#visual
ggplot(da37404.0001, aes(x=RTYP, y=happy, fill=RTYP)) + 
  geom_boxplot(alpha=.5) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="PRGn")+
  scale_x_discrete() + xlab("Relationship type") +
  ylab("Happyness on relationship ")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#communication
#good A D E H B 
#A
print(da37404.0001$RTELNEED)

da37404.0001$comma <- revalue(da37404.0001$RTELNEED, c("(1) Never"="1",
                                                       "(3) Occasionally"="3","(4) Somewhat Frequently"="4",
                                                       "(5) Very Frequently"="5","(2) Very Little"="2" ))
print(da37404.0001$comma)
da37404.0001$comma= as.numeric(da37404.0001$comma)
by(da37404.0001$comma,da37404.0001$RTYP,summary)
#communication b
print(da37404.0001$RSHWNEED)

da37404.0001$commb <- revalue(da37404.0001$RSHWNEED, c("(1) Never"="1",
                                                       "(3) Occasionally"="3","(4) Somewhat Frequently"="4",
                                                       "(5) Very Frequently"="5","(2) Very Little"="2" ))
print(da37404.0001$commb)
da37404.0001$commb= as.numeric(da37404.0001$commb)
by(da37404.0001$commb,da37404.0001$RTYP,summary)
#communication d
print(da37404.0001$RSTCONVO)

da37404.0001$commd <- revalue(da37404.0001$RSTCONVO, c("(1) Never"="1",
                                                       "(3) Occasionally"="3","(4) Somewhat Frequently"="4",
                                                       "(5) Very Frequently"="5","(2) Very Little"="2" ))
print(da37404.0001$commd)
da37404.0001$commd= as.numeric(da37404.0001$commd)
by(da37404.0001$commd,da37404.0001$RTYP,summary)
#communication e
print(da37404.0001$RSENSE)

da37404.0001$comme <- revalue(da37404.0001$RSENSE, c("(1) Never"="1",
                                                       "(3) Occasionally"="3","(4) Somewhat Frequently"="4",
                                                       "(5) Very Frequently"="5","(2) Very Little"="2" ))
print(da37404.0001$comme)
da37404.0001$comme= as.numeric(da37404.0001$comme)
by(da37404.0001$comme,da37404.0001$RTYP,summary)
#communication h Gives spouse space when they are troubled or stressed?
print(da37404.0001$RGIVSPAC)

da37404.0001$commh <- revalue(da37404.0001$RGIVSPAC, c("(1) Never"="1",
                                                     "(3) Occasionally"="3","(4) Somewhat Frequently"="4",
                                                     "(5) Very Frequently"="5","(2) Very Little"="2" ))
print(da37404.0001$commh)
da37404.0001$commh= as.numeric(da37404.0001$commh)
by(da37404.0001$commh,da37404.0001$RTYP,summary)

#add communication A D E H B 
da37404.0001$commpos<-da37404.0001$commh + da37404.0001$commb + da37404.0001$comme + da37404.0001$commd +
                      da37404.0001$comma
print(da37404.0001$commpos)
summary(da37404.0001$commpos)
by(da37404.0001$commpos,da37404.0001$RTYP,summary)

s=by(da37404.0001$commpos,da37404.0001$RTYP,summary)
describe(s[["(1) Gay"]])
describe(s[["(2) Lesbian"]])
describe(s[["(3) Straight"]])
describe(da37404.0001$commpos)
#ANOVA
one.way<- aov(commpos~ RTYP, data=da37404.0001 )
summary(one.way)
#visual
ggplot(da37404.0001, aes(x=RTYP, y=commpos, fill=RTYP)) + 
  geom_boxplot(alpha=.5) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="PRGn")+
  scale_x_discrete() + xlab("Relationship type") +
  ylab("Possitive communication with partner ")

#relationship///
ggscatter(da37404.0001, x = "commpos", y = "happy", 
          color = "indianred", 
          shape = 10,
          size = 3,
          add = "reg.line", add.params = list(color = "black", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "commpos", ylab = "happy")


#relationship///
ggplot(da37404.0001, aes(x=commpos, y=happy)) +
  geom_point(size=5, shape=19, color="darkseagreen3")+
  geom_smooth(method=lm,  linetype="dashed",
              color="gray22", fill="grey0")

p <- ggplot(da37404.0001, aes(x=commpos, y=happy))
p + geom_bin2d(bins=7)

p <- ggplot(da37404.0001, aes(x=commpos, y=happy))
p + geom_bin2d(bins=7) + scale_fill_distiller(palette = "BuPu")

p <- ggplot(da37404.0001, aes(x=commpos, y=happy))
p + geom_bin2d(bins=7) + scale_fill_distiller(palette = "RdGy") 

res <- cor.test(da37404.0001$genderid, da37404.0001$poscope, 
                method = "spearman")
res




by(da37404.0001$happy,da37404.0001$RTYP,summary)

p <- ggplot(da37404.0001, aes(x=genderid, y=poscope))
p + geom_bin2d(bins=7) + scale_fill_distiller(palette = "RdBu") 

p <- ggplot(da37404.0001, aes(x=genderid, y=carepos))
p + geom_bin2d(bins=7) + scale_fill_distiller(palette = "RdBu") 

res <- cor.test(da37404.0001$genderid, da37404.0001$poscope, 
                method = "spearman")
res

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#/////////////////////////////////////////////////////////////////////////////////////////
#test2
library(readxl)
DVR2 <- read_excel("C:/Users/magal/Desktop/DV relationship/Excel/DVR2.xls")
View(DVR2)

summary(DVR2)


DVR2$Gay <- revalue(DVR2$Gay, c("Not at all in the past 12 months"="1",
                            "Once every few months"="3","Once a month"="4",
                            "Two or three times a month"="5","A few times over the past 12 months"="2",
                            "Once or more a week"="6"))
print(DVR2$Gay)


DVR2$Gay = factor(DVR2$Gay,
                levels = c("1", "2", "3", "4", "5", "6"),
                ordered = TRUE)


DVR2$Lesbian <- revalue(DVR2$Lesbian, c("Not at all in the past 12 months"="1",
                                "Once every few months"="3","Once a month"="4",
                                "Two or three times a month"="5","A few times over the past 12 months"="2",
                                "Once or more a week"="6"))
print(DVR2$Lesbian)


DVR2$Lesbian = factor(DVR2$Lesbian,
                  levels = c("1", "2", "3", "4", "5", "6"),
                  ordered = TRUE)

DVR2$Straight <- revalue(DVR2$Straight, c("Not at all in the past 12 months"="1",
                                        "Once every few months"="3","Once a month"="4",
                                        "Two or three times a month"="5","A few times over the past 12 months"="2",
                                        "Once or more a week"="6"))
print(DVR2$Straight)


DVR2$Straight = factor(DVR2$Straight,
                  levels = c("1", "2", "3", "4", "5", "6"),
                  ordered = TRUE)

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
DVR2$Straight = factor(DVR2$Straight,
                       levels = c("Not at all in the past 12 months", "A few times over the past 12 months",
                                  "Once every few months", "Once a month", "Two or three times a month", "Once or more a week"),
                       ordered = TRUE)
DVR2$Lesbian = factor(DVR2$Lesbian,
                       levels = c("Not at all in the past 12 months", "A few times over the past 12 months",
                                  "Once every few months", "Once a month", "Two or three times a month", "Once or more a week"),
                       ordered = TRUE)
DVR2$Gay = factor(DVR2$Gay,
                       levels = c("Not at all in the past 12 months", "A few times over the past 12 months",
                                  "Once every few months", "Once a month", "Two or three times a month", "Once or more a week"),
                       ordered = TRUE)
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
str(DVR2)
summary(DVR2)
print(DVR2$t)
class(DVR)

DVR2 <- as.data.frame(DVR2)

likert(DVR2)

Result = likert(DVR2)

plot(Result,
     type="bar",col=c("papayawhip", "bisque2", "darksalmon", "slategray4","slategray3","slategray2"))



plot(Result,
     type="bar",col=c("bisque", "bisque3", "gray79", "orchid", "plum","slategray2"))

plot(Result,
     type="bar",col=c("papayawhip", "bisque2", "darksalmon", "lightsalmon","slategray2","slategray"))

plot(Result,
     type="bar",col=c("papayawhip", "bisque2", "darksalmon", "slategray2","slategray","steelblue"))

plot(Result,
     type="bar",col=c("papayawhip", "bisque2", "darksalmon", "slategray4","slategray3","slategray2"))

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
#Stress all rtyp
library(readxl)
STR <- read_excel("C:/Users/magal/Desktop/DV relationship/Excel/STR.xls")
View(STR)
summary(STR)

class(STR)
STR <- as.data.frame(STR)
class(STR)

print(STR$Expresses_that_they_are_on_my_side)

STR$Expresses_that_they_are_on_my_side = factor(STR$Expresses_that_they_are_on_my_side,
                  levels = c("Very Rarely", "Rarely",
                             "Sometimes", "Often", "Very Often"),
                  ordered = TRUE)

STR$See_stressful_situations_in_a_different_light = factor(STR$See_stressful_situations_in_a_different_light,
                  levels = c("Very Rarely", "Rarely",
                             "Sometimes", "Often", "Very Often"),
                  ordered = TRUE)
STR$Listens_to_me = factor(STR$Listens_to_me,
                  levels = c("Very Rarely", "Rarely",
                             "Sometimes", "Often", "Very Often"),
                  ordered = TRUE)
STR$Takes_on_things_that_I_normally_do= factor(STR$Takes_on_things_that_I_normally_do,
                  levels = c("Very Rarely", "Rarely",
                             "Sometimes", "Often", "Very Often"),
                  ordered = TRUE)



likert(STR)

Result = likert(STR)

plot(Result,
     type="bar",col=c("bisque", "bisque3", "gray79", "plum","slategray2"))

plot(Result,
     type="bar",col=c("papayawhip", "bisque2", "darksalmon", "lightsalmon","slategray"))
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#stress RTYP G
library(readxl)
STRG <- read_excel("C:/Users/magal/Desktop/DV relationship/Excel/STRG.xls")
View(STRG)
summary(STRG)

class(STRG)
STRG <- as.data.frame(STRG)
class(STRG)

print(STRG$Expresses_that_they_are_on_my_side)

STRG$Expresses_that_they_are_on_my_side = factor(STRG$Expresses_that_they_are_on_my_side,
                                                levels = c("Very Rarely", "Rarely",
                                                           "Sometimes", "Often", "Very Often"),
                                                ordered = TRUE)

STRG$See_stressful_situations_in_a_different_light = factor(STRG$See_stressful_situations_in_a_different_light,
                                                           levels = c("Very Rarely", "Rarely",
                                                                      "Sometimes", "Often", "Very Often"),
                                                           ordered = TRUE)
STRG$Listens_to_me = factor(STRG$Listens_to_me,
                           levels = c("Very Rarely", "Rarely",
                                      "Sometimes", "Often", "Very Often"),
                           ordered = TRUE)
STRG$Takes_on_things_that_I_normally_do= factor(STRG$Takes_on_things_that_I_normally_do,
                                               levels = c("Very Rarely", "Rarely",
                                                          "Sometimes", "Often", "Very Often"),
                                               ordered = TRUE)



likert(STRG)

Result = likert(STRG)

plot(Result,
     type="bar",col=c("bisque", "bisque3", "gray79", "plum","slategray2"))

plot(Result,
     type="bar",col=c("papayawhip", "bisque2", "darksalmon", "lightsalmon","slategray"))
#stressL//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
library(readxl)
STRL <- read_excel("C:/Users/magal/Desktop/DV relationship/Excel/STRL.xls")
View(STRL)
summary(STRL)

class(STRL)
STRL <- as.data.frame(STRL)
class(STRL)

print(STRL$Expresses_that_they_are_on_my_side)

STRL$Expresses_that_they_are_on_my_side = factor(STRL$Expresses_that_they_are_on_my_side,
                                                 levels = c("Very Rarely", "Rarely",
                                                            "Sometimes", "Often", "Very Often"),
                                                 ordered = TRUE)

STRL$See_stressful_situations_in_a_different_light = factor(STRL$See_stressful_situations_in_a_different_light,
                                                            levels = c("Very Rarely", "Rarely",
                                                                       "Sometimes", "Often", "Very Often"),
                                                            ordered = TRUE)
STRL$Listens_to_me = factor(STRL$Listens_to_me,
                            levels = c("Very Rarely", "Rarely",
                                       "Sometimes", "Often", "Very Often"),
                            ordered = TRUE)
STRL$Takes_on_things_that_I_normally_do= factor(STRL$Takes_on_things_that_I_normally_do,
                                                levels = c("Very Rarely", "Rarely",
                                                           "Sometimes", "Often", "Very Often"),
                                                ordered = TRUE)



likert(STRL)

Result = likert(STRL)

plot(Result,
     type="bar",col=c("bisque", "bisque3", "gray79", "plum","slategray2"))

plot(Result,
     type="bar",col=c("papayawhip", "bisque2", "darksalmon", "lightsalmon","slategray"))



#stress S///////////////////////////////////////////////////////////////////////////////////////////////////////////
library(readxl)
STRS <- read_excel("C:/Users/magal/Desktop/DV relationship/Excel/STRS.xls")
View(STRS)
summary(STRS)

class(STRS)
STRS <- as.data.frame(STRS)
class(STRS)

STRS$Expresses_that_they_are_on_my_side = factor(STRS$Expresses_that_they_are_on_my_side,
                                                 levels = c("Very Rarely", "Rarely",
                                                            "Sometimes", "Often", "Very Often"),
                                                 ordered = TRUE)




STRS$See_stressful_situations_in_a_different_light = factor(STRS$See_stressful_situations_in_a_different_light,
                                                            levels = c("Very Rarely", "Rarely",
                                                                       "Sometimes", "Often", "Very Often"),
                                                            ordered = TRUE)
STRS$Listens_to_me = factor(STRS$Listens_to_me,
                            levels = c("Very Rarely", "Rarely",
                                       "Sometimes", "Often", "Very Often"),
                            ordered = TRUE)
STRS$Takes_on_things_that_I_normally_do= factor(STRS$Takes_on_things_that_I_normally_do,
                                                levels = c("Very Rarely", "Rarely",
                                                           "Sometimes", "Often", "Very Often"),
                                                ordered = TRUE)


library(likert)
likert(STRS)
Result = likert(STRS)
plot(Result,
     type="bar",col=c("papayawhip",
                      "bisque2", 
                      "darksalmon", 
                      "lightsalmon",
                      "slategray"))






#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#reg sex and happyness
print(da37404.0001$SEXFRQ)                     
da37404.0001$SEXFRQ <- revalue(da37404.0001$SEXFRQ, c("(1) Not at all in the past 12 months"="1",
                                "(3) Once every few months"="3","(4) Once a month"="4",
                                "(5) Two or three times a month"="5","(2) A few times over the past 12 months"="2",
                                "(6) Once or more a week"="6")) 

print(da37404.0001$SEXFRQ)
da37404.0001$SEXFRQ= as.numeric(da37404.0001$SEXFRQ)
by(da37404.0001$SEXFRQ,da37404.0001$RTYP,summary)


#ANOVA
one.way<- aov(SEXFRQ~ RTYP, data=da37404.0001 )
summary(one.way)




