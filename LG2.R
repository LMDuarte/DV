by(da37404.0001$SEXFRQ,da37404.0001$RTYP,summary)

sx=by(da37404.0001$SEXFRQ,da37404.0001$RTYP,summary)



sx[["(1) Gay"]]
proportions (sx[["(1) Gay"]])
proportions (sx[["(2) Lesbian"]])
proportions (sx[["(3) Straight"]])


gsx= proportions (sx[["(1) Gay"]])

print(gsx)

barplot(gsx, ylim=c(0.00,0.30),main="sex frequency",names.arg=c("1",
                                             "2",
                                             "3",
                                             "4",
                                             "5",
                                             "6"))
                                             

lsx= proportions (sx[["(2) Lesbian"]])

print(lsx)

barplot(lsx, ylim=c(0.00,0.30),main="Lesbian sex frequency",names.arg=c("1",
                                                                "2",
                                                                "3",
                                                                "4",
                                                                "5",
                                                                "6"))
ssx= proportions (sx[["(3) Straight"]])

print(ssx)

barplot(ssx, ylim=c(0.00,0.30),main="straignt sex frequency",names.arg=c("1",
                                                                "2",
                                                                "3",
                                                                "4",
                                                                "5",
                                                                "6"))

