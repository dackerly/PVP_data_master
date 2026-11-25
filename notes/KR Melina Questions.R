KR cleanup - questions for Melina


Plots:
L422   # NEED TO REVIEW ALL THIS WITH MELINA


L665
# a few errors in Types - fix these in csvs?
## EDIT AFTER MELINA FIX

L695
# assign cumulative tag pulled variable - run again after fixing 2020 - Melina
tAllh$Tag.Pulled.Cum <- 0
tAllh$Tag.Pulled.Cum[which(tAllh$Tag.Pulled.18==1)] <- 1
tAllh$Tag.Pulled.Cum[which(tAllh$Tag.Pulled.19==1)] <- 1
tAllh$Tag.Pulled.Cum[which(tAllh$Tag.Pulled.20=="1")] <- 1
table(tAllh$Tag.Pulled.Cum)

## EDIT AFTER MELINA FIX
tp20comments <- which(tAllh$Tag.Pulled.20 %in% c('LOST','NEW SA','NOT FOUND','NOT FOUND IN 19 OR 20','NOT FOUND, DOSER KILL','TAG MOVED TO RSPRT','TYPE IN 19, NOT TR'))
write.csv(tAllh[tp20comments,c('Plot','Num','Tag.Pulled.20')],'data/tp20problems.csv')


# badXY in plots