library(data.table); library(mgcv); library(gratia)

dat <- fread('c:/users/darpa2/downloads/MASTER FILE - Synced SAV and WQ Data for ALL Segments - CBWQData_ANALYSISFILE.csv')

james <- dat[grepl('^JMS', CBSeg2003)]
james[, ':='(date_num = as.numeric(as.Date(SampleDate, '%m/%d/%Y')),
             CBSeg2003 = as.factor(CBSeg2003),
             Station = as.factor(Station))]

# Mess around with modeling secchi depth
m1 <- gam(MeasureValue ~ s(date_num, m = 2) + 
            s(date_num, CBSeg2003,bs = 'fs', m = 2) +
            s(Station, bs = 're'),
          data = james,
          subset = Parameter == 'SECCHI',
          method = 'REML')

m2 <- gam(MeasureValue ~ s(date_num, m = 2) + 
            s(date_num, CBSeg2003,bs = 'fs', m = 2) +
            s(Station, bs = 're'),
          data = james, family = 'Gamma',
          subset = Parameter == 'SECCHI',
          method = 'REML')

m3 <- gam(MeasureValue ~ s(date_num, m = 2) + 
            s(date_num, CBSeg2003,bs = 'fs', m = 2) +
            s(Station, bs = 're'),
          data = james, family = Gamma('log'),
          subset = Parameter == 'SECCHI',
          method = 'REML')

m4 <- gam(MeasureValue ~ s(date_num) + 
            s(date_num, by = CBSeg2003, m = 1) +
            s(CBSeg2003, bs = 're') +
            s(Station, bs = 're'),
          data = james, family = Gamma(),
          subset = Parameter == 'SECCHI',
          method = 'REML')

draw(m1)

#Consider beta modeling, where secchi depth is a proportion of the water column




mb <- gam(MeasureValue/TotalDepth ~ s(date_num) + 
            s(date_num, by = CBSeg2003, m = 1) +
            s(CBSeg2003, bs = 're') +
            s(Station, bs = 're'),
          data = james, family = betar,
          subset = Parameter == 'SECCHI',
          method = 'REML')














## Ignore all this -- just here in reserve
k <- dcast(james[Parameter %in% c('SECCHI', 'TURB_NTU', 'KD')],
           Station + SampleDate + Layer ~ Parameter,
           value.var = 'MeasureValue',
           fun.aggregate = function(.) median(., na.rm= T))

k <- k[complete.cases(k)]
k <- k[, SampleDate := as.Date(SampleDate, format = '%m/%d/%Y')]
k[, SampleDate := as.numeric(SampleDate)]
k[, Station := as.factor(Station)]


m2 <- gam(SECCHI ~ s(SampleDate) + s(Station, bs = 're'),
          data = k, family = Gamma())


new_dat <- data.frame(SampleDate = seq(min(k$SampleDate), max(k$SampleDate), length.out = 200),
                      Station = k$Station[1])

dat <- predict(m2, new_dat, type = 'terms', terms = 's(SampleDate)',
               se.fit = T)

dat <- as.data.table(dat)
dat[, ':='(fit = 1/fit,
           lci = 1/(fit - se.fit*1.96),
           uci = 1/(fit + se.fit*1.96),
           x = seq(min(k$SampleDate), max(k$SampleDate), length.out = 200))]


library(ggplot2)

ggplot(data = james) +
  geom_histogram(aes(x = MeasureValue)) +
  facet_wrap(~ Parameter)


sech <- james[Parameter == 'SECCHI']
james[, Station := as.factor(Station)]

library(mgcv)
turb kd sechhi
m1 <- gam(list( ~ s(year) + s(Station, bs = 're'),
                data = james)
          
          
          library(gratia)
          appraise(m1)
          