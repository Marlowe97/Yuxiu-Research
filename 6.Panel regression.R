library(Formula)
library(plm)
setwd('C:/Users/chong/python/assignment1')
sam <- read.csv('assign4.csv',row.names = 1)
# pgr <- pdata.frame(sam,index = c("sic3", "yeara"),
#                    drop.index = TRUE)
# gr_pool <- plm(roa ~ lnage+lnmv,
#                data = pgr,
#                model = "within")

library(Matrix)
library(lfe)
library(bife)
# pool <- lm(formula = roa ~ log(crspage+1) + log(mv+1) 
#            + D_.3 + D_.2 +D_.1 + D_0 + D_1 + D_2 + 
#              D_3,data = sam,
#            na.action = na.omit)

summary(fixed_q)
fixed_roa <- felm(formula = roa ~  D_.3 + D_.2 +D_.1 + 
                    D_0 + D_1 + D_2 + D_3 + log(crspage+1) + 
                    log(mv+1)|sic3 + yeara|0|sic3,
                  data = sam,
                  na.action = na.omit)
fixed_q <- felm(formula = q ~  D_.3 + D_.2 +D_.1 + 
                  D_0 + D_1 + D_2 + D_3 + log(crspage+1) + 
                  log(mv+1)|sic3 + yeara|0|sic3,
                data = sam,
                na.action = na.omit)
fixed_divyld <- felm(formula = divyld ~  D_.3 + D_.2 +D_.1 + 
                       D_0 + D_1 + D_2 + D_3 + log(crspage+1) + 
                       log(mv+1)|sic3 + yeara|0|sic3,
                     data = sam,
                     na.action = na.omit)
fixed_invest <- felm(formula = inv ~  D_.3 + D_.2 +D_.1 + 
                       D_0 + D_1 + D_2 + D_3 + log(crspage+1) + 
                       log(mv+1)|sic3 + yeara|0|sic3,
                     data = sam,
                     na.action = na.omit)
fixed_grow <- felm(formula = growth3yr ~  D_.3 + D_.2 +D_.1 + 
                       D_0 + D_1 + D_2 + D_3 + log(crspage+1) + 
                       log(mv+1)|sic3 + yeara|0|sic3,
                     data = sam,
                     na.action = na.omit)
sam1 <- na.omit(sam[c('newceo','D_.3','D_.2','D_.1','D_0',
                      'D_1','D_2','D_3','crspage','mv',
                      'yeara','sic3')])
fixed_ceo <- bife(newceo ~ D_.3 + D_.2 + D_.1 + 
                    D_0 + D_1 + D_2 + D_3 + log(crspage+1) +
                    log(mv+1) + fixed(yeara)|sic3,
                  data = sam1)
summary(fixed_ceo)
# library(survival)
# fixed_ceo1 <- clogit(newceo ~ D_.3 + D_.2 + D_.1 + 
#                        D_0 + D_1 + D_2 + D_3 + log(crspage+1) +
#                        log(mv+1) + strata(yeara) + strata(sic3),
#                      data = sam1)

# library(stargazer)
# stargazer(fixed_roa,fixed_q,fixed_divyld,fixed_invest,
#           fixed_ceo,
#           covariate.labels = c('D_.3','D_.2','D_.1','D0',
#                                'D1','D2','D3','LNAGE','LNMV'),
#           keep.stat = c('n','rsq'),
#           no.space = TRUE)

fit1 <- as.data.frame(summary(fixed_q)$coefficient)
fit2 <- as.data.frame(summary(fixed_roa)$coefficient)
fit3 <- as.data.frame(summary(fixed_divyld)$coefficient)
fit4 <- as.data.frame(summary(fixed_invest)$coefficient)

df <- data.frame(fit1[,c(1,3)],
                 fit2[,c(1,3)],
                 fit3[,c(1,3)],
                 fit4[,c(1,3)],
                 fixed_ceo$par_corr$beta[1:9],
                 fixed_ceo$par_corr$beta[1:9]/fixed_ceo$par_corr$se_beta[1:9])

write.csv(df,file = "C:/Users/chong/python/assignment4/table.csv")
