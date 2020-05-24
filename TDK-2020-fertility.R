
library(timeSeries)
library(tseries)
library(forecast)
library(ggplot2)
library(mFilter)
library(MTS)
library(readxl)
library(writexl) 
library(vars)
library(rms)
library(car)
library(broom)
library(urca)
library(fUnitRoots)
library(knitr)
library(scales)
library(tidyr)
library(dplyr)
library(ggpmisc)
library(naniar)
library(tsDyn)
library(data.table)
library(formattable)
library(readxl)
library(DT)
library(rio)
library(sparkline)
library(viridis)
library(hrbrthemes)
library(plotly)
library(ggthemes)
library(kableExtra)
library(extrafont)



load("C:/Users/user/Desktop/szakdolgozat/fertiliy/R files/Szakdolgozat/Thesis_Variables_2020.04.10.RData")


MyTheme <-   theme_economist() + theme(
  legend.title = element_blank(),
  plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
  axis.title = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
  axis.ticks.length = unit(5, "points"),
  panel.grid.major = element_line(colour = "grey", 
                                  size = rel(1.75))
)

df=data.frame(LiveBirthAndFertility$Year, LiveBirthAndFertility$LiveBirthTotal/LiveBirthAndFertility$LiveBirthTotal[1], LiveBirthAndFertility$LiveBirthTo1000/LiveBirthAndFertility$LiveBirthTo1000[1], LiveBirthAndFertility$TotalFertility/LiveBirthAndFertility$TotalFertility[1])
names(df)=c("Year","Összes született gyermek", "Ezer nőre jutó született gyermekek száma", "Teljes termékenységi arány")
df=df %>% gather(key="variable", value = "value", -Year)

p1 <- ggplot(df, aes(x = Year, y = value)) + 
  geom_line(aes(color = variable), size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand=c(0,0),limits=c(0.5, 1.5)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(title = "1. ábra. Születési mutatók bázisindexe", subtitle = "1960=100%",
       caption="A bázisindex használatát a három mutató együttesen való ábrázolásának céljából tartottam indokoltnak.") +
  xlab("Év")+
  ylab("Százalék") +
  scale_color_economist() +
  MyTheme + theme(
    legend.text = element_text(size = 10)
  )

df=data.frame(LiveBirthAndFertility$Year, LiveBirthAndFertility$TotalFertility,rep(2.1,length(LiveBirthAndFertility$TotalFertility)))
names(df)=c("Year", "Teljes termékenységi arány","Az egyszerű reprodukciót jelentő érték")
df=df %>% gather(key="variable", value = "value", -Year)

p2 <- ggplot(df, aes(x = Year, y = value)) + 
  geom_line(aes(color = variable,linetype=variable), size = 2) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(title = "2. ábra. Magyarország termékenységi rátájának alakulása", subtitle="1960-2018",
  caption="Megjegyzés: Az ábra alapján jól látható, hogy az érték a vizsgált időszakban jelentősen a 2,1-es egyszerű reprodukció határát jelentő érték alatt maradt, globális minimumát (1,23) pedig 2011-ben érte el."     
       ) +
  scale_color_manual(values = c("#e3120b","#244747")) +
  scale_linetype_manual(values = c("longdash","solid"))+
  ylab("Gyermek/anya")+
  xlab("Év")+
  MyTheme

t1 <- tibble::tribble(
  ~"Szerző", ~"Szerző által mely országra fogalmazódott meg a hipotézis", ~"Forrás alapján elemzésem fókuszába helyezett indikátorok",
  "*Tárkányi*",                                            "Magyarország",                       "jövedelem és foglalkoztatás mutatói",
  "*Cumming* és *Dettling*",                                      "Egyesült Királyság",                      "megtakarítások és eladósodás mutatói",
  "*Levitt* és *Dubner*",                                        "Egyesült Államok",            "bűnözési statisztikák, császármetszések aránya",
  "*Duflo* és *Banerjee*",                                                   "India",           "nyugdíjkifizetések mértéke, szegénységi mutatók"
) %>% data.frame()
names(t1)=c("Szerző", "Szerző által mely országra fogalmazódott meg a hipotézis", "Forrás alapján elemzésem fókuszába helyezett indikátorok")
t1 <- t1 %>% kable(caption = "1. táblázat. Szakirodalom áttekintése alapján dolgozatom témájának releváns gazdasági- és társadalmi indikátorainak köre", align = c("l",rep("c",2))) %>%
  kable_styling(bootstrap_options = "striped",full_width = T, fixed_thead = T)


df=data.frame(LiveBirthAndFertility$Year,
              hpfilter(LiveBirthAndFertility$TotalFertility, freq = 100)$trend
              ,LiveBirthAndFertility$TotalFertility)

names(df)=c("Year", "HP","TFR")

p3 <- ggplot(df, aes(x = Year)) + 
  geom_line(aes(y=TFR, color = "Magyarország teljes termékenységi arányszáma"),linetype="solid", size = 2) +
  geom_line(aes(y=HP,color = "HP-szűrővel leválaszott trend"),linetype="longdash", size = 2) +
  scale_y_continuous(expand=c(0,0), limits = c(0,2.5)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(title = "3. ábra. Magyarország termékenységi rátájának alakulása és annak trendje", subtitle="1960-2018, Trend leválasztása Hodrick-Prescott szűrő segítségével (alfa=100)") +
  scale_color_manual(values = c("Magyarország teljes termékenységi arányszáma"="#244747","HP-szűrővel leválaszott trend"="#e3120b")) +
  ylab("Gyermek/anya")+
  xlab("Év")+
  MyTheme

df=data.frame(LiveBirthAndFertility$Year[-1],diff(LiveBirthAndFertility$TotalFertility))
names(df)=c("x","y")

p4 <- ggplot(df, aes(x=x, y=y)) +
  geom_hline(yintercept = 0, size=1.3, color="#4a4a4a") +
  geom_line(size = 2, color="#e3120b") +
  scale_y_continuous(expand=c(0,0), limits = c(-0.5, 0.5)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(title = "4. ábra. Magyarország termékenységi rátájának növekménytagjai", subtitle="1961-2018, Differenciázás segítségével az eredeti idősor determinisztikus, illetve sztochasztikus trendje is eltávolítható") +
  ylab("Gyermek/anya")+
  xlab("Év")+
  MyTheme

Myacf <- acf(diff(LiveBirthAndFertility$TotalFertility), plot = FALSE, lag.max = 20)
Myacf <- with(Myacf, data.frame(lag, acf))
Mypacf <- acf(diff(LiveBirthAndFertility$TotalFertility), plot = FALSE, type = "partial", lag.max = 20)
Mypacf <- with(Mypacf, data.frame(lag, acf))
df=data.frame(
  0:20,
  Myacf$acf,
  c(1,Mypacf$acf)
)
names(df)=c("lag","acf","pacf")
df=df %>% gather("variable","value",-"lag")
variable_names_p5 = list("acf" = "Korrelogram (ACF)",
                      "pacf" = "Parciális korrelogram (PACF)")

variable_labeller_p5 <- function(variable, value) {
  return(variable_names_p5[value])
}

p5 <- ggplot(df, aes(x=lag, y=value)) +
  geom_hline(yintercept = 0, color="grey38", size=1) +
  geom_line(size=1.2, color="#244747") +
  geom_point( 
    shape=21, fill="#336666", color="#244747",size=3,stroke=1.5) +
  facet_wrap(~ variable, ncol = 1,labeller = variable_labeller_p5) +
  labs(title="5. ábra. Magyarország termékenységi rátájának növekménytagjainak korrelogramja",
       subtitle="Autokorreláció és parciális autokorreláció függvény értékei egyes késleltetések mellett"
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0)) +
  xlab("Késleltetés száma") +
  ylab("") +
  theme_economist(dkpanel = T) + theme(
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
    axis.ticks.length = unit(5, "points"),
    panel.grid.major = element_line(colour = "grey", 
                                    size = rel(1.75)),
    panel.spacing = unit(2, "lines")
  )

# Table 2, but imported back from excel
x=diff(LiveBirthAndFertility$TotalFertility)
x=head(x,-10)
auto.arima(x, ic = "aicc",trace = T, method ="CSS")

t2 <- tibble::tribble(
  ~Modell,     ~AIC,
  "ARMA (2, 2) konstans nélkül", -242.885,
  "ARMA (0, 0) konstans nélkül", -240.849,
  "ARMA (1, 0) konstans nélkül", -245.811,
  "ARMA (0, 1) konstans nélkül", -247.251,
  "ARMA (0, 0) konstanssal", -241.485,
  "ARMA (1, 1) konstans nélkül", -243.801,
  "ARMA (0, 2) konstans nélkül", -244.906,
  "ARMA (1, 2) konstans nélkül", -242.047,
  "ARMA (0, 1) konstanssal", -248.674,
  "ARMA (1, 1) konstanssal", -245.643,
  "ARMA (0, 2) konstanssal", -246.473,
  "ARMA (1, 0) konstanssal", -247.669,
  "ARMA (1, 2) konstanssal",  -244.05
) %>% data.frame() %>% kable( caption = "2. táblázat. A magyar termékenységi arányszám növekménytagjaira illesztett ARMA-modellekhez tartozó információs kritériumok", align = c("l",rep("c",2))) %>%
  kable_styling(bootstrap_options = "striped",full_width = T, fixed_thead = T)

# Table 3
df=matrix(nrow = 12,ncol = 3)
df[1,3]=adf.test(LiveBirthAndFertility$LiveBirthTotal)$p.value
df[2,3]=adf.test(diff(LiveBirthAndFertility$LiveBirthTotal))$p.value
df[3,3]=adf.test(log(LiveBirthAndFertility$LiveBirthTotal))$p.value
df[4,3]=adf.test(diff(log(LiveBirthAndFertility$LiveBirthTotal)))$p.value
df[5,3]=adf.test(LiveBirthAndFertility$LiveBirthTo1000)$p.value
df[6,3]=adf.test(diff(LiveBirthAndFertility$LiveBirthTo1000))$p.value
df[7,3]=adf.test(log(LiveBirthAndFertility$LiveBirthTo1000))$p.value
df[8,3]=adf.test(diff(log(LiveBirthAndFertility$LiveBirthTo1000)))$p.value
df[9,3]=adf.test(LiveBirthAndFertility$TotalFertility)$p.value
df[10,3]=adf.test(diff(LiveBirthAndFertility$TotalFertility))$p.value
df[11,3]=adf.test(log(LiveBirthAndFertility$TotalFertility))$p.value
df[12,3]=adf.test(diff(log(LiveBirthAndFertility$TotalFertility)))$p.value
df=data.frame(df)
df[,3]=percent(df[,3],d=2)
df[,1]=c(rep("Összes születés",4),rep("Ezer főre eső születés",4),rep("TTA",4))
df[,2]=rep(c("x","diff(x)","log(x)","diff(log(x))"),3)
names(df)=c("Születési mutató","Transzformáció","ADF-teszthez tartozó p-érték")
t3 <- kable(df,caption = "3. táblázat. Megfontolásra kerülő modellek", align = c("l",rep("c",3))) %>%
  kable_styling(bootstrap_options = "striped",full_width = T, fixed_thead = T)


x1 = 1:12 #collector vectors
x2 = 1:12
x3 = 1:12
x4 = 1:12
x = ts(LiveBirthAndFertility$LiveBirthTotal)
MyArima = auto.arima(head(x,-10), ic = "aic", trace = F)
x1[1] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[1] = sum(abs(tail(x, 10) - forecast(MyArima, 10)$mean) /
              tail(x, 10) * 100) / 10
x3[1] = checkresiduals(MyArima, plot = F)$p.value
x4[1] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(head(x,-10),
                     ic = "aic",
                     trace = F,
                     method = "CSS")
x1[2] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[2] = sum(abs(tail(x, 10) - forecast(MyArima, 10)$mean) /
              tail(x, 10) * 100) / 10
x3[2] = checkresiduals(MyArima, plot = F)$p.value
x4[2] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(diff(log(head(x,-10))), ic = "aic", trace = F)
x1[3] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[3] = sum(abs((tail(exp(
  cumsum(c(
    log(x[1]), MyArima$fitted, forecast(MyArima, 10)$mean
  ))
), 10) - tail(x, 10))) / tail(x, 10) * 100) / 10
x3[3] = checkresiduals(MyArima, plot = F)$p.value
x4[3] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(diff(log(x)),
                     ic = "aic",
                     trace = F,
                     method = "CSS")
x1[4] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[4] = sum(abs((tail(exp(
  cumsum(c(
    log(x[1]), MyArima$fitted, forecast(MyArima, 10)$mean
  ))
), 10) - tail(x, 10))) / tail(x, 10) * 100) / 10
x3[4] = checkresiduals(MyArima, plot = F)$p.value
x4[4] = jarque.bera.test(na.omit(MyArima$residuals))$p.value


x = ts(LiveBirthAndFertility$LiveBirthTo1000)
MyArima = auto.arima(head(x,-10), ic = "aic", trace = F)
x1[5] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[5] = sum(abs(tail(x, 10) - forecast(MyArima, 10)$mean) /
              tail(x, 10) * 100) / 10
x3[5] = checkresiduals(MyArima, plot = F)$p.value
x4[5] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(head(x,-10),
                     ic = "aic",
                     trace = F,
                     method = "CSS")
x1[6] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[6] = sum(abs(tail(x, 10) - forecast(MyArima, 10)$mean) /
              tail(x, 10) * 100) / 10
x3[6] = checkresiduals(MyArima, plot = F)$p.value
x4[6] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(diff(log(head(x,-10))), ic = "aic", trace = F)
x1[7] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[7] = sum(abs((tail(exp(
  cumsum(c(
    log(x[1]), MyArima$fitted, forecast(MyArima, 10)$mean
  ))
), 10) - tail(x, 10))) / tail(x, 10) * 100) / 10
x3[7] = checkresiduals(MyArima, plot = F)$p.value
x4[7] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(diff(log(x)),
                     ic = "aic",
                     trace = F,
                     method = "CSS")
x1[8] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[8] = sum(abs((tail(exp(
  cumsum(c(
    log(x[1]), MyArima$fitted, forecast(MyArima, 10)$mean
  ))
), 10) - tail(x, 10))) / tail(x, 10) * 100) / 10
x3[8] = checkresiduals(MyArima, plot = F)$p.value
x4[8] = jarque.bera.test(na.omit(MyArima$residuals))$p.value



x = ts(LiveBirthAndFertility$TotalFertility)
MyArima = auto.arima(head(x,-10), ic = "aic", trace = F)
x1[9] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                "")
x2[9] = sum(abs(tail(x, 10) - forecast(MyArima, 10)$mean) /
              tail(x, 10) * 100) / 10
x3[9] = checkresiduals(MyArima, plot = F)$p.value
x4[9] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(head(x,-10),
                     ic = "aic",
                     trace = F,
                     method = "CSS")
x1[10] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                 "")
x2[10] = sum(abs(tail(x, 10) - forecast(MyArima, 10)$mean) /
               tail(x, 10) * 100) / 10
x3[10] = checkresiduals(MyArima, plot = F)$p.value
x4[10] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(diff(log(head(x,-10))), ic = "aic", trace = F)
x1[11] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                 "")
x2[11] = sum(abs((tail(exp(
  cumsum(c(
    log(x[1]), MyArima$fitted, forecast(MyArima, 10)$mean
  ))
), 10) - tail(x, 10))) / tail(x, 10) * 100) / 10
x3[11] = checkresiduals(MyArima, plot = F)$p.value


x4[11] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

MyArima = auto.arima(diff(log(x)),
                     ic = "aic",
                     trace = F,
                     method = "CSS")
x1[12] = paste("ARMA (", paste(MyArima$arma[1:2], collapse = ", "), ")", sep =
                 "")
x2[12] = sum(abs((tail(exp(
  cumsum(c(
    log(x[1]), MyArima$fitted, forecast(MyArima, 10)$mean
  ))
), 10) - tail(x, 10))) / tail(x, 10) * 100) / 10
x3[12] = checkresiduals(MyArima, plot = F)$p.value

x4[12] = jarque.bera.test(na.omit(MyArima$residuals))$p.value

df = matrix(ncol = 7, nrow = 12)
df = data.frame(
  c(
    rep("Összes születés", 4),
    rep("Ezer főre eső születés", 4),
    rep("TTA", 4)
  ),
  rep(c(
    "diff(x)", "diff(x)", "log(diff(x))", "log(diff(x))"
  ), 3),
  
  x1,
  rep(c("ML", "CSS"), 6),
  percent(x2 / 100, d = 2),
  percent(x3, d = 2),
  percent(x4, d = 2)
)
names(df) = c(
  "Születési mutató",
  "Transzformáció",
  "Illesztett modell",
  "Becslés módszere",
  "MAPE",
  "Ljung-Box (p-érték)",
  "Jarque-Bera (p-érték)"
)
rownames(df) = NULL



t4 <- kable(df,caption = "4. táblázat. ARMA-modellekkel készített ex-post előrejelzések eredményei",align = c("l",rep("c",6))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = T, fixed_thead = F)



x=auto.arima(head(ts(LiveBirthAndFertility$TotalFertility),-10),
             ic = "aic",
             trace = F)
#checkresiduals(x)

df=data.frame(x=x$residuals)

p6 <- ggplot(data = df) +
  stat_function(fun = dnorm, args = list(mean = mean(df$x), sd = sd(df$x)),aes(color="Azonos várható értékő és szórású normáleloszlás gyakorisággörbéje"),geom = "area",fill="#244747", size=0,alpha=0.5)+
  geom_density(aes(x, color="Reziduumok gyakorisági görbéje"),fill="#e3120b",alpha=0.5,linetype="solid" ,position = "stack",size=1.2) + 
  scale_color_manual(values = c("Reziduumok gyakorisági görbéje" = "#e3120b","Azonos várható értékő és szórású normáleloszlás gyakorisággörbéje" = "#244747")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,10)) +
  labs(title="6. ábra. Reziduumok eloszlásának vizsgálata gyakorisági görbén",
       subtitle = "A TTA-ra illesztett ARIMA(0,1,1) modell reziduumai a normál eloszlásnál csúcsosabb eloszlással rendelkeznek (Leptokurtikus)"
  ) +
  xlab("") +
  ylab("") +
  MyTheme +
  theme(
    panel.grid.major = element_line(colour = "white", 
                                    size = rel(1)),
    panel.grid.major.x  = element_line(colour = "white", 
                                       size = rel(1)),
    legend.title = element_blank(),
    legend.position = "none",
  )

x = ts(LiveBirthAndFertility$TotalFertility,
       frequency = 1,
       start = 1960)
h=10
MyArima=auto.arima(x,ic = "aic",trace = F,method = "CSS")
Myforecast=forecast(MyArima,level = c(95), h = h)
MyFrame=matrix(ncol = 5,nrow = length(x)+h)
MyFrame[,1]=c(1960:(1960+length(x)+h-1))
MyFrame[1:length(x),2]=x
MyFrame[1:length(x),3]=Myforecast$fitted
MyFrame[((length(x)+1):(length(x)+h)),3]=Myforecast$mean
MyFrame[((length(x)+1):(length(x)+h)),4]=Myforecast$lower
MyFrame[((length(x)+1):(length(x)+h)),5]=Myforecast$upper
x=c(1960:(1960+length(Myforecast$x)+h-1))
y1=c(1960:(1960+length(Myforecast$x)+h-1))
for (i in 1:(length(Myforecast$x)+h)) {
  y1[i]=as.numeric( MyFrame[i,2])
}
y2=c(Myforecast$fitted,Myforecast$mean)
y3=c(1960:(1960+length(Myforecast$x)+h-1))
for (i in 1:(length(Myforecast$x)+h)) {
  y3[i]=as.numeric( MyFrame[i,4])
}

y4=c(1960:(1960+length(Myforecast$x)+h-1))
for (i in 1:(length(Myforecast$x)+h)) {
  y4[i]=as.numeric( MyFrame[i,5])
}
df=tbl_df(data.frame(x,y1,y2,y3,y4))

df1=df[1:3]
df2=data.frame(y3=c(rep(NA,lengths(df)[1]),df$y3),y4=c(rep(NA,lengths(df)[1]),df$y4))
df1=df1 %>% gather("variable","value",-"x")
df1["variable"]=ifelse(df1$variable=="y1","Eredeti idősor","Pontbecslés")
df2["x"]=df1["x"]
df2=na.exclude(df2)

p7<- ggplot(df) +
  geom_ribbon(data=df2,aes(x=x,ymin=y3,ymax=y4,fill="95%-os konfidencia intervallum"), alpha = 0.3) +
  geom_line(data=df1,
            aes(x=x,y=value,color=variable),size=2) +
  scale_x_continuous(expand=c(0,0)) +
  coord_cartesian(ylim=c(0,2.5)) +
  scale_color_manual(values=c("#e3120b","#336666")) +
  scale_fill_manual(name = "", values = c("95%-os konfidencia intervallum" = "#336666")) +
  ylab("Gyermek/anya")+
  xlab("Év")+
  labs(title = "7. ábra. Előrejelzés a teljes termékenységi arányszámra", subtitle = "ARIMA (0,1,1) modell, a következő 10 évre") +
  MyTheme

df=data.frame(c(ifelse(is.na(t(head(FertilityRates,1))),paste(names(FertilityRates),"*",sep=""),names(FertilityRates)),
                ifelse(is.na(t(head(FertilityRates,1))),paste(names(FertilityRates),"*",sep=""),names(FertilityRates))),
              c(rep("1960",length(FertilityRates)),rep("2017",length(FertilityRates))),
              c(t(head(FertilityRates,1)),t(tail(FertilityRates,1))))
names(df)=c("Country","Year","Value")
df=subset(df,Country!="Year")
ggplot(df, aes(x=Country, y=Value,fill=Year)) + 
  geom_bar(stat="identity",position="dodge",color="black")+
  ylab("Gyermek/nő")+
  xlab("Ország")+
  scale_y_continuous(expand=c(0,0)) + 
  coord_cartesian(ylim = c(0, 7.5))+
  #scale_fill_manual(values=c("#336666", "#e3120b"))+
  scale_color_economist() +
  expand_limits(y=0) + 
  labs(title = "A teljes termékenységi arányszám alakulása globálisan",
       subtitle ="Az OECD országok és az OECD partnerországainak adatai, 1960-ban és 2017-ben",
       caption = "A *-gal jelölt országok esetében nem áll rendelkezésre 1960-as adat."
  ) +
  MyTheme + theme(
    axis.text.x = element_text(angle = 90, vjust = 0.45,size = 11),
  )

df=data.frame(c(ifelse(is.na(t(head(FertilityRates,1))),paste(names(FertilityRates),"*",sep=""),names(FertilityRates)),
                ifelse(is.na(t(head(FertilityRates,1))),paste(names(FertilityRates),"*",sep=""),names(FertilityRates))),
              c(rep("1960",length(FertilityRates)),rep("2017",length(FertilityRates))),
              c(t(head(FertilityRates,1)),t(tail(FertilityRates,1))))
names(df)=c("Country","Year","Value")
df=subset(df,Country!="Year")

# 8th plot
p8 <- ggplot(df, aes(x=Country, y=Value,fill=Year)) + 
  geom_bar(stat="identity",position="dodge",color="black")+
  ylab("Gyermek/nő")+
  xlab("Ország")+
  scale_y_continuous(expand=c(0,0)) + 
  coord_cartesian(ylim = c(0, 7.5))+
  #scale_fill_manual(values=c("#336666", "#e3120b"))+
  scale_color_economist() +
  expand_limits(y=0) + 
  labs(title = "8. ábra. A teljes termékenységi arányszám alakulása globálisan",
       subtitle ="Az OECD országok és az OECD partnerországainak adatai, 1960-ban és 2017-ben",
       caption = "A *-gal jelölt országok esetében nem áll rendelkezésre 1960-as adat."
  ) +
  MyTheme + theme(
    axis.text.x = element_text(angle = 90, vjust = 0.45,size = 11),
  )

# Correlation neighbour countries
df = FertilityRates[-1]
df = df[, !names(df) %in% c("OAVG", "EU28")]
Tcor=0
Ncor=0
NNcor=0
Tn=0
Nn=0
NNn=0
for (i in 1:length(df)) {
  for (j in 1:length(df)) {
    if (i>j) {
      x = df[i]
      y = df[j]
      xy=data.frame(x=x,y=y)
      names(xy)=c("x","y")
      xy=na.exclude(xy)
      Mycor=cor(xy$x,xy$y)
      Tcor=Tcor+Mycor
      Tn=Tn+1
      if (!is.na(NeighbourCountry[i,j])) {
        Ncor=Ncor+Mycor
        Nn=Nn+1
      } else {
        NNcor=NNcor+Mycor
        NNn=NNn+1
      }
    }
  }
}
cat(paste(
  "Összes ország között a korrelációk átlaga: ",
  percent(Tcor / Tn, digits = 0 , format = "d"),
  "\n",
  sep = ""
))
cat(paste(
  "Szomszédos országok között a korrelációk átlaga: ",
  percent(Ncor / Nn, digits = 0, format = "d"),
  "\n",
  sep = ""
))
cat(paste(
  "Nem szomszédos országok között a korrelációk átlaga: ",
  percent(NNcor / NNn, digits = 0, format = "d"),
  sep = ""
))

df=FertilityRates[-1]
NdiffFertilityRates=vector()
Years=vector()

Delta=vector()
for (i in 1:length(df)) {
  x=df[i]
  names(x)="x"
  x=x$x
  x=na.exclude(x)
  Delta[i]=tail(x,1)-head(x,1)
  x=ts(x)
  NdiffFertilityRates[i]=ndiffs(
    x,
    alpha = 0.05,
    test = "adf",
    max.d = 10,
    selectlags = "AIC",
    type = "trend"
  )
  y=data.frame(y=FertilityRates$Year,x=df[i])
  y=na.exclude(y)
  Years[i]=paste(min(y$y),"-",max(y$y),sep = "")
}

V=sapply(df, sd,na.rm=T)/sapply(df, mean,na.rm=T)

df=data.frame(Years,Delta,percent(V,d=0),NdiffFertilityRates)
names(df)=c("Megfigyelt évek","Abszolút változás","Varibilitás","Idősor integráltságának rendje")
kable(df,caption="Teljes termékenységi arányszám nemzetközi összehasonlításban", align=c(rep("c",4))) %>%
  column_spec(1, width = "1em", border_right = T) %>%
  column_spec(5, width = "2em") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = F, fixed_thead = T)
# Cointegrity
df=FertilityRates[-1]
CointOutput = matrix(nrow = length(df),
                     ncol = length(df),
                     dimnames = list(names(df), names(df)))
for (i in 1:length(df)) {
  for (j in 1:length(df)) {
    CointOutput[i,j]=0
    if (i!=j) {
      
      if (NdiffFertilityRates[i]==NdiffFertilityRates[j] & NdiffFertilityRates[i]>0) {
        Mylm=data.frame(y=df[i], x=df[j])
        Mylm=na.exclude(Mylm)
        names(Mylm)=c("y","x")
        Mylm=lm(y~x,data=Mylm)
        x=Mylm$residuals
        x=ts(x)
        x=ndiffs(
          x,
          alpha = 0.05,
          test = "adf",
          max.d = 10,
          selectlags = "AIC",
          type = "trend"
        )
        if ((NdiffFertilityRates[i]-1)==x) {
          CointOutput[i,j]=2
        } else  {
          CointOutput[i,j]=1
        }
      } 
    }
  }
}
CointOutput=data.frame(CointOutput)

df=data.frame(v=rownames(CointOutput),CointOutput)

df = df %>% gather(key = "variable", value = "Coint",-v)
names(df) = c("x", "y", "Coint")

df[,3]=ifelse(df[,3]==0,"A teszt nem elvégezhető",ifelse(df[,3]==1,"Nem kointegráltak","Kointegráltak"))


p10 <- ggplot(df, aes(x, y, fill = factor(Coint))) +
  geom_tile(color="black") +
  labs(title = "10. ábra. Kointegrációs tesztek eredményei") +
  scale_fill_manual(values=c("#acc8d4", "#244747", "#8abbd0")) +
  xlab("OLS során függő változó")+
  ylab("OLS során regresszor")+
  MyTheme +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.45,colour = "black", size = 11),
    axis.ticks.y  = element_line(colour = "black", size=0.6),
    axis.ticks.length.y = unit(.2, "cm"),
    axis.ticks.x  = element_line(colour = "black", size=0.6),
    axis.ticks.length.x = unit(.2, "cm"),
    axis.line.x = element_blank()
  )

df=CointOutput
df=df[-c(19,42),-c(19,42)]
df[,]=df[,]+1
cat(paste("Összes párosítás (OAVG és EU28 nélkül): ",sum(df==3)+sum(df==2)+sum(df==1)-length(df),"\n",sep = ""))
cat(paste("Összes elvégezhető teszt (OAVG és EU28 nélkül): ",sum(df==3)+sum(df==2),"\n",sep = ""))
cat(paste("Összes kointegrálás (OAVG és EU28 nélkül): ",sum(df==3),"\n",sep = ""))

df=(df*NeighbourCountry)
df[is.na(df)] <- 0
cat(paste("Összes szomszédos párosítás: ",sum(df==3)+sum(df==2)+sum(df==1),"\n",sep = ""))
cat(paste("Összes szomszédos elvégezhető teszt: ",sum(df==3)+sum(df==2),"\n",sep = ""))
cat(paste("Összes szomszédos kointegrálás: ",sum(df==3),sep = ""))

df=subset(CointOutput,select="HUN")
df=data.frame(names(CointOutput),df)
df=subset(df,HUN=="2")
cat(paste("Magyarországgal kointegráló országok (OLS során Magyarország a regresszor): ",paste((df[,1]),collapse = ", "), "\n"))

df=subset(t(CointOutput),select="HUN",HUN="2")
df=data.frame(names(CointOutput),df)
df=subset(df,HUN=="2")
cat(paste("Magyarországgal kointegráló országok (OLS során Magyarország a függő változó): ",paste((df[,1]),collapse = ", ")))

HUN <- ts(FertilityRates["HUN"], start = 1960)
CZE <- ts(FertilityRates["CZE"], start = 1960)
HUN_d1 = diff(HUN)
CZE_d1 <- diff(CZE)
MyLm <- lm(HUN ~ CZE)
MyRes <- MyLm$residuals
MyFitted = MyLm$fitted.values
df = matrix(nrow = length(HUN), ncol = 7)
df[, 1] = 1960:2017
df[, 2] = HUN
df[, 3] = CZE
df[, 4] = MyFitted
df[-1, 5] = HUN_d1
df[-1, 6] = CZE_d1
df[, 7] = MyRes
df = data.frame(df)
names(df) = c("Year", "HUN", "CZE", "MyFitted", "HUN_d1", "CZE_d1", "MyRes")
df = df %>% gather(key = "variable", value = "value", -Year)

y = data.frame(unique(df["variable"]), c(1, 1, 1, 2, 2, 3))
df = data.frame(df, MyFacet = plyr::join(df["variable"], y)[, 2])

y = data.frame(unique(df["variable"]),
               c("HUN", "CZE", "OLS becslése (HUN)", "HUN", "CZE", "HUN"))
df = data.frame(df, MyCol = plyr::join(df["variable"], y)[, 2])


variable_names_p9 = list("1" = "Transzformálatlan idősorok",
                      "2" = "Differenciázott idősorok (d=1)",
                      "3" = "OLS maradéktagja")

variable_labeller_p9 <- function(variable, value) {
  return(variable_names_p9[value])
}


p9 <- ggplot(df, aes(Year, value, color = MyCol)) +
  geom_line(size = 2) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~ MyFacet,
             scales = "free_y",
             ncol = 1,
             labeller = variable_labeller_p9) +
  labs(title = "9. ábra. A magyar és cseh TTA idősorok között fennálló kointegráció") +
  xlab("Év") +
  ylab("Gyermek/anya") +
  scale_colour_economist() +
  theme_economist(dkpanel = T) + theme(
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
    axis.ticks.length = unit(5, "points"),
    panel.grid.major = element_line(colour = "grey", 
                                    size = rel(1.75))
  )

df=Input[-1]
NdiffInput=vector()
for (i in 1:length(df)) {
  x=df[i]
  x=na.exclude(x)
  x=ts(x)
  NdiffInput[i]=ndiffs(x,test="adf",max.d = 5)
}
for (i in 1:length(df)) {
  if (NdiffInput[i]>1) {
    x=df[i]
    x=na.exclude(x)
    if (sum(x<0)==0) {
      x=diff(log(x))
      x=ts(x)  
      if (ndiffs(x,test="adf",max.d = 5)==0) {
        NdiffInput[i]="ld"
      }
    }
  }
}
df = data.frame(NdiffInput)
names(df) = "Alkalmazott differenciák száma"
rownames(df) = names(Input)[-1]
kable(df, caption = "Alkalmazott differenciák száma a magyar gazdasági-társadalmi változókon a satcionaritás biztosításának érdekében", align = "c") %>%
  column_spec(2, width = "2em", border_left = T) %>%
  column_spec(1, width = "3em") %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = F,
    fixed_thead = T,
  ) %>%
  footnote(general = "ld-vel jelöltem azokat az esetek, \namelyekben a változó logaritmusának \nvettem elsőrendű differenciáját.")

df = Input[-1]
d_input = matrix(nrow = lengths(df)[1], ncol = length(df))
for (i in 1:length(df)) {
  d = NdiffInput[i]
  x = data.frame(c(1:lengths(df)[1]), df[i])
  x = na.exclude(x)
  t = x[, 1]
  x = x[, 2]
  if (d == "ld") {
    x = diff(log(x))
    t = t[-1]
    d_input[t, i] = x
  } else  {
    d = as.numeric(d)
    if (d > 0) {
      t = t[-c(1:d)]
      x = diff(x, diff = d)
      d_input[t, i] = x
    } else{
      d_input[t, i] = x
    }
  }
}
d_input = data.frame(d_input)
names(d_input) = names(df)
rownames(d_input) = 1960:2017

d_livebirthandfertility=data.frame(
  LiveBirthTotal=c(NA,diff(LiveBirthAndFertility$LiveBirthTotal)),
  LiveBirthTo1000=c(NA,diff(LiveBirthAndFertility$LiveBirthTo1000)),
  TotalFertility=c(NA,diff(LiveBirthAndFertility$TotalFertility))
)
d_livebirthandfertility=head(d_livebirthandfertility,-1)

y = c(NA, diff(head(
  LiveBirthAndFertility$TotalFertility, -1
)))
Lags = vector()
x_AIC = vector()
y_AIC = vector()
x_cause = vector()
inst_cause = vector()
y_cause = vector()
x = data.frame(d_input$Marriage, y)
names(x) = c("x", "y")
x = na.exclude(x)
x = ts(x)
p = 0
Stop = 0
while (Stop == 0) {
  p = p + 1
  if (p > 1) {
    remove(MyVar)
    remove(MyVarSum)
  }
  try({
    MyVar <- vars::VAR((x), p = p, type = "const")
    MyVarSum <- summary(MyVar)
  }
  , silent = T)
  if (exists("MyVarSum") == F) {
    Stop = 1
  } else {
    Lags[p] = p
    x_AIC[p] = sum(MyVar$varresult$x$residuals * MyVar$varresult$x$residuals) *
      exp(2 * (MyVar$varresult$x$rank + 1) / MyVar$obs)
    y_AIC[p] = sum(MyVar$varresult$y$residuals * MyVar$varresult$y$residuals) *
      exp(2 * (MyVar$varresult$y$rank + 1) / MyVar$obs)
    Mytest = causality(MyVar, cause = "x")
    x_cause[p] = Mytest$Granger$p.value[1]
    inst_cause[p] = Mytest$Instant$p.value[1]
    Mytest = causality(MyVar, cause = "y")
    y_cause[p] = Mytest$Granger$p.value[1]
  }
}
df = data.frame(Lags, x_AIC, y_AIC, x_cause * 100, inst_cause * 100, y_cause *
                  100)
names(df) = c("Lags", 3, 1, 2, 5, 4)
df = df %>% gather(key = "variable", value = "value",-Lags)
variable_names_p11 = list(
  "1" = "Modell alapján becsült TTA reziduumaiból számított AIC",
  "2" = "Házasság kötések száma Granger-oka-e a TTA-nak : F-próba p-értéke (százalék)",
  "3" = "Modell alapján becsült házasság kötések számának reziduumaiból számított AIC",
  "4" = "TTA Granger-oka-e a házasságkötések alakulásának : F-próba p-értéke (százalék)",
  "5" = "Fennáll-e egy idejű Granger-okság : próba p-értéke (százalék)"
)
variable_labeller_p11 <- function(variable, value) {
  return(variable_names_p11[value])
}

df2=df
df2[3]=ifelse(df2[2]!=3 & df2[2]!=1, -Inf, NA)
df2["value2"]=ifelse(df2[2]!=3 & df2[2]!=1,5,NA)
names(df2)=c("Lags","variable","value","value2")

p11 <- ggplot(data = df, aes(Lags, value)) +
  geom_ribbon(data=df2,aes(x=Lags,ymin=value, ymax=value2, fill="5%-os szignifikanciaszint"), alpha=0.5) +
  scale_fill_manual(values = c("5%-os szignifikanciaszint" = "#f8766d")) +
  geom_line(size = 1.2, color="#244747") +
  geom_point( 
    shape=21, fill="#336666", color="#244747",size=3,stroke=1.5) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("") +
  labs(title = "11. ábra. Granger-oksági tesztek módszertani bemutatása",
       subtitle= "A teljes termékenységi arányszám és a házasságkötések számából kapott VAR-modellek eredményei",
       caption="A vízszintes tengelyen mindegyik esetben az alkalmazott késleltetések száma szerepel,\nmíg a függőleges tengely az összes esetben a vizsgált mutató szerint értelmezendő") +
  xlab("Felhasznált késleltetések száma") +
  facet_wrap( ~ variable,
              ncol = 1,
              scales = "free_y",
              labeller = variable_labeller_p11) +
  theme_economist(dkpanel = T) + theme(
    plot.title = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
    axis.ticks.length = unit(5, "points"),
    panel.grid.major = element_line(colour = "grey", 
                                    size = rel(1.75)),
    legend.title = element_blank()
  )

y = c(NA, diff(head(
  LiveBirthAndFertility$TotalFertility, -1
)))
GrangerOutput = matrix(nrow = length(d_input), ncol = 4)
for (i in 1:length(d_input)) {
  x = data.frame(d_input[i], y)
  names(x) = c("x", "y")
  x = na.exclude(x)
  x = ts(x)
  p = 0
  Stop = 0
  while (Stop == 0) {
    p = p + 1
    if (p > 1) {
      remove(MyVar)
      remove(MyVarSum)
    }
    try({
      MyVar <- vars::VAR((x), p = p, type = "const")
      MyVarSum <- summary(MyVar)
    }
    , silent = T)
    if (exists("MyVarSum") == F) {
      Stop = 1
    } else {
      ActualAICofx = sum(MyVar$varresult$x$residuals * MyVar$varresult$x$residuals) *
        exp(2 * (MyVar$varresult$x$rank + 1) / MyVar$obs)
      ActualAICofy = sum(MyVar$varresult$y$residuals * MyVar$varresult$y$residuals) *
        exp(2 * (MyVar$varresult$y$rank + 1) / MyVar$obs)
      if (p == 1) {
        LstAICx = ActualAICofx
        LstAICy = ActualAICofy
        Cinst = ""
        Cx = ""
        Cy = ""
      }
      
      if (ActualAICofy <= LstAICy) {
        LstAICy = ActualAICofy
        
        Mytest = causality(MyVar, cause = "x")
        if (Mytest$Granger$p.value[1] < 0.05) {
          if (Cx == "") {
            Cx = p
          } else  {
            Cx = paste(Cx, p, sep = ", ")
          }
          Cinst = Mytest$Instant$p.value[1]
        }
      }
      
      if (ActualAICofx <= LstAICx) {
        LstAICx = ActualAICofx
        Mytest = causality(MyVar, cause = "y")
        if (Mytest$Granger$p.value[1] < 0.05) {
          if (Cy == "") {
            Cy = p
          } else {
            Cy = paste(Cy, p, sep = ", ")
          }
          Cinst = Mytest$Instant$p.value[1]
        }
      }
    }
  }
  if (Cinst < 0.05) {
    if (Cx=="" & Cy=="") {
      GrangerOutput[i, 4] = ""
    } else {
      GrangerOutput[i, 4] = "van"
    }
    
  } else {
    GrangerOutput[i, 4] = ""
  }
  GrangerOutput[i, 1] = p-1
  GrangerOutput[i, 2] = Cx
  GrangerOutput[i, 3] = Cy
}
GrangerOutput = data.frame(NdiffInput, GrangerOutput)
names(GrangerOutput) = c("d","max p","x->y", "x->y", "x-y")
rownames(GrangerOutput) = names(d_input)


t5 <- kable(GrangerOutput, align =c("c","c","c","c"),caption = "5. táblázat. A teljes termékenységi arányszám és a vizsgált gazdasági-társadalmi változók közötti Granger-okság feltárására irányuló próbák eredménye") %>%
  kable_styling(bootstrap_options = "striped", full_width = T, fixed_thead = T)


y = c(NA, diff(head(
  LiveBirthAndFertility$LiveBirthTo1000, -1
)))
GrangerOutput = matrix(nrow = length(d_input), ncol = 4)
for (i in 1:length(d_input)) {
  x = data.frame(d_input[i], y)
  names(x) = c("x", "y")
  x = na.exclude(x)
  x = ts(x)
  p = 0
  Stop = 0
  while (Stop == 0) {
    p = p + 1
    if (p > 1) {
      remove(MyVar)
      remove(MyVarSum)
    }
    try({
      MyVar <- vars::VAR((x), p = p, type = "const")
      MyVarSum <- summary(MyVar)
    }
    , silent = T)
    if (exists("MyVarSum") == F) {
      Stop = 1
    } else {
      ActualAICofx = sum(MyVar$varresult$x$residuals * MyVar$varresult$x$residuals) *
        exp(2 * (MyVar$varresult$x$rank + 1) / MyVar$obs)
      ActualAICofy = sum(MyVar$varresult$y$residuals * MyVar$varresult$y$residuals) *
        exp(2 * (MyVar$varresult$y$rank + 1) / MyVar$obs)
      if (p == 1) {
        LstAICx = ActualAICofx
        LstAICy = ActualAICofy
        Cinst = ""
        Cx = ""
        Cy = ""
      }
      
      if (ActualAICofy <= LstAICy) {
        LstAICy = ActualAICofy
        
        Mytest = causality(MyVar, cause = "x")
        if (Mytest$Granger$p.value[1] < 0.05) {
          if (Cx == "") {
            Cx = p
          } else  {
            Cx = paste(Cx, p, sep = ", ")
          }
          Cinst = Mytest$Instant$p.value[1]
        }
      }
      
      if (ActualAICofx <= LstAICx) {
        LstAICx = ActualAICofx
        Mytest = causality(MyVar, cause = "y")
        if (Mytest$Granger$p.value[1] < 0.05) {
          if (Cy == "") {
            Cy = p
          } else {
            Cy = paste(Cy, p, sep = ", ")
          }
          Cinst = Mytest$Instant$p.value[1]
        }
      }
    }
  }
  if (Cinst < 0.05) {
    if (Cx=="" & Cy=="") {
      GrangerOutput[i, 4] = ""
    } else {
      GrangerOutput[i, 4] = "van"
    }
  } else {
    GrangerOutput[i, 4] = ""
  }
  GrangerOutput[i, 1] = p-1
  GrangerOutput[i, 2] = Cx
  GrangerOutput[i, 3] = Cy
}
GrangerOutput = data.frame(NdiffInput, GrangerOutput)
names(GrangerOutput) = c("d","max p","x->y", "x->y", "x-y")
rownames(GrangerOutput) = names(d_input)



t6 <- kable(GrangerOutput, align =c("c","c","c","c"),caption = "6. táblázat. Az ezer főre jutó éves születésszám és a vizsgált gazdasági-társadalmi változók közötti Granger-okság feltárására irányuló próbák eredménye") %>%
  kable_styling(bootstrap_options = "striped", full_width = T, fixed_thead = T)

y = c(NA, diff(head(
  LiveBirthAndFertility$LiveBirthTotal, -1
)))
GrangerOutput = matrix(nrow = length(d_input), ncol = 4)
for (i in 1:length(d_input)) {
  x = data.frame(d_input[i], y)
  names(x) = c("x", "y")
  x = na.exclude(x)
  x = ts(x)
  p = 0
  Stop = 0
  while (Stop == 0) {
    p = p + 1
    if (p > 1) {
      remove(MyVar)
      remove(MyVarSum)
    }
    try({
      MyVar <- vars::VAR((x), p = p, type = "const")
      MyVarSum <- summary(MyVar)
    }
    , silent = T)
    if (exists("MyVarSum") == F) {
      Stop = 1
    } else {
      ActualAICofx = sum(MyVar$varresult$x$residuals * MyVar$varresult$x$residuals) *
        exp(2 * (MyVar$varresult$x$rank + 1) / MyVar$obs)
      ActualAICofy = sum(MyVar$varresult$y$residuals * MyVar$varresult$y$residuals) *
        exp(2 * (MyVar$varresult$y$rank + 1) / MyVar$obs)
      if (p == 1) {
        LstAICx = ActualAICofx
        LstAICy = ActualAICofy
        Cinst = ""
        Cx = ""
        Cy = ""
      }
      
      if (ActualAICofy <= LstAICy) {
        LstAICy = ActualAICofy
        
        Mytest = causality(MyVar, cause = "x")
        if (Mytest$Granger$p.value[1] < 0.05) {
          if (Cx == "") {
            Cx = p
          } else  {
            Cx = paste(Cx, p, sep = ", ")
          }
          Cinst = Mytest$Instant$p.value[1]
        }
      }
      
      if (ActualAICofx <= LstAICx) {
        LstAICx = ActualAICofx
        Mytest = causality(MyVar, cause = "y")
        if (Mytest$Granger$p.value[1] < 0.05) {
          if (Cy == "") {
            Cy = p
          } else {
            Cy = paste(Cy, p, sep = ", ")
          }
          Cinst = Mytest$Instant$p.value[1]
        }
      }
    }
  }
  if (Cinst < 0.05) {
    if (Cx=="" & Cy=="") {
      GrangerOutput[i, 4] = ""
    } else {
      GrangerOutput[i, 4] = "van"
    }
    
  } else {
    GrangerOutput[i, 4] = ""
  }
  GrangerOutput[i, 1] = p-1
  GrangerOutput[i, 2] = Cx
  GrangerOutput[i, 3] = Cy
}
GrangerOutput = data.frame(NdiffInput, GrangerOutput)
names(GrangerOutput) = c("d","max p","x->y", "x->y", "x-y")
rownames(GrangerOutput) = names(d_input)

t7 <- kable(GrangerOutput, align =c("c","c","c","c"),caption = "7. táblázat. Az évenkénti születésszám és a vizsgált gazdasági-társadalmi változók közötti Granger-okság feltárására irányuló próbák eredménye") %>%
  kable_styling(bootstrap_options = "striped", full_width = T, fixed_thead = T)

df=data.frame(d_input$Activs,d_livebirthandfertility$LiveBirthTotal)
names(df)=c("Activs","LiveBirthTotal")
df=ts(df,start = 1960)
df=na.exclude(df)
MyVar=vars::VAR(df,p=18,type = "const")

df=select(data.frame(summary(MyVar)$varresult$Activs$coefficients),Estimate)
df=data.frame(df[seq(from=2, to=nrow(df), by=2),])
df["l"]=1:18
rownames(df)=NULL
df=df[c(2,1)]
names(df)=c("Késleltetések száma","Béta")
t8 <- kable(df, align = c("c","c"),
      caption="8. táblázat. Az aktívak számából (d=ld) és a teljes születésszámból a VAR(18) modellben aktívak számára becsült egyenletben szereplő születésszám késleltetett értékeihez tartozó koefficiensek értéke"
)

df=data.frame(d_input$Consumption,d_livebirthandfertility$LiveBirthTotal)
names(df)=c("Consumption","LiveBirthTotal")
df=ts(df,start = 1960)
df=na.exclude(df)
MyVar=vars::VAR(df,p=18,type = "const")
df1 <- data.frame(0:18,irf(MyVar, impulse = "Consumption", response = c("Consumption","LiveBirthTotal"),
                           n.ahead = 18, ortho = TRUE)$irf)
names(df1)=c("Lags","Consumption","LiveBirthTotal")
df2 <- df1
for (i in 2:nrow(df2)) {
  df2[i,2]= df1[i,2]+df2[i-1,2]
  df2[i,3]= df1[i,3]+df2[i-1,3]
}

df1=data.frame(c(rep("IRF",nrow(df1)),rep("CIRF",nrow(df1))),
               c(rep(df1$Lags,2)),
               c(df1$Consumption,rep(NA,nrow(df1))),
               c(df1$LiveBirthTotal,rep(NA,nrow(df1)))
)
names(df1)=c("place","Lags","Egy főre eső fogyasztás indexe (d=1)","Teljes születésszám (d=1)")

df1=gather(df1, key=variable, value=value, -c("Lags", "place"))

df1["valuemin"]=ifelse(as.numeric(df1$value)>=0,0,df1$value)
df1["valuemax"]=ifelse(as.numeric(df1$value)<=0,0,df1$value)


names(df2)=c("Lags","Egy főre eső fogyasztás indexe (d=1)","Teljes születésszám (d=1)")

df2["place"]=rep("CIRF",nrow(df2))
df2=gather(df2, key=variable, value=value, -c("Lags", "place"))

p12 <- ggplot(data=filter(df1,place=="IRF"),aes(x=Lags)) +
  geom_ribbon(aes(ymin=valuemin,ymax=valuemax, fill="IRF"), alpha = 0.4, color="#e3120b", size=1) +
  scale_fill_manual(values = c("IRF"="#e3120b")) +
  geom_line(data=df2, aes(x=Lags, y=value, color="CIRF"), size=2) +
  scale_color_manual(values = c("CIRF"="#244747")) +
  geom_hline(yintercept = 0) +
  facet_wrap(.~variable, scales = "free_y") +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + xlab("Sokk óta eltelt évek száma") +
  labs(title="12. ábra. A fogyasztás sokkjának születésszámban lecsapódó sokkja",
       subtitle = "Impulzus és kumulált impulzus válaszfüggvény"
  ) +
  MyTheme

df=data.frame(d_input$HouseholdDebt,d_livebirthandfertility$TotalFertility)
names(df)=c("HouseholdDebt","TotalFertility")

df=na.exclude(df)
df=ts(df)
MyVar=vars::VAR(df,p=4,type = "const")
p <- data.frame(0:5,irf(MyVar, impulse = "TotalFertility", response = c("HouseholdDebt","TotalFertility"),
                        n.ahead = 5, ortho = TRUE)$irf)

names(p)=c("t","2","1")
variable_names_p13 = list(
  "2" = "Háztartások adósságállománya\nrendelkezésre álló jövedelmük \nszázalékában (d=ld)",
  "1" = "Teljes termékenységi arányszám\n(d=1)"
)

variable_labeller_p13 <- function(variable, value) {
  return(variable_names_p13[value])
}
p = p %>% gather(key = "variable", value = "value", -t)

p13 <- ggplot(p, aes(t, value)) +
  geom_hline(yintercept=0, color="#244747" ,size=1) +
  geom_line(size = 1.5,color="#e3120b") +
  scale_x_discrete(expand = c(0, -1),limits=c(0:5)) +
  scale_y_continuous(expand = c(0.02,0.02)) +
  facet_wrap(~ variable, ncol = 2,labeller = variable_labeller_p13,scale="free") +
  labs(title = "13. ábra. A teljes termékenységi arányszámban bekövetkező sokk lecsapódása a háztartások adósságállományában",subtitle ="Impulzus válasz-függvény",caption="A VAR-modellben szereplő késleltetések száma: 4") +
  xlab("Sokkot követően eltelt évek száma") + ylab("") +
  theme_economist(dkpanel = T) + theme(
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
    axis.ticks.length = unit(5, "points"),
    panel.grid.major = element_line(colour = "grey", 
                                    size = rel(1.75)),
    panel.spacing = unit(3, "lines")
  )

df=data.frame(year=1960:2017,head(LiveBirthAndFertility$TotalFertility,-1),Input$HouseholdDebt)
names(df)=c("year","TTA","HHD")
df=na.exclude(df)
l=3
df$HHD=c(
  rep(NA,l),
  df$HHD[l:nrow(df)-3]
)
df=na.exclude(df)

mean_df_HHD=mean(df$HHD)

p14 <- ggplot(data=df) +
  geom_hline(aes(yintercept = mean_df_HHD,linetype = "Átlag érték" ),size=1,color="black") +
  scale_linetype_manual(values= c("Átlag érték" = "dashed"))  +
  geom_vline(xintercept = mean(df$TTA),linetype = "dashed",size=1,color="black") +
  geom_point(aes(x=TTA, y=HHD),shape=21, fill="#8abbd0", color="#8abbd0",size=4,stroke=2, alpha = 0.7) +
  geom_abline(aes(color = "Analitikus regressziófüggvény", slope=lm(HHD~TTA, data=df)$coefficients[2], intercept =lm(HHD~TTA, data=df)$coefficients[1]), size=2) +
  annotate(
    geom = 'segment',
    y = Inf,
    yend = Inf,
    x = -Inf,
    xend = Inf,
    color="gray",
    size= 1.5
  ) +
  ylab("Háztartások adósságállománya 3 évvel később a rendelkezésre\nálló jövedelmük arányában (százalék)") +
  xlab("Teljes termékenységi arányszám") +
  labs(title = "14. ábra. A háztartások adósságállománya és a TTA közti kapcsolat",
       subtitle="A függőleges tengelyen a háztatások jövedelmének 3 évvel későbbi értéke szerepel"
  ) +
  scale_color_manual(values =c("Analitikus regressziófüggvény" = "#e3120b")) +
  theme_economist_white(gray_bg = F) + theme(
    axis.ticks.length = unit(5, "points"),
    axis.ticks.y  = element_line(colour = "gray", size=1.1),
    axis.ticks.length.y = unit(.3, "cm"),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
    legend.key.size = unit(3,"line")
  )

df <- df %>% select(TTA, HHD) # correlation
df
cor(df)
lm(HHD~TTA, data=df)

#Family benefits
df=data.frame(d_input$FamilyBenefits,d_livebirthandfertility$TotalFertility)
names(df)=c("FamilyBenefits","TTA")
df=ts(df,start = 1960)
df=na.exclude(df)
MyVar=vars::VAR(df,p=1,type = "const")

cat(paste("Családtámogatás előjele a modellben:" ,format(round(MyVar$varresult$TTA$coefficients[1], digits=2))))


df=data.frame(head(LiveBirthAndFertility$LiveBirthTo1000,-1),Input$Marriage)
names(df)=c("y","x")
v=ts(lm(y~x,df)$residuals)
ndiffs(v)

p15 <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(aes(x=1960:2017, y=v), color="#1167a6", size=2) +
  scale_x_continuous(expand=c(0,0)) +
  xlab("Év") + ylab("") + labs(
    title="15. ábra. A születésszám az utóbbi években elmarad a házasságkötések számától",
    subtitle="Az ezer főre eső születésszámnak ezer főre eső házasságkötéssel magyarázott OLS reziduumai") +
  MyTheme


df=data.frame(d_input$UnemploymentT,d_livebirthandfertility$TotalFertility)
names(df)=c("UnemploymentT","TTA")
df=ts(df,start = 1960)
df=na.exclude(df)
MyVar=vars::VAR(df,p=5,type = "const")
p <- data.frame(0:10,irf(MyVar, impulse = "UnemploymentT", response = c("UnemploymentT","TTA"),
                         n.ahead = 10, ortho = TRUE)$irf)

names(p)=c("t","1","2")
variable_names_p16 = list("1" = "Munkanélküliségi ráta (d=ld)",
                      "2" = "Teljes termékenységi arányszám (d=1)")

variable_labeller_p16 <- function(variable, value) {
  return(variable_names_p16[value])
}
p = p %>% gather(key = "variable", value = "value", -t)
p16 <- ggplot(p, aes(t, value)) +
  geom_hline(yintercept=0, color="#244747" ,size=1) +
  geom_line(size = 1.5,color="#e3120b") +
  scale_x_discrete(expand = c(0, -1),limits=c(0:10)) +
  scale_y_continuous(expand = c(0.02,0.02)) +
  facet_wrap(~ variable, ncol = 1,labeller = variable_labeller_p16,scale="free") +
  labs(title = "16. ábra. Munkanélküliségben bekövetkező sokk lecsapódása",subtitle ="Impulzus válasz-függvény",caption="A VAR-modellben szereplő késleltetések száma: 5") +
  xlab("Sokkot követően eltelt évek száma") + ylab("") +
  theme_economist(dkpanel = T) + theme(
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
    axis.ticks.length = unit(5, "points"),
    panel.grid.major = element_line(colour = "grey", 
                                    size = rel(1.75)),
    panel.spacing = unit(3, "lines")
  )


df=data.frame(Input$Year,d_input$Gini,d_livebirthandfertility$TotalFertility)
names(df)=c("Year","Gini","TTA")
df=na.exclude(df)
df=gather(df,key = "variable",value="value",-Year )

p17 <- ggplot(data=df) +
  geom_line(aes(x=Year, y=value, color=variable), size=1.5) +
  geom_point(aes(x=Year, y=value, color=variable), size=3)+
  scale_color_economist() +
  xlab("Év") + ylab("") + labs(title = "17. ábra. A Gini-mutató (d=ld) és a teljes termékenységi ráta (d=1) együttmozgása") + scale_x_continuous(expand=c(0,0)) +
  MyTheme

df=data.frame(d_input$CriminalTotal,d_livebirthandfertility$LiveBirthTotal)
names(df)=c("CriminalTotal","LiveBirthTotal")
df=na.exclude(df)
df=ts(df,start = 1965)
MyVar=vars::VAR(df,p=16,type = "const")
cat(
  paste(
    "A VAR(p=16) modell inverz gyökei:",
    paste0(format(round(vars::roots(MyVar),2)),
           collapse = ", "
    )
  )
)

df=data.frame(d_input$CriminalTotal,d_livebirthandfertility$LiveBirthTotal)
names(df)=c("CriminalTotal","LiveBirthTotal")
df=na.exclude(df)
df=ts(df,start = 1965)
MyVar=vars::VAR(df,p=12,type = "const")
cat(
  paste(
    "A VAR(p=12) modell inverz gyökei:",
    paste0(format(round(vars::roots(MyVar),2)),
           collapse = ", "
    )
  )
)

df=data.frame(d_input$CriminalTotal,d_livebirthandfertility$LiveBirthTotal)
names(df)=c("CriminalTotal","LiveBirthTotal")
df=na.exclude(df)
df=ts(df,start = 1965)
MyVar=vars::VAR(df,p=12,type = "const")
p <- data.frame(0:17,irf(MyVar, impulse = "CriminalTotal", response = c("CriminalTotal","LiveBirthTotal"),
                         n.ahead = 17, ortho = TRUE)$irf)

names(p)=c("t","1","2")
variable_names_p18 = list("1" = "Bűnelkövetők száma (d=1)",
                      "2" = "Évenkénti összes születésszám (d=1)")

variable_labeller_p18 <- function(variable, value) {
  return(variable_names_p18[value])
}
p = p %>% gather(key = "variable", value = "value", -t)

p18 <- ggplot(p, aes(t, value)) +
  geom_hline(yintercept=0, color="#244747" ,size=1) +
  geom_line(size = 1.5,color="#e3120b") +
  scale_x_discrete(expand = c(0, -1),limits=c(0:17)) +
  scale_y_continuous(expand = c(0.02,0.02)) +
  facet_wrap(~ variable, ncol = 1,labeller = variable_labeller_p18,scale="free") +
  labs(title = "18. ábra. Bűnelkövetők számában bekövetkező sokk lecsapódása",subtitle ="Impulzus válasz-függvény",caption="A VAR-modellben szereplő késleltetések száma: 12") +
  xlab("Sokkot követően eltelt évek száma") + ylab("") +
  theme_economist(dkpanel = T) + theme(
    legend.title = element_blank(),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
    axis.ticks.length = unit(5, "points"),
    panel.grid.major = element_line(colour = "grey", 
                                    size = rel(1.75)),
    panel.spacing = unit(3, "lines")
  )
metadata_final_pooled <- tibble::tribble(
  ~Változó.neve,    ~Hivatkozott.név,                             ~Mértékegység, ~Megfigyelt.évek,
  "Termékenységi ráta",               "TTA",                              "Gyermek/Nő",      "1960-2018",
  "Évenkénti összes születésszám",    "LiveBirthTotal",                                      "Fő",      "1960-2018",
  "Ezer főre eső évenkénti születésszám",   "LiveBirthTo1000",                        "1000 főre eső db",      "1960-2018",
  "Alkalmazásban állók havi bruttó átlagkeresete a nemzetgazdaságban",       "NominalWage",                                      "Ft",      "1960-2017",
  "Gazdaságilag aktív népesség száma, január 1-én",            "Activs",                                 "Ezer fő",      "1960-2017",
  "Egy keresőre jutó reálkereset indexe, 1960 = 100%",        "RealIncome",                                "Százalék",      "1960-2017",
  "Házasságkötés (ezer lakosra)",          "Marriage",                        "1000 főre eső db",      "1960-2017",
  "Egy főre eső fogyasztás indexe",       "Consumption",                                "Százalék",      "1960-2017",
  "Bűncselekmények száma",             "Crime",                                   "Darab",      "1965-2017",
  "Bűnelkövetők száma (fiatalkorú)",     "CriminalYoung",                                      "Fő",      "1965-2017",
  "Bűnelkövetők száma (felnőtt)",     "CriminalAdult",                                      "Fő",      "1965-2017",
  "Bűnelkövetők száma (összesen)",     "CriminalTotal",                                      "Fő",      "1965-2017",
  "Bruttó nemzeti kibocsátás bázisindexe, 1960 = 100%",           "GDP1960",                                "Százalék",      "1960-2017",
  "Egy főre jutó bruttó nemzeti kibocsátás bázisindexe, 1960 = 100%",        "GDPCAP1960",                                "Százalék",      "1960-2017",
  "Anyagi deprivációban élők aránya lakhatás tekintetében",     "MDeprivationT",                                "Százalék",      "2005-2017",
  "18 év alatti anyagi deprivációban élők aránya lakhatás tekintetében",   "MDeprivation018",                                "Százalék",      "2005-2017",
  "18 és 65 éves kor közötti anyagi deprivációban élők aránya lakhatás tekintetében",  "MDeprivation1865",                                "Százalék",      "2005-2017",
  "Teljes termékenységi ráta országonként",    "FertilityRates",                              "Gyermek/Nő",      "1960-2017",
  "Megtakarítási ráta",            "Saving",                           "GDP százaléka",      "1995-2017",
  "Háztartások adóság állománya",     "HouseholdDebt", "Rendelkezésre álló  jövedelem százaléka",      "1995-2017",
  "Gini mutató",              "Gini",                   "0 és 1 közötti mutató",      "2006-2016",
  "90/10 mutató",            "P90P10",                                   "Arány",      "2006-2016",
  "Relatív szegénységi ráta a 0-17 éves lakosság körében",    "PovertyRate017",                                   "Arány",      "2006-2016",
  "Relatív szegénységi ráta a 18-65 éves lakosság körében",   "PovertyRate1865",                                   "Arány",      "2006-2016",
  "Relatív szegénységi ráta a 66 évesnél idősebb lakosság körében",     "PovertyRate66",                                   "Arány",      "2006-2016",
  "Relatív szegénységi ráta a teljes lakosság körében",    "PovertyRateTot",                                   "Arány",      "2006-2016",
  "Munkanélküliségi ráta a férfi népesség körében",     "UnemploymentM",                                "Százalék",      "1999-2017",
  "Munkanélküliségi ráta a női népesség körében",     "UnemploymentW",                                "Százalék",      "1999-2017",
  "Munkanélküliségi ráta a teljes népesség körében",     "UnemploymentT",                                "Százalék",      "1999-2017",
  "1000 főre eső császármetszések száma", "CaesareanSections",                        "1000 főre eső db",      "2004-2017",
  "Állam által fizetett nyugdíjak mértéke a GDP százalékában",     "PensionPublic",                                "Százalék",      "1999-2015",
  "Állami családtámogatás mértéke a GDP százalékában",    "FamilyBenefits",                                "Százalék",      "1999-2015"
) %>% data.frame()

F1 <- metadata_final_pooled %>% head(14)
names(F1) <- c("Változó neve",	"Hivatkozott név","	Mértékegység",	"Megfigyelt évek")
F1 <- F1 %>% kable(caption = "F1. Központi Statisztika Hivatal oldaláról származó adatok (letöltés dátuma: 2020. 02. 01)", align = c("l",rep("c",3))) %>%
               kable_styling(bootstrap_options = "striped",full_width = T, fixed_thead = T)

F2 <- metadata_final_pooled %>% head(17) %>% tail(3)
names(F2) <- c("Változó neve",	"Hivatkozott név","	Mértékegység",	"Megfigyelt évek")
F2 <- F2 %>% kable(caption = "F2. Eurostat oldaláról származó adatok (letöltés dátuma: 2020. 03. 01)", align = c("l",rep("c",3))) %>%
  kable_styling(bootstrap_options = "striped",full_width = T, fixed_thead = T)

F3 <- metadata_final_pooled %>%  tail(-17)
names(F3) <- c("Változó neve",	"Hivatkozott név","	Mértékegység",	"Megfigyelt évek")
F3 <- F3 %>% kable(caption = "F3. OECD oldaláról származó adatok (letöltés dátuma: 2020. 01. 04)", align = c("l",rep("c",3))) %>%
  kable_styling(bootstrap_options = "striped",full_width = T, fixed_thead = T)


