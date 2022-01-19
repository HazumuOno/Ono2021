##### ライブラリー読み込み #####
library(vars)
library(rugarch)
library(urca)
library(forecast)
library(tidyverse)
library(lubridate)
library(gtools)
library(texreg)
# select <- dplyr::select ##selectがコンフリクトしたときに
# "Extract.r"は再帰的二項アルゴリズムのための関数
# https://stimson.web.unc.edu/software/ よりダウンロードしてください
source("Extract.r")


############### 経済変数の準備 ############################## 
### 日経平均以外はデータに修正が入るためか、最新のデータを用いるとやや結果の数値がズレます(大きな影響はないと思われます)
## 日経平均株価 ##
# https://fred.stlouisfed.org/series/NIKKEI225
N225 <- read_csv("economic_data/NIKKEI225.csv",na = ".") %>% rename_at(2,function(x) "N225")
# 月次終値に
N225_M <- N225 %>% na.omit() %>% mutate(DATE = DATE %>% `day<-`(1)) %>% group_by(DATE) %>% slice(n()) %>% ungroup()

## 失業率 ##
# https://fred.stlouisfed.org/series/LRUNTTTTJPM156S
#UNEMP <- read_csv("economic_data/LRUNTTTTJPM156S.csv",na = ".") %>% rename_at(2,function(x) "UNEMP") #最新のデータ
UNEMP <- read_csv("economic_data/unemp_ts.csv" #locale = locale(encoding = "cp932")
) %>% 
  mutate(DATE=str_c(year,"-",month,"-01") %>% ymd()) %>% 
  select(-year,-month,-unemp_raw) %>% rename(UNEMP=unemp)

## 消費者物価指数 ##
# https://fred.stlouisfed.org/series/JPNCPIALLMINMEI

#CPI <- read_csv("economic_data/JPNCPIALLMINMEI.csv",na = ".") %>% rename_at(2,function(x) "CPI") #最新のデータ
CPI <- read_csv("economic_data/cpi.csv",na = ".") %>% rename_at(2,function(x) "CPI")
## GDP成長率(4半期) ##
# https://fred.stlouisfed.org/series/LORSGPORJPQ659S
GDP_Q <- read_csv("economic_data/gdp_q.csv",na = ".") %>% rename_at(2,function(x) "GDP_Q") %>%
  mutate(date = ymd(date)) %>% 
  rename(DATE=date)
#GDP_Q <- read_csv("economic_data/LORSGPORJPQ659S.csv",na = ".") %>% rename_at(2,function(x) "GDP_Q") #最新のデータ

## GDP成長率(月次標準化) ##
# https://fred.stlouisfed.org/series/JPNLORSGPNOSTSAM
GDP_M <- read_csv("economic_data/JPNLORSGPNOSTSAM.csv",na = ".") %>% rename_at(2,function(x) "GDP_M")

#月次データ化
ECON_VALUES_M <- N225_M %>% left_join(UNEMP) %>% left_join(CPI) %>% left_join(GDP_M) %>% left_join(GDP_Q) %>% fill(GDP_Q)



############### 支持理由データの確認 ############################## 


# データの読み込み
data_support <- read_csv("data_support.csv")

#支持理由の日本語準備
reason_ja <- c(他に適当な人がいないから="yes_noother",首相を信頼するから="yes_shinrai",誰がなっても同じだから="yes_allsame",
                           政策が良いから="yes_policy",なんとなく="yes_nantonaku",社会党内閣ができては困るから="yes_nojsp",
                           自由陣営にいいから = "yes_liberal",首相の属する党を支持しているから = "yes_party",
                           "連立内閣/自民党単独内閣/非自民党内閣だから" = "yes_regime",
                           印象が良いから = "yes_insho", リーダーシップがあるから = "yes_leadership",内閣支持率="CABINETapp"
)
reason_ja_short <- c(他に人がいない="yes_noother",首相を信頼="yes_shinrai",誰でも同じ="yes_allsame",
                        政策が良い="yes_policy",なんとなく="yes_nantonaku",反社会党内閣="yes_nojsp",
                        自由陣営にいい = "yes_liberal",首相の政党を支持 = "yes_party",
                        "連立/単独内閣" = "yes_regime",
                        印象が良い = "yes_insho", リーダーシップ = "yes_leadership",内閣支持率="CABINETapp"
)

reason_df <- tibble(reason = reason_ja, 支持理由 = names(reason_ja),理由=names(reason_ja_short)) %>%
  mutate(order=row_number())%>% mutate(支持理由=reorder(支持理由,order),
                                           理由=reorder(理由,order))

##### 表1 #####
data_support %>% select(starts_with("yes_"),date,CABINETapp) %>% pivot_longer(-date) %>% na.omit() %>% 
  left_join(reason_df,c("name" = "reason")) %>% 
  group_by(支持理由) %>% summarise(N = n(),
                               平均値 = mean(value),
                               中央値 = median(value),
                               最大値 = max(value),
                               最小値= max(value),
                               標準偏差 = sd(value),
                               期間 = str_c(min(date)," - ", max(date)))



##### 図1 #####

#ggplot・extract用にlong変形
reasons_long <- data_support %>% select(date,starts_with("yes_") ) %>% 
  pivot_longer(cols = -date,names_to = "reason",values_to = "rate") %>% na.omit()  %>% 
  left_join(reason_df,c("reason")) 

reasons_long %>% 
  ggplot(aes(x=date,y=rate#, colour=reorder(支持理由,order))
  )) +
  geom_line(alpha=0.4,size=0.3) + facet_wrap(~支持理由,ncol=3)+ # geom_smooth(span=0.1,se=F,alpha=0.4)+
  theme_bw(base_family = "HiraginoSans-W3") + theme(legend.title = element_blank(),
                                                    axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_discrete(h = c(0, 280)) + xlab("") + ylab("") +
  #scale_colour_brewer( palette="Set3") + 
  scale_x_date(limits = as.Date(c("1960-01-01","2020-01-01")),
               breaks = seq(as.Date("1960-01-01"),as.Date("2020-01-01"), by = "10 years"),
               minor_breaks = seq(as.Date("1960-01-01"),as.Date("2020-01-01"), by = "1 years"),
               date_labels = "%Y") + geom_vline(xintercept = as.Date("1993-08-01"),size=0.2 ,lty=5) 


############### 再帰的二項モデルの適用 ############################## 

#extractによる抽出
result_mood <- extract(varname = reasons_long$reason,
                      date = reasons_long$date,
                      index=reasons_long$rate,
                      unit = "M",npass = 2,smoothing = T)

#データのまとめ
df_moods <- tibble(date = data_support$date,
          mood1_valence =result_mood$latent1,
          mood2_vague = result_mood$latent2)

##### 表2 #####
tibble(reason = result_mood$varname,
          N = result_mood$N,
          第一軸=result_mood$loadings1,
          第二軸=result_mood$loadings2) %>% left_join(reason_df %>% select(reason,理由)) %>% 
  select(理由,N,第一軸,第二軸) %>% arrange(理由)
  

##### 図2 #####

df_moods_longer <- df_moods %>% rename(`支持ムード1:政権能力支持`=mood1_valence,
                    `支持ムード2:漠然支持`=mood2_vague) %>% 
  pivot_longer(cols = -date,names_to = "mood")


ggplot(df_moods_longer,
       aes(x = date,y=value,lty=mood)
       ) + geom_line()+
  theme_bw(base_family = "HiraginoSans-W3") + 
  theme(legend.title=element_blank(),legend.position = c(0.85,0.9)
  ) +
  scale_x_date(limits = as.Date(c("1960-01-01","2020-01-01")),
               breaks = seq(as.Date("1960-01-01"), 
                            as.Date("2020-01-01"), 
                            by = "5 years"),
               minor_breaks = seq(as.Date("1960-01-01"), 
                                  as.Date("2020-01-01"), 
                                  by = "1 years"),
               date_labels = "%Y"
  ) + ylab("") + xlab("")# + ggtitle("内閣支持ムードの変化")


##### 図3 #####

df_mood2_GDP_longer <- df_moods %>% left_join(ECON_VALUES_M,by = c("date" = "DATE")) %>% select(date,mood2_vague,GDP_Q) %>% 
  rename(四半期GDP成長率=GDP_Q,`支持ムード2:漠然支持`=mood2_vague) %>% pivot_longer(-date)

ggplot(df_mood2_GDP_longer,
       aes(x=date,y=value,lty=name)
) + geom_line() +
  theme_bw(base_family = "HiraginoSans-W3") + 
  theme(legend.title=element_blank(),legend.position = c(0.8,0.85)
  ) + 
  scale_x_date(limits = as.Date(c("1960-01-01","2020-01-01")),
               breaks = seq(as.Date("1960-01-01"), 
                            as.Date("2020-01-01"), 
                            by = "5 years"),
               minor_breaks = seq(as.Date("1960-01-01"), 
                                  as.Date("2020-01-01"), 
                                  by = "1 years"),
               date_labels = "%Y"
  ) + xlab("") + ylab("")


############### 支持ムードによる内閣支持率の回帰 ############################## 

df_moods <- df_moods %>%
  mutate(mood1_valence = mood1_valence - (min(mood1_valence)),
         mood2_vague = mood2_vague - (min(mood2_vague)),
         CABINETapp = data_support$CABINETapp)

lm_CABapp_mood <- lm(data=df_moods,CABINETapp ~ mood1_valence + mood2_vague)

summary(lm_CABapp_mood)

df_moods <- df_moods %>%
  mutate(mood1_valence = mood1_valence * coef(lm_CABapp_mood)[2],
         mood2_vague = mood2_vague * coef(lm_CABapp_mood)[3],
         intercept= coef(lm_CABapp_mood)[1])


##### 図4 #####
df_moods_longer <- df_moods %>% transmute(date=date,
                                          `漠然支持*1.12`=mood2_vague ,
                                          `政権能力支持*2.40` = mood1_valence ,
                                          `定数(9.68)`=intercept
                                          ) %>%
  pivot_longer(-date)  %>% mutate(name = name %>% fct_relevel("政権能力支持*2.40","漠然支持*1.12"))

ggplot(df_moods_longer,
       aes(x = date,y=value)
       ) + geom_vline(xintercept = as.Date("1993-08-01"), lty=5) + 
  geom_area(aes(fill=name),alpha=0.5) +
  geom_line(data = df_moods %>% select(date,CABINETapp) %>% mutate(lty="内閣支持率"),
            mapping = aes(x=date,y=CABINETapp,lty=lty),alpha=0.8) +
  theme_bw(base_family = "HiraginoSans-W3") + scale_fill_grey(start = 0.1, end = 0.7) +
  theme(legend.title=element_blank(),legend.position = c(0.15,0.82)
  ) +
  scale_x_date(limits = as.Date(c("1960-01-01","2020-01-01")),
               breaks = seq(as.Date("1960-01-01"), 
                            as.Date("2020-01-01"), 
                            by = "5 years"),
               minor_breaks = seq(as.Date("1960-01-01"), 
                                  as.Date("2020-01-01"), 
                                  by = "1 years"),
               date_labels = "%Y"
  ) + ylab("") + xlab("")

############### 時系列分析の準備 ############################## 

##### 4半期データ化 #####

# 支持率・支持ムード
df_moods_Q <- df_moods %>% select(-intercept) %>% 
  group_by(year = date %>% year,
           Q = date %>% quarter()
  )  %>% filter(n()==3) %>% #内閣支持率データが2017年11月で終わるため、3時点あるものに限定したが、揃える必要はなかったかも
  slice(2) %>% ungroup()

#経済変数
ECON_VALUES_Q <- ECON_VALUES_M %>% group_by(year = DATE %>% year(),Q = DATE %>% quarter()) %>% 
  slice(3) %>% select(-GDP_M) %>% ungroup() 

#外生変数
Exogen_Q <- data_support %>% 
  select(date,PM_duration:after55) %>%
  group_by(year = date %>% year,
           Q = date %>% quarter()
  ) %>% 
  slice(2) %>% 
  ungroup() %>% 
  mutate(PM_duration = log(PM_duration))


df_all_Q <- df_moods_Q %>% 
  left_join(ECON_VALUES_Q,by=c("year","Q")) %>% 
  left_join(Exogen_Q,by=c("year","Q","date")) %>% na.omit()


##### 支持変数・経済変数の単位根検定 #####
df_all_Q %>% select(mood1_valence:CABINETapp,
                    N225:GDP_Q) %>% map(. %>% ur.df(type = "trend",selectlags = "AIC") %>% summary())


# 単位根のあったものを取り出し、(対数)差分をとるなどの処理
# 差分をとる関数
get_delta <- function(x,n=1,log=F){
  if(log==T){x <- log(x)}
  v <- x
  for(i in 1:n){
    v1 <- lag(v)
    v <- v - v1
  }
  v
}

#差分系列の作成
df_diffs_Q <- ECON_VALUES_Q %>% 
transmute(year,Q,UNEMP_D = UNEMP %>% get_delta(),
            N225_LD = N225 %>% get_delta(log=T),
            CPI_LD = CPI %>% get_delta(log=T))

#差分系列の単位根検定
df_Q_VAR <- df_all_Q %>% left_join(df_diffs_Q,by=c("year","Q")) 

df_Q_VAR %>% select(UNEMP_D,N225_LD,CPI_LD) %>% map(. %>% ur.df(type = "trend",selectlags = "AIC") %>% summary())

#差分系列

# 

##### グレンジャー因果性検定 #####
#結果を格納するデータフレーム
names_ts <- df_Q_VAR %>% select(N225_LD,GDP_Q,UNEMP_D,CPI_LD,mood1_valence,mood2_vague,CABINETapp) %>% colnames()

#外生変数
Exogen_Q_VAR <- df_Q_VAR %>% select(PM_duration:after55)

gtest_table <- permutations(7,2,v = names_ts) %>% as_tibble() %>% 
  rename(X = V1, Y = V2) %>% 
  mutate(formula = str_c(Y," ~ ",X),
         F_test = NA_real_,
         p =  NA_real_)

for(i in 1:42){
  x_temp <- gtest_table[i,"X"] %>% pull(X)
  y_temp <- gtest_table[i,"Y"] %>% pull(Y)
  tbl_var_temp <- df_Q_VAR %>% select(x_temp,y_temp)
  var_result_temp <- VAR(tbl_var_temp
                         ,
                         exogen = Exogen_Q_VAR
                         ,
                         type = "const",p = 1)
  gtest_temp <- causality(var_result_temp,cause=x_temp)
  gtest_table$F_test[i] <- gtest_temp$Granger$statistic
  gtest_table$p[i] <- gtest_temp$Granger$p.value
  }

##### 表3 #####
gtest_table %>% mutate(coef = case_when( p<0.01 ~ "***",
                                          p<0.05 ~ "**",
                                          p<0.1 ~ "*",
                                          TRUE ~ ""
),
coef = str_c(F_test %>%round(2) %>% format(digits=2,nsmall = 2)
             ,coef)
) %>% 
  select(X,Y,coef) %>% 
  pivot_wider(names_from = Y,values_from=coef) %>% 
  slice(6,3,7,2,4,5,1) %>% select(1,6,3,7,2,4,5,8)%>%
  column_to_rownames("X")

############### 時系列分析 ##############################
##### VAR用のデータ #####
VARdata_mood1_Q <- df_Q_VAR %>% select(CPI_LD,N225_LD,GDP_Q,mood1_valence)
VARdata_mood2_Q <- df_Q_VAR %>% select(CPI_LD,N225_LD,GDP_Q,mood2_vague)
VARdata_CABINETapp_Q <- df_Q_VAR %>% select(CPI_LD,N225_LD,GDP_Q,CABINETapp)

##### 政権能力支持ムード #####
VARselect(VARdata_mood1_Q,
          exogen = Exogen_Q_VAR
          ,lag.max = 8) 

VAR_result_mood1 <- VAR(VARdata_mood1_Q,
                        exogen = Exogen_Q_VAR,
                        type = "const",p  = 5)

set.seed(2021)
IRF_mood1 <- vars::irf(VAR_result_mood1,
                       response="mood1_valence",
                       cumulative=F,
                       n.ahead=8,boot=T,runs=500)

##### 漠然支持ムード #####
VARselect(VARdata_mood2_Q,
          exogen = Exogen_Q_VAR
          ,lag.max = 8) 

VAR_result_mood2 <- VAR(VARdata_mood2_Q,
                        exogen = Exogen_Q_VAR,
                        type = "const",p  = 5)

set.seed(2021)
IRF_mood2 <- vars::irf(VAR_result_mood2,
                       response="mood2_vague",
                       cumulative=F,
                       n.ahead=8,boot=T,runs=500)

##### 内閣支持率 #####
VARselect(VARdata_CABINETapp_Q,
          exogen = Exogen_Q_VAR
          ,lag.max = 8) 

VAR_result_CABINETapp <- VAR(VARdata_CABINETapp_Q,
                             exogen = Exogen_Q_VAR,
                             type = "const",p  = 5)

set.seed(2021)
IRF_CABINETapp <- vars::irf(VAR_result_CABINETapp,
                       response="CABINETapp",
                       cumulative=F,
                       n.ahead=8,boot=T,runs=500)

##### IRFグラフのプロット #####
## ブートストラップの結果を取得する関数 ##
get_irf_result <- function(x){
  names_temp <- x$irf %>% names()
  irf_temp <- x$irf %>% as.data.frame()
  names(irf_temp) <- names_temp
  irf_temp %>% as_tibble() %>% mutate(time=row_number()) %>% 
    gather(key=impulse,value = response,-time) %>% 
    mutate(lower=x$Lower %>% unlist,
           upper=x$Upper %>% unlist)
}


irf_results_q <- IRF_mood1 %>% get_irf_result() %>% mutate(value="支持ムード1:政権能力支持") %>% 
  bind_rows(IRF_mood2 %>% get_irf_result() %>% mutate(value="支持ムード2:漠然支持"),
            IRF_CABINETapp %>% get_irf_result() %>% mutate(value="内閣支持率"))

##### 日経平均株価のIRFグラフ #####
irf_N225 <- irf_results_q %>% filter(impulse=="N225_LD") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2) + xlab("経過時間(四半期)") + ylab("ショックへの反応")+
  theme_bw(base_family ="HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = c(0:8),minor_breaks = NULL)
irf_N225

##### GDPのIRFグラフ #####
irf_GDP <- irf_results_q %>% filter(impulse=="GDP_Q") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2) + xlab("経過時間(四半期)") + ylab("ショックへの反応")+
  theme_bw(base_family ="HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = c(0:8),minor_breaks = NULL)
irf_GDP

##### CPIのIRFグラフ #####
irf_CPI <- irf_results_q %>% filter(impulse=="CPI_LD") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2) + xlab("経過時間(四半期)") + ylab("ショックへの反応")+
  theme_bw(base_family ="HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = c(0:8),minor_breaks = NULL)
irf_CPI


############### Appendix ##############################

##### 内閣支持率の予測値に対する政権能力支持の比率 #####
df_moods %>%
  transmute(date, `支持ムード1*2.40` = mood1_valence*100/(mood1_valence + mood2_vague + intercept)) %>% 
  ggplot(aes(x = date,y=`支持ムード1*2.40`)
) + geom_vline(xintercept = as.Date("1993-08-01"), lty=5) + 
  geom_area(colour="black",alpha=0.5) +
  theme_bw(base_family = "HiraginoSans-W3") + scale_fill_grey(start = 0.1, end = 0.7) +
  theme(legend.title=element_blank(),legend.position ="none" #c(0.8,0.8)
  ) +
  scale_x_date(limits = as.Date(c("1960-01-01","2020-01-01")),
               breaks = seq(as.Date("1960-01-01"), 
                            as.Date("2020-01-01"), 
                            by = "5 years"),
               minor_breaks = seq(as.Date("1960-01-01"), 
                                  as.Date("2020-01-01"), 
                                  by = "1 years"),
               date_labels = "%Y"
  ) + ylim(0,101) + ylab("") + xlab("")

##### 内閣支持率の予測値に対する漠然支持の比率 #####
df_moods %>%
  transmute(date, `支持ムード2*1.12` = mood2_vague*100/(mood1_valence + mood2_vague + intercept)) %>% 
  ggplot(aes(x = date,y=`支持ムード2*1.12`)
  ) + geom_vline(xintercept = as.Date("1993-08-01"), lty=5) + 
  geom_area(colour="black",alpha=0.5) +
  theme_bw(base_family = "HiraginoSans-W3") + scale_fill_grey(start = 0.1, end = 0.7) +
  theme(legend.title=element_blank(),legend.position ="none" #c(0.8,0.8)
  ) +
  scale_x_date(limits = as.Date(c("1960-01-01","2020-01-01")),
               breaks = seq(as.Date("1960-01-01"), 
                            as.Date("2020-01-01"), 
                            by = "5 years"),
               minor_breaks = seq(as.Date("1960-01-01"), 
                                  as.Date("2020-01-01"), 
                                  by = "1 years"),
               date_labels = "%Y"
  ) + ylim(0,101) + ylab("") + xlab("")



##### コントロール変数の効果 #####
list(VAR_result_mood1$varresult$mood1_valence, 
     VAR_result_mood2$varresult$mood2_vague, 
     VAR_result_CABINETapp$varresult$CABINETapp) %>% 
  screenreg(custom.model.names = c("政権能力支持","漠然支持","内閣支持率"),
            omit.coef = "^N225_.*|GDP_.*|^CPI.*|^mood.*|^CABINETapp.*",
            custom.coef.names = c("定数項","首相在任期間(対数)","非自民党首班","衆院選までの残り月数","1993年8月以降"))

##### 月次データの分析 #####
#経済変数・支持変数
VARdata_M <- ECON_VALUES_M %>% transmute(DATE,
                            N225_LD = N225 %>% log() %>% get_delta(),
                            GDP_M,
                            CPI_LD = CPI%>% log() %>% get_delta()) %>%
  right_join(df_moods,by=c("DATE"="date")) %>% select(-intercept)

#外生変数
Exogen_M <-  data_support %>% 
  select(PM_duration:after55) %>% 
  mutate(PM_duration = log(PM_duration))

#ADF検定
VARdata_M %>% select(-DATE) %>% map(. %>% ur.df(type="trend",selectlags = "AIC") %>% summary)

#VAR用データ
VARdata_M_mood1 <- VARdata_M %>% select(CPI_LD,N225_LD,GDP_M,mood1_valence)
VARdata_M_mood2 <- VARdata_M %>% select(CPI_LD,N225_LD,GDP_M,mood2_vague)
VARdata_M_CABINETapp <- VARdata_M %>% select(CPI_LD,N225_LD,GDP_M,CABINETapp)

#ムード1
VARselect(VARdata_M_mood1,
          exogen = Exogen_M
          ,lag.max = 24) 

VARresult_M_mood1 <- VAR(VARdata_M_mood1,
                        exogen = Exogen_M ,
                        type = "const",p  = 12)

set.seed(2021)
IRF_mood1_M <- vars::irf(VARresult_M_mood1,
                       response="mood1_valence",
                       cumulative=F,
                       n.ahead=24,boot=T,runs=500)

#ムード2
VARselect(VARdata_M_mood2,
          exogen = Exogen_M
          ,lag.max = 24) 

VARresult_M_mood2 <- VAR(VARdata_M_mood2,
                         exogen = Exogen_M ,
                         type = "const",p  = 12)

set.seed(2021)
IRF_mood2_M <- vars::irf(VARresult_M_mood2,
                         response="mood2_vague",
                         cumulative=F,
                         n.ahead=24,boot=T,runs=500)

#内閣支持率
VARselect(VARdata_M_CABINETapp,
          exogen = Exogen_M
          ,lag.max = 24) 

VARresult_M_CABINETapp <- VAR(VARdata_M_CABINETapp,
                         exogen = Exogen_M ,
                         type = "const",p  = 12)

set.seed(2021)
IRF_CABINETapp_M <- vars::irf(VARresult_M_CABINETapp,
                         response="CABINETapp",
                         cumulative=F,
                         n.ahead=24,boot=T,runs=500)


##### IRFグラフ #####
irf_results_m <- IRF_mood1_M %>% get_irf_result() %>% mutate(value="支持ムード1:政権能力支持") %>% 
  bind_rows(IRF_mood2_M %>% get_irf_result() %>% mutate(value="支持ムード2:漠然支持"),
            IRF_CABINETapp_M %>% get_irf_result() %>% mutate(value="内閣支持率"))

#GDP
irf_GDP_m <- irf_results_m %>% filter(impulse=="GDP_M") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2) + xlab("経過時間(月)") + ylab("ショックへの反応")+
  theme_bw(base_family ="HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = seq(0,24,3),minor_breaks = NULL)
irf_GDP_m

#日経平均
irf_N225_m <- irf_results_m %>% filter(impulse=="N225_LD") %>% 
  ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2)+  xlab("経過時間(月)") +
  ylab("ショックへの反応")+
  theme_bw(base_family = "HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = seq(0,24,3),minor_breaks = NULL)
irf_N225_m

irf_cpi_m <- irf_results_m %>% filter(impulse=="CPI_LD") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),alpha=0.3,
                            colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2)+  xlab("経過時間(月)") + ylab("ショックへの反応")+
  theme_bw(base_family = "HiraginoSans-W3") + facet_grid(. ~ value)  +
  scale_x_continuous(breaks = seq(0,24,3),minor_breaks = NULL)
irf_cpi_m

##### ARFIMA変換を施した分析 #####

#長期記憶性の確認
df_Q_VAR %>% 
  select(GDP_Q,UNEMP,CPI,CABINETapp,mood1_valence,mood2_vague,N225) %>%
  mutate(N225 = log(N225)) %>% 
  map(. %>% fracdiff::fdGPH()) 

#ARFIMA変換
df_Q_VAR_ARF <- df_Q_VAR %>% mutate(GDP_Q_ARF = GDP_Q %>% 
                                      autoarfima(method = c("partial")) %>% magrittr::extract2(1) %>% residuals() %>% as.numeric(), mood1_ARF = mood1_valence %>% 
                                      autoarfima(method = c("partial")) %>% magrittr::extract2(1) %>% residuals() %>% as.numeric(),
                                    mood2_ARF = mood2_vague %>% 
                                      autoarfima(method = c("partial")) %>% magrittr::extract2(1) %>% residuals() %>% as.numeric())

#VAR用データ
VARdata_mood1_Q_ARF <- df_Q_VAR_ARF %>% 
  select(CPI_LD,N225_LD,GDP_Q_ARF,mood1_ARF)

VARdata_mood2_Q_ARF <- df_Q_VAR_ARF %>% 
  select(CPI_LD,N225_LD,GDP_Q_ARF,mood2_ARF)

##ムード1
VARselect(VARdata_mood1_Q_ARF,
          exogen = Exogen_Q_VAR
          ,lag.max = 8) 

VAR_result_mood1_ARF <- VAR(VARdata_mood1_Q_ARF,
                        exogen = Exogen_Q_VAR,
                        type = "const",p  = 4)

set.seed(2021)
IRF_mood1_ARF <- vars::irf(VAR_result_mood1_ARF,
                       response="mood1_ARF",
                       cumulative=F,
                       n.ahead=8,boot=T,runs=500)

##ムード2
VARselect(VARdata_mood2_Q_ARF,
          exogen = Exogen_Q_VAR
          ,lag.max = 8) 

VAR_result_mood2_ARF <- VAR(VARdata_mood2_Q_ARF,
                            exogen = Exogen_Q_VAR,
                            type = "const",p  = 5)

set.seed(2021)
IRF_mood2_ARF <- vars::irf(VAR_result_mood2_ARF,
                           response="mood2_ARF",
                           cumulative=F,
                           n.ahead=8,boot=T,runs=500)


irf_results_q_ARF <- IRF_mood1_ARF %>% get_irf_result() %>% mutate(value="支持ムード1:政権能力支持") %>% 
  bind_rows(IRF_mood2_ARF %>% get_irf_result() %>% mutate(value="支持ムード2:漠然支持"))

##### 日経平均株価のIRFグラフ #####
irf_N225_ARF <- irf_results_q_ARF %>% filter(impulse=="N225_LD") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2) + xlab("経過時間(四半期)") + ylab("ショックへの反応")+
  theme_bw(base_family ="HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = c(0:8),minor_breaks = NULL)
irf_N225_ARF

##### GDPのIRFグラフ #####
irf_GDP_ARF <- irf_results_q_ARF %>% filter(impulse=="GDP_Q_ARF") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2) + xlab("経過時間(四半期)") + ylab("ショックへの反応")+
  theme_bw(base_family ="HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = c(0:8),minor_breaks = NULL)
irf_GDP_ARF

##### CPIのIRFグラフ #####
irf_CPI_ARF <- irf_results_q_ARF %>% filter(impulse=="CPI_LD") %>% ggplot(aes(x=time-1,y=response)) + 
  geom_line() + geom_ribbon(mapping = aes(ymax=upper,ymin=lower),
                            alpha=0.3,colour="black", lty=3) + 
  geom_hline(yintercept = 0, lty=2) + xlab("経過時間(四半期)") + ylab("ショックへの反応")+
  theme_bw(base_family ="HiraginoSans-W3") + facet_grid(. ~ value) +
  scale_x_continuous(breaks = c(0:8),minor_breaks = NULL)
irf_CPI_ARF

##### 記述統計 #####
#4半期
df_Q_VAR %>% 
  select(N225,GDP_Q,CPI,UNEMP,PM_duration,time_dissolution,after55,PM_NOTLDP) %>% 
  mutate(PM_duration = exp(PM_duration)) %>% 
  gather() %>% group_by(key) %>% 
  summarise(N=n(),平均値=mean(value),中央値=median(value),標準偏差=sd(value),最大値=max(value),最小値=min(value))

#月次
data_support %>% left_join(ECON_VALUES_M,by=c("date" = "DATE")) %>% 
  select(N225,GDP_M,CPI,PM_duration,time_dissolution,after55,PM_NOTLDP) %>% 
  gather() %>% group_by(key) %>% 
  summarise(N=n(),平均値=mean(value),中央値=median(value),標準偏差=sd(value),最大値=max(value),最小値=min(value))

