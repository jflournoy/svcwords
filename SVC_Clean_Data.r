library(knitr)
opts_chunk$set(
	echo=F,warning=F,message=F,error=F)

library(MASS)
library(boot)
library(knitr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(elasticnet)
library(glmnet)
library(doMC)

mturk_data_dir <- 'mturk_data'

svc.imp<-read.csv(file.path(mturk_data_dir, 'SVC_Norming.csv'),stringsAsFactors=F)
mturk.imp<-rbind(
	read.csv(file.path(mturk_data_dir, 'Batch_2047596_batch_results.csv'),stringsAsFactors=F),
	read.csv(file.path(mturk_data_dir, 'Batch_2044018_batch_results.csv'),stringsAsFactors=F),
	read.csv(file.path(mturk_data_dir, 'Batch_2041668_batch_results.csv'),stringsAsFactors=F))

# head(svc.imp)
dim(svc.imp)

# head(mturk.imp)
dim(mturk.imp)
# View(mturk.imp)

kable(mturk.imp %>% filter(WorkerId %in% WorkerId[duplicated(WorkerId)]))

qtext<-svc.imp[1,]
svc<-svc.imp[-1,]

# head(svc)
names(svc)
names(mturk.imp)

names(qtext)[grepl('popular',qtext[1,])]
names(qtext)[grepl('popular',qtext[1,])]
names(qtext)[grepl('blank',qtext[1,])]

svc$Q20<-str_trim(svc$Q20)
mturk.imp$WorkerId<-str_trim(mturk.imp$WorkerId)
svc$Q20[svc$Q20=='A2H4HIDU6FFAMQ']<-'A2H4HYDU6FFAMQ'
svc$numNA <- apply(svc,1,function(x){sum(is.na(x))})

table(svc$numNA)
checkthese<-c(
	'V1',
	'Q20',
	'mTurkCode',
	'consent_1',
	'consent_2',
	'consent_3',
	'consent_4',
	'AccPop_16',
	'ContStat_16',
	'AccPop_63',
	'ContStat_63',
	'numNA')
mturk_cols<-c(
	'WorkerId',
	'WorkTimeInSeconds',
	'Answer.surveycode')


check<-left_join(
	mturk.imp[,mturk_cols],
	svc[,checkthese],
	by=c('WorkerId'='Q20')) %>%
	mutate(SameCode=str_trim(mTurkCode) == str_trim(Answer.surveycode))



dim(check)
names(check)

table(check[,c('AccPop_63')],useNA='ifany')
table(check[,c('ContStat_63')],useNA='ifany')
table(check[,c('AccPop_16')],useNA='ifany')
table(check[,c('ContStat_16')],useNA='ifany')
table(check[,c('SameCode')],useNA='ifany')

check %>% filter(
	!AccPop_16 %in% 1:5 | 
	!ContStat_16 %in% 1:5 | 
	!SameCode | 
	numNA > 6) %>% kable

kable(check)


# Very poor quality, with clear indications that HIT was not attended to, and did not meet criteria or follow instructions
rejected<-c(
	'A38RG04G2OC3UQ',
	'A3699R3L9NVDWY',
	'A30N5H4N4C22N8',
	'A5BE7AD48CGRI',
	'A3G8YLXW8BU1GP',
	'AQB67HZSYQBF2',
	'A35T6IAX6MHNHC',
	'A2DIINP8HJJ2BW',
	'AD2JL6BFSL5UF'
	)

check_rej<-filter(check,!WorkerId %in% rejected)

check_rej %>% filter(WorkerId %in% WorkerId[duplicated(WorkerId)]) %>% kable

dontuseSurveyIDs<-c(
	'R_2bhh4UUPXQ593TL',
	'R_6o1i38hx8KhpbcV',
	'R_eOKSehpq8xJeGPj',
	'R_8faGuSLj9AD1cLr',
	'R_exJcy3JdMwLX3jn',
	'R_9EPN7tVKOYKf0J7',
	'R_esmIDRIveuVpYKV',
	'R_0V2KtHgQ4t1k5yR'
)

check_rej_cln<-filter(check_rej,!V1 %in% dontuseSurveyIDs)

dim(check)
dim(check_rej)
dim(check_rej_cln)

check_latlon<-left_join(
	check_rej_cln,
	svc[,c('Q20','LocationLatitude','LocationLongitude')],
	by=c('WorkerId'='Q20')) %>%
	mutate(LocationLatitude=as.numeric(LocationLatitude),
		LocationLongitude=as.numeric(LocationLongitude)) %>%
	arrange(LocationLongitude)%>%
	mutate(index=1:n())

check_latlon%>%
	select(one_of('WorkerId','LocationLatitude','LocationLongitude','index')) %>%
	arrange(WorkerId) %>% kable

#+cache=T,echo=F,warning=F,message=F,error=F,results='hide'
# aMap <- get_map("Eugene, Oregon", zoom=1)
#+cache=F,echo=F,warning=F,message=F,error=F,results='hide'
# str(aMap)

# ggbase<-ggplot(data=check_latlon,
# 	aes(
# 		x=LocationLongitude,
# 		y=LocationLatitude))
# #+fig.width=10,fig.height=12,echo=F,warning=F,message=F,error=F,results='hide'
# ggmap(aMap,base_layer=ggbase,extent='normal')+
# 	geom_point(color='red',position=position_jitter(width=0,height=0),alpha=.5) +
# 	geom_rug(sides='b',position='jitter')+
# 	geom_text(aes(x=LocationLongitude,y=-50,label=index),
# 		alpha=1,position=position_jitter(w=5,h=20),size=3)

# kable(check_latlon %>% filter(index >= 78))

# Did not follow instructions for HIT with regard to geographic location, and possibly language requirement.
dontUseGeoWorkerId<-c(
	'A2J237J8KM3OCS',
	'A1TMLYFTY2MEFH',
	'A2S2UWI1G2H8IQ',
	'A1P7ZED14W8QPK',
	'A20LJFKHTEXZEW',
	'A2Z3T0MW3HKW18',
	'A37AJI03M37NPJ',
	'A1ZBCPFZH8XT7X',
	'AB5EN36L2TOZ3',
	'A2QEX61YIEGLL3',
	'AGN9KSJLLFK1U',
	'A3I26RTI1BXIH0')

check_cleanest<-filter(check_rej_cln,!WorkerId %in% dontUseGeoWorkerId)

#+echo=F,warning=F,message=F,error=F,results='markup'
# dim(check_cleanest)

cleanDat<-left_join(
	check_cleanest[,c('WorkerId','V1')],
	svc,
	by=c('WorkerId'='Q20','V1'='V1'))
dim(cleanDat)
# names(cleanDat)

##
## Importing and checking the final 23
##

#'
#' # CHECK 23
#'

#+results='markup'
svc.imp.23<-read.csv(file.path(mturk_data_dir, 'SVC_Norming_23.csv'),stringsAsFactors=F)
mturk.imp.23<-read.csv(file.path(mturk_data_dir, 'Batch_2052027_batch_results.csv'),stringsAsFactors=F)

dim(svc.imp.23)
dim(mturk.imp.23)

svc.23<-svc.imp.23[-1,]

svc.23$Q20<-str_trim(svc.23$Q20)
mturk.imp.23$WorkerId<-str_trim(mturk.imp.23$WorkerId)
svc.23$Q20[svc.23$mTurkCode==3873282]<-'A3A8JSTNP8BD86'
svc.23$numNA <- apply(svc.23,1,function(x){sum(is.na(x))})

table(svc.23$numNA)

check.23<-left_join(
	mturk.imp.23[,mturk_cols],
	svc.23[,checkthese],
	by=c('WorkerId'='Q20')) %>%
	mutate(SameCode=str_trim(mTurkCode) == str_trim(Answer.surveycode))

dim(check.23)
names(check.23)

table(check.23[,c('AccPop_63')],useNA='ifany')
table(check.23[,c('ContStat_63')],useNA='ifany')
table(check.23[,c('AccPop_16')],useNA='ifany')
table(check.23[,c('ContStat_16')],useNA='ifany')
table(check.23[,c('SameCode')],useNA='ifany')

check.23 %>% filter(
	!AccPop_16 %in% 1:5 | 
	!ContStat_16 %in% 1:5 | 
	!SameCode | 
	numNA > 6) %>% kable

kable(check.23)

reject.23<-c(
	'A1EPKYRRR8OMKY',
	'A3A8JSTNP8BD86')

check_cleanest.23<-filter(check.23,!WorkerId %in% reject.23)

cleanDat.23<-left_join(
	check_cleanest.23[,c('WorkerId','V1')],
	svc.23,
	by=c('WorkerId'='Q20','V1'='V1'))
dim(cleanDat.23)

##
##
##

cleanDat.old<-cleanDat
cleanDat<-rbind(cleanDat.old,cleanDat.23)

dim(cleanDat)
# 
save(list=c('qtext','cleanDat'),file='svcClean.Rda')
