#'---
#'title: SVC Item Norming
#'author: John Flournoy
#'output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'---
#'
#' This is an EFA for the ratings of the self on all items. Only 70 of the 77 have totally complete data, and to be conservative, I'm just looking at those 70. We don't lose too much without those 7 and it's easier to satisfy assumptions of EFA without missing data. Haven't looked closely at what they're missing yet though.
#'
#+echo=F,warning=F,message=F,error=F,results='hide'
# library(rmarkdown);render('svc_norm.r')
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
library(moments)
library(psych)
library(GGally)

boot.rlm<-function(dat,i){
	d<-dat[i,]
	col<-dim(d)[2]
	mod<-rlm(d[,1]~d[,2:col])
	coef(mod)
}

load(file='svcClean.Rda')

#+results='markup'
library(tidyr)

qtext_descs<-qtext %>% 
	select(
		matches('SelfRating'),
		matches('AccPop'),
		matches('ContStat')) %>%
	gather(key,value) %>%
	mutate(desc_tmp=sub('.*\\.\\.\\.-(.*)','\\1',value),
		desc=paste(desc_tmp,sub('(Self|Acc|Cont).*','\\1',key),sep='_')) %>%
	select(key,desc)

cleanDat_l<-gather(cleanDat,key,value,age:SES.Ladder,-transition,-SelfPopMor) %>%
	left_join(qtext_descs,by=c('key'='key')) %>%
	mutate(desc=ifelse(is.na(desc),key,desc),
	       value = as.numeric(value))
#'
#' # Descriptives
#'
#' These histograms by themselves tell a really interesting story -- you can see that there is a some overlap between Acc and Cont ratings, but some items distinguish between them. These line up with a lot of our *a priori* ideas -- see anger and jealousy. Pretty cool.
#'
#' Aside from the status items, you also can see the scores on depression and substance use, among other things. Keep in mind the middle 2 CES_D items are reverse coded. SES ladder is mostly between 3-7 on a 9 point scale.
#'

hist(as.numeric(cleanDat$age))
summary(as.numeric(cleanDat$age))
sd(as.numeric(cleanDat$age))

#+fig.width=10,fig.height=100,cache=T,echo=F,warning=F,message=F,error=F
ggplot(cleanDat_l,aes(x=value))+
	geom_histogram()+
	facet_wrap(~desc,ncol=4,scales='free')


#+fig.width=10,fig.height=33,cache=T,echo=F,warning=F,message=F,error=F
cleanDat_l%>% select(desc,value) %>%
	filter(grepl('_Self',desc)) %>%
	ggplot(aes(x=value))+
		geom_histogram()+
		facet_wrap(~desc,ncol=4)


cleanDat_l %>% select(desc,value) %>%
	filter(grepl('_Self',desc)) %>%
	group_by(desc) %>% mutate(value=as.numeric(value)) %>%
	summarise(
		skew=skewness(value,na.rm=T),
		pval=agostino.test(na.omit(value))$p.value,
		mean=mean(value,na.rm=T),
		sd=sd(value,na.rm=T)) %>%
	arrange(abs(skew)) %>% kable

#+cache=T,echo=F,warning=F,message=F,error=F
library(psych)

cleanDat_w<-cleanDat_l %>% 
	select(WorkerId,V1,desc,value) %>%
	spread(desc,value)
# names(cleanDat_w)

num_both_status<-dim(filter(
	cleanDat_w,
	SelfPop_1 > 3,
	SelfPop_2 > 3))[1]

num_neither_status<-dim(filter(
	cleanDat_w,
	SelfPop_1 <= 3,
	SelfPop_2 <= 3))[1]

num_acconly_status<-dim(filter(
	cleanDat_w,
	SelfPop_1 <= 3,
	SelfPop_2 > 3))[1]

num_contonly_status<-dim(filter(
	cleanDat_w,
	SelfPop_1 > 3,
	SelfPop_2 <= 3))[1]


#'
#' The plot below seems to indicate that a good number of people consider
#' themselves high status in both ways (all responses in the 
#' top right quadrant). There doesn't seem to be perfect
#' separation between the two. However, note that the bottom left quadrant
#' reflects responses from people that think neither type of status
#' describes them well (`r num_neither_status` people). Also note that the overlap group is only comprises
#' `r num_both_status` people. Number of primarily accepted status = `r num_acconly_status`,
#' and primarily controversial status participants = `r num_contonly_status`.
#'

cleanDat_w%>%
	filter(!is.na(SelfPop_2),!is.na(SelfPop_1)) %>%
	ggplot(aes(x=factor(SelfPop_1),y=factor(SelfPop_2)))+
		geom_bin2d()+
		geom_rug(position='jitter')+
		geom_hline(yintercept=3.5,color='red',size=1)+
		geom_vline(xintercept=3.5,color='red',size=1)+
		ylab('Self-rated controversial high status')+
		xlab('Self-rated accepted high status')+
		ggtitle('Counts for each combination of accepted and controversial status')

cleanDat_w%>%
	select(SelfPop_1,SelfPop_2) %>%
	filter(!is.na(SelfPop_1),!is.na(SelfPop_2)) %>%
	table %>% 
	as.data.frame %>%
	spread(SelfPop_2,Freq) %>%
	rename(Cont_v_Acc=SelfPop_1) %>% 
	kable(caption='Controversial versus Accepted Status Frequencies')

#'	
#' # Self Ratings
#'

selfRateDat<-select(cleanDat_w,matches('_Self'),matches('SelfPop')) %>% 
	mutate_each(funs(as.numeric))
	
#'
#' ## EFA to look at latent constructs  
#'
#' The more traditional approach would be to use EFA. What we hope to 
#' get is 2 factors the correspond to Accepted High Status and Controversial
#' High Status.
#'
#' However, the scree plot shows *very* clearly a 3 factor solution will 
#' account for most of the variance, with no real gain if we increase beyond 4.
#'

selfRateDatCor<-cor(select(selfRateDat,-matches('SelfPop')),use='pairwise.complete.obs')

colnames(selfRateDatCor) <- sub(' _',  '_', colnames(selfRateDatCor))
rownames(selfRateDatCor) <- sub(' _',  '_', rownames(selfRateDatCor))
longCor <- selfRateDatCor %>% as.data.frame %>%
	mutate(rows=rownames(selfRateDatCor)) %>%
	gather(cols, value, -rows)
scale.max.r =  max(abs(longCor$value))

longCor %>% ggplot(aes(x=cols, y=rows, fill=value)) +
	geom_bin2d()+
	scale_fill_gradientn(limits=c(-1, 1),
			     colours = c('blue', 'blue', 
					 'aliceblue', 'white', 
					 'mistyrose', 'red', 'red'), 
			     values = scales::rescale(c(-1, -scale.max.r,-.1, 0, .1,scale.max.r, 1)),
			     breaks=c(-.3, .3))

scree(selfRateDatCor)

selfRateFA<-fa(
	select(selfRateDat,-matches('SelfPop')),
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')

selfRateComm<-as.data.frame(selfRateFA$communality) %>%
	mutate(item=row.names(.)) %>%
	rename(comm=`selfRateFA$communality`)
sortedSelfFA<-as.data.frame(fa.sort(loadings(selfRateFA))[,]) %>%
	mutate(item=rownames(.)) %>% select(item,PA3,PA1,PA2)
printableSortedSelfFA<-cbind(item=sortedSelfFA[,1],
	apply(sortedSelfFA[,2:4],2,function(x){
		ifelse(abs(x) < .1, ' ', sprintf('%.2f',x))
	})) %>%
	as.data.frame %>% 
	left_join(selfRateComm)

#'
#' Here's the raw output, if you like, but see below for a prettier table.
#' Notice the factors are allowed to correlate.
#'

print(selfRateFA,sort=T, cut=.29)


#' 
#' There are no items with really poor loadings on all three factors, but
#' there are a lot with high cross loadings. Anything > .3 is potentially
#' suspect (even that is a little liberal). I've pulled out 13 such items:
#'
#' ```
#' admired_Self          1  0.62  0.25  0.36 0.65 0.35 2.0
#' cool_Self            16  0.60  0.27  0.41 0.68 0.32 2.2
#' leader_Self          34  0.57  0.12  0.32 0.49 0.51 1.7
#' adventurous_Self      2  0.50  0.29  0.33 0.51 0.49 2.4
#' poor_Self            44 -0.46 -0.22  0.33 0.45 0.55 2.3
#' daring_Self          17  0.43  0.08  0.38 0.36 0.64 2.0
#' comforting_Self      12  0.34  0.44  0.00 0.40 0.60 1.9
#' angry_Self            4 -0.32 -0.09  0.71 0.64 0.36 1.4
#' grumpy_Self          27 -0.42 -0.05  0.66 0.62 0.38 1.7
#' jealous_Self         32 -0.39  0.02  0.56 0.44 0.56 1.8
#' patient_Self         42  0.13  0.43 -0.27 0.36 0.64 1.9
#' mean_Self            38  0.01 -0.32  0.70 0.67 0.33 1.4
#' fake_Self            21 -0.14 -0.40  0.48 0.51 0.49 2.1
#' ```
#'

#'
#' ## Some modifications
#'
#' We can try again after dropping items with too much noise/cross loading

dropthese<-c(
	'admired_Self',
	'cool_Self',
	'leader_Self',
	'adventurous_Self',
	'poor_Self',
	'daring_Self',
	'comforting_Self',
	'angry_Self',
	'grumpy_Self',
	'jealous_Self',
	'patient_Self',
	'mean_Self',
	'fake_Self')

selfRateDat_lim<-select_(selfRateDat,.dots=paste0('-',dropthese))
# names(selfRateDat_lim)
scree(select(selfRateDat_lim,-matches('SelfPop')))

selfRateFA<-fa(
	select(selfRateDat_lim,-matches('SelfPop')),
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
print(selfRateFA,sort=T, cut=.29)

#' 
#' There are still some bad items:
#' 
#' ```
#' athletic_Self         2  0.20 -0.24  0.14 0.15 0.85 2.6
#' funny_Self           17  0.32 -0.44  0.20 0.43 0.57 2.3
#' attractive_Self       3  0.36 -0.52  0.31 0.62 0.38 2.5
#' dependent_Self       12  0.17  0.35  0.37 0.20 0.80 2.4
#' ```
#' 

dropthese2 <- c(
	'athletic_Self',  
	'funny_Self',     
	'attractive_Self',
	'dependent_Self') 

selfRateDat_lim2 <- select_(selfRateDat_lim,.dots=paste0('-',dropthese2))
# names(selfRateDat_lim)
scree(select(selfRateDat_lim2,-matches('SelfPop')))

selfRateFA<-fa(
	select(selfRateDat_lim2,-matches('SelfPop')),
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
print(selfRateFA,sort=T, cut=.29)

#' 
#' This should finish off our cleaning:
#' 
#' ```
#' trendy_Self          42  0.11 -0.66  0.38 0.69 0.31 1.7
#' social_Self          38  0.27 -0.61  0.38 0.72 0.28 2.1
#' popular_Self         28  0.29 -0.60  0.39 0.73 0.27 2.2
#' flirty_Self          12  0.10 -0.54  0.40 0.54 0.46 1.9
#' outgoing_Self        26  0.19 -0.53  0.34 0.52 0.48 2.0
#' ```
#' 

dropthese3 <- c(
'trendy_Self',  
'social_Self',  
'popular_Self', 
'flirty_Self',  
'outgoing_Self')

selfRateDat_lim3 <- select_(selfRateDat_lim2,.dots=paste0('-',dropthese3))
# names(selfRateDat_lim)
scree(select(selfRateDat_lim3,-matches('SelfPop')))

selfRateFA<-fa(
	select(selfRateDat_lim3,-matches('SelfPop')),
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
print(selfRateFA,sort=T, cut=.29, short=T)

#'
#' After three rounds of item exclusion, we come up with a pretty clear factor structure.
#' 
#' 1. PA1: Prosocial, "girl scout".
#' 2. PA2: Depressed, submissive, negative.
#' 3. PA3: Anti-social, dominant, aggressive.
#'
#' ### Scale scores
#'
#'

itemScaleGroups<-as.data.frame(selfRateFA$loadings[,]) %>%
	mutate(item=rownames(.)) %>%
	group_by(item) %>%
	do({
		sorted<-.[,c('PA1','PA2','PA3')][order(as.data.frame(abs(.[,c('PA1','PA2','PA3')])))]
		data.frame(
			max=sorted[[3]],
			maxPA=names(sorted)[3])
	})

itemScaleGroupsClean1 <- itemScaleGroups

Rscore<-function(x,min,max){
	r<-max-x+min
}

selfRateFAScaleScores<-cleanDat_w %>% 
	select(WorkerId,one_of(itemScaleGroups$item)) %>% 
	gather(item,resp,-WorkerId) %>%
	left_join(itemScaleGroups) %>%
	mutate(resp=as.numeric(resp)) %>%
	mutate(R_resp=ifelse(
		sign(max)<0,
		Rscore(resp,1,6),
		resp)) %>%
	group_by(WorkerId,maxPA) %>%
	summarise(
		item_ex=sub('_Self','',item[[1]]),
		scale_score=mean(R_resp>3,na.rm=T)) %>%
	select(WorkerId,item_ex,scale_score) %>%
	spread(item_ex,scale_score) %>%
	left_join(select(cleanDat_w,WorkerId,matches('SelfPop'),matches('YSUBS'))) %>%
	mutate_at(.funs = funs(as.numeric), .vars = vars(-WorkerId)) %>%
  ungroup()

#+fig.width=10, fig.height=10
ggpairs(selfRateFAScaleScores %>% select(agressive, awkward, calm, SelfPop_1, SelfPop_2),
	lower=list(continuous='smooth'), diag=list(continuous='bar'))

selfRateFAScaleScoreClean1 <- selfRateFAScaleScores

#'
#' ## Remove highly skewed items...
#'
#' The factors are a bit skewed. This may be due to highly skewed items. What's the effect of removing them?
#'
#' They include:
#'
#' - nice_Self	
#' - welcoming_Self	
#' - friendly_Self	
#' - ugly_Self	
#' - sympathetic_Self
#' - trustworthy_Self
#' - smart_Self	
#' - fair_Self	
#' - fake_Self	
#' - loyal_Self	
#' - respectful_Self	
#'

dropthese <- c(
	'nice_Self',	
	'welcoming_Self',	
	'friendly_Self',	
	'ugly_Self',	
	'sympathetic_Self',
	'trustworthy_Self',
	'smart_Self',	
	'fair_Self',	
	'fake_Self',	
	'loyal_Self',	
	'respectful_Self')	

selfRateDat_lim<-select_(selfRateDat,.dots=paste0('-',dropthese))
# names(selfRateDat_lim)
scree(select(selfRateDat_lim,-matches('SelfPop')))

selfRateFA<-fa(
	select(selfRateDat_lim,-matches('SelfPop')),
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
print(selfRateFA,sort=T, cut=.29)


#'
#' That helps a bit, but we could drop a few more skewed items...
#'
#'
#' - agressive_Self	
#' - giving_Self	
#' - snobby_Self	
#' - angry_Self	
#' - honest_Self	
#' - motivated_Self	
#' - rude_Self	
#' - mean_Self	
#'

dropthese2 <- c(
	'agressive_Self',	
	'giving_Self',	
	'snobby_Self',	
	'angry_Self',	
	'honest_Self',	
	'motivated_Self',	
	'rude_Self',	
	'mean_Self')	

selfRateDat_lim2<-select_(selfRateDat_lim,.dots=paste0('-',dropthese2))
# names(selfRateDat_lim)
scree(select(selfRateDat_lim2,-matches('SelfPop')))

selfRateFA<-fa(
	select(selfRateDat_lim2,-matches('SelfPop')),
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
print(selfRateFA,sort=T, cut=.29)

#'
#' I'm going to trim just a couple items for low loadings, or high cross loadings
#'
#' ```
#' athletic_Self         3                   0.19 0.81 2.7
#' trendy_Self          43 -0.51  0.51       0.68 0.32 2.0
#' selfish_Self         38  0.33  0.49 -0.40 0.49 0.51 2.7
#' ```
#'

dropthese3 <- c(
'athletic_Self',
'trendy_Self',  
'selfish_Self')

selfRateDat_lim3<-select_(selfRateDat_lim2,.dots=paste0('-',dropthese3))
# names(selfRateDat_lim)
scree(select(selfRateDat_lim3,-matches('SelfPop')))

selfRateFA<-fa(
	select(selfRateDat_lim3,-matches('SelfPop')),
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
print(selfRateFA,sort=T, cut=.29, short=T)

#'
#' 1. PA1 Dynamic
#' 2. PA3 Negative
#' 3. PA2 Prosocial
#'

#'
#' These clusters of items may be related to status 
#' We can correlate self rated accepted and controversial status 
#' with the factor scores to start exploring this.
#'

selfPopDat<-cleanDat_w[,c('SelfPop_1','SelfPop_2')]%>%mutate_each(funs(as.numeric))

#'
#' ### Correlations
#'
#' In the output below, SelfPop_1 is controversial, and SelfPop_2 is accepted.
#'
#' Controversial status is significantly positively correlated with dynamism and 
#' negativity, and negatively with prosociality.
#' 
#' Accepted status is significantly positively associated with dynamism and negatively
#' with negativity, and non-significantly correlated with prosociality.

selfRateDat_nopop<-select(selfRateDat_lim3,-matches('SelfPop'))

#mean imputation just to get a score for everyone...
#not perfect, but it'll do

selfRateDat_nopop_imputed<-apply(
	selfRateDat_nopop,
	2,
	function(x){
		ifelse(is.na(x),mean(x,na.rm=T),x)
	})


corr.test(
	as.data.frame(selfPopDat),
	as.data.frame(predict(selfRateFA,selfRateDat_nopop_imputed,missing=T,use='pairwise.complete.obs')),
	use='pairwise.complete.obs',
	adjust='none')

# Two people left the controversial status unrated.

#'
#' Grain of salt: No correction for multiple comparison here. 
#'
#' Slightly bigger grain of salt: 8 folks are missing a rating on
#' a couple items here and there, so I impute the mean so we can get
#' predicted factor scores. This means that significance values and CIs close
#' to threshold below should be treated lightly (but it doesn't change things
#' too much).
#'
#' This gets more interesting in the regressions.
#'
#' ### Regressing Controversial Status
#'
#' Here's the raw output for regressing controversial status on the component scores -- see below for a table with CIs.
#'

summary(selfPopContLM<-rlm(selfPopDat[,c('SelfPop_1')]~predict(selfRateFA,selfRateDat_nopop_imputed)))

#+cache=T,echo=F,warning=F,message=F,error=F
set.seed(3133)
rlmSelfContBoot<-boot(
	cbind(
		selfPopDat[,c('SelfPop_1')],
		predict(selfRateFA,selfRateDat_nopop_imputed)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

#'
#' Results from bootstrap estimates:
#' Controversial status is significantly negatively predicted by the
#' prosocial component and significantly positively predicited by 
#' the dominant-agressive component.
#' 

bootTableCont<-cbind(c(1:3),as.numeric(rlmSelfContBoot$t0[2:4]),rbind(
	t(as.matrix(boot.ci(rlmSelfContBoot,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfContBoot,type='bca',index=3)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfContBoot,type='bca',index=4)$bca[c(4,5)]))))

kable(
	bootTableCont,
	col.names=c('component','est','lower 95%','upper 95%'),
	digits=2,
	caption='Controversial status relation with component scores, bootstrapped CIs')

#'
#' ### Regressing Accepted Status
#'
#'
#' Here's the raw output for an rlm of accepted status on the component scores. Again, scroll down for a nice table.
#'

summary(selfPopAccLM<-rlm(selfPopDat[,c('SelfPop_2')]~predict(selfRateFA,selfRateDat_nopop_imputed)))

#+cache=T,echo=F,warning=F,message=F,error=F
set.seed(3133)
rlmSelfAccBoot<-boot(
	cbind(
		selfPopDat[,c('SelfPop_2')],
		predict(selfRateFA,selfRateDat_nopop_imputed)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

bootTableAcc<-cbind(c(1:3),as.numeric(rlmSelfAccBoot$t0[2:4]),rbind(
	t(as.matrix(boot.ci(rlmSelfAccBoot,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfAccBoot,type='bca',index=3)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfAccBoot,type='bca',index=4)$bca[c(4,5)]))))

#'
#' Accepted status is significantly predicted by higher
#' prosociality, less depression/negativity, and more dominance/aggression.
#'

kable(
	bootTableAcc,
	col.names=c('component','est','lower 95%','upper 95%'),
	digits=2,
	caption='Accepted status relation with component scores, bootstrapped CIs')

#+cache=T,echo=F,warning=F,message=F,error=F
#'
#' ### What has EFA told us?
#'
#' Well, we have three factors that are helpfully related to self reported
#' status. This is what we want. All three factors seem relevant.
#'
#' ### Scale scores
#'
#' The above was all tested with factor scores, but we'll be using scale scores.
#' Let's test that:
#'

itemScaleGroups<-as.data.frame(selfRateFA$loadings[,]) %>%
	mutate(item=rownames(.)) %>%
	group_by(item) %>%
	do({
		sorted<-.[,c('PA1','PA3','PA2')][order(as.data.frame(abs(.[,c('PA1','PA3','PA2')])))]
		data.frame(
			max=sorted[[3]],
			maxPA=names(sorted)[3])
	})

Rscore<-function(x,min,max){
	r<-max-x+min
}

selfRateFAScaleScores<-cleanDat_w %>% 
	select(WorkerId,one_of(itemScaleGroups$item)) %>% 
	gather(item,resp,-WorkerId) %>%
	left_join(itemScaleGroups) %>%
	mutate(resp=as.numeric(resp)) %>%
	mutate(R_resp=ifelse(
		sign(max)<0,
		Rscore(resp,1,6),
		resp)) %>%
	group_by(WorkerId,maxPA) %>%
	summarise(
		item_ex=sub('_Self','',item[[1]]),
		scale_score=mean(R_resp>3,na.rm=T)) %>%
	select(WorkerId,item_ex,scale_score) %>%
	spread(item_ex,scale_score) %>%
	left_join(select(cleanDat_w,WorkerId,matches('SelfPop'),matches('YSUBS'))) %>%
	mutate_at(.funs=funs(as.numeric),.vars = vars(-WorkerId)) %>%
  ungroup()

itemScaleGroups2 <- itemScaleGroups

#+fig.width=10, fig.height=10
ggpairs(selfRateFAScaleScores %>% select(admired, awkward, calm, SelfPop_1, SelfPop_2),
	lower=list(continuous='smooth'), diag=list(continuous='bar'))

#'
#' ## Which Cleaned Set to Use?
#'
#' Looking at the distribution of both sets of items (those cleaned by removing 
#' items with bad loadings, versus those cleaned by removing skewed items first)
#' indicates that both have a factor that has a skewed distribution. So we haven't
#' gained anything there. Additionally, both seem to predict the two different kinds
#' status. 
#' 
#' The prosocial factor in the first cleaned set has more items than that in the second set. For the fMRI paradigm, it will be helpful to be able to use this larger set.
#'
#' Below I'll test the factor scores (binarized, as we'll get off the scanner) to 
#' predict the two status types.
#'
#' ### Re-examining the first set of items
#' 

selfRateFAScaleScores <- selfRateFAScaleScoreClean1 

#'
#' See raw summary, and below that, tables of bootstrapped CIs.
#'

summary(rlm(
	SelfPop_1~1+agressive+awkward+calm,
	data=selfRateFAScaleScores))
summary(rlm(
	SelfPop_2~1+agressive+awkward+calm,
	data=selfRateFAScaleScores))
corr.test(
	select(selfRateFAScaleScores,matches('SelfPop')),
	select(selfRateFAScaleScores,agressive,awkward,calm),
	use='pairwise.complete.obs',adjust='none',
	method='kendall')

#+cache=T
set.seed(3133)
rlmSelfContBootFAScale<-boot(
	as.matrix(select(selfRateFAScaleScores,
		SelfPop_1,
		agressive,awkward,calm)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

bootTableContFAScale<-cbind(as.numeric(rlmSelfContBootFAScale$t0[2:4]),rbind(
	t(as.matrix(boot.ci(rlmSelfContBootFAScale,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfContBootFAScale,type='bca',index=3)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfContBootFAScale,type='bca',index=4)$bca[c(4,5)]))))
rownames(bootTableContFAScale)<-names(rlmSelfContBootFAScale$t0[2:4])
kable(
	bootTableContFAScale,
	col.names=c('est','lower 95%','upper 95%'),
	digits=2,
	caption='Controversial status relation with scale scores, bootstrapped CIs')

#+cache=T
set.seed(3133)
rlmSelfAccBootFAScale<-boot(
	as.matrix(select(selfRateFAScaleScores,
		SelfPop_2,
		agressive,awkward,calm)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

bootTableAccFAScale<-cbind(as.numeric(rlmSelfAccBootFAScale$t0[2:4]),rbind(
	t(as.matrix(boot.ci(rlmSelfAccBootFAScale,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfAccBootFAScale,type='bca',index=3)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfAccBootFAScale,type='bca',index=4)$bca[c(4,5)]))))
rownames(bootTableAccFAScale)<-names(rlmSelfAccBootFAScale$t0[2:4])
kable(
	bootTableAccFAScale,
	col.names=c('est','lower 95%','upper 95%'),
	digits=2,
	caption='Accepted status relation with scale scores, bootstrapped CIs')
#+cache=T

#'
#' We see that, controlling for the other factors, high ratings on the agressive traits tends to predict high controversial popular status, while low ratings on the awkward traits tends to predict high ratings on prosocial status.
#'

#'
#'
#'
#' ### A Little CFA
#'
#' This CFA is not performed on an independent data set.
#'

library(lavaan)


itemScaleGroups <- itemScaleGroupsClean1 

itemScaleGroupsCFA<-itemScaleGroups

itemScaleGroupsCFA$item<-sub('[ -]','',itemScaleGroupsCFA$item)

dataCFA<-select(cleanDat_w,matches('_Self')) %>% mutate_each(funs(as.numeric))
names(dataCFA)<-sub('[ -]','',names(dataCFA))

cat(selfReportCFAmodel<-paste(
	tapply(itemScaleGroupsCFA$item,itemScaleGroupsCFA$maxPA,function(x){
		paste0(sub('_Self','',x[1]),'=~',
			paste0(x,collapse='+'))
	}) %>% paste0(collapse='\n'),'\n'))


srCFA<-cfa(selfReportCFAmodel,data=dataCFA)

summary(srCFA,fit.measures=T,standardized=T)

#' Ultimately, when we reduce the items to scale scores, only the strongest
#' predictors survive (though note the correlations above among the different factors). Aggression, all 
#' else equal, predicts Controversial High Status. Awkwardness (similar, probably, to Extraversion), all else
#' equal, predicts accepted status.

library(semPlot)
#+cache=T,fig.width=10,fig.height=10
semPaths(semPlotModel(srCFA),whatLabels='std',layout='tree',rotation=4)

#'
#' We can examine reliability of the latent variables and scale scores. Of course, these will be inflated because we've selected variable sets that are highly correlated.
#' 

library(semTools)
reliability(srCFA)

alpha(dataCFA[,itemScaleGroupsCFA$item[itemScaleGroupsCFA$maxPA=='PA1']],check.keys=T)
alpha(dataCFA[,itemScaleGroupsCFA$item[itemScaleGroupsCFA$maxPA=='PA2']],check.keys=T)
alpha(dataCFA[,itemScaleGroupsCFA$item[itemScaleGroupsCFA$maxPA=='PA3']],check.keys=T)

#'
#' # EFA on items picked through discussion
#'
#' We ultimately were not able to exclude as many items as we wanted to. Through discussion we decided to keep a subset of items that had been rejected from set 1 or set 2. This EFA examines this larger set:
#'

freshItems <- c('considerate_Self',
'nice_Self',
'helpful_Self',
'genuine_Self',
'caring_Self',
'friendly_Self',
'trustworthy_Self',
'loyal_Self',
'giving_Self',
'sympathetic_Self',
'welcoming_Self',
'fair_Self',
'respectful_Self',
'calm_Self',
'honest_Self',
'humble_Self',
'motivated_Self',
'insecure_Self',
'lonely_Self',
'depressed_Self',
'awkward_Self',
'loner_Self',
'plain_Self',
'shy_Self',
'boring_Self',
'confident_Self',
'pushover_Self',
'ugly_Self',
'trendy_Self',
'social_Self',
'popular_Self',
'flirty_Self',
'outgoing_Self',
'controlling_Self',
'bossy_Self',
'angry_Self',
'mean_Self',
'grumpy_Self',
'agressive_Self',
'snobby_Self',
'rude_Self',
'selfish_Self',
'jealous_Self',
'stubborn_Self',
'risky_Self',
'fake_Self',
'attractive_Self',
'cool_Self')

names(selfRateDat) <- sub(' _',  '_', names(selfRateDat))
freshSelfRate <- select_(selfRateDat,.dots=freshItems)
scree(freshSelfRate)

selfRateFA<-fa(
	freshSelfRate,
	nfactors=3,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
print(selfRateFA,sort=T, 
      cut=.29,
      short=T)

itemScaleGroups<-as.data.frame(selfRateFA$loadings[,]) %>%
	mutate(item=rownames(.)) %>%
	group_by(item) %>%
	do({
		sorted<-.[,c('PA1','PA2','PA3')][order(as.data.frame(abs(.[,c('PA1','PA2','PA3')])))]
		data.frame(
			max=sorted[[3]],
			maxPA=names(sorted)[3])
	})

cat(selfReportCFAmodel<-paste(
	tapply(itemScaleGroups$item,itemScaleGroups$maxPA,function(x){
		paste0(sub('_Self','',x[1]),'=~',
			paste0(x,collapse='+'))
	}) %>% paste0(collapse='\n'),'\n'))

semPaths(lavaan::lavaanify(selfReportCFAmodel))
srCFA<-cfa(selfReportCFAmodel,data=freshSelfRate)
summary(srCFA)
semPaths(srCFA, what='std', whatLabels='std')

itemScaleGroups<-as.data.frame(selfRateFA$loadings[,]) %>%
  mutate(item=rownames(.)) %>%
  group_by(item) %>%
  do({
    sorted<-.[,c('PA1','PA3','PA2')][order(as.data.frame(abs(.[,c('PA1','PA3','PA2')])))]
    data.frame(
      max=sorted[[3]],
      maxPA=names(sorted)[3])
  })

Rscore<-function(x,min,max){
  r<-max-x+min
}

selfRateFAScaleScores<-cleanDat_w %>% 
  rename(`boring_Self` = `boring _Self`) %>%
  select(WorkerId,one_of(itemScaleGroups$item)) %>% 
  gather(item,resp,-WorkerId) %>%
  left_join(itemScaleGroups) %>%
  mutate(resp=as.numeric(resp)) %>%
  mutate(R_resp=ifelse(
    sign(max)<0,
    Rscore(resp,1,6),
    resp)) %>%
  group_by(WorkerId,maxPA) %>%
  summarise(
    item_ex=sub('_Self','',item[[1]]),
    scale_score=mean(R_resp>3,na.rm=T)) %>%
  select(WorkerId,item_ex,scale_score) %>%
  spread(item_ex,scale_score) %>%
  left_join(select(cleanDat_w,WorkerId,matches('SelfPop'),matches('YSUBS'))) %>%
  mutate_at(.funs=funs(as.numeric),.vars = vars(-WorkerId)) %>%
  ungroup()

itemScaleGroups_fresh <- itemScaleGroups

#+fig.width=10, fig.height=10
ggpairs(selfRateFAScaleScores %>% select(agressive, attractive, calm, SelfPop_1, SelfPop_2),
        lower=list(continuous='smooth'), diag=list(continuous='bar'))

#'
#' ## Items we used in SvC TAG
#'

items_used <- read.csv('svcTraits.csv', header = F)
items_used$used_items <- TRUE
items_used <- items_used[,c(1,5)]
names(items_used) <- c('items', 'used_items') 

item_comp <- full_join(
  mutate(items_used,
         items = as.character(items),
         items = case_when(
           items == 'aggressive' ~ 'agressive',
           TRUE ~ items)),
  data.frame(items = sub('_Self', '', freshItems),
             fresh_items = TRUE)) %>%
  full_join(data.frame(items = sub(' *_Self', '', itemScaleGroupsClean1$item),
                       set1 = TRUE)) %>%
  full_join(data.frame(items = sub(' *_Self', '', itemScaleGroups2$item),
                       set2 = TRUE)) %>%
  mutate_at(.funs = funs(ifelse(is.na(.), F, .)), 
            .vars = vars(-items)) %>%
  mutate(items = case_when(
           items == 'agressive' ~ 'aggressive',
           TRUE ~ items))


#'
#' Are there any set 1 items that are not in the final set?
#'

filter(item_comp, set1 == T, used_items == F)

#'
#' A lot of items we used were not in set 1:
#'

filter(item_comp, set1 == F, used_items == T)

#'
#' Are there any set 2 items that are not in the final set?
#'

filter(item_comp, set2 == T, used_items == F)

#'
#' A lot of items we used were not in set 2:
#'

filter(item_comp, set2 == F, used_items == T)

#'
#' Are there any fresh set items that are not in the final set?
#'

filter(item_comp, fresh_items == T, used_items == F)

#'
#' There are very few items we used that are not in the fresh set:
#' 

filter(item_comp, fresh_items == F, used_items == T)

#'
#' Which items in the final set were used not analyzed here?
#'

filter(item_comp, used_items == T, set1 == F, set2 == F, fresh_items == F)

