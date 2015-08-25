#'
#' This is an EFA for the ratings of the self on all items. Only 70 of the 77 have totally complete data, and to be conservative, I'm just looking at those 70. We don't lose too much without those 7 and it's easier to satisfy assumptions of EFA without missing data. Haven't looked closely at what they're missing yet though.
#'
#+echo=F,warning=F,message=F,error=F,results='hide'
# library(rmarkdown);render('svc_norm_2factor.r')
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
	mutate(desc=ifelse(is.na(desc),key,desc))
#'
#' # Descriptives
#'
#' These histograms by themselves tell a really interesting story -- you can see that there is a some overlap between Acc and Cont ratings, but some items distinguish between them. These line up with a lot of our *a priori* ideas -- see anger and jealousy. Pretty cool.
#'
#' Aside from the status items, you also can see the scores on depression and substance use, among other things. Keep in mind the middle 2 CES_D items are reverse coded. SES ladder is mostly between 3-7 on a 9 point scale.
#'

#+fig.width=10,fig.height=100,cache=T,echo=F,warning=F,message=F,error=F
ggplot(cleanDat_l,aes(x=value))+
	geom_histogram()+
	facet_wrap(~desc,ncol=4,scales='free')

#+cache=F,echo=F,warning=F,message=F,error=F
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
		geom_hline(y=3.5,color='red',size=1)+
		geom_vline(x=3.5,color='red',size=1)+
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

# meana<-function(x){
# 	mean(as.numeric(x),na.rm=T)
# }

# hiscale<-function(x){
# 	sum(x>3,na.rm=T)
# 	# *mean(as.numeric(x[x>3]),na.rm=T)
# }
# loscale<-function(x){
# 	sum(x<4,na.rm=T)
# 	# *mean(as.numeric(x[x<4]),na.rm=T)
# }

# ratiolohi<-function(x){
# 	rez<-(loscale(x))/(hiscale(x))
# 	# rez^((rez<1)*(-1)+(rez>=1)*1)
# 	rez
# }

# meansDistl<-function(x){
# 	l<-mean(as.numeric(x[x<4]),na.rm=T)
# 	h<-mean(as.numeric(x[x>3]),na.rm=T)
# 	abs(3.5-l)
# }

# meansDisth<-function(x){
# 	l<-mean(as.numeric(x[x<4]),na.rm=T)
# 	h<-mean(as.numeric(x[x>3]),na.rm=T)
# 	abs(h-3.5)
# }

# splitMeanRatio<-cleanDat_w %>% 
# 	summarise_each(
# 		funs(ratiolohi),
# 		matches('_Self')) %>% gather %>%
# 	arrange(value)
	
# hist(splitMeanRatio$value)

# kable(splitMeanRatio)

# splitMeans<-cleanDat_w %>% 
# 	summarise_each(
# 		funs(meansDisth,meansDistl),
# 		matches('_Self')) %>% gather %>%
# 	arrange(value)
	
# # hist(splitMeans$value)

# adat<-splitMeans %>%
# 	mutate(key=sub('_Self','',key)) %>%
# 	separate(key,c('adj','scalepart'),sep='_') %>%
# 	spread(scalepart,value) 

# adat$resid<-resid(lm(meansDisth~meansDistl,data=adat))

# adat %>%
# 	ggplot(aes(x=meansDistl,y=meansDisth))+
# 	geom_point()+
# 	geom_text(aes(label=adj))


# kable(splitMeans)

#' # Self Ratings
#'
#' ## Best predictors of status
#'
#' First thing, let's see what items are best at predicting status using
#' regularized regression for multinomial responses. This uses
#' cross validation to give us the set of predictors that leads
#' to the best out-of-sample prediction (least error). We probably
#' want these in our set of final items.
#'

selfRateDat<-select(cleanDat_w,matches('_Self'),matches('SelfPop')) %>% 
	mutate_each(funs(as.numeric))
	
x.miss<-as.matrix(select(selfRateDat,-matches('SelfPop')))
y.miss<-as.matrix(select(selfRateDat,matches('SelfPop')))
y<-y.miss[complete.cases(y.miss),]
x<-apply(x.miss,2,function(a){
	a_bar<-mean(a,na.rm=T)
	a[is.na(a)]<-a_bar
	a
})[complete.cases(y.miss),]

dimnames(x)[[2]]<-sub('[ -]','',dimnames(x)[[2]])

f<-as.formula(paste('V1~',paste(dimnames(x)[[2]],collapse='+')))

modelDF<-model.frame(f,as.data.frame(cbind(y[,1],x)))

modmat<-model.matrix(f,modelDF)[,-1]

#'
#' The first plot shows how prediction error varies as a function of
#' the number of parameters in the model, and how much weight each
#' gets (the size of the coefficient).
#'
#' The second set of plots actually show the size of each coefficient
#' predicting both types of status as the constraint parameter, lambda,
#' is tightened (left-to-right).
#'

#+cache=T
registerDoMC(cores=7)
set.seed(3133)
selfStatusClassifier.cv<-cv.glmnet(
	x=modmat,y=y,
	nfolds=dim(x)[1],
	parallel=T,
	family='mgaussian',
	type.measure='deviance',alpha=1)
#+cache=F
plot(selfStatusClassifier.cv)

test<-glmnet(
	x=x,y=y,
	family='mgaussian',alpha=1)

#+fig.height=10,fig.width=10
par(mfrow=c(2,1))
plot(test,label=T,xvar='lambda')
abline(v=log(selfStatusClassifier.cv$lambda.min))
abline(v=log(selfStatusClassifier.cv$lambda.1se))

selfStatCoef<-coef(selfStatusClassifier.cv,s=selfStatusClassifier.cv$lambda.min)

#'
#' This table shows all the non-zero coefficients.
#'

(LASSOpredSelfRep<-data.frame(
	xName=selfStatCoef$SelfPop_1@Dimnames[[1]],
	ContStat=as.numeric(as.matrix(selfStatCoef$SelfPop_1)),
	AccStat=as.numeric(as.matrix(selfStatCoef$SelfPop_2))) %>%
	filter(ContStat>0|AccStat>0)) %>%
	kable(digits=3,caption='Best predictors of self-report status')

#'
#' If we were merely trying to predict status from a future data set
#' containing these same items, the above table would be the best subset
#' to use.
#'
#' While we're interested in predicting status, we're not actually
#' asking about status. Rather, we're asking about traits that
#' we think relate to status. So while the above set of items
#' might tell us who is likely to have one or the other kind of status,
#' in the context of an fMRI analysis it would be hard to argue
#' that activity observed during self evaluation of these traits is 
#' some measure of status-related brain activity.
#'
#' ## EFA to look at latent constructs  
#'
#' The more traditional approach would be to use EFA. What we hope to 
#' get is 2 factors the correspond to Accepted High Status and Controversial
#' High Status.
#'
#' However, the scree plot shows *very* clearly a 4 factor solution will 
#' account for most of the variance, with no real gain if we increase beyond 4.
#' Ignoring that, we want the best 2 factor solution so that we can run
#' reasonable contrasts between these two sets. We can think of them
#' as super-factors, perhaps.
#'

selfRateDatCor<-cor(select(selfRateDat,-matches('SelfPop')),use='pairwise.complete.obs')

scree(selfRateDatCor)

# selfRateFA<-fa(
# 	select(selfRateDat,-matches('SelfPop')),
# 	nfactors=4,
# 	rotate='oblimin',
# 	scores='tenBerge',
# 	fm='pa',
# 	use='pairwise.complete.obs')

# selfRateFA3<-fa(
# 	select(selfRateDat,-matches('SelfPop')),
# 	nfactors=3,
# 	rotate='oblimin',
# 	scores='tenBerge',
# 	fm='pa',
# 	use='pairwise.complete.obs')
# print(selfRateFA3,sort=T)

selfRateFA2<-fa(
	select(selfRateDat,-matches('SelfPop')),
	nfactors=2,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')
# print(selfRateFA2,sort=T)

# selfRateFA4<-selfRateFA
selfRateFA<-selfRateFA2

selfRateComm<-as.data.frame(selfRateFA$communality) %>%
	mutate(item=row.names(.)) %>%
	rename(comm=`selfRateFA$communality`)
sortedSelfFA<-as.data.frame(fa.sort(loadings(selfRateFA))[,]) %>%
	mutate(item=rownames(.)) %>% select(item,PA1,PA2)
printableSortedSelfFA<-cbind(item=sortedSelfFA[,1],
	apply(sortedSelfFA[,2:3],2,function(x){
		ifelse(abs(x) < .1, ' ', sprintf('%.2f',x))
	})) %>%
	as.data.frame %>% 
	left_join(selfRateComm)

#'
#' Here's the raw output, if you like, but see below for a prettier table.
#' Notice the factors are allowed to (anti) correlate.
#'

print(selfRateFA,sort=T)

#'
#' These factors are pretty interpretable, if you ask me. 
#'
#' 1. First (PA1), is surgency/dynamism/extraversion: Popular; not depressed, lonely, or insecure; trendy, confident, popular, social. Daring and adventurous.
#' 2. Third (PA2), we have...aggression (reversed, 'cause EFA)? Rude, mean, bossy, snobby, as well as lazy and fake, not sympathetic, not trustworthy.
#'
#' Here's nice table:
#'
#+cache=F
kable(printableSortedSelfFA,align=c(rep('r',5)),digits=3,format='pandoc')

#'
#' I think this plot is supposed to show how much separation we get -- not bad actually.
#' 

plot(selfRateFA)

#'
#' These clusters of items may still be related to status (and they're
#' actually pretty close to the expected factors). We can correlate self rated accepted and controversial status 
#' with the factor scores to start exploring this.
#'

selfPopDat<-cleanDat_w[,c('SelfPop_1','SelfPop_2')]%>%mutate_each(funs(as.numeric))

#'
#' ### Correlations
#'
#' In the output below, SelfPop_1 is controversial, and SelfPop_2 is accepted.
#'
#' Also:
#'
#' 1. 'Admired' (PA1; named after its first item) 
#' 3. 'Aggressive' (PA2)
#'
#' We see that controversial status is correlated with 'Aggressive'.
#'
#' Accepted status is highly correlated with 'Admired'.
#'
selfRateDat_nopop<-select(selfRateDat,-matches('SelfPop'))

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
#' Controversial status is significantly predicted by 
#' higher 'Admired' scores (1), and higher 'Aggression' scores (2; remember the sign reversal). 
#'

bootTableCont<-cbind(c(1:2),as.numeric(rlmSelfContBoot$t0[2:3]),rbind(
	t(as.matrix(boot.ci(rlmSelfContBoot,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfContBoot,type='bca',index=3)$bca[c(4,5)]))))

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

bootTableAcc<-cbind(c(1:2),as.numeric(rlmSelfAccBoot$t0[2:3]),rbind(
	t(as.matrix(boot.ci(rlmSelfAccBoot,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfAccBoot,type='bca',index=3)$bca[c(4,5)]))))

#'
#' Accepted status is significantly predicted by higher 'Admired' scores,
#' but not by 'Aggressive' scores.
#'

kable(
	bootTableAcc,
	col.names=c('component','est','lower 95%','upper 95%'),
	digits=2,
	caption='Accepted status relation with component scores, bootstrapped CIs')

#+cache=F,echo=F,warning=F,message=F,error=F

#'
#' ### What has EFA told us?
#'
#' Well, we have four factors that we've forced into 2 that are helpfully
#'  related to self reported
#' status. This is what we want.
#'
#' ## EFA using machine learning to select items
#'
#' Now we need a principled way to select items. We could use some 
#' communality cutoff, or loading strength. But we know we want
#' 48 items, so let's start there. We can use regularized regression
#' methods to let the machine learn what 48 items (or 50 so we can drop a couple if they're a little out of balance)
#' give us the 
#' best set of factors. We can then compare that to the EFA results
#' as well as to the earlier classification.
#'

selfRateDatCompleteCov<-cov(selfRateDat_nopop,use='pairwise.complete.obs')
#+cache=T
selfRateSPCA<-lapply(
	seq(.96,1.02,.01),
	function(x){
		spca(selfRateDatCompleteCov,2,type='Gram',trace=T,para=rep(x,2))
	})
#+cache=F
(nitems<-lapply(
	selfRateSPCA,
	function(x){
		c(para=unique(x$para),
			Nitems=sum(apply(x$loadings,1,function(a) any(abs(a)>0))))
	}))
#'
#' A penalty parameter of .97 (#2) gives us the number of items we want 
#' (50 unique, leaving 2 to drop).
#'

kable(
	ifelse(
		abs(selfRateSPCA[[2]]$loadings) > 0,
		round(selfRateSPCA[[2]]$loadings,3),
		' '),
	align=rep('r',2))

# selfRateSPCA48<-lapply(
# 	seq(1.01,1.02,.002),
# 	function(x){
# 		spca(selfRateDatCompleteCov,2,type='Gram',trace=T,para=rep(x,2))
# 	})
# #+cache=F
# (nitems48<-lapply(
# 	selfRateSPCA48,
# 	function(x){
# 		c(para=unique(x$para),
# 			Nitems=sum(apply(x$loadings,1,function(a) any(abs(a)>0))))
# 	}))

# kable(
# 	cbind(ifelse(
# 		abs(selfRateSPCA48[[5]]$loadings) > 0,
# 		round(selfRateSPCA48[[5]]$loadings,3),
# 		' '),
# 	ifelse(
# 		abs(selfRateSPCA[[2]]$loadings) > 0,
# 		round(selfRateSPCA[[2]]$loadings,3),
# 		' ')),
# 	align=rep('r',2))

#'
#' The next obvious step is to look and see what a regular EFA
#' tells us about this selection of items
#'

selfRateElasticItems<-as.data.frame(selfRateSPCA[[2]]$loadings) %>%
	mutate(item=rownames(.)) %>%
	group_by(item) %>%
	summarise(all0=all(PC1==0,PC2==0)) %>%
	filter(all0==F) %>%
	select(item)

eItemDF<-select(selfRateDat,one_of(selfRateElasticItems$item))

#'
#' Whammo, 4 factors again. We'll force it into 2 again.
#'

scree(cor(eItemDF,use='pairwise.complete.obs'))

eItemFA<-fa(
	eItemDF,
	nfactors=2,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')

print(eItemFA,sort=T)

plot(eItemFA)

selfRateComm_EN<-as.data.frame(eItemFA$communality) %>%
	mutate(item=row.names(.)) %>%
	rename(comm=`eItemFA$communality`)
sortedSelfFA_EN<-as.data.frame(fa.sort(loadings(eItemFA))[,]) %>%
	mutate(item=rownames(.)) %>% select(item,PA1,PA2)
printableSortedSelfFA_EN<-cbind(item=sortedSelfFA_EN[,1],
	apply(sortedSelfFA_EN[,2:3],2,function(x){
		ifelse(abs(x) < .1, ' ', sprintf('%.2f',x))
	})) %>%
	as.data.frame %>% 
	left_join(selfRateComm_EN)

kable(printableSortedSelfFA_EN,align=c(rep('r',4)),digits=2)

#'
#' We have 24 items in 'Admired' and 26 in 'Agressive', so
#' we should drop the 2 worst 'Agressive' items: 'Studious' and 'Rule Follower'
#' 

eItemDF_2drop<-select(
	selfRateDat,
	one_of(selfRateElasticItems$item),
	-studious_Self,
	-`rule-follower_Self`)

eItemFA_2drop<-fa(
	eItemDF_2drop,
	nfactors=2,
	rotate='oblimin',
	scores='tenBerge',
	fm='pa',
	use='pairwise.complete.obs')

print(eItemFA_2drop,sort=T)

plot(eItemFA_2drop)

selfRateComm_EN_2drop<-as.data.frame(eItemFA_2drop$communality) %>%
	mutate(item=row.names(.)) %>%
	rename(comm=`eItemFA_2drop$communality`)
sortedSelfFA_EN_2drop<-as.data.frame(fa.sort(loadings(eItemFA_2drop))[,]) %>%
	mutate(item=rownames(.)) %>% select(item,PA1,PA2)
printableSortedSelfFA_EN_2drop<-cbind(item=sortedSelfFA_EN_2drop[,1],
	apply(sortedSelfFA_EN_2drop[,2:3],2,function(x){
		ifelse(abs(x) < .1, ' ', sprintf('%.2f',x))
	})) %>%
	as.data.frame %>% 
	left_join(selfRateComm_EN_2drop)

kable(printableSortedSelfFA_EN_2drop,align=c(rep('r',4)),digits=2)
#'
#' For reference, the original EFA with items that also appear above in bold.
#'

printableSortedSelfFA %>%
	mutate(
		item=ifelse(item %in% names(eItemDF_2drop),
			paste0('**',sub(' ','',item),'**'),
			item)) %>%
	kable(align=c(rep('r',5)),digits=3,format='pandoc',caption='Original EFA, retained items bolded')

#'
#' ## Pick items by X-loading and Communality
#'
#' We could also pick items based on cross loadings (minimizing) and communality (maximizing).
#'

as.data.frame(selfRateFA$loadings[,]) %>%
	mutate(
		comm=selfRateFA$communality,
		item=rownames(.)) %>%
	group_by(item) %>%
	do({
		sorted<-.[,c('PA1','PA2')][order(abs(.[,c('PA1','PA2')]))]
		data.frame(
			max=sorted[[2]],
			maxPA=names(sorted)[2],
			min=sorted[[1]],
			minPA=names(sorted)[1],
			ssXload=sum(sorted[1]^2),
			comm=.$comm[[1]])
	}) %>% ungroup %>%
	mutate(
		item=ifelse(item %in% selfRateElasticItems$item,
			paste0('**',sub(' ','',item),'**'),
			item),
		scalemax=scale(abs(max)),
		scalessX=scale(-ssXload^.5),
		scalecomm=scale(comm)) %>% 
	group_by(item) %>%
	mutate(crit=mean(c(scalemax,scalessX,scalecomm))) %>%
	select(-min,-minPA) %>%
	ungroup %>% arrange(maxPA,crit,abs(max),ssXload,comm) %>%
	kable(digits=2)

#'
#' Based on this table, we could drop items with low scores on 'crit',
#' which is just an average of Z scores for each of our criteria
#' (main factor loading, cross-loadings, and communality). This way is 
#' is more of an art than a mathematical method, so....
#'
#' I'm going to set this route aside for now, because the SPCA is made
#' for item selection, especially if we get good prediction below. (The basis
#' behind SPCA is the same as PCA/EFA, which is is to identify
#' loadings on factors that capture the maximum amount of variance in the 
#' data. In light of that, the 48 items should do what we want).
#'

#'
#' ## Regressions with new items
#'

#'
#' ### 48 items: Regressing Controversial Status
#'

eitemDF_imputed<-select(
	as.data.frame(selfRateDat_nopop_imputed),
	one_of(selfRateElasticItems$item),
	-studious_Self,
	-`rule-follower_Self`)

summary(selfPopContLM_en<-rlm(selfPopDat[,c('SelfPop_1')]~predict(eItemFA_2drop,eitemDF_imputed)))

#+cache=T,echo=F,warning=F,message=F,error=F
set.seed(3132)
rlmSelfContBoot_en<-boot(
	cbind(
		selfPopDat[,c('SelfPop_1')],
		predict(eItemFA_2drop,eitemDF_imputed)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

#'
#' Results from bootstrap estimates:
#' Controversial status is significantly predicted by 
#' and higher aggression scores (2; the sign shifted again, btw). 
#' Estimates for 'admired' are barely just include 0 in the CI (1).
#'

bootTableCont_en<-cbind(c(1:2),as.numeric(rlmSelfContBoot_en$t0[2:3]),rbind(
	t(as.matrix(boot.ci(rlmSelfContBoot_en,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfContBoot_en,type='bca',index=3)$bca[c(4,5)]))))

kable(
	bootTableCont_en,
	col.names=c('component','est','lower 95%','upper 95%'),
	digits=2,
	caption='Controversial status relation with component scores, bootstrapped CIs')

#'
#' ### 48 item: Regressing Accepted Status
#'
#'
#' Here's the raw output for an rlm of accepted status on the component scores. Again, scroll down for a nice table.
#'

summary(selfPopAccLM_en<-rlm(selfPopDat[,c('SelfPop_2')]~predict(eItemFA_2drop,eitemDF_imputed)))

#+cache=T,echo=F,warning=F,message=F,error=F
set.seed(3132)
rlmSelfAccBoot_en<-boot(
	cbind(
		selfPopDat[,c('SelfPop_2')],
		predict(eItemFA_2drop,eitemDF_imputed)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

bootTableAcc_en<-cbind(c(1:2),as.numeric(rlmSelfAccBoot_en$t0[2:3]),rbind(
	t(as.matrix(boot.ci(rlmSelfAccBoot_en,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfAccBoot_en,type='bca',index=3)$bca[c(4,5)]))))

#'
#' Accepted status is significantly predicted by higher 'Admired' 
#' (1), and not by 'Aggressive' (2).
#'

kable(
	bootTableAcc,
	col.names=c('component','est','lower 95%','upper 95%'),
	digits=2,
	caption='Accepted status relation with component scores, bootstrapped CIs')

#'
#' ### 48 items collapsed into 2 scales
#'
#' Let's see how this scale works if we just use them as a scale, taking
#' the mean across items (we've been using predicted factor scores before).
#'

#we need the items grouped into factors so we can produce scales

itemScaleGroups<-as.data.frame(eItemFA_2drop$loadings[,]) %>%
	mutate(item=rownames(.)) %>%
	group_by(item) %>%
	do({
		sorted<-.[,c('PA1','PA2')][order(abs(.[,c('PA1','PA2')]))]
		data.frame(
			max=sorted[[2]],
			maxPA=names(sorted)[2])
	})

Rscore<-function(x,min,max){
	r<-max-x+min
}

selfRateSPCAScaleScores<-cleanDat_w %>% 
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
		scale_score=mean(R_resp,na.rm=T)) %>%
	select(WorkerId,item_ex,scale_score) %>%
	spread(item_ex,scale_score) %>%
	left_join(select(cleanDat_w,WorkerId,matches('SelfPop'),matches('YSUBS'))) %>%
	mutate_each(funs(as.numeric),-WorkerId)


selfRateSPCAScaleScoresBin<-cleanDat_w %>% 
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
	mutate_each(funs(as.numeric),-WorkerId)

#'
#' See raw summary, and below that, tables of bootstrapped CIs. It's what
#' we've come to expect.
#'

summary(rlm(
	SelfPop_1~1+admired+agressive,
	data=selfRateSPCAScaleScores))
summary(rlm(
	SelfPop_2~1+admired+agressive,
	data=selfRateSPCAScaleScores))
corr.test(
	select(selfRateSPCAScaleScores,matches('SelfPop')),
	select(selfRateSPCAScaleScores,admired,agressive),
	use='pairwise.complete.obs',adjust='none',
	method='spearman')


#+cache=T
set.seed(3132)
rlmSelfContBootSPCAScale<-boot(
	as.matrix(select(selfRateSPCAScaleScores,
		SelfPop_1,
		admired,agressive)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

bootTableContSPCAScale<-cbind(as.numeric(rlmSelfContBootSPCAScale$t0[2:3]),rbind(
	t(as.matrix(boot.ci(rlmSelfContBootSPCAScale,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfContBootSPCAScale,type='bca',index=3)$bca[c(4,5)]))))
rownames(bootTableContSPCAScale)<-names(rlmSelfContBootSPCAScale$t0[2:3])
kable(
	bootTableContSPCAScale,
	col.names=c('est','lower 95%','upper 95%'),
	digits=2,
	caption='Controversial status relation with scale scores, bootstrapped CIs')

#+cache=T
set.seed(3132)
rlmSelfAccBootSPCAScale<-boot(
	as.matrix(select(selfRateSPCAScaleScores,
		SelfPop_2,
		admired,agressive)),
	boot.rlm,
	10000,
	parallel='multicore',
	ncpus=7)

bootTableAccSPCAScale<-cbind(as.numeric(rlmSelfAccBootSPCAScale$t0[2:3]),rbind(
	t(as.matrix(boot.ci(rlmSelfAccBootSPCAScale,type='bca',index=2)$bca[c(4,5)])),
	t(as.matrix(boot.ci(rlmSelfAccBootSPCAScale,type='bca',index=3)$bca[c(4,5)]))))
rownames(bootTableAccSPCAScale)<-names(rlmSelfAccBootSPCAScale$t0[2:3])
kable(
	bootTableAccSPCAScale,
	col.names=c('est','lower 95%','upper 95%'),
	digits=2,
	caption='Accepted status relation with scale scores, bootstrapped CIs')
#+cache=F

#'
#' Quick check for binarizing, which is what we'd see from scan data
#'

summary(rlm(
	SelfPop_1~1+admired+agressive,
	data=selfRateSPCAScaleScoresBin))
summary(rlm(
	SelfPop_2~1+admired+agressive,
	data=selfRateSPCAScaleScoresBin))
corr.test(
	select(selfRateSPCAScaleScoresBin,matches('SelfPop')),
	select(selfRateSPCAScaleScoresBin,admired,agressive),
	use='pairwise.complete.obs',adjust='none',
	method='spearman')

#'
#' The Aggressive scale, predicts Controversial High Status, and
#' holding it constant, the Admired scale also predicts
#' Controversial status, though less strongly. The Admired scale
#' predicts Accepted High Status.
#'

#'
#' ### A Little CFA
#'
#' Grain of salt: We don't think this should actually reflect the 
#' constructs in our data -- we know there are probably actually
#' 4 factors, but this tells us how things look if we were to really go for
#' this model. It is not the worst I've ever seen!
#'

library(lavaan)

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

library(semPlot)
#+cache=T,fig.width=10,fig.height=10
semPaths(semPlotModel(srCFA),whatLabels='std',layout='tree',rotation=4)

# modificationIndices(srCFA,sort=T,maximum.number=100)

#'
#' Estimates of reliability vary below between the CFA and 
#' the alphas on the raw scales, but I'm not sure why (probably because
#' the CFA relaibility takes into account loadings and other things).
#'

library(semTools)
reliability(srCFA)

alpha(dataCFA[,itemScaleGroupsCFA$item[itemScaleGroupsCFA$maxPA=='PA1']],check.keys=T)
alpha(dataCFA[,itemScaleGroupsCFA$item[itemScaleGroupsCFA$maxPA=='PA2']],check.keys=T)

#'
#' What conclusions can we draw? The CFA indicates that we could do better
#' in this model. But the scale alphas coefficients are very good.
#' However, there are a lot of cross loadings in the EFA, and we totally
#' ignore those, which is why the CFA fit is so shabby. This can
#' especially be the case with many items per scale.
#'

#'
#' # Next Steps
#'
#' This is pretty good to go I think, conditional on your feedback. However,
#' it will be important to check this all in the ratings of a typical
#' Controversial and Accepted High Status target. This is the plan:
#' 
#' 1. For each participant's rating of each type of High Status teen
#' we can get scores on the scales we just created, and then take
#' a mean across those ratings. 
#'     - We would expect to see that people think
#' that a typical Controversial High Status target is high on the Aggressive
#' scale, and maybe somewhat high on the Admired scale. 
#'     - We'd also expect that Accepted High status targets are rated
#' high on the 'Admired' scale.
#'
#' 2. For each scale (and even each item), I want to know how much agreement
#' there is among raters on how well it describes these two types of targets.
#'
#' 3. Finally, we have data on depression and substance use, but both
#' sets of items need to be modeled as coming from a different-than-normal
#' distribution (substance use especially). 
#'
#' Here's a preview of status-type analysis in the meantime. I'm less certain
#' of this approach in general, and very happy for any guidance.
#'

#'
#' # Status-type ratings  
#'
#' Now we can use the data about what people think is characteristic of the two status types to figure out what best classifies the two types. Hopefully we'll get good overlap with the above items.
#'
#' First, let's just see how a CFA fits with our model from Self Report
#' data:
#'

statTypeDat<-select(cleanDat_w,matches('_Acc'),matches('_Cont')) %>% 
	mutate_each(funs(as.numeric)) %>%
	mutate(index=1:n()) %>%
	gather(key,value,-index) %>%
	separate(key,c('adj','status'),sep='_') %>%
	spread(adj,value) %>%
	mutate(accepted_status=as.numeric(status=='Acc'))

names(statTypeDat)<-sub(' ','',names(statTypeDat))

cat(statTypeCFAmodel<-paste(
	tapply(itemScaleGroupsCFA$item,itemScaleGroupsCFA$maxPA,function(x){
		paste0(sub('_Self','_FAC',x[1]),'=~',
			paste0(sub('_Self','',x),collapse='+'))
	}) %>% paste0(collapse='\n'),'\n'))

statTypeCFA<-cfa(statTypeCFAmodel,data=statTypeDat)

summary(statTypeCFA,fit.measures=T,standardized=T)

library(semPlot)
#+cache=T,fig.width=10,fig.height=10
semPaths(semPlotModel(srCFA),whatLabels='std',layout='tree',rotation=4)
#+cache=F

statTypeCor<-select(statTypeDat,-index,-status,-accepted_status,-blank) %>%
	cor(use='pairwise.complete.obs')
scree(statTypeCor)
statTypeFA<-fa(statTypeCor,nfactors=2,n.obs=100,rotate='oblimin',scores='tenBerge',fm='pa')

sortedStatTypeFA<-fa.sort(loadings(statTypeFA))[,]
printableSortedStatTypeFA<-apply(sortedStatTypeFA,2,function(x){
	ifelse(abs(x) < .1, ' ', sprintf('%.2f',x))
})

#'
#' The EFA below tells a slightly different story than above, but there 
#' are a lot of cross loadings, and I don't know whether I should trust 
#' this method entirely
#' on this type of data. This is a really well fitting EFA, though.
#'

print(statTypeFA,sort=T)

kable(printableSortedStatTypeFA,align=rep('r',4))

#'
#' Here's the same sparse PCA method from before (will it match?!). 
#' Again, to be taken with a grain of salt.
#'

statTypeCov<-select(statTypeDat,-index,-status,-accepted_status,-blank) %>%
	cov(use='pairwise.complete.obs')
#+cache=F
statTypeSPCA<-lapply(
	seq(1,1.08,.02),
	function(x){
		spca(statTypeCov,2,type='Gram',trace=T,para=rep(x,2))
	})
#+cache=F
(nitems<-lapply(
	statTypeSPCA,
	function(x){
		c(para=unique(x$para),
			Nitems=sum(apply(x$loadings,1,function(a) any(abs(a)>0))))
	}))
#A penalty parameter of 1.06 (#4) gives us the number of items we want (48 unique)

kable(
	ifelse(
		abs(statTypeSPCA[[4]]$loadings) > 0,
		round(statTypeSPCA[[4]]$loadings,3),
		' '),
	align=rep('r',4))

#'
#' Another approach: Using glmnet, we can get a good set of cross-validated classifying features -- in our case, adjectives.
#'

registerDoMC(cores=7)

x.miss<-as.matrix(select(statTypeDat,-index,-status,-accepted_status,-blank))
y<-as.factor(statTypeDat$status)
x<-apply(x.miss,2,function(a){
	a_bar<-median(a,na.rm=T)
	a[is.na(a)]<-a_bar
	a
})

#+cache=T
statusClassifier.cv<-cv.glmnet(
	x,y,
	nfolds=dim(x)[1],
	parallel=T,
	family='binomial',
	type.measure='deviance')
#+cache=F
plot(statusClassifier.cv)

#'
#' These are the items that give have the most information for classification
#' of the type of status. These should be the items that show the biggest 
#' differences between accepted and controversial ratings. Hopefully, they
#' all show up in our factor analyses above, too.
#'
#' This table summarizes the GLMnet, machine learning, predictive items
#' across self ratings and type ratings. 
#'

data.frame(
	xName=selfStatCoef$SelfPop_1@Dimnames[[1]],
	SelfContPred=as.numeric(as.matrix(selfStatCoef$SelfPop_1)),
	SelfAccPred=as.numeric(as.matrix(selfStatCoef$SelfPop_2)),
	TypeDistinguish=as.numeric(coef(statusClassifier.cv,s='lambda.min'))) %>%
	mutate(
		SelfRepDistinguish=abs(SelfContPred) > 0 & abs(SelfAccPred) > 0 & sign(SelfContPred) != sign(SelfAccPred),
		BothSelfAndType=abs(SelfContPred) > 0 & abs(SelfAccPred) > 0 & abs(TypeDistinguish) > 0) %>%
	mutate_each(funs(ifelse(.,TRUE,'')),SelfRepDistinguish,BothSelfAndType) %>%
	kable(digits=2,align=c('l','r','r','r','r','l'))

