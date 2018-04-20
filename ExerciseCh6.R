
library(foreign)
library(lavaan)

edps744 <- read.spss("/Users/Matthew/Documents/Classes/SP18/Second Book/Finch and French latent variable modeling with r webiste materials/Data for book website/edps744.sav",to.data.frame = T)
attach(edps744)



# Example 1
install.packages("systemfit")
library(systemfit)
eqn1<-ats50~ags1+mps1
eqn2<-ags1~ats50
eqSystem<-list(attc=eqn1, ags=eqn2)
attc.2sls<-systemfit(eqSystem, method="2SLS", inst=list(~ags2+mps6, 
                                                        ~ats29))
summary(attc.2sls)




# Example 2
detach(edps744)
edps744.nomiss <- na.omit(edps744)
attach(edps744.nomiss)
ags1_c<-ags1-mean(ags1)
ags2_c<-ags2-mean(ags2)
ags5_c<-ags5-mean(ags5)
ags6_c<-ags6-mean(ags6)
ags7_c<-ags7-mean(ags7)
ags12_c<-ags12-mean(ags12)
ats6_c<-ats6-mean(ats6)
ats17_c<-ats17-mean(ats17)
ats31_c<-ats31-mean(ats31)
ats38_c<-ats38-mean(ats38)
ats48_c<-ats48-mean(ats48)
ats58_c<-ats58-mean(ats58)
int1<-ags1_c*ats6_c
int2<-ags2_c*ats17_c
int3<-ags5_c*ats31_c
int4<-ags6_c*ats38_c
int5<-ags7_c*ats48_c
int6<-ags12_c*ats58_c
edps744.int<-data.frame(edps744.nomiss,ats6_c,ats17_c,ats31_c,ats38_c,ats48_c,ats58_c,ags1_c,ags2_c,ags5_c,ags6_c,ags7_c,ags12_c,int1,int2,int3,int4,int5
                        ,int6)
attc.model.interaction<-'
mastery=~ags1_c+ags2_c+ags5_c+ags6_c+ags7_c+ags12_c
frustration=~ats6_c+ats17_c+ats31_c+ats38_c+ats48_c+ats58_c
int=~int1+int2+int3+int4+int5+int6
attc=~ats50+ats29+ats40+ats35

attc~a*mastery+b*frustration+c*int
'

attc.model.interaction.fit<-sem(attc.model.interaction, data=edps744.int, estimator="WLSMV")
summary(attc.model.interaction.fit, fit.measures=T, standardized=T)

detach(edps744.nomiss)

# Example 3
# source('http://openmx.psyc.virginia.edu/getOpenMx.R')
# source('http://www.brandmaier.de/semtree/getsemtree.R')
install.packages(c("OpenMx","semtree"))
library(OpenMx)
library(semtree)
attach(edps744)
tree_data<-data.frame(ags1, ags2, ags5, ags6, ags7, ags12, mps_self)
manifests<-c("ags1", "ags2", "ags5", "ags6", "ags7", "ags12")
latents <- c("mastery")
detach(edps744)
attach(tree_data)
masteryModel <- mxModel(name="Test One Factor",
                        type="RAM",
                        manifestVars = manifests,
                        latentVars = latents,
                        mxPath(from=latents, to=manifests),
                        mxPath(from=manifests, arrows=2),
                        mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
                        mxPath(from="one",
                               to=manifests,
                               arrows=1,
                               free=TRUE,
                               values=1,
                        ),
                        mxData(observed=tree_data, type="raw")
)
mastery.tree<-semtree(masteryModel)
plot(mastery.tree)
parameters(mastery.tree)
se(mastery.tree)
mastery.tree.pruned<-prune(mastery.tree, max.depth=2)
plot(mastery.tree.pruned)

detach(tree_data)


