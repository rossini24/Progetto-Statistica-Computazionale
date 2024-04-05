#script HOMEWORK 2 Rossini Luca Valli Giacomo 

library(tidyverse)
library(mclust)
library(ggcorrplot)
library(caret)
library(Rmixmod)
library(heatmaply)

data <- read_csv("C:/Users/Utente/Desktop/LUCA/università/TERZO_ANNO/STATISTICA COMPUTAZIONALE/HOMEWORK_2/spotify/data.csv") #percorso personale per importare il file csv. file csv acilmente reperibile su kaggle
dim(data)
View(data) #dataset contenente 30 variabili e 1000 osservazioni 
#la prima variabile fa riferimento al nome del file mentre l'ultima è l'etichetta (il genere musicale)
table(data[,ncol(data)])  #10 generi distinti

#li salvo in tibble per lavorare con il tidyverse
dati_musica= as_tibble(data[,-c(1,30)])
labels_musica= as_tibble(data[,30])

#################################################
#ANALISI ESPLORATIVA 
#################################################

#matrice di correlazione
correlazioni= cor( dati_musica)
heatmaply_cor(correlazioni)
ggcorrplot(correlazioni, type = c('lower'), show.diag = T, title = 'matrice di corelazione', ggtheme = ggplot2::theme_light())


#zoom della matrice di corelazione su delle specifiche variabili 
correlazioni_zoom= cor(dati_musica[,1:8])
ggcorrplot(correlazioni_zoom,type = c('lower'), show.diag = T, show.legend = F, lab= T )

#analisi delle componenti principali 

pca_musica= princomp(dati_musica, cor = T)
summary(pca_musica)
features = c(names(dati_musica)[apply(pca_musica$loadings[,1:7], 2, function(x) which(x**2==max(x**2)))])  #queset sono le 6 variabili che spiegano il  99,98% della varianza 
#si salvano in features le 7 variabili che spiegano più dell' 80% della varianza, calcolate tramite il princomp 

musica_dati_pca= dati_musica[,features]
str(musica_dati_pca)

#variabili che utiliziamo :
#[1] "mfcc8"              "rolloff"            "mfcc16"             "tempo"              "zero_crossing_rate"
#[6] "mfcc3"              "mfcc19"             "label"  

#produciamo un'analisi delle variabili in boxplot
#consideriamo soltanto le variabili ricavate dalla pca
#boxplot delle varaibili in base al genere 

#1 rolloff
ggplot( data =  musica_dati_pca, aes(y=rolloff, fill=label))+
  geom_boxplot()+
  labs(title = "ROLLOFF", y= ' ') +
  theme(plot.title = element_text(hjust = .5))

#2 mfcc8
ggplot( data =  musica_dati_pca, aes(y=mfcc8, fill=label))+
  geom_boxplot()+
  labs(title = "MFCC8", y= ' ') +
  theme(plot.title = element_text(hjust = .5))

#3 mfcc16
ggplot( data =  musica_dati_pca, aes(y=mfcc16, fill=label))+
  geom_boxplot()+
  labs(title = "MFCC16", y= ' ') +
  theme(plot.title = element_text(hjust = .5))

#4 tempo
ggplot( data =  musica_dati_pca, aes(y=tempo, fill=label))+
  geom_boxplot()+
  labs(title = "TEMPO", y= ' ') +
  theme(plot.title = element_text(hjust = .5))

#5 zero_crossing_rate
ggplot( data =  musica_dati_pca, aes(y=zero_crossing_rate, fill=label))+
  geom_boxplot()+
  labs(title = "ZERO CROSING RATE", y= ' ') +
  theme(plot.title = element_text(hjust = .5))

#6 mfcc3
ggplot( data =  musica_dati_pca, aes(y=mfcc3, fill=label))+
  geom_boxplot()+
  labs(title = "MFCC3", y= ' ') +
  theme(plot.title = element_text(hjust = .5))

#7 mfcc19
ggplot( data =  musica_dati_pca, aes(y=mfcc19, fill=label))+
  geom_boxplot()+
  labs(title = "MFCC19", y= ' ') +
  theme(plot.title = element_text(hjust = .5))

#restringiamo il dataset considerando solo le osservazioni provenienti dal genere metal classico e pop 
#sono tre generi ben distinti in modo da facilitare l'algoritmo nella classificazione che faremo in seguito 

m_c_p =musica_dati_pca %>% #tibble contenente solo i 3 generi elencati in precedenza, 7 variabili (selezionate tramite la princomp) e l'ottava è l'etichetta osservata 
  filter( label == 'classical' | label == 'pop' | label== 'metal' )

#analisi delle distribuzioni dei generi 

#densità sulla variabile rolloff
ggplot(data= m_c_p, aes(x= rolloff, y=after_stat(density), fill= label))+ 
  geom_density(alpha= 0.6 , position = 'identity')

#densità sulla variabile zero_crossing_rate
ggplot(data= m_c_p, aes(x= zero_crossing_rate, y=after_stat(density), fill= label))+ 
  geom_density(alpha= 0.6 , position = 'identity')

###########################################
#MODEL BASED CLUSTERING
###########################################

mclustICL(m_c_p[,-8], G= 3) #è stato selezionato tramite ICL il modello EEV
a = Mclust(m_c_p[,-8], G=3, modelNames= 'EEV')
a$classification #il vettore di etichette che è stato generato 

classError(a$classification, m_c_p$label) #5% di errore di classificazione 

#produciamo un grafico

#il vettore highlight_indices contiene tutte le osservazioni classificate errate 
highlight_indices <- c( 1,21,57,73,123,140,187,207,210,225,236,251,253,261,296,298)

# Grafico a dispersione con evidenziazione di osservazioni specifiche
ggplot(p_m_c_new, aes(x = rolloff, y = mfcc8, color = label)) +
  geom_point()  +
  geom_point(data = m_c_p[m_c_p$label == "classical" & rownames(m_c_p) %in% highlight_indices, ], color = "lightcoral", size = 4, shape = 'square') +
  geom_point(data = m_c_p[m_c_p$label == "pop" & rownames(m_c_p) %in% highlight_indices, ], color = "lightblue", size = 4, shape = 'square')+
  geom_point(data = m_c_p[m_c_p$label == "metal" & rownames(m_c_p) %in% highlight_indices, ], color = "lightgreen", size = 4, shape = 'square')
#Entropia

EN= -sum( rowSums(a$z*log(a$z)))
#entropia relativa dividendo per n*log(k) k=3 e n= 300
EN/300*log(3)  
#confusion matrix
mc_class= a$classification
mc_class <- as.factor(mc_class)
levels(mc_class) <- c('metal','classical','pop')
confusionMatrix(mc_class, as.factor(m_c_p$label))

#distanza di kullback leibner tra i gruppi 

#1= METAL
#2= CLASSICA
#3= POP

mu1= as.matrix(a$parameters$mean[,1])
mu2= as.matrix(a$parameters$mean[,2])
mu3= as.matrix(a$parameters$mean[,3])

sigma_1= a$parameters$variance$sigma[,,1]
sigma_2= a$parameters$variance$sigma[,,2]
sigma_3= a$parameters$variance$sigma[,,3]

KL_S = function(mu1, mu2, sigma_1, sigma_2)
{
  0.5*t(mu1-mu2)%*%(solve(sigma_1) + solve(sigma_2))%*%(mu1-mu2)+ 0.5*sum(diag(sigma_1%*%solve(sigma_2) + solve(sigma_1)%*%sigma_2)) - length(mu1)[1]
}

drop( KL_S(mu1,mu2,sigma_1, sigma_2)) #distanza tra metal e classico
drop( KL_S(mu2,mu3,sigma_2, sigma_3)) #distanza tra classico e pop
drop( KL_S(mu1,mu3,sigma_1, sigma_3)) #distanza tra metal e pop


#########################################
#MODEL BASED CLASSIFICATION
#########################################

#EDDA
#MODEL CHOICE 

musica= as.data.frame(m_c_p)
musica_dati= musica[,-8]
musica_labels= unlist( musica[8])

n= nrow(musica_dati) #lunghezza del campione 
perm= sample(n) #estraggo ogni indice di posizione in maniera causale 

osservazioni= perm[241:n]  #in osservazioni salvo i 60 (20% i 300) indici di posizione che vorrò classificare 


cv_bic_model= as.matrix(cbind(rep(NA,20), rep(NA,20), rep(NA,20)), nrow= 20, ncol=3)
colnames(cv_bic_model)= c('cv', 'BIC', 'model name')  #per confrontare 20 best result

for ( v in 1: dim(cv_model)[1])
{
  pr_music= mixmodLearn(musica_dati[-osservazioni,], as.factor(musica_labels[-osservazioni]), models=mixmodGaussianModel(family='all') , criterion=c('CV', 'BIC'))
  cv_bic_model[v,1]=round(pr_music@bestResult@criterionValue[1],3)
  cv_bic_model[v,2]=round(pr_music@bestResult@criterionValue[2],3)
  cv_bic_model[v,3]=pr_music@bestResult@model
  
}
cv_bic_model

#si nota che il modello migliore scelto con cross validation (v=10 come da default) è il modello "Gaussian_p_L_Ck"

#CLASSIFICHIAMO LE 20 OSSEVAZIONI ESTRATTE SULLA BASE DEL TRAINING SET 

pr_music['bestResult']
PREDICTION<- mixmodPredict(data=musica_dati[osservazioni,], classificationRule=pr_music["bestResult"])

PREDICTION@partition #il vettore contenente le etichette 

classError(PREDICTION@partition, musica_labels[osservazioni]) #un 'accuracy del 95%

#stampiamo la confusion matrix 
prediction = as.factor(PREDICTION['partition'])
levels(prediction)= c('classical', 'metal', 'pop')

confusionMatrix(data = prediction, reference = as.factor(musica_labels[osservazioni]))

#MDA

#dalla model choice svolta in precedenza abbiamo scelto il modello Gaussian_pk_L_Ck corrispondete ad EVV

#procediamo con la MDA utilizzando la funzione MclustDA
#quest'ultima funzione ci serve per capire quante misture e quindi quanti ulteriori gruppi 
#ci sono all'interno dei generi lavorando sempre con il modello EVV

#il problema di questa funzione è che non implementa cross validation lo facciamo noi manualmente 

G= 10
V= 5 #10 #15 #20 #si può provare con diversi valori del V_fold
B= round(n/V)
perm = sample(n)
err = matrix(NA ,G,V) #creiamo una matrice nulla dove poi andremo a salvare i valori di missclassificazione 

for (g in 1:G)
{
  for(v in 1:V)
  {
    validation_set= perm[(B*(v-1)+1):(B*v)]
    mod= MclustDA(training_set[,-8], training_set$label, G=g, modelNames = 'EVV')
    err[g,v]= sum(predict(mod, test_set[,-8])$class != test_set$label) /B
  }
}
err
which.min(round(rowMeans(err),4))  #3 gruppi all'interno di ogni gruppo principale 

#stampiamo un grafico relativo alla scelta dei sottogruppi 

#grafico

cer <- as.data.frame(cbind(1:G,rowMeans(err)))
colnames(cer) <- c('G','Errore')
ggplot(cer)+
  geom_line(aes(x=G,y=Errore),linewidth=1,col='lightblue')+
  geom_point(aes(x=G,y=Errore),size=5,col='grey')+
  geom_vline(xintercept = 3,linetype=2,col='black')+
  scale_y_continuous(n.breaks = 10)+
  scale_x_continuous(n.breaks = 10)+
  theme_classic()+
  theme(title = element_text(size=12, face='bold'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),    
        axis.text.x = element_text(color="black"),
        axis.ticks = element_line(color = "black"))


mod= MclustDA(training_set[,-8], training_set$label, G=3, modelNames = 'EVV')
pred= predict(mod, test_set[,-8])

 sum(predict(mod, test_set[,-8])$class != test_set$label) /nrow(test_set) #0.0333


