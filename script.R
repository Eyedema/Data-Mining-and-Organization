# import the dataset
dataset = read.csv("./Data/dataset.csv")

# modify the type of column otherwise the ifelse returns index position instead of value
dataset$Scuola_provenienza = as.character(dataset$Scuola_provenienza)
dataset$Coorte = as.character(dataset$Coorte)
dataset$Esame_Matematica = as.character(dataset$Esame_Matematica)

# change scuola_provenienza to match the readme
dataset$Scuola_provenienza = with(dataset,
                                  ifelse(
                                    Scuola_provenienza %in% c('AL', 'IA', 'IPC', 'LL', 'XX', ''),
                                    'Altro',
                                    Scuola_provenienza
                                  ))


# change empty esame_matematica rows to show that the exam was not taken and rows where the mark was zero with the mean of other marks
dataset$Esame_Matematica = with(dataset,
                                ifelse(Esame_Matematica %in% (''), 'Non superato', Esame_Matematica))

dataset$Esame_Matematica = with(dataset,
                                ifelse(Esame_Matematica %in% ('MATEMATICA I'), 'EsameMatematica', Esame_Matematica))


dataset$Voto_Matematica = with(dataset, ifelse(Voto_Matematica %in% (0), Voto_medio, Voto_Matematica))
dataset$Voto_Matematica = with(dataset, ifelse(Esame_Matematica %in% ('Non superato'), 0, Voto_Matematica))


# first try: remove the crediti_matematica column and see if it has an impact
dataset$Crediti_Matematica = NULL

# rescale the data with the z-score normalization: v' = (v-mean)/std_dev
rescale_to_01 <- function(dataset, anno) {
  subset_data = subset(dataset, dataset$Coorte == anno)
  subset_data$Voto_test = scale(subset_data$Voto_test)
  return(subset_data)
}

subset2010 = rescale_to_01(dataset, 2010)
subset2011 = rescale_to_01(dataset, 2011)
subset2012 = rescale_to_01(dataset, 2012)
subset2013 = rescale_to_01(dataset, 2013)
subset2014 = rescale_to_01(dataset, 2014)
subset2015 = rescale_to_01(dataset, 2015)
subset2016 = rescale_to_01(dataset, 2016)
dataset = plyr::rbind.fill(subset2010,
                           subset2011,
                           subset2012,
                           subset2013,
                           subset2014,
                           subset2015,
                           subset2016)
dataset <- na.omit(dataset)
dataset[c(5, 6, 7, 10)] <- lapply(dataset[c(5, 6, 7, 10)], function(x) c(scale(x)))
# export the data to csv
# on weka i'm going to ignore cds, coorte, genere, crediti_totale, scuola_provenienza; i'll use these classes (columns) to do some more analyses to see how
# the cluster i found correlate to the actual classes that k-means or the algorithm i used created
write.csv(dataset,
          "./Data/dataset_preprocessed.csv",
          row.names = FALSE)