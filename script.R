# import the dataset
dataset = read.csv("./Data/dataset.csv")

# modify the type of column otherwise the ifelse returns index position instead of value
dataset$Scuola_provenienza = as.character(dataset$Scuola_provenienza)

dataset$Esame_Matematica = as.character(dataset$Esame_Matematica)

# change scuola_provenienza to match the readme
dataset$Scuola_provenienza = with(dataset,
                                  ifelse(
                                    Scuola_provenienza %in% c('AL', 'IA', 'IPC', 'LL', 'XX'),
                                    'Altro',
                                    Scuola_provenienza
                                  ))

# remove the rows with empty scuola_provenienza
dataset = with(dataset, dataset[!(is.na(Scuola_provenienza) |
                                    Scuola_provenienza == ""),])

# change empty esame_matematica rows to show that the exam was not taken
dataset$Esame_Matematica = with(dataset,
                                ifelse(Esame_Matematica %in% (''), 'Non superato', Esame_Matematica))

dataset$Esame_Matematica = with(dataset,
                                ifelse(Esame_Matematica %in% ('MATEMATICA I'), 'EsameMatematica', Esame_Matematica))


dataset$Voto_Matematica = with(dataset, ifelse(Esame_Matematica %in% ('Non superato'), 0, Voto_Matematica))


# first try: remove the crediti_matematica column and see if it has an impact
dataset$Crediti_Matematica = NULL

# rescale the data
rescale_to_01 <- function(dataset, anno) {
  subset_data = subset(dataset, dataset$Coorte == anno)
  subset_data$Voto_test = scales::rescale(subset_data$Voto_test, to = c(0, 1))
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

# export the data to csv
write.csv(dataset,
          "./Data/dataset_preprocessed.csv",
          row.names = FALSE)