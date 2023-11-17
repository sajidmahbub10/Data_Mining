View(Data)


library(tidyverse)
fraud <- Data %>% filter(Data$isFraud == 1)

View(fraud)

notFraud <- Data %>% filter(Data$isFraud !=1 )

View(notFraud)

fraud <- fraud[1:3000,]
notFraud <- notFraud[1:3000,]

newData <- rbind(fraud,notFraud)

View(newData)

write.csv(train_data, file = "D:/11th Semester/traindata.csv", row.names = FALSE)

colnames(newData)
