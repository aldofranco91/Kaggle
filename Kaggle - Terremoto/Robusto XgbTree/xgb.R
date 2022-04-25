ree

Modelo

Test = read.csv(file = "RobustTesting.csv", header = FALSE)

Forecast = predict(object = Modelo, newdata = Test)

# send_telegram_message("Modelo acabado!!!")

## Ver aqui tema de los nombre

Submission = read.csv(file = "sample_submission.csv", header = TRUE)

Submission$time_to_failure = Forecast

write.csv(Submission, file = paste("Submission",Modelo$method,".csv"),row.names=FALSE)

save.image(file = paste(Modelo$method,today(),"_.RData"))


