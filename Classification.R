require(ROCR)

#data_train - training data - Required
#data_test - testing data - Required

data_train_x = data_train %>% select(1:6)
data_train_y = data_train %>% select(7)
data_test_x = data_test %>% select(1:6)
data_test_y = data_test %>% select(7)

svm.model = svm(as.factor(class) ~ ., data = data_train, type = 'C-classification', kernel = 'polynomial', probability = TRUE)
svm.pred = predict(svm.model, newdata = data_test_x)
svm.cm = table(data_test_y$class, svm.pred)

nvb.model = naiveBayes(as.factor(class) ~ ., data = data_train)
nvb.pred = predict(nvb.model, newdata = data_test_x)
nvb.cm = table(data_test_y$class, nvb.pred)

ct.model = ctree(as.factor(class) ~ ., data = data_train)
ct.pred = predict(ct.model, newdata = data_test_x)
ct.cm = table(data_test_y$class, ct.pred)

print(tibble(svm = f_value(svm.cm), nvb = f_value(nvb.cm), ct = f_value(ct.cm)))

svm.prob = predict(svm.model, type="prob", newdata= data_test_x, probability = TRUE)
svm.rocr = prediction(attr(svm.prob, "probabilities")[,2], data_test_y$class)
svm.perf = performance(svm.rocr, "tpr","fpr")
nvb.prob = predict(nvb.model, newdata=data_test_x, type="raw")
nvb.rocr = prediction(nvb.prob[,2], data_test_y$class)
nvb.perf = performance(nvb.rocr, "tpr","fpr")
ct.prob = predict(ct.model, newdata=data_test_x, type="prob")
ct.rocr = prediction(ct.prob[,2], data_test_y$class)
ct.perf = performance(ct.rocr, "tpr","fpr")
plot(svm.perf, col=4, main="ROC curves of different machine learning classifier")
legend(0.6, 0.6, c('svm', 'ctree', 'Naive Bayes'), 4:6)
plot(ct.perf, col=5, add=TRUE)
plot(nvb.perf, col=6, add=TRUE)
abline(0,1,col="#000000", lty=2)
