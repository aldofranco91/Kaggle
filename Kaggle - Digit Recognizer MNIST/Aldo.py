%reset -f

import pandas as pd
import numpy as np

from sklearn import svm, metrics
from sklearn.cross_validation import cross_val_score

Train = pd.read_csv("train.csv")
Test = pd.read_csv("test.csv")

X_train = Train.values[:, 1:].astype(float)
Y_train = Train.values[:, 0]

Modelo = svm.SVC()

clf = svm.SVC(kernel="rbf")

cross_val_score(Modelo,X_train, Y_train, scoring='accuracy', cv=5)
