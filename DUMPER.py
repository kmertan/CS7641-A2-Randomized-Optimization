# -*- coding: utf-8 -*-
"""
Created on Fri Jan 20 14:23:40 2017

@author: JTay
"""

import numpy as np

import sklearn.model_selection as ms
import pandas as pd
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import SelectFromModel

cancer = pd.read_csv('./data/breast_cancer.csv')
cancer = pd.get_dummies(cancer, columns = ['class'], prefix = 'class')
cancer['class'] = cancer['class_2.0']
cancer.drop(['class_2.0', 'class_4.0'], axis = 1, inplace = True)
cancer = cancer.astype(np.int64)

cancerX = cancer.drop('class',1).copy().values
cancerY = cancer['class'].copy().values


cancer_trgX, cancer_tstX, cancer_trgY, cancer_tstY = ms.train_test_split(cancerX, cancerY, test_size=0.3, random_state=0,stratify=cancerY)

pipe = Pipeline([('Scale',StandardScaler())])
trgX = pipe.fit_transform(cancer_trgX,cancer_trgY)
trgY = np.atleast_2d(cancer_trgY).T
tstX = pipe.transform(cancer_tstX)
tstY = np.atleast_2d(cancer_tstY).T
trgX, valX, trgY, valY = ms.train_test_split(trgX, trgY, test_size=0.2, random_state=1,stratify=trgY)     
tst = pd.DataFrame(np.hstack((tstX,tstY)))
trg = pd.DataFrame(np.hstack((trgX,trgY)))
val = pd.DataFrame(np.hstack((valX,valY)))
tst.to_csv('data/c_test.csv',index=False,header=False)
trg.to_csv('data/c_trg.csv',index=False,header=False)
val.to_csv('data/c_val.csv',index=False,header=False)
