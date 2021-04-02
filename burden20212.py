# -*- coding: utf-8 -*-
"""
Created on Fri Feb  5 20:12:49 2021

@author: admin
"""

from sklearn.ensemble import RandomForestRegressor
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import KFold
from sklearn.model_selection import ShuffleSplit
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.base import clone
from itertools import combinations
from keras import Sequential
from keras.layers.core import Dense,Activation,Dropout
from keras import optimizers
from tqdm import tqdm,trange
import random

def Set_network(n_hide,n_input):
    global model
    model=Sequential()
    model.add(Dense(input_dim=n_input,units=n_hide,kernel_initializer='normal',activation='relu'))
    #model.add(PReLU(alpha_initializer='zeros', alpha_regularizer=None, alpha_constraint=None, shared_axes=None))
    #model.add(LeakyReLU(alpha=1))
    model.add(Dropout(0.2))
    #model.add(Dense(input_dim=n_hide,output_dim=n_hide,activation='relu',kernel_initializer='normal'))
    #model.add(Dense(input_dim=n_hide,output_dim=n_hide,kernel_initializer='normal'))
    #model.add(PReLU(alpha_initializer='zeros', alpha_regularizer=None, alpha_constraint=None, shared_axes=None))
    #model.add(Dropout(0.2))
    model.add(Dense(input_dim=n_hide,units=1,kernel_initializer='normal'))
    #model.add(PReLU(alpha_initializer'zeros', alpha_regularizer=None, alpha_constraint=None, shared_axes=None))
    #model.add(LeakyReLU(alpha=1))

def Network_train(opt,Setlr,dlcs,sjsl,nepochs):
    global train,score
    Adam=optimizers.Adam(lr=Setlr, beta_1=0.9, beta_2=0.999, epsilon=1e-08, decay=sjsl, amsgrad=True)
    sgd=optimizers.SGD(lr=Setlr, momentum=dlcs, decay=sjsl, nesterov=False)
    Adagrad=optimizers.Adagrad(lr=Setlr, epsilon=1e-06)
    model.compile(loss='mean_squared_error', optimizer=opt,metrics=['mae'])
    #train=model.fit(x_data.iloc[train_index,:],y_data.iloc[train_index,:],
                    #validation_data=(x_val,y_val),epochs=nepochs,batch_size=16)
    train=model.fit(x_train,y_train,validation_split=0.11,epochs=nepochs,batch_size=16,verbose=0)
    score=model.evaluate(x_test,y_test,batch_size=16)

def rmse(obs,pre):
    return np.sqrt(mean_squared_error(obs, pre))

def caculate_cor():
    global r_test,r_train,y_test_pred,y_train_pred,rmse_test,rmse_train
    y_test_pred=pd.DataFrame(model.predict(x_test).reshape(y_test.shape),index=test_index)
    r_test=np.corrcoef(y_test_pred[0],y_test[colnames[0]])
    y_train_pred=pd.DataFrame(model.predict(x_train).reshape(y_train.shape),index=train_index)
    r_train=np.corrcoef(y_train_pred[0],y_train[colnames[0]])
    rmse_test=rmse(y_test[colnames[0]],y_test_pred[0])
    rmse_train=rmse(y_train[colnames[0]],y_train_pred[0])

class SBS():
    def __init__(self, estimator, k_features, scoring,
                 test_size=0.25, random_state=1):
        self.scoring = scoring
        self.estimator = clone(estimator)
        self.k_features = k_features
        self.test_size = test_size
        self.random_state = random_state

    def fit(self, X, y):
        
        X_train, X_test, y_train, y_test =             train_test_split(X, y, test_size=self.test_size,
                             random_state=self.random_state)

        dim = X_train.shape[1]
        self.indices_ = tuple(range(dim))
        self.subsets_ = [self.indices_]
        score = self._calc_score(X_train, y_train, 
                                 X_test, y_test, self.indices_)
        self.scores_ = [score]

        while dim > self.k_features:
            scores = []
            subsets = []

            for p in combinations(self.indices_, r=dim - 1):
                score = self._calc_score(X_train, y_train, 
                                         X_test, y_test, p)
                scores.append(score)
                subsets.append(p)

            best = np.argmax(scores)
            self.indices_ = subsets[best]
            self.subsets_.append(self.indices_)
            dim -= 1

            self.scores_.append(scores[best])
        self.k_score_ = self.scores_[-1]

        return self

    def transform(self, X):
        return X[:, self.indices_]

    def _calc_score(self, X_train, y_train, X_test, y_test, indices):
        self.estimator.fit(X_train[:, indices], y_train)
        y_pred = self.estimator.predict(X_test[:, indices])
        score = self.scoring(y_test, y_pred)
        return score
    


    
    
    

'''
ANN
'''    
 
writer1=pd.ExcelWriter('ann-op.xlsx')
writer2=pd.ExcelWriter('ann-cor.xlsx')
for i in trange(3):
    frame=pd.read_excel('clearance2021.xlsx',sheet_name=i+2)
    x_data=frame.iloc[:,1:30]
    y_data=frame.iloc[:,30:]
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()


    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    stdsc=StandardScaler()
    x_data=pd.DataFrame(stdsc.fit_transform(x_data))
    x_data.columns=x_names
    
    prelist=[]
    corlist_train=[]
    corlist_test=[]
    rmsel_train=[]
    rmsel_test=[]
    o=[]
    
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            global x_train,y_train,x_test,y_test
            x_train=x_data.iloc[train_index,:]
            #x_train=pd.DataFrame(stdsc.fit_transform(x_train))
            #x_train.columns=x_names
            
            y_train=y_data.iloc[train_index,:]
            
            x_test=x_data.iloc[test_index,:]
            #x_test=pd.DataFrame(stdsc.fit_transform(x_test))
            #x_test.columns=x_names
        
            y_test=y_data.iloc[test_index,:]
            
            Set_network(58,29)
            Network_train('Adam',0.1,0.9,0,200)
            caculate_cor()
            corlist_train.append(r_train[1,0])
            corlist_test.append(r_test[1,0])
            rmsel_train.append(rmse_train)
            rmsel_test.append(rmse_test)
            o.append(y_train[colnames[0]])
            o.append(y_train_pred[0])
            o.append(y_test[colnames[0]])
            o.append(y_test_pred[0])
            pbar.update(1)

    cordf=pd.DataFrame({'train':corlist_train,'test':corlist_test,
                        'rmse_train':rmsel_train,'rmse_test':rmsel_test})
    obs_pre_df=pd.DataFrame([y_data[colnames[0]],o[1],o[5],o[9],o[13],o[17],o[21],o[25],o[29],o[33],o[37],
                            o[3],o[7],o[11],o[15],o[19],o[23],o[27],o[31],o[35],o[39]]).T
    obs_pre_df.columns=(colnames[0],'train1','train2','train3','train4','train5',
                        'train6','train7','train8','train9','train10',
                        'test1','test2','test3','test4','test5',
                        'test6','test7','test8','test9','test10')
    
    obs_pre_df.to_excel(writer1,sheet_name=colnames[0])
    cordf.to_excel(writer2,sheet_name=colnames[0])

writer1.save()
writer2.save()
"""
SBS
"""
rf=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,max_features=5)
sbs=SBS(rf,k_features=15,scoring=r2_score)
score_list=[]
for i in trange(3):

    frame=pd.read_excel('clearance2021.xlsx',sheet_name=i+2)
    x_names=frame.iloc[:,1:29].columns.values.tolist()
    x_data=frame.iloc[:,1:29].values
    y_data=frame.iloc[:,29].values
    sbs.fit(x_data,y_data)
    
    k_feat = [len(k) for k in sbs.subsets_]
    score_list.append(sbs.scores_)
    
    plt.plot(k_feat, sbs.scores_, marker='o')
    plt.ylim([0, 1])
    plt.ylabel('r2')
    plt.xlabel('Number of features')
    plt.grid()
    plt.tight_layout()
    # plt.savefig('images/04_08.png', dpi=300)
    plt.show()
    
    k=sbs.scores_.index(max(sbs.scores_))
    globals()['kb'+str(i)]=list(sbs.subsets_[k])
    globals()['colindexb'+str(i)]=[]
    for j in globals()['kb'+str(i)]:
        globals()['colindexb'+str(i)].append(x_names[j])
        
score_df=pd.DataFrame(score_list)
score_df=score_df.stack().unstack(0)
xl=pd.ExcelFile('clearance2021.xlsx')
namelist=xl.sheet_names
score_df.columns=namelist[2:5]
score_df.to_excel('sbs-score.xlsx',sheet_name='sheet1')

"""
Random Forest
"""

model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=5#,min_samples_leaf=2
                                )

#writer1=pd.ExcelWriter('all-rf-op.xlsx')
writer2=pd.ExcelWriter('all-rf-cor.xlsx')
#writer3=pd.ExcelWriter('all-rf-imp.xlsx')
#writer5=pd.ExcelWriter('all-val.xlsx')
for i in trange(3):
    frame=pd.read_excel('clearance2021.xlsx',sheet_name=i+2)
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    valdata=frame.iloc[val,:]
    val_x=valdata.iloc[:,1:29]
    val_y=valdata.iloc[:,29:]
    frame=frame.iloc[model_index,:]
    #x_data=frame[globals()['colindexb'+str(i)]]
    x_data=frame.iloc[:,1:29]
    y_data=frame.iloc[:,29:]
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    #stdsc=StandardScaler()
    #prelist=[]
    #vallist=[]
    corlist_train=[]
    corlist_test=[]
    rmsel_train=[]
    rmsel_test=[]
    #o=[]
    #imp=[]
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=5#,min_samples_leaf=2
                                )
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
           #global x_train,y_train,x_test,y_test
            x_train=x_data.iloc[train_index,:]
            #x_train=pd.DataFrame(stdsc.fit_transform(x_train))
            x_train.columns=x_names
            
            y_train=y_data.iloc[train_index,:]
            
            x_test=x_data.iloc[test_index,:]
           #x_test=pd.DataFrame(stdsc.fit_transform(x_test))
            x_test.columns=x_names
        
            y_test=y_data.iloc[test_index,:]
            
            model.fit(x_train,y_train)
            #val_one=model.predict(val_x)
            #vallist.append(val_one.T)
            caculate_cor()
            corlist_train.append(r_train[1,0])
            corlist_test.append(r_test[1,0])
            rmsel_train.append(rmse_train)
            rmsel_test.append(rmse_test)
            #o.append(y_train[colnames[0]])
            #o.append(y_train_pred[0])
            #o.append(y_test[colnames[0]])
            #o.append(y_test_pred[0])
            #pbar.update()           
            #imp.append(model.feature_importances_)

    #plt.show()        
    cordf=pd.DataFrame({'train':corlist_train,'test':corlist_test,
                        'rmse_train':rmsel_train,'rmse_test':rmsel_test})
    #obs_pre_df=pd.DataFrame([y_data[colnames[0]],o[1],o[5],o[9],o[13],o[17],o[21],o[25],o[29],o[33],o[37],
                            #o[3],o[7],o[11],o[15],o[19],o[23],o[27],o[31],o[35],o[39]]).T
    #obs_pre_df.columns=(colnames[0],'train1','train2','train3','train4','train5',
                        #'train6','train7','train8','train9','train10',
                        #'test1','test2','test3','test4','test5',
                        #'test6','test7','test8','test9','test10')
    #vresult=pd.DataFrame(vallist,columns=val).T
    #vresult['observe']=val_y

    #imp_df=pd.DataFrame(imp,columns=globals()['colindex'+str(i)])
    #imp_df=pd.DataFrame(imp,columns=x_names)
    #obs_pre_df.to_excel(writer1,sheet_name=colnames[0])
    cordf.to_excel(writer2,sheet_name=colnames[0])
    #imp_df.to_excel(writer3,sheet_name=colnames[0])
    #vresult.to_excel(writer5,sheet_name=colnames[0])
       
#writer1.save()
writer2.save()
#writer3.save()
#writer5.save()

"""
ss-index output
"""
writer6=pd.ExcelWriter('ss-index-all.xlsx')
l_train_list=[]
l_test_list=[]
for i in trange(3):
    frame=pd.read_excel('clearance2021.xlsx',sheet_name=i+2)
    corresult=pd.read_excel('all-rf-cor.xlsx',sheet_name=i-1)
    max_index=np.argmax(corresult['train'],axis=0)   
    
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    #x_data=frame[globals()['colindex'+str(i)]]
    frame=frame.iloc[model_index,:]
    x_data=frame.iloc[:,1:30]
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    index_list=[]
    for train_index in ss.split(x_data):
        index_list.append(train_index)
    index_train_list=index_list[max_index][0]
    index_df=pd.DataFrame(index_train_list)
    index_df.to_excel(writer6,sheet_name=namelist[i])

    l_train=len(index_list[0][0]) 
    l_test=len(index_list[0][1])
    l_train_list.append(l_train)
    l_test_list.append(l_test) 
writer6.save()

len_df=pd.DataFrame({'train':l_train_list,'test':l_test_list})
len_df.to_excel('ss_len.xlsx')


"""
SVM
"""
from sklearn import svm
from sklearn.preprocessing import StandardScaler

#predf=pd.read_excel('experiment materials.xlsx',sheet_name=1).iloc[0:5,0:31]

writer1=pd.ExcelWriter('all-svm-op.xlsx')
writer2=pd.ExcelWriter('all-svm-cor.xlsx')
#writer4=pd.ExcelWriter('all-val.xlsx')
#writer5=pd.ExcelWriter('all-pre.xlsx')
#writer6=pd.ExcelWriter('ss-index-all.xlsx')
for i in trange(3):
    frame=pd.read_excel('clearance2021.xlsx',sheet_name=i+2)
    random.seed(1234)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    #x_data=frame[globals()['colindex'+str(i)]]
    valdata=frame.iloc[val,:]
    val_x=valdata.iloc[:,1:30]
    val_y=valdata.iloc[:,30:]
    frame=frame.iloc[model_index,:]
    stdsc=StandardScaler()
    x_data=frame.iloc[:,1:30]
    x_data=pd.DataFrame(stdsc.fit_transform(x_data))
    y_data=frame.iloc[:,30:]
    colnames=y_data.columns.values.tolist()
    y_data=pd.DataFrame(y_data[colnames[0]]/max(y_data[colnames[0]]))
    x_names=x_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    #stdsc=StandardScaler()
    prelist=[]
    vallist=[]
    corlist_train=[]
    corlist_test=[]
    rmsel_train=[]
    rmsel_test=[]
    o=[]
    model=svm.SVR(kernel='rbf')
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index,:]
            x_train.columns=x_names
            y_train=y_data.iloc[train_index,:]            
            x_test=x_data.iloc[test_index,:]
            x_test.columns=x_names        
            y_test=y_data.iloc[test_index,:]            
            model.fit(x_train,y_train)
            '''
            val_one=model.predict(val_x)
            vallist.append(val_one.T)
            pre_one=model.predict(predf)
            prelist.append(pre_one.T)
            '''
            caculate_cor()
            corlist_train.append(r_train[1,0])
            corlist_test.append(r_test[1,0])
            rmsel_train.append(rmse_train)
            rmsel_test.append(rmse_test)
            scatter_loss_plot()
            o.append(y_train[colnames[0]])
            o.append(y_train_pred[0])
            o.append(y_test[colnames[0]])
            o.append(y_test_pred[0])
            pbar.update()           

    plt.show()        
    cordf=pd.DataFrame({'train':corlist_train,'test':corlist_test,
                        'rmse_train':rmsel_train,'rmse_test':rmsel_test})
    obs_pre_df=pd.DataFrame([y_data[colnames[0]],o[1],o[5],o[9],o[13],o[17],o[21],o[25],o[29],o[33],o[37],
                            o[3],o[7],o[11],o[15],o[19],o[23],o[27],o[31],o[35],o[39]]).T
    obs_pre_df.columns=(colnames[0],'train1','train2','train3','train4','train5',
                        'train6','train7','train8','train9','train10',
                        'test1','test2','test3','test4','test5',
                        'test6','test7','test8','test9','test10')
    '''
    presult=pd.DataFrame(prelist,columns=['T','C','S','M','L']).T
    vresult=pd.DataFrame(vallist,columns=val).T
    vresult['observe']=val_y
    '''
    obs_pre_df.to_excel(writer1,sheet_name=colnames[0])
    cordf.to_excel(writer2,sheet_name=colnames[0])
    #presult.to_excel(writer4,sheet_name=colnames[0])
    #vresult.to_excel(writer5,sheet_name=colnames[0])
       
writer1.save()
writer2.save()
#writer3.save()
#writer4.save()
#writer5.save()



'''
permutation test
'''
writer1=pd.ExcelWriter('permutation.xlsx')

for i in trange(3):
    frame=pd.read_excel('clearance2021.xlsx',sheet_name=i+2)
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
    #x_data=frame[globals()['colindex'+str(i)]]
    valdata=frame.iloc[val,:]
    val_x=valdata.iloc[:,1:30]
    val_y=valdata.iloc[:,30:]
    frame=frame.iloc[model_index,:]
    x_data=frame.iloc[:,1:30]
    x_data.index=range(len(x_data))
    y_data=frame.iloc[:,30:]
    y_data.index=range(len(y_data))
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    r2_list=[]
    q2_list=[]
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=5#,min_samples_leaf=2
                                )
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index,:]
            x_train.columns=x_names
            y_train=y_data.iloc[train_index,:]
            x_test=x_data.loc[test_index,:]
            x_test.columns=x_names        
            y_test=y_data.loc[test_index,:]
            
            for j in trange(5):
                for k in range(10):
                    random.seed(i+j+k)
                    per_index=np.random.choice(train_index,round((j+1)*0.2*len(x_train)),False)
                    y_train_per=y_train.copy()
                    for i_index in per_index:
                        y_train_per.loc[i_index,:]=np.random.uniform(0,max(y_data[colnames[0]]))
                    model.fit(x_train,np.array(y_train_per).ravel())
                    r2_list.append(
                        np.corrcoef(y_train.iloc[:,0],y_train_per.iloc[:,0])[0,1])
                    y_array=np.array(y_test).ravel()
                    rss=np.sum((y_array-model.predict(x_test))**2)                          
                    tss=np.sum((y_array-np.mean(y_array))**2)
                    q2=1-rss/tss
                    q2_list.append(q2)
                
            model.fit(x_train,np.array(y_train).ravel())
            r2_list.append(1)
            rss=np.sum((y_array-model.predict(x_test))**2)                          
            tss=np.sum((y_array-np.mean(y_array))**2)
            q2=1-rss/tss
            q2_list.append(q2)
            pbar.update() 
             
    perdf=pd.DataFrame({'r2':r2_list,'q2':q2_list})
    
    plt.ylim(-2,1)
    plt.xlim(0,1)
    plt.plot(perdf['r2'],perdf['q2'],'.')
    plt.show()
    perdf.to_excel(writer1,sheet_name=colnames[0])
writer1.save()
