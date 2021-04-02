# -*- coding: utf-8 -*-
"""
Created on Wed Feb 17 09:52:47 2021

@author: admin
"""
from sklearn.ensemble import RandomForestRegressor
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import ShuffleSplit
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import StandardScaler
from tqdm import tqdm,trange
import random
from keras import Sequential
from keras.layers.core import Dense,Activation,Dropout
from keras import optimizers

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
    
"""
Dose-rf-regression
"""
writer1=pd.ExcelWriter('dose-rf-op.xlsx')
writer2=pd.ExcelWriter('dose-rf-cor.xlsx')

for i in trange(15):
    if i<12:        
        frame=pd.read_excel('imm/RF20210202.xlsx',sheet_name=i+1)
    else:
        frame=pd.read_excel('burden/clearance2021.xlsx',sheet_name=i-10)
        
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)

    frame=frame.loc[model_index,:]
    x_data=frame['Dose']
    x_data.index=range(len(x_data))
    y_data=frame.iloc[:,(frame.shape[1]-1):]
    y_data.index=range(len(y_data))
    x_names=x_data.name
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    corlist_train=[]
    corlist_test=[]
    rmsel_train=[]
    rmsel_test=[]
    o=[]
    imp=[]
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=1#,min_samples_leaf=2
                                )
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index].values.reshape(-1,1)     
            y_train=y_data.iloc[train_index,:]          
            x_test=x_data.iloc[test_index].values.reshape(-1,1) 
            y_test=y_data.iloc[test_index,:]       
            model.fit(x_train,y_train)
            caculate_cor()
            corlist_train.append(r_train[1,0])
            corlist_test.append(r_test[1,0])
            rmsel_train.append(rmse_train)
            rmsel_test.append(rmse_test)
            o.append(y_train[colnames[0]])
            o.append(y_train_pred[0])
            o.append(y_test[colnames[0]])
            o.append(y_test_pred[0])
            pbar.update()           
      
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
3 features-rf-regression
"""
writer1=pd.ExcelWriter('3-rf-op.xlsx')
writer2=pd.ExcelWriter('3-rf-cor.xlsx')

for i in trange(15):
    if i<12:        
        frame=pd.read_excel('imm/RF20210202.xlsx',sheet_name=i+1)
    else:
        frame=pd.read_excel('burden/clearance2021.xlsx',sheet_name=i-10)
        
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)

    frame=frame.loc[model_index,:]
    x_data=frame[['Zeta','SSA','Dose']]
    x_data.index=range(len(x_data))
    y_data=frame.iloc[:,(frame.shape[1]-1):]
    y_data.index=range(len(y_data))
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    corlist_train=[]
    corlist_test=[]
    rmsel_train=[]
    rmsel_test=[]
    o=[]
    imp=[]
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=2#,min_samples_leaf=2
                                )
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index,:]
            x_train.columns=x_names
            y_train=y_data.iloc[train_index,:]          
            x_test=x_data.iloc[test_index,:]
            x_test.columns=x_names
            y_test=y_data.iloc[test_index,:]       
            model.fit(x_train,y_train)
            caculate_cor()
            corlist_train.append(r_train[1,0])
            corlist_test.append(r_test[1,0])
            rmsel_train.append(rmse_train)
            rmsel_test.append(rmse_test)
            o.append(y_train[colnames[0]])
            o.append(y_train_pred[0])
            o.append(y_test[colnames[0]])
            o.append(y_test_pred[0])
            pbar.update()           
      
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
feature--shuffling
"""
writer1=pd.ExcelWriter('feature-shuffle.xlsx')
for i in trange(15):
    if i<12:        
        frame=pd.read_excel('imm/RF20210202.xlsx',sheet_name=i+1)
    else:
        frame=pd.read_excel('burden/clearance2021.xlsx',sheet_name=i-10)
        
    random.seed(i)
    val=random.sample(range(0,len(frame)),5)
    model_index=list(frame.index)
    for j in val:
        model_index.remove(j)
        
    frame=frame.loc[model_index,:]
    if i<11:
        x_data=frame.iloc[:,0:31]
        y_data=frame.iloc[:,31:]
    else if i=12
        x_data=frame.iloc[:,1:30]
        y_data=frame.iloc[:,30:]
        
    x_data.index=range(len(x_data))
    y_data.index=range(len(y_data))
    
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    
    rmsel_train=[]
    rmsel_test=[]
    
    model=RandomForestRegressor(n_estimators=500,oob_score=True,random_state=0,
                                max_features=2#,min_samples_leaf=2
                                )
    with tqdm(total=10) as pbar:
        for train_index , test_index in ss.split(x_data,y_data):
            x_train=x_data.iloc[train_index,:]
            x_train.columns=x_names
            y_train=y_data.iloc[train_index,:]          
            x_test=x_data.iloc[test_index,:]
            x_test.columns=x_names
            y_test=y_data.iloc[test_index,:]       
            model.fit(x_train,np.array(y_train).ravel())
            caculate_cor()
            rmsel_train.append(rmse_train)
            rmsel_test.append(rmse_test)        
            for j in range(len(x_names)):
                random.seed(j)
                shufffeature=np.random.permutation(x_train.iloc[:,j])  
                for k in range(len(x_train)):
                    x_train.iloc[k,j]=shufffeature[k]
                model.fit(x_train,np.array(y_train).ravel())
                caculate_cor()
                rmsel_train.append(rmse_train)
                rmsel_test.append(rmse_test)
            pbar.update()   
            
    cordf=pd.DataFrame({'rmse_train':rmsel_train,'rmse_test':rmsel_test})
    cordf.to_excel(writer1,sheet_name=colnames[0])    
writer1.save()

"""
ANN-Multi-label
"""

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

def Set_network(n_hide,n_input,n_output):
    global model
    model=Sequential()
    model.add(Dense(input_dim=n_input,units=n_hide,kernel_initializer='normal',activation='relu'))
    model.add(Dropout(0.2))
    model.add(Dense(input_dim=n_hide,units=n_output,kernel_initializer='normal'))
  
writer1=pd.ExcelWriter('multi-ann-cor.xlsx')
writer2=pd.ExcelWriter('multi-ann-op.xlsx')
for i in range(6):
    frame=pd.read_excel('multilabel.xlsx',sheet_name=i)
    if i<5:
        x_data=frame.iloc[:,0:31]
        y_data=frame.iloc[:,31:]
    else:
        x_data=frame.iloc[:,0:28]
        y_data=frame.iloc[:,28:]    
        
    x_names=x_data.columns.values.tolist()
    colnames=y_data.columns.values.tolist()
    x_len=len(x_names)
    y_len=len(colnames)
    
    stdsc=StandardScaler()
    x_data=pd.DataFrame(stdsc.fit_transform(x_data))
    x_data.columns=x_names
    #y_data=pd.DataFrame(stdsc.fit_transform(y_data))
    #y_data.columns=colnames
    
    ss=ShuffleSplit(n_splits=10, test_size=0.1,random_state=0)
    
    corlist_train1=[]
    corlist_test1=[]
    corlist_train2=[]
    corlist_test2=[]
    corlist_train3=[]
    corlist_test3=[]
    o1=[]
    o2=[]
    o3=[]
    for train_index , test_index in ss.split(x_data,y_data):
        global x_train,y_train,x_test,y_test
        x_train=x_data.iloc[train_index,:]         
        y_train=y_data.iloc[train_index,:]         
        x_test=x_data.iloc[test_index,:]     
        y_test=y_data.iloc[test_index,:]         
        Set_network(2*x_len,x_len,y_len)
        Network_train('Adam',0.1,0.9,0,500)
        y_train_pred=pd.DataFrame(model.predict(x_train).reshape(y_train.shape),index=train_index)
        y_test_pred=pd.DataFrame(model.predict(x_test).reshape(y_test.shape),index=test_index)
        
        r_train1=np.corrcoef(y_train_pred[0],y_train[colnames[0]])
        r_train2=np.corrcoef(y_train_pred[1],y_train[colnames[1]])
        r_test1=np.corrcoef(y_test_pred[0],y_test[colnames[0]])
        r_test2=np.corrcoef(y_test_pred[1],y_test[colnames[1]])         
        corlist_train1.append(r_train1[1,0])
        corlist_test1.append(r_test1[1,0])
        corlist_train2.append(r_train2[1,0])
        corlist_test2.append(r_test2[1,0])
        
        if y_len>2:
            r_train3=np.corrcoef(y_train_pred[2],y_train[colnames[2]])
            r_test3=np.corrcoef(y_test_pred[2],y_test[colnames[2]])
            corlist_train3.append(r_train3[1,0])
            corlist_test3.append(r_test3[1,0])      
            
        o1.append(y_train_pred[0])
        o1.append(y_test_pred[0])
        o2.append(y_train_pred[1])
        o2.append(y_test_pred[1])
        if y_len>2:
            o3.append(y_train_pred[2])
            o3.append(y_test_pred[2])
          
    cordf1=pd.DataFrame({'train':corlist_train1,'test':corlist_test1})
    cordf2=pd.DataFrame({'train':corlist_train2,'test':corlist_test2}) 
    obs_pre_df1=pd.DataFrame([y_data[colnames[0]],o1[0],o1[2],o1[4],o1[6],o1[8],o1[10],o1[12],o1[14],o1[16],o1[18],
                              o1[1],o1[3],o1[5],o1[7],o1[9],o1[11],o1[13],o1[15],o1[17],o1[19]]).T
    obs_pre_df2=pd.DataFrame([y_data[colnames[0]],o2[0],o2[2],o2[4],o2[6],o2[8],o2[10],o2[12],o2[14],o2[16],o2[18],
                              o2[1],o2[3],o2[5],o2[7],o2[9],o2[11],o2[13],o2[15],o2[17],o2[19]]).T
    cordf1.to_excel(writer1,sheet_name=colnames[0])  
    cordf2.to_excel(writer1,sheet_name=colnames[1])  
    obs_pre_df1.to_excel(writer2,sheet_name=colnames[0])
    obs_pre_df2.to_excel(writer2,sheet_name=colnames[1])
    if y_len>2:
        cordf3=pd.DataFrame({'train':corlist_train3,'test':corlist_test3}) 
        obs_pre_df3=pd.DataFrame([y_data[colnames[0]],o3[0],o3[2],o3[4],o3[6],o3[8],o3[10],o3[12],o3[14],o3[16],o3[18],
                                  o3[1],o3[3],o3[5],o3[7],o3[9],o3[11],o3[13],o3[15],o3[17],o3[19]]).T
        cordf3.to_excel(writer1,sheet_name=colnames[2]) 
        obs_pre_df3.to_excel(writer2,sheet_name=colnames[2])

writer1.save()
writer2.save()