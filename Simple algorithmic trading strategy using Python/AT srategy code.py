# In[1]:

from pyramid.arima import auto_arima
from arch import arch_model
from pandas_datareader import DataReader
from datetime import datetime
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import rpy2.robjects.packages as rpackages
rugarch = rpackages.importr('rugarch')
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri


# In[230]:

# Fetching Data
gspc = DataReader("^GSPC",  "yahoo", datetime(2000,10,1), datetime(2019,1,1))['Adj Close']


# In[229]:

#Computing logged Returns
gspc_returns = np.log(gspc).diff().dropna()


# In[33]:

#window length => lenght of time series to forecast next point
#forecast_length => Number of point we will forecast using previous data
window_length = 500
forecast_length = len(gspc_returns) - window_length


# In[ ]:


forecasted_returns = []
for d in range(0,forecast_length):
    #getting the last 500 points
    snp_returns_rolling = gspc_returns[d:window_length+d]
    #fitting arima to find optimal params
    model = auto_arima(snp_returns_rolling)
    model.fit(snp_returns_rolling)
    #extracting p and q, require for feeding  into garch model
    p_ = model.order[0]
    o_ = model.order[1]
    q_ = model.order[2]
    arma_order = str(tuple([p_,q_]))
    #fitting Garch Model
    garch_spec = rugarch.ugarchspec(
    mean_model=robjects.r("list(armaOrder = c{arma_order})".format(arma_order=arma_order)),
    variance_model=robjects.r('list(garchOrder=c(1,1))'),
    distribution_model='std')
    pandas2ri.activate()
    r_dataframe = pandas2ri.py2ri(snp_returns_rolling)
    # Train R GARCH model on returns as %
    garch_fitted =  rugarch.ugarchfit(garch_spec, r_dataframe, solver = 'hybrid')
    pandas2ri.deactivate()
    #forecasting next point
    fore = rugarch.ugarchforecast(garch_fitted, n_ahead=1)
    forecast = np.array(fore.slots['forecast'].rx2('seriesFor')).flatten()[0]
    #storing signal, signal is basically sign of the forecast of return
    forecasted_returns.append({'date':gspc_returns.index[window_length+d].date(), 'signal':np.sign(forecast)})
    print(gspc_returns.index[window_length+d].date())
    print(arma_order,forecast)


# In[287]:

#converting list to df
returns_df = pd.DataFrame(forecasted_returns)
#Buy and hold returns are same as returns
returns_df['Buy and Hold'] = returns_df.date.apply(lambda x: gspc_returns[x])
#streategy return are actual return * signal
returns_df['Strategy'] = returns_df['Buy and Hold']*(returns_df["signal"])
final_returns = returns_df[['date','Buy and Hold', 'Strategy','signal']]


# In[289]:



final_returns.to_csv("buy_and_hold_vs_garch")


# In[288]:

#computing cummulative retuns for graph
final_returns['Buy and Hold']=final_returns['Buy and Hold'].cumsum()
final_returns['Strategy'] = final_returns['Strategy'].cumsum()
final_returns.plot(x='date',y=['Strategy','Buy and Hold'],figsize=(10,8))
