
import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns 
import statsmodels.api as sm


#Calculando medidas de variação e dispersão

medidas = df.describe().T
medidas['variacao'] = medidas['standard deviation'] / medidas['mean']

#Adicionado a mediana 
medidas['mediana'] = df.median()

