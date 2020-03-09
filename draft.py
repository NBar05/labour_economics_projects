import pandas as pd
import numpy as np
import math as m

data = pd.read_stata("r23ind.dta", convert_categoricals=False)
data_cleaning = data[['IWWRKNOW', 'IWWAGELM', 'IWKIDS', 'IWBIRTHY', 'IWHOURLM', 'IWDISABL', 'IWMARIST', 'IWHIEDUL', 'IWGENDER']]

for column in ['IWWRKNOW', 'IWWAGELM', 'IWKIDS', 'IWBIRTHY', 'IWHOURLM', 'IWDISABL', 'IWMARIST', 'IWHIEDUL', 'IWGENDER']:
    data_cleaning[column] = data_cleaning[column].mask(data_cleaning[column] > 99999990, errors='ignore')

data_cleaning['work'] = data_cleaning['IWWRKNOW'].transform(lambda x: 1 if x == 1 else (0 if x in [2, 3, 4, 5] else np.nan))

data_cleaning = data_cleaning[(pd.isna(data_cleaning.IWWAGELM)) | (data_cleaning.IWWAGELM < 80000)]

data_cleaning = data_cleaning[(pd.notna(data_cleaning.IWWAGELM)) | (data_cleaning.work != 1)]
data_cleaning['wage'] = np.nan
for i in data_cleaning.index:
    if data_cleaning['work'][i] == 1:
        data_cleaning['wage'][i] = data_cleaning['IWWAGELM'][i]

data_cleaning = data_cleaning[(pd.notna(data_cleaning.IWHOURLM)) | (pd.isna(data_cleaning.wage))]
data_cleaning = data_cleaning[(pd.isna(data_cleaning.IWHOURLM)) | (pd.notna(data_cleaning.wage))]

data_cleaning['hour_wage'] = data_cleaning['wage'] / data_cleaning['IWHOURLM']

data_cleaning['female'] = data_cleaning['IWGENDER'].transform(lambda x: 1 if x == 2 else (0 if x == 1 else np.nan))
data_cleaning['married'] = data_cleaning['IWMARIST'].transform(lambda x: 1 if x in [2, 3, 6] else (0 if x in [1, 4, 5] else np.nan))
data_cleaning['nkids'] = data_cleaning['IWKIDS'].transform(lambda x: x if x < 100 else np.nan)
data_cleaning['age'] = data_cleaning['IWBIRTHY'].transform(lambda x: 2014 - x)
data_cleaning = data_cleaning[data_cleaning.age <= 72]
data_cleaning['age2'] = data_cleaning['age'].transform(lambda x: x*x)
data_cleaning['lnwage'] = data_cleaning['wage'].transform(lambda x: m.log(x))
data_cleaning['disabl'] = data_cleaning['IWDISABL'].transform(lambda x: 1 if x in [1, 5] else (0 if x in [2, 6] else np.nan))

data_cleaning['femarried'] = data_cleaning['female'] * data_cleaning['married']

data_cleaning['educ1'] = data_cleaning['IWHIEDUL'].transform(lambda x: 1 if x in range(3, 7) else (0 if x in range(0, 3) or x in range(7, 15) else np.nan))
data_cleaning['educ2'] = data_cleaning['IWHIEDUL'].transform(lambda x: 1 if x in range(10, 12) else (0 if x in range(0, 10) or x in range(12, 15) else np.nan))
data_cleaning['educ3'] = data_cleaning['IWHIEDUL'].transform(lambda x: 1 if x == 8 or x in range(12, 15) else (0 if x in range(0, 8) or x in range(10, 12) else np.nan))

for i in data_cleaning.columns:
    print(str(i) + ': ' + str(pd.isna(data_cleaning[str(i)]).sum()))

data_p = data_cleaning[['lnwage', 'work', 'age', 'age2', 'educ1', 'educ2', 'educ3', 'female', 'married', 'femarried', 'nkids', 'disabl']]
