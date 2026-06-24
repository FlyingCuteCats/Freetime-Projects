import pandas as pd
import numpy as np

def indexise(data, yoycolnum = 1, momcolnum = 2, base = "2020-01-01", percentage = True):
    
    data['Date'] = pd.to_datetime(data['Date'])
    data = data.sort_values('Date')
    
    data_yoy = data.iloc[:, [0, yoycolnum]].dropna()
    data_mom = data.iloc[:, [0, momcolnum]].dropna()
    date_end = data['Date'].max()
    date_yoy_start = data_yoy['Date'].min() - pd.DateOffset(months=12)
    date_mom_start = data_mom['Date'].min() - pd.DateOffset(months=1)

    # indice dataframe initialisation
    indice = pd.DataFrame(columns=['Date', 'Index Y', 'Index M', 'Index'])
    col_rec = len(indice.columns) - 2
    indice['Date'] = pd.date_range(start=min(date_yoy_start, date_mom_start), end=date_end, freq='MS')
    indice['Date'] = pd.to_datetime(indice['Date'])
    indice = indice.merge(data, on='Date', how='left')
    # after merging make sure yoycolnum and momcolnum still work
    indice = indice.set_index('Date')
    indice.loc[base, ["Index Y", "Index M", "Index"]] = 100

    # base year computation
    base_index = indice.index.get_loc(pd.Timestamp(base))
    base_next_index = base_index + 12
    for i in range(base_index + 1, base_next_index):
        # index = previous index cum mom
        indice.iloc[i, 2] = indice.iloc[i - 1, 2] * (1 + indice.iloc[i, momcolnum+col_rec] / 100)

    # computation of first data following base year
    # indice.iloc[base_next_index, 0] = indice.iloc[base_index, 2] * (1 + indice.iloc[base_next_index, yoycolnum+col_rec] / 100) # yoy
    # indice.iloc[base_next_index, 1] = indice.iloc[base_next_index - 1, 2] * (1 + indice.iloc[base_next_index, momcolnum+col_rec] / 100) # mom
    # indice.iloc[base_next_index, 2] = indice.iloc[base_next_index, [0, 1]].mean()

    # computation of following years
    for i in range(base_next_index, len(indice)):
        # index_y = previous year, same month index cum yoy
        if pd.isna(indice.iloc[i, yoycolnum+col_rec]):
            indice.iloc[i, 0] = pd.NA
        else:
            indice.iloc[i, 0] = indice.iloc[i - 12, 2] * (1 + indice.iloc[i, yoycolnum+col_rec] / 100)

        # index_m = previous month index cum mom
        if pd.isna(indice.iloc[i, momcolnum+col_rec]):
            indice.iloc[i, 1] = pd.NA
        else:
            indice.iloc[i, 1] = indice.iloc[i - 1, 2] * (1 + indice.iloc[i, momcolnum+col_rec] / 100)

        # index = average of index_y and index_m
        indice.iloc[i, 2] = indice.iloc[i, [0, 1]].mean()

    # computation of preceding years
    for i in range(base_index - 1, -1,-1):
        # index_y = following year, same month index subtract yoy
        if pd.isna(indice.iloc[i + 12, yoycolnum+col_rec]):
            indice.iloc[i, 0] = pd.NA
        else:
            indice.iloc[i, 0] = indice.iloc[i + 12, 2] / (1 + indice.iloc[i + 12, yoycolnum+col_rec] / 100)

        # index_m = following month index subtract mom
        if pd.isna(indice.iloc[i + 1, momcolnum+col_rec]):
            indice.iloc[i, 1] = pd.NA
        else:
            indice.iloc[i, 1] = indice.iloc[i + 1, 2] / (1 + indice.iloc[i + 1, momcolnum+col_rec] / 100)

        # index = average of index_y and index_m
        indice.iloc[i, 2] = indice.iloc[i, [0, 1]].mean()

    return indice['Index']