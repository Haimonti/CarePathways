import pandas as pd

ad_df = pd.read_csv("/content/admissions.csv")
pt_df = pd.read_csv("/content/patients.csv")
cols = ["subject_id","admission_type","marital_status","race","admittime","dischtime"]
df_phy_ad = ad_df[cols]
df_phy_ad = pd.merge(df_phy_ad,pt_df[["subject_id","gender","anchor_age"]], on = "subject_id", how="outer")
# ----- calculating LOS and appending LOS column ----------
df_phy_ad['admittime'] = pd.to_datetime(df_phy_ad['admittime'])
df_phy_ad['dischtime'] = pd.to_datetime(df_phy_ad['dischtime'])
df_phy_ad["LOS"] = round((df_phy_ad['dischtime'] - df_phy_ad['admittime']).dt.total_seconds() / (3600*24),2)
df_phy_ad.drop(['admittime','dischtime'], axis = 1, inplace = True)
# ----- binary/numeric represenation mapping ------
gender_nums = {"F":1,"M":2,"nan":0}
race_nums = {'WHITE':1, 'OTHER':2, 'BLACK/AFRICAN AMERICAN':3, 'UNABLE TO OBTAIN':4, 'UNKNOWN':5,
 'WHITE - RUSSIAN':6, 'BLACK/CAPE VERDEAN':7,
 'NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER':8, 'PORTUGUESE':9,
 'WHITE - OTHER EUROPEAN':10, 'HISPANIC/LATINO - PUERTO RICAN':11, 'ASIAN':12,
 'ASIAN - CHINESE':13, 'HISPANIC/LATINO - DOMINICAN':14,
 'HISPANIC/LATINO - SALVADORAN':15, 'BLACK/AFRICAN':16,
 'HISPANIC/LATINO - GUATEMALAN':17, 'ASIAN - SOUTH EAST ASIAN':18,
 'WHITE - BRAZILIAN':19, 'SOUTH AMERICAN':20, 'HISPANIC OR LATINO':21,
 'ASIAN - KOREAN':22, 'BLACK/CARIBBEAN ISLAND':23, 'HISPANIC/LATINO - MEXICAN':24,
 'PATIENT DECLINED TO ANSWER':25, 'HISPANIC/LATINO - CUBAN':26,
 'AMERICAN INDIAN/ALASKA NATIVE':27, 'MULTIPLE RACE/ETHNICITY':28,
 'WHITE - EASTERN EUROPEAN':29, 'HISPANIC/LATINO - HONDURAN':30,
 'HISPANIC/LATINO - CENTRAL AMERICAN':31, 'ASIAN - ASIAN INDIAN':32,
 'HISPANIC/LATINO - COLUMBIAN':33,"nan":0 }
marital_nums = {'WIDOWED':1,'SINGLE':2, 'MARRIED':3, 'DIVORCED':4, "nan":5}
admission_nums = {'URGENT':1, 'EW EMER.':2,'EU OBSERVATION':3, 'OBSERVATION ADMIT':4,
 'SURGICAL SAME DAY ADMISSION':5, 'AMBULATORY OBSERVATION':6, 'DIRECT EMER.':7,
 'DIRECT OBSERVATION':8, 'ELECTIVE':9, "nan":0}
 # ---------- mapping values ------------
df_phy_ad["admission_type"] = df_phy_ad["admission_type"].map(admission_nums)
df_phy_ad["admission_type"] = df_phy_ad["admission_type"].fillna(0)
df_phy_ad["marital_status"] = df_phy_ad["marital_status"].map(marital_nums)
df_phy_ad["marital_status"] = df_phy_ad["marital_status"].fillna(0)
df_phy_ad["race"] = df_phy_ad["race"].map(race_nums)
df_phy_ad["race"] = df_phy_ad["race"].fillna(0)
df_phy_ad["gender"] = df_phy_ad["gender"].map(gender_nums)
df_phy_ad["gender"] = df_phy_ad["gender"].fillna(0)
df_phy_ad["LOS"] = df_phy_ad["LOS"].fillna(0)
unique_ids = "Unique number of Subject_Ids: " + str(len(df_phy_ad["subject_id"].unique()))
print(unique_ids)
# ---- converting to final csv output -------
df_phy_ad.to_csv("PhysicalandAdmission.csv", index = False)
print(df_phy_ad)

