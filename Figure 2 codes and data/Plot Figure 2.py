# -*- coding: utf-8 -*-
"""
Created on Tue Aug 23 13:54:45 2022

Author: Shafiqah Azman

Note: These codes are used to produce Fig. 2 in paper:
    Forecasting the Volatility of Cryptocurrencies in the Presence of COVID-19 
    with the State Space Model and Kalman Filter
    by Azman et al. (2022)
    
    Before proceed, change directory in Line 17 

"""

import os
os.chdir("C:/Users/HP/OneDrive - Universiti Malaya/[WRITING AND DEFENCE]/KF paper")

import pandas as pd
import matplotlib.pyplot as plt

def format_number(data_value, indx):
    if data_value >= 10_000:
        formatter = format(int(data_value), ',')
    else:
        formatter = format(int(data_value))
    return formatter


# BTC
BTC_precovid = pd.read_csv('BTC precovid.csv')
BTC_covid = pd.read_csv('BTC covid.csv')

index_pre=BTC_precovid.index
index_during=pd.RangeIndex(start=len(index_pre)+1, stop=len(BTC_precovid.index)+len(BTC_covid.index)+1, 
                           step=1)

fig, ax = plt.subplots(figsize=(25, 10))
plt.plot(index_pre, BTC_precovid["V1"].values,label="Pre-COVID", color='blue', linewidth=4)
plt.plot(index_during, BTC_covid["V1"].values, color='orange', linewidth=4, label="During COVID")
plt.xlabel('Hours', fontsize=30)
plt.ylabel('Observed volatility',fontsize=30)
plt.legend(loc='upper left', fontsize=30)
plt.ylim([0, 2])
plt.xticks(fontsize=30)
plt.yticks(fontsize=30)
ax.xaxis.set_major_formatter(format_number)
plt.show()

# ETH
ETH_precovid = pd.read_csv('ETH precovid.csv')
ETH_covid = pd.read_csv('ETH covid.csv')

index_pre=ETH_precovid.index
index_during=pd.RangeIndex(start=len(index_pre)+1, stop=len(ETH_precovid.index)+len(ETH_covid.index)+1, 
                           step=1)

fig, ax = plt.subplots(figsize=(25, 10))
plt.plot(index_pre, ETH_precovid["V1"].values,label="Pre-COVID", color='blue', linewidth=4)
plt.plot(index_during, ETH_covid["V1"].values, color='orange', linewidth=4, label="During COVID")
plt.xlabel('Hours', fontsize=30)
plt.ylabel('Observed volatility',fontsize=30)
plt.legend(loc='upper left', fontsize=30)
plt.ylim([0, 2])
plt.xticks(fontsize=30)
plt.yticks(fontsize=30)
ax.xaxis.set_major_formatter(format_number)
plt.show()


# LTC
LTC_precovid = pd.read_csv('LTC precovid.csv')
LTC_covid = pd.read_csv('LTC covid.csv')

index_pre=LTC_precovid.index
index_during=pd.RangeIndex(start=len(index_pre)+1, stop=len(LTC_precovid.index)+len(LTC_covid.index)+1, 
                           step=1)

fig, ax = plt.subplots(figsize=(25, 10))
plt.plot(index_pre, LTC_precovid["V1"].values,label="Pre-COVID", color='blue', linewidth=4)
plt.plot(index_during, LTC_covid["V1"].values, color='orange', linewidth=4, label="During COVID")
plt.xlabel('Hours', fontsize=30)
plt.ylabel('Observed volatility',fontsize=30)
plt.legend(loc='upper left', fontsize=30)
plt.ylim([0, 2])
plt.xticks(fontsize=30)
plt.yticks(fontsize=30)
ax.xaxis.set_major_formatter(format_number)
plt.show()

# XRP
XRP_precovid = pd.read_csv('XRP precovid.csv')
XRP_covid = pd.read_csv('XRP covid.csv')

index_pre=XRP_precovid.index
index_during=pd.RangeIndex(start=len(index_pre)+1, stop=len(XRP_precovid.index)+len(XRP_covid.index)+1, 
                           step=1)

fig, ax = plt.subplots(figsize=(25, 10))
plt.plot(index_pre, XRP_precovid["V1"].values,label="Pre-COVID", color='blue', linewidth=4)
plt.plot(index_during, XRP_covid["V1"].values, color='orange', linewidth=4, label="During COVID")
plt.xlabel('Hours', fontsize=30)
plt.ylabel('Observed volatility',fontsize=30)
plt.legend(loc='upper left', fontsize=30)
plt.ylim([0, 2])
plt.xticks(fontsize=30)
plt.yticks(fontsize=30)
ax.xaxis.set_major_formatter(format_number)
plt.show()

# BCH
BCH_precovid = pd.read_csv('BCH precovid.csv')
BCH_covid = pd.read_csv('BCH covid.csv')

index_pre=BCH_precovid.index
index_during=pd.RangeIndex(start=len(index_pre)+1, stop=len(BCH_precovid.index)+len(BCH_covid.index)+1, 
                           step=1)

fig, ax = plt.subplots(figsize=(25, 10))
plt.plot(index_pre, BCH_precovid["V1"].values,label="Pre-COVID", color='blue', linewidth=4)
plt.plot(index_during, BCH_covid["V1"].values, color='orange', linewidth=4, label="During COVID")
plt.xlabel('Hours', fontsize=30)
plt.ylabel('Observed volatility',fontsize=30)
plt.legend(loc='upper left', fontsize=30)
plt.ylim([0, 2])
plt.xticks(fontsize=30)
plt.yticks(fontsize=30)
ax.xaxis.set_major_formatter(format_number)
plt.show()

