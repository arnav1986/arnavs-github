
# coding: utf-8

# In[1]:


import numpy as np # library to handle data in a vectorized manner

import pandas as pd # library for data analsysis
pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

import json # library to handle JSON files

get_ipython().system("conda install -c conda-forge geopy --yes # uncomment this line if you haven't completed the Foursquare API lab")
from geopy.geocoders import Nominatim # convert an address into latitude and longitude values

import requests # library to handle requests
from pandas.io.json import json_normalize # tranform JSON file into a pandas dataframe

# Matplotlib and associated plotting modules
import matplotlib.cm as cm
import matplotlib.colors as colors

# import k-means from clustering stage
from sklearn.cluster import KMeans

get_ipython().system("conda install -c conda-forge folium=0.5.0 --yes # uncomment this line if you haven't completed the Foursquare API lab")
import folium # map rendering library

print('Libraries imported.')


# In[2]:


a = pd.read_csv('output.csv')


# In[3]:


a.drop(['Unnamed: 0'], axis=1, inplace=True )


# A list of all the Localitis in Canada ordered by Postcode

# In[9]:


a


# In[28]:


address = 'Toronto, Canada'

geolocator = Nominatim()
location = geolocator.geocode(address)
latitude = location.latitude
longitude = location.longitude
print('The geograpical coordinate of Toronto are {}, {}.'.format(latitude, longitude))


# In[11]:


a.rename(columns={'Neighbourhood\n': 'Neighbourhood'}, inplace = True)


# In[12]:


a.dtypes


# We Visualize all the locations in Canada with markers concentrated in the Toronto area. 

# In[29]:


map_toronto = folium.Map(location=[latitude, longitude], zoom_start=10)

# add markers to map
for lat, lng, borough, neighbourhood in zip(a['Latitude'], a['Longitude'], a['Borough'], a['Neighbourhood']):
    label = '{}, {}'.format(neighbourhood, borough)
    label = folium.Popup(label, parse_html=True)
    folium.CircleMarker(
        [lat, lng],
        radius=5,
        popup=label,
        color='blue',
        fill=True,
        fill_color='#3186cc',
        fill_opacity=0.7,
        parse_html=False).add_to(map_toronto)  
    
map_toronto


# Now, let's visualize the areas around 'Scarborough' and plot markers on it

# In[31]:


b = a[a['Borough'] == 'Scarborough']. reset_index(drop = True)
b.head()


# In[34]:


address = 'Scarborough, Canada'

geolocator = Nominatim()
location = geolocator.geocode(address)
latitude1 = location.latitude
longitude1 = location.longitude
print('The geograpical coordinate of Scarborough are {}, {}.'.format(latitude, longitude))


# In[36]:


map_scarborough = folium.Map(location=[latitude1, longitude1], zoom_start=10)

# add markers to map
for lat, lng, borough, neighbourhood in zip(b['Latitude'], b['Longitude'], b['Borough'], b['Neighbourhood']):
    label = '{}, {}'.format(neighbourhood, borough)
    label = folium.Popup(label, parse_html=True)
    folium.CircleMarker(
        [lat, lng],
        radius=5,
        popup=label,
        color='blue',
        fill=True,
        fill_color='#3186cc',
        fill_opacity=0.7,
        parse_html=False).add_to(map_scarborough)  
    
map_scarborough


# Let's visualize only the areas in and around 'Toronto' and cluster only the surrounding neighbourhoods

# In[39]:


c = a[a['Borough'] == 'Downtown Toronto'].reset_index(drop = True)
c.head()


# In[41]:


address = 'Downtown Toronto, Canada'

geolocator = Nominatim()
location = geolocator.geocode(address)
latitude2 = location.latitude
longitude2 = location.longitude
print('The geograpical coordinate of Downtown Toronto are {}, {}.'.format(latitude, longitude))


# In[42]:


map_downtown = folium.Map(location=[latitude2, longitude2], zoom_start=10)

# add markers to map
for lat, lng, borough, neighbourhood in zip(c['Latitude'], c['Longitude'], c['Borough'], c['Neighbourhood']):
    label = '{}, {}'.format(neighbourhood, borough)
    label = folium.Popup(label, parse_html=True)
    folium.CircleMarker(
        [lat, lng],
        radius=5,
        popup=label,
        color='blue',
        fill=True,
        fill_color='#3186cc',
        fill_opacity=0.7,
        parse_html=False).add_to(map_downtown)  
    
map_downtown

