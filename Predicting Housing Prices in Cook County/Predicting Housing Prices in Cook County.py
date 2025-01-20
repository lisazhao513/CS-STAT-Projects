# Lisa Zhao

# Developed a linear regression model to predict housing prices based on multiple features, such as number of bedrooms, square footage, and location.
# Applied data preprocessing techniques including handling missing values, encoding categorical variables, and feature scaling for model readiness.
# Evaluated model performance using RMSE and cross-validation to assess generalization across different subsets of the data.


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import OneHotEncoder
from sklearn import linear_model as lm
from sklearn.model_selection import train_test_split

# Plot settings
plt.rcParams['figure.figsize'] = (12, 9)
plt.rcParams['font.size'] = 12


## Import Data ##
initial_data = pd.read_csv("cook_county_train.csv", index_col='Unnamed: 0')

training_data = initial_data[initial_data['Sale Price'] >= 500]
training_data['Log Sale Price'] = np.log(training_data['Sale Price'])


## Exploratory Data Analysis ##
def plot_distribution(data, label):
    fig, axs = plt.subplots(nrows=2)

    sns.displot(
        data[label], 
        ax=axs[0]
    )
    sns.boxplot(
        x=data[label],
        width=0.3, 
        ax=axs[1],
        showfliers=False,
    )
    spacer = np.max(data[label]) * 0.05
    xmin = np.min(data[label]) - spacer
    xmax = np.max(data[label]) + spacer
    axs[0].set_xlim((xmin, xmax))
    axs[1].set_xlim((xmin, xmax))

    axs[0].xaxis.set_visible(False)
    axs[0].yaxis.set_visible(False)
    axs[1].yaxis.set_visible(False)

    plt.subplots_adjust(hspace=0)
    fig.suptitle("Distribution of " + label)
    plt.show()

plot_distribution(training_data, label='Log Sale Price')


## Feature Engineering ##
def remove_outliers(data, variable, lower=-np.inf, upper=np.inf):
    return data[(data[variable] <= upper) & (data[variable] > lower)]

def log_transform(data):
    data['Log Sale Price'] = np.log(training_data['Sale Price'])
    return data

def add_total_bedrooms(data):
    with_rooms = data.copy()
    with_rooms['Bedrooms'] = with_rooms['Description'].str.extract(r'(\d+) of which are bedrooms').astype(int)
    return with_rooms

def add_total_bathrooms(data):
    with_rooms = data.copy()
    with_rooms['Bathrooms'] = with_rooms['Description'].str.extract(r'(\d+.\d+) of which are bathrooms').astype(float)
    return with_rooms

def find_expensive_neighborhoods(data, n=3, metric=np.median):
    neighborhoods = data.groupby('Neighborhood Code')['Log Sale Price'].agg(metric).sort_values(ascending = False).index[:n]
    
    # This makes sure the final list contains the generic int type used in Python3, not specific ones used in NumPy.
    return [int(code) for code in neighborhoods]

def add_in_expensive_neighborhood(data, expensive_neighborhoods):
    data['in_expensive_neighborhood'] = data['Neighborhood Code'].isin(expensive_neighborhoods).astype(int)
    return data

def substitute_roof_material(data):
    new_data = data.copy()
    new_data['Roof Material'] = new_data['Roof Material'].replace([1, 2, 3, 4, 5, 6], ['Shingle/Asphalt', 'Tar & Gravel', 'Slate', 'Shake', 'Tile', 'Other'])
    return new_data

def ohe_roof_material(data):
    oh_enc = OneHotEncoder()
    oh_enc.fit(data[['Roof Material']])
    ohe_data = oh_enc.transform(data[['Roof Material']]).toarray()
    ohe_df = pd.DataFrame(data = ohe_data, columns = oh_enc.get_feature_names_out(), index = data.index)
    new_data = data.join(ohe_df) 
    return new_data

def ohe_property_class(data):
    oh_enc = OneHotEncoder()
    oh_enc.fit(data[['Repair Condition', 'Property Class']])
    ohe_data = oh_enc.transform(data[['Repair Condition', 'Property Class']]).toarray()
    ohe_df = pd.DataFrame(data = ohe_data, columns = oh_enc.get_feature_names_out(), index = data.index)
    data_copy = data.join(ohe_df)
    return data_copy

def select_columns(data, *columns):
    """Select only columns passed as arguments."""
    return data.loc[:, columns]


def feature_engine(data, is_test_set=False):
    if not is_test_set:
        data = remove_outliers(data, 'Sale Price', lower = data['Sale Price'].quantile(0.01), upper = data['Sale Price'].quantile(0.99))
        data['Log Sale Price'] = np.log(data['Sale Price'] + 0.00001)

        # remove other outliers
        data = remove_outliers(data, 'Building Square Feet', lower = data['Building Square Feet'].quantile(0.01), upper = data['Building Square Feet'].quantile(0.99))
        data = remove_outliers(data, 'Estimate (Building)', lower = data['Estimate (Building)'].quantile(0.01), upper = data['Estimate (Building)'].quantile(0.99))
        # data = remove_outliers(data, 'Garage 1 Size', lower = data['Garage 1 Size'].quantile(0.01), upper = data['Garage 1 Size'].quantile(0.99))
    else:
        data['Log Building Square Feet'] = np.log(data['Building Square Feet'] + 0.00001)
        data['Log Estimate (Building)'] = np.log(data['Estimate (Building)'] + 0.00001)

    data['Log Building Square Feet'] = np.log(data['Building Square Feet'] + 0.00001)
    data['Log Estimate (Building)'] = np.log(data['Estimate (Building)'] + 0.00001)
    
    data = add_total_bedrooms(data)
    data = add_total_bathrooms(data)

    data = ohe_property_class(data)

    if is_test_set:
        X = data.loc[:, ['Bedrooms', 'Bathrooms', 'Log Building Square Feet', 'Log Estimate (Building)', 
                         'Property Class_278', 'Fireplaces', 
                         'Road Proximity', 'Central Air', 
                         'Repair Condition_1.0']]
        return X
    else:
        X = data.loc[:, ['Bedrooms', 'Bathrooms', 'Log Building Square Feet', 'Log Estimate (Building)',
                         'Property Class_278', 'Fireplaces', 
                         'Road Proximity', 'Central Air', 
                         'Repair Condition_1.0']]
        Y = data.loc[:, 'Log Sale Price']
        
        return X, Y


## Tune Model ##
def rmse(predicted, actual):
    return np.sqrt(np.mean((actual - predicted)**2))

data = pd.read_csv('cook_county_train.csv')
Train, Val = train_test_split(data, random_state=123)
X_train, Y_train = feature_engine(Train)
X_val = feature_engine(Val.drop(['Sale Price'], axis = 1), is_test_set=True)
Y_val = np.log(Val['Sale Price'])

lm_model = lm.LinearRegression(fit_intercept=True)
lm_model.fit(X_train, Y_train)

val_predictions = lm_model.predict(X_val)
rmse_value = rmse(np.exp(val_predictions), np.exp(Y_val))
print("RMSE on Train Set: ", rmse_value)


## Evaluate Model ##
test_data = pd.read_csv('cook_county_train.csv')
X_test = feature_engine(test_data, is_test_set=True)    
test_predictions = lm_model.predict(X_test)
Y_test = np.log(test_data['Sale Price'])

rmse_value = rmse(np.exp(test_predictions), np.exp(Y_test))
print("RMSE on Test Set: ", rmse_value)