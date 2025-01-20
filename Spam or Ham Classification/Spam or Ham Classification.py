# Lisa Zhao

# Developed a text classification model using logistic regression to identify spam emails based on keyword frequency and email length features.
# Performed feature engineering, including tokenization and length-based features, to improve classification accuracy.
# Achieved robust performance using a train-validation-test split approach, validated with confusion matrix and accuracy metrics.


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_theme(style = "whitegrid", 
        color_codes = True,
        font_scale = 1.5)
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix


## Import Data ##
data = pd.read_csv("data.csv")

original_train, test = train_test_split(data, test_size=0.1, random_state=123)

original_train['email'] = original_train['email'].str.lower()
test['email'] = test['email'].str.lower()

# first_ham = original_train.loc[test['spam'] == 0, 'email'].iloc[0]
# first_spam = original_train.loc[test['spam'] == 1, 'email'].iloc[0]
# print("Ham Email:")
# print(first_ham)
# print("-------------------------------------------------")
# print("Spam Email:")
# print(first_spam)

train, val = train_test_split(original_train, test_size = 0.1, random_state = 123)


## Feature Engineering ##
def words_in_texts(words, texts):
    indicator_array = []
    for w in words:
        a = texts.str.contains(w).astype(int)
        indicator_array.append(a)

    indicator_array = np.array(indicator_array).T
    return indicator_array


## Exploratory Data Analysis ##
df = train.copy()
df = train.reset_index(drop=True) 
plt.figure(figsize=(8,6))

# define list of words
words = ['here', 'date', 'our', 'like', 'post', 'you']

words_array = words_in_texts(words, df['email'])
word_df = pd.DataFrame(words_array, columns = words)

word_df['type'] = df['spam']
word_df['type'].replace([0,1], ['ham', 'spam'], inplace = True)

word_melt = word_df.melt("type")
final_df = pd.DataFrame(word_melt.groupby(['type', 'variable']).agg(sum)['value']).reset_index()

final_df.loc[final_df['type'] == 'ham', 'value'] = final_df.loc[final_df['type'] == 'ham', 'value'] / sum(word_melt['type'] == 'ham')
final_df.loc[final_df['type'] == 'spam', 'value'] = final_df.loc[final_df['type'] == 'spam', 'value'] / sum(word_melt['type'] == 'spam')

sns.barplot(final_df, x = 'variable', y = 'value', hue = 'type')
plt.xlabel('Words')
plt.ylabel('Proportions of Emails')
plt.title('Frequency of Words in Ham/Spam Emails')

plt.tight_layout()
# plt.show()


df = train.copy()
def remove_outliers(data, variable, lower=-np.inf, upper=np.inf):
    return data[(data[variable] <= upper) & (data[variable] > lower)]
    
# length of subject as a feature
df['sub_len'] = df['subject'].str.len()
sns.boxplot(x = df['spam'], y = df['sub_len'])

# length of email as a feature
df['email_len'] = df['email'].str.len()
df = remove_outliers(df, 'email_len', lower = df['email_len'].quantile(0.05), upper = df['email_len'].quantile(0.95))

plt.subplot(1, 2, 1) 
sns.boxplot(x = df['spam'], y = df['sub_len'])
plt.ylabel('subject length')
plt.suptitle('length of subject and emails respectively in spam/ham emails')

plt.subplot(1, 2, 2) 
sns.boxplot(x = df['spam'], y = df['email_len'])
plt.ylabel('email length')
plt.tight_layout()
# plt.show()


## Classification ##
def process_data(data):
    # words selection
    words = ['date', 'our', 'congrats', 'html', 'please', 'offer', 'url', '.org']
    word_columns = words_in_texts(words, data['email']) 

    # length of subject
    data = data.fillna('')
    data['sub_len'] = data['subject'].str.len()
    sub_len_columns = data['sub_len']

    #length of emails
    data['email_len'] = data['email'].str.len()
    email_len_columns = data['email_len']
    data = remove_outliers(data, 'email_len', lower = data['email_len'].quantile(0.05), upper = data['email_len'].quantile(0.95))

    X = np.concatenate((word_columns, pd.DataFrame(sub_len_columns), pd.DataFrame(email_len_columns)),axis=1)
    return X


## Tune Model ##
X_train = process_data(train)
Y_train = train['spam']

model = LogisticRegression(fit_intercept=True, penalty='l1', solver='liblinear')
model.fit(X_train,Y_train)

train_predictions = model.predict(X_train)
training_accuracy = np.mean(train_predictions == train["spam"])
print("Training Accuracy: ", training_accuracy)


X_val = process_data(val)
val_predictions = model.predict(X_val)
val_accuracy = np.mean(val_predictions == val["spam"] )
print("Validation Accuracy: ", val_accuracy)


## Evaluate Model ##
X_test = process_data(test)
test_predictions = model.predict(X_test)
test_accuracy = np.mean(test_predictions == test["spam"] )
print("Testing Accuracy: ", test_accuracy)

cm = confusion_matrix(test['spam'], test_predictions)
plt.figure(figsize=(6, 5))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=['Ham', 'Spam'], yticklabels=['Ham', 'Spam'])
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.title('Confusion Matrix')
plt.show()