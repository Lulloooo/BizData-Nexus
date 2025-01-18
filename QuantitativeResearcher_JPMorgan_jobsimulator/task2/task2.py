############################## PACKAGES
import pandas as pd
from datetime import datetime
import numpy as np

############################## DATA LOADING
data = pd.read_csv("task1/Act&Pred_prices.csv")
data = pd.DataFrame(data)
data["Dates"] = pd.to_datetime(data["Dates"], format = "%Y-%m-%d")
#build a new editable database
data['month'] = data['Dates'].dt.strftime('%m-%Y')
#print(data)

############################## FUNCTIONS
#Display prices at a given date
def actual_and_estimates(month):
    # Initialize the DataFrame to store results regardless of conditions
    results_df = pd.DataFrame(columns=['Date', 'Actual', 'Estimated'])
    # Check if the entered month exists in the 'month' column
    if month in data['month'].values:
        # Print the Prices and Predicted values for rows where the 'month' matches
        results = data.loc[data['month'] == month, ['Prices', 'Predicted']]
             
        # Print the actual and estimated values for each row
        for index, row in results.iterrows():
            #print(f"Natural gas prices (actual and estimated) for {month} are:")
            #print(f"Actual value is {row['Prices']} and estimated value is {row['Predicted']}.")
            # Create a DataFrame for the current row
            current_row_df = pd.DataFrame({
                'Date': [month],
                'Actual': [row['Prices']],
                'Estimated': [row['Predicted']]
            })
            # Concatenate the current row DataFrame to results_df
            results_df = pd.concat([results_df, current_row_df], ignore_index=True)
            #print(results_df)
    else:
        print(f"No data found for the specified month: {month}.")

    # Return the DataFrame with results for the month
    return results_df
#Check if withdrawl date > injection date, if so price the contract
def contractValidityCheck(df):
    # Ensure the date format consistency
    df['InjectionDate'] = pd.to_datetime(df['InjectionDate'], format='%m-%d-%Y')
    df['WithdrawalDate'] = pd.to_datetime(df['WithdrawalDate'], format='%m-%d-%Y')
    
    # Define the new column based on the condition
    df['ValidContractValue'] = np.where(
        df['InjectionDate'] < df['WithdrawalDate'],
        df['ContractValue'],
        np.nan
    )
    
    # Convert back to the desired format for display purposes
    df['InjectionDate'] = df['InjectionDate'].dt.strftime('%m-%d-%Y')
    df['WithdrawalDate'] = df['WithdrawalDate'].dt.strftime('%m-%d-%Y')
    
    return df
#Display the values of the contract
def valuesprint(df):
    for index, row in df.iterrows():
        injection_date = row['InjectionDate']
        withdrawal_date = row['WithdrawalDate']
        ContractValue = row["ContractValue"]
        if pd.notna(ContractValue):  # Check if ContractValue is not NaN
            print(f"For Injection date \"{injection_date}\" and Withdrawal date \"{withdrawal_date}\", the contract value is \"{ContractValue:.2f}\".")
        else:
            print(f"Injection date \"{injection_date}\" is after Withdrawal date \"{withdrawal_date}\". Re-enter the parameters.")

############################## INPUTS
### Ask for the necessary inputs
print("Insert the following information about the contract:")
print("----------------------------------------------------")

### Injection Date
print("Injection Dates (MM-DD-YYYY):")
injdates = []
# number of elements as input
ninj = int(input("Enter how many dates you want to consider: "))
print(f"{ninj}")
# iterating till the range
for i in range(ninj):
    injdates2 = (input("Enter date in MM-DD-YYYY format: "))
    try:
    # adding the element
        date_obj = datetime.strptime(injdates2, "%m-%d-%Y")
        injdates.append(date_obj)  # Add datetime object to the list
    except ValueError:
        print(f"Invalid date forma for {injdates2}. Please enter date in MM-DD-YYYY format. ")    
print(f"Injection Dates are:")
for date in injdates:
    print(date.strftime("%m-%d-%Y")) 
print("_____________________________")
#injection values
print(f"At the injection dates natural gas purchasing price is:")
injres_all = pd.DataFrame(columns=['InjectionDate', 'Actual', 'Estimated'])
for date in injdates:
    injdatemth = date.strftime('%m-%Y')
    injval = actual_and_estimates(injdatemth)
        # Check if the results DataFrame is not empty
    if not injval.empty:
        # Add the 'datesOff' column with the formatted date value
        # Drop the 'Date' column from injval
        injval = injval.drop(columns=['Date'])
        injval['InjectionDate'] = date.strftime('%m-%d-%Y')
        injval = injval.reindex(['InjectionDate','Actual','Estimated'], axis=1)
        # Append to the all_results_df DataFrame
        injres_all = pd.concat([injres_all, injval], ignore_index=True)
  
print("Injection dates and relative natural gas prices:")
print(injres_all)

### Withdrawal Date
print("Withdrawal Dates (MM-DD-YYYY):")
witdates = []
# number of elements as input
nwit = int(input("Enter how many dates you want to consider: "))
print(f"{nwit}")
# iterating till the range
for i in range(nwit):
    witdates2 = (input("Enter date in MM-DD-YYYY format: "))
    try:
    # adding the element
        witdate_obj = datetime.strptime(witdates2, "%m-%d-%Y")
        witdates.append(witdate_obj)  # Add datetime object to the list
    except ValueError:
        print(f"Invalid date forma for {witdates2}. Please enter date in MM-DD-YYYY format. ")    
print(f"Withdrawal Dates are:")
for witdate in witdates:
    print(witdate.strftime("%m-%d-%Y")) 
print("_____________________________")
#withdrawal values
print(f"At the Withdrawal dates natural gas purchasing price is:")
witres_all = pd.DataFrame(columns=['WithdrawalDate', 'Actual', 'Estimated'])
for witdate in witdates:
    witdatemth = witdate.strftime('%m-%Y')
    witval = actual_and_estimates(witdatemth)
        # Check if the results DataFrame is not empty
    if not witval.empty:
        # Add the 'datesOff' column with the formatted date value
        # Drop the 'Date' column from injval
        witval = witval.drop(columns=['Date'])
        witval['WithdrawalDate'] = witdate.strftime('%m-%d-%Y')
        witval = witval.reindex(['WithdrawalDate','Actual','Estimated'], axis=1)
        # Append to the all_results_df DataFrame
        witres_all = pd.concat([witres_all, witval], ignore_index=True)
#print the results
print("Withdrawal dates and relative natural gas prices:")
print(witres_all)

### Injection and Withdrawal rates
print("Input the Injection (Rate1) and Withdrawl (Rate2) Rate (NN.DD):")
injwtrates = []
# iterating till the range
for i in range(0, 2):
    rate = float(input(f"Input the rate number {i+1}:"))
    # adding the element
    injwtrates.append(rate)  
print(f"Injection/Withdrawl Rates are (respectively): {injwtrates}")
print("___________________________________")

### Maximum volume
print("Input the Maximum Volume:")
vol = float(input())
print(f"Maximum Volume is {vol}")
print("___________________________________")

### Storage Costs
# creating an empty list
print("Input the Storage Costs (II.DD):")
storcost = []
# number of elements as input
n = int(input("Enter number of elements : "))
# iterating till the range
for i in range(0, n):
    storcos = float(input(f"Input the storage cost {i+1}:"))
    # adding the element
    storcost.append(storcos)  
print(f"Storage costs are {storcost}")
print("___________________________________")
print(f"Total Storage cost is:")
totstorcost = sum(storcost)
print(totstorcost)

############################## FINAL DF
#Build a combined df for dates considering all the combinations
# Create all combinations
dates_df = pd.merge(injres_all, witres_all, how='cross')
#sort it by Injection Date
dates_df = dates_df.sort_values(by=['InjectionDate', 'WithdrawalDate']).reset_index(drop=True)
#rename the column
dates_df= dates_df.rename(columns={
    'Actual_x': 'InjActual',
    "Estimated_x": "InjExpected",
    'Actual_y': 'WitActual',
    "Estimated_y" : "WitExpected"
})
#set up the final df
final_df = dates_df
#add injection rates
final_df["InjectionRate"] = injwtrates[0]
#add the withdrawal rate
final_df["WithdrawalRate"] = injwtrates[1]
#add the volumn
final_df["Volume"] = vol
#add the costs
final_df["StorageCosts"] = totstorcost
# Determine injection price by choosing actual if available, otherwise expected
final_df['InjectionPrice'] = np.where(
    final_df['InjActual'].notna(), final_df['InjActual'], final_df['InjExpected']
)
# Determine withdrawal price by choosing actual if available, otherwise expected
final_df['WithdrawalPrice'] = np.where(
    final_df['WitActual'].notna(), final_df['WitActual'], final_df['WitExpected']
)
pd.set_option('display.max_columns', None)
#compute the contract value
final_df["ContractValue"] = ((final_df["WithdrawalPrice"] - final_df["InjectionPrice"])*final_df["Volume"]) - (final_df["InjectionRate"] + final_df["WithdrawalRate"] + final_df["StorageCosts"])


############################## RESULTS (CONTRACT VALUE)
print("CONTRACTS VALUE")
print("_______________________________________")
#build a df
printed_df = final_df.loc[:, ['InjectionDate', 'WithdrawalDate', "ContractValue"]]
printed_df = contractValidityCheck(printed_df)
printed_df = printed_df.loc[:, ['InjectionDate', 'WithdrawalDate', "ValidContractValue"]]
printed_df = printed_df.rename(columns = {"ValidContractValue": "ContractValue"})
#display contract value                      
valuesprint(printed_df)
#and in a df
print("_______________________________________")
print("Summarizing with a clear view:")
print("")
print(printed_df)