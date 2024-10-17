#import the needed packages
import pandas as pd
import mysql.connector
from sqlalchemy import create_engine, text

#STEP 1 DATABASE ACCESSING & ENGINE
#access to the etl_database df
mydb = mysql.connector.connect(
  host="localhost",
  user="Luca Albertini",
  password="Luchino10!",
  database="etl_database"
)
#set the engine (SQLAlchemy) to communicate with SQL
engine = create_engine("mysql+mysqlconnector://Luca Albertini:Luchino10!@localhost/etl_database")


#LOCAL DF LOADING
#load the table we will import on MySQL
tab1 = pd.read_csv("in_data/FutureEmixFinalLong.csv")
#visualize it
print(tab1)
#df has 3 column: State (string), Period (string), Emissions (float)

#SET THE RECEIVING TABLE ON SQL
#let's create a table on which we will insert the data later on
mytable = mydb.cursor()
mytable.execute("CREATE TABLE emission_data (State VARCHAR(255) PRIMARY KEY, Period VARCHAR(255), Emission FLOAT(8, 6))")#this configuration build a table with the 3 cols above and with State as the primary key

#if mistekes were made, alter the table with the code:
# this is for adding two primary keys and avoid issues witht duplicates
#this is the query
alter_table_query = """
ALTER TABLE emission_data 
DROP PRIMARY KEY, 
ADD PRIMARY KEY (State, Period),
CHANGE COLUMN Emissions Emission FLOAT(8, 6);
"""
# this execute the query above
with engine.connect() as connection:
    connection.execute(text(alter_table_query))


#PANDA DF CONVERSION
# convert the tab1 to a format for mySQL table
tab1.to_sql('emission_data', con=engine, if_exists='append', index=False)

#UPDATE THE PANDA DF
tab1 = mydb.cursor()
#set the new value
sql = "INSERT INTO emission_data (State, Period, Emission) VALUES (%s, %s, %s)"
values = ("FakeState" , "Pre 2022" , "0.000001")
#add the line to the table
tab1.execute(sql, values)
#commite the changes
mydb.commit()
#print the output
print(tab1.rowcount, "record inserted.")

