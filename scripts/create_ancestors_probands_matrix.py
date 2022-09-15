#####
# Laurence Gagnon
# 22 juillet 2021
# Creation of the ancestors-probands table with the count of the appreance of each ancestors in the genealogy of each probands
#####

# Load packages
import sys
from collections import defaultdict
import statistics
import pandas as pd
import numpy as np


# Get the path + the file
my_path='/your/path/'
asc_file=my_path + 'genealogy/file'  ## A genealogy file with 4 columns including the individuals, fathers, mothers and sex
pro_file=my_path + 'probands/file'  ## A list of the probands

print(asc_file)
print(pro_file)

# First dictionary
asc=open(asc_file, 'r')
parents_dict=dict()
ind_lst=[]

for l in asc:
        allind=l.rstrip('\n\r').split(" ")[0]
        father=l.rstrip('\n\r').split(" ")[1]
        mother=l.rstrip('\n\r').split(" ")[2]
        parents_dict[allind] = [father,mother]
        ind_lst.append(allind)
asc.close()

# Open the probands
pro=open(pro_file, 'r')
pro_lst=[]

for l in pro:
        pro_ind=l.rstrip().split(" ")[0]
        pro_lst.append(pro_ind)
pro.close()
print(pro_lst[0:10])

# Create the recursive function
def find_ancestor(n):
    if (n == "0") :
        return 
    
    else :
        dad=parents_dict[n][0]
        mom=parents_dict[n][1]
  
        if (dad == "0") or (mom == "0") : 
            return 
        
        else :
            find_ancestor(dad)
            find_ancestor(mom)
            
            ancestor_lst.append(dad)
            ancestor_lst.append(mom)
            return
        
        return


# Run the recursive function
ancestor_dict=dict()

print('Start the recursive function')
for ind in pro_lst :
    ancestor_lst = []
    find_ancestor(ind)
    ancestor_dict[ind]=ancestor_lst


# Create le DataFrame
## List of the probands
print(len(pro_lst))

## List of everyone (ancestors + probands)
print(len(ind_lst))

## Remove probands in the list of everyone to just keep the ancestors
ancestor_lst = [x for x in ind_lst if x not in pro_lst]
print(len(ancestor_lst))


## Create an empty DataFrame
df = pd.DataFrame(columns = pro_lst, index = ancestor_lst )


# Fill the DataFrame
for probands in df.columns :
    for ID in df.index:

        if ID in set(ancestor_dict[probands]):
            df.loc[[ID], [probands]] = ancestor_dict[probands].count(ID)

        else:
            df.loc[[ID], [probands]] = 0



# save the DataFrame
df.to_csv("/pat/to/the/saving/file", sep=" ")









