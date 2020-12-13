# Ashrals Salary Report

This program generates a detailed salary report for a fake company names Ashrals, Ltd. This company sales costumes, shoes, and accessories to costume shops. The company purchases their products from 3 vendors and store their products in 2 different warehouses. 

## File Validation
The data files come in separately from the 3 different vendors (**_UNSORTED-CH20P4F20.TXT_**, **_UNSORTED-LA10P4F20.TXT_**, **_UNSORTED-NY30P4F20.TXT_**). The files are sorted and merged together into one data file (**_MERGEDIINVENTORY.TXT_**). The merged file is then validated.
* If there is an error with the vendor or warehouse ID:
    * Send record to error file (**_ERRORS-DSR.TXT_**)
* If there is no error with the vendor or warehouse ID:
    * Send record to inventory file for processing (**_CORRECT-INVENTORY.TXT_**)

## The inventory report generated:  
* Groups data by Vendor (_Los Angeles_, _Chicago_, _New York_)
    * Groups data by Warehouse within the Vendor (_Birmingham_ or _Huntsville_)
        * Groups data by Costume within the Warehouse
            * Costume Name
            * Costume  Size
            * Costume Type
            * Number in stock
            * ReOrder Price
            * Costume Price
* Displays the total for each costume group, warehouse group, and for all three vendors
        
## Files Included
* The code file: **_ASHRALS.cbl_** 
* The incoming **unsorted** data files: 
    * **_UNSORTED-CH20P4F20.TXT_**
    * **_UNSORTED-LA10P4F20.TXT_**
    * **_UNSORTED-NY30P4F20.txt_**
* The generated salary report: **_ASHRALS-DSR.txt_**
