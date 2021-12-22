# zbpc

Some utilities for BPC, namely a generic hierarchy browser - ZBPC_BROWSE_HIER - to retrieve sub-nodes and base members of any node for any BPC hierarchy combining with member properties and description. UI is ALV grid.


BadIs for BPC 10.
https://answers.sap.com/questions/10904355/usage-of-the-write-back-pre-process-badi.html

BadIs for BPC 10.1 implementing UJ_CUSTOM_LOGIC for more complex allocation process: 
- ZCL_BPC_ALLOC_SGA / ZCL_BPC_SGA_HOURS_BASED_ALLOC1 / ZCL_BPC_ALLOC_SGA2 ->IF_UJ_CUSTOM_LOGIC~EXECUTE 
along with 
- DM prompt file 2_SGA_ANA_INC_BASED_ALLOCATION, 2_SGA_HOURS_BASED_ALLOCATION_RED
and 
- 3 Logic Scripts OBS_MBRS_VARIABLES.LGF, CUST_BADI_RED_ALLOC.lgf, CUST_BADI_SGA_ALLOC.lgf

Requires a Dedicated Entity Hierarchy to aggregate the drivers and define receiver member for the allocation posting.


BPC Embedded Custom planning Function Type and Class/Methods



