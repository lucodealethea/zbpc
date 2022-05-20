# zbpc

Some utilities for BPC, namely a generic hierarchy browser - ZBPC_BROWSE_HIER - to retrieve sub-nodes and base members of any node for any BPC hierarchy combining with member properties and description. UI is ALV grid. ABAP RTTS ABAP eventually to be be replaced with HANA/new ABAP 753 Hierarchy Functions to allow more transparent consumption.

BadIs for UJ_CUSTOM_LOGIC Enhancement Spot

ZCL_BPC_ADBC_CUSTOM_LOGIC: 
BPC DATAMANAGER Logic Script Calling UJ_CUSTOM_LOGIC Badi (ABAP Class via ADBC) to pass SQL string returning BPC Model (CT_DATA like) dataset.
This implies having DDIC View created (via CDS) on top of BPC Model(InfoCube-750 or aDSO-753 depending on SAP_ABA release ). Ideally, Views also Available on BPC Dimensions and BPC Dimension Hierarchies

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



