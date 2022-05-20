class ZCL_UJR_PRE_PROCESS_EX definition
  public
  final
  create public .

public section.
  type-pools UJR0 .

  interfaces IF_BADI_INTERFACE .
  interfaces IF_UJR_WB_PRE_PROCESS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UJR_PRE_PROCESS_EX IMPLEMENTATION.


METHOD if_ujr_wb_pre_process~pre_process.
**********************************************************************
* Sample BAdI implementation
* - Disaggregation only happens on Entity dimension
* - Disaggregate value into all children base members averagely
* - It is highly recommended BAdI only implements for InputSchedule
*   which means I_MODULE_ID = 'MAN'
* Logic:
*	Package splitting will base on processed data. 
* If there are 30000 records before process and 50000 after that, 
* the package size is 40000; the records will be split into 2 packages.
*	Member access check will base on processed data:
* If user send data to member A on which he does not have access, 
* and the BADI replaces member A with member B which user has access, 
* then the data will be successfully submitted.
*	Only those data successfully passed all checksum (member access check, work status checks, etc.) will be written to cube.
* Next:
*	Script logic will be executed on those transaction data successfully written to cube.
*	Audit will be executed on those transaction data successfully written to cube.
********************************************************************************************************
    DATA: ls_entity TYPE ujr_s_dim_handler,
    lt_entity_members TYPE uja_t_dim_member, " dimension members of Entity
    lo_entity TYPE REF TO if_uja_dim_data, " Object Reference to Dimension
    lt_hier_info TYPE uja_t_hier, " Hierachies Infos
    ls_hier_info TYPE uja_s_hier,
    lt_hier_name TYPE uja_t_hier_name, " Hierarchies name list
    lt_attr_list TYPE uja_t_attr, " Attributes Infos
    ls_attr_list TYPE uja_s_attr,
    lt_attr_name TYPE uja_t_attr_name, " Attributes name list
    lf_non_base TYPE uj_flg, " 'X'=non base member; ' '=base member
    lt_entity_mbr TYPE uja_t_dim_member, " childen entity members
    l_num_base TYPE i, " number of children entity members
    lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <ls_dim_obj> TYPE ujr_s_dim_handler,
    <ls_record> TYPE any,
    <l_entity> TYPE uj_dim_member, " Entity member of current records
    <lt_entity_mbr> TYPE HASHED TABLE, " All entity members
    <ls_entity_mbr> TYPE any,
    <lf_calc> TYPE uj_flg, " 'Y'=non base member
    <lf_storedcalc> TYPE any, " 'Y'=non base member
    <l_base_mbr> TYPE uj_dim_member,
    <l_keyfigure> TYPE any. " Keyfigure
    " Find the Entity dimension by its type
    LOOP AT it_dim_obj ASSIGNING <ls_dim_obj> WHERE dim_type = uj00_cs_dim_type-entity.
      lo_entity ?= <ls_dim_obj>-dim_obj.
      ls_entity = <ls_dim_obj>.
    ENDLOOP.
    " Get hierachy (PARENTH1, PARENTH2 ...)
    lo_entity->get_hier_list( IMPORTING et_hier_info = lt_hier_info ).
    LOOP AT lt_hier_info INTO ls_hier_info.
      APPEND ls_hier_info-hier_name TO lt_hier_name.
    ENDLOOP.
    " Get necessary attributes (CALC and STORED_CALC)
    lo_entity->get_attr_list( IMPORTING et_attr_list = lt_attr_list ).
    LOOP AT lt_attr_list INTO ls_attr_list
    WHERE attribute_name = ujr0_c_attr_calc
      OR attribute_name = ujr0_c_attr_storedcalc.
    APPEND ls_attr_list-attribute_name TO lt_attr_name.
    ENDLOOP.
    " Get Members
    CALL METHOD lo_entity->read_mbr_data
      EXPORTING
        if_ret_hashtab = abap_true
        it_attr_list   = lt_attr_name " columns:attributes name list
        it_hier_list   = lt_hier_name " columns:hierarchies name list
      IMPORTING
        er_data        = lr_data.
    ASSIGN lr_data->* TO <lt_entity_mbr>.
    " preparation: create data structure and assign fields
    CREATE DATA lr_data LIKE LINE OF ct_array.
    ASSIGN lr_data->* TO <ls_record>.
    ASSIGN COMPONENT ls_entity-dimension OF STRUCTURE <ls_record> TO <l_entity>.
    ASSIGN COMPONENT ujr0_c_keyfigure OF STRUCTURE <ls_record> TO <l_keyfigure>.
    LOOP AT ct_array INTO <ls_record>.
      READ TABLE <lt_entity_mbr>
      WITH TABLE KEY (ujr0_c_member_id) = <l_entity>
      ASSIGNING <ls_entity_mbr>.
      IF sy-subrc = 0.
        " lf_non_base = <lf_calc>=Y OR <lf_storedcalc>=Y.
        ASSIGN COMPONENT ujr0_c_attr_calc OF STRUCTURE <ls_entity_mbr> TO <lf_calc>.
        lf_non_base = <lf_calc>.
        ASSIGN COMPONENT ujr0_c_attr_storedcalc OF STRUCTURE <ls_entity_mbr> TO <lf_storedcalc>.
        IF sy-subrc = 0 AND <lf_storedcalc> = ujr0_cs_calc-calculated_member.

          lf_non_base = ujr0_cs_calc-calculated_member.
        ENDIF.
        " Disaggregate non base member
        IF lf_non_base = ujr0_cs_calc-calculated_member.
          " A more precise version is to retrieve only accessible member of IS_USER
          CALL METHOD lo_entity->get_children_mbr
            EXPORTING
              i_parent_mbr     = <l_entity> " Parent
              i_level          = -99 " -99 = All children in any level; -1 = direct child
              if_only_base_mbr = abap_true " Only base member
            IMPORTING
              et_member        = lt_entity_mbr.
          " Re-calculate the keyfigure, divide by N = number of base members
          " Usually it doesn't matter with IF_CALC_DELTA = false,
          " if the operation is linear mathematically.
          DESCRIBE TABLE lt_entity_mbr LINES l_num_base.
          " Avoid divide by zero
          IF l_num_base > 0.
            <l_keyfigure> = <l_keyfigure> / l_num_base.
            " Copy N times with new base members
            LOOP AT lt_entity_mbr ASSIGNING <l_base_mbr>.
              <l_entity> = <l_base_mbr>.
              " When IF_CALC_DELTA = true, appending means the latest records take effects,
              " previous records with same dimension member will be overwritten.
              " The newly appended records will also be looped and processed.
              APPEND <ls_record> TO ct_array.
            ENDLOOP.
          ENDIF. " divide by zero
          " Remove the old one
          DELETE ct_array.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
METHOD IF_UJR_WB_PRE_PROCESS~PRE_PROCESS.

DATA: ls_account TYPE ujr_s_dim_handler
, lo_account TYPE REF TO if_uja_dim_data " Object Reference to Dimension
, lt_attr_list TYPE uja_t_attr " Attributes Infos
, ls_attr_list TYPE uja_s_attr
, lt_attr_name TYPE uja_t_attr_name " Attributes name list
, ls_message TYPE UJ0_S_MESSAGE
, lr_data TYPE REF TO data
, log_msg TYPE string
, s_round TYPE string
.

FIELD-SYMBOLS: <ls_dim_obj> TYPE ujr_s_dim_handler
, <ls_record> TYPE ANY
, <l_account> TYPE uj_dim_member " Entity member of current records
, <lt_account_mbr> TYPE HASHED TABLE " All entity members
, <ls_account_mbr> TYPE ANY
, <lf_rounding> TYPE ANY " Round to X decimal places
, <l_keyfigure> TYPE ANY " Keyfigure
.

LOOP AT it_dim_obj ASSIGNING <ls_dim_obj> WHERE dim_type = uj00_cs_dim_type-account.
lo_account ?= <ls_dim_obj>-dim_obj.
ls_account = <ls_dim_obj>.
ENDLOOP.

" Get necessary attributes

lo_account->get_attr_list( IMPORTING et_attr_list = lt_attr_list ).

LOOP AT lt_attr_list INTO ls_attr_list
WHERE attribute_name = 'ROUNDING'.

APPEND ls_attr_list-attribute_name TO lt_attr_name.

ENDLOOP.

" Get Members

CALL METHOD lo_account->read_mbr_data

EXPORTING
if_ret_hashtab = abap_true
it_attr_list = lt_attr_name " columns:attributes name list
IMPORTING
er_data = lr_data.

ASSIGN lr_data->* TO <lt_account_mbr>.

" Preparation: create data structure and assign fields

CREATE DATA lr_data LIKE LINE OF ct_array.

ASSIGN lr_data->* TO <ls_record>.

LOOP AT ct_array ASSIGNING <ls_record>.
ASSIGN COMPONENT ls_account-dimension OF STRUCTURE <ls_record> TO <l_account>.
IF sy-subrc = 0.
READ TABLE <lt_account_mbr> WITH TABLE KEY (ujr0_c_member_id) = <l_account> ASSIGNING <ls_account_mbr>.
CHECK sy-subrc = 0.

" Round keyfigure based on the ROUNDING property of account.

ASSIGN COMPONENT ujr0_c_keyfigure OF STRUCTURE <ls_record> TO <l_keyfigure>.
ASSIGN COMPONENT 'ROUNDING' OF STRUCTURE <ls_account_mbr> TO <lf_rounding>.
IF sy-subrc = 0.
MOVE <lf_rounding> TO s_round.
CONDENSE s_round NO-GAPS.

IF s_round CO '0123456789' AND s_round IS NOT INITIAL.

<l_keyfigure> = round( val = <l_keyfigure> dec = s_round mode = 2 ). "ROUND_HALF_UP

ENDIF.
ENDIF.

ELSE.
cl_ujk_logger=>log( 'INVALID DIMENSION!' ).
ENDIF.

ENDLOOP.

ENDMETHOD.  
ENDCLASS.
