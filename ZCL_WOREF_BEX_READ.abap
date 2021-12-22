class ZCL_WOREF_BEX_READ definition
  public
  final
  create public .

public section.

  interfaces IF_RSPLFA_SRVTYPE_IMP_EXEC .

  types:
    ty_t_w3query   TYPE TABLE OF w3query .
  types:
    ty_columns     TYPE TABLE OF rrx_x_axis_data .
  types:
    ty_rows        TYPE TABLE OF rrx_x_axis_data .
  types:
    ty_cells       TYPE HASHED TABLE OF bapi6111cd WITH UNIQUE KEY cell_ordinal .
  types:
    ty_ht_bex_data TYPE HASHED TABLE OF zbpce_bex_data_st WITH UNIQUE DEFAULT KEY WITH NON-UNIQUE SORTED KEY k2 COMPONENTS query_id recordno .

  class-data:
    gt_message TYPE TABLE OF bapireturn .
  class-data GS_MESSAGE type BAPIRETURN .
  class-data GT_BEX type TY_HT_BEX_DATA .

  methods PARALLEL_PROCESS_END
    importing
      !P_TASK type CLIKE
    exporting
      !E_T_DATA type ZBPCE_BEX_DATA_TT .
  class-methods BEX_READ
    importing
      !I_QUERY type RSBOLAP_WSP_QUERY_NAME
      !I_T_PARAMETER type TY_T_W3QUERY
    exporting
      !E_T_MESSAGES type RRMS_T_MESG
      !E_T_AXIS_DATA_COLUMNS type TY_COLUMNS
      !E_T_AXIS_DATA_ROWS type TY_ROWS
      !E_T_CELL_DATA type TY_CELLS
      !E_T_DATA type ZBPCE_BEX_DATA_TT
    exceptions
      INVALID_INPUT
      INVALID_VARIABLE_VALUES
      NO_APPLICABLE_DATA
      NO_AUTHORITY
      ABORT .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA p_sent_jobs TYPE i .
    DATA p_received_jobs TYPE i .
    DATA p_current_jobs TYPE i .
ENDCLASS.



CLASS ZCL_WOREF_BEX_READ IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_WOREF_BEX_READ=>BEX_READ
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_QUERY                        TYPE        RSBOLAP_WSP_QUERY_NAME
* | [--->] I_T_PARAMETER                  TYPE        TY_T_W3QUERY
* | [<---] E_T_MESSAGES                   TYPE        RRMS_T_MESG
* | [<---] E_T_AXIS_DATA_COLUMNS          TYPE        TY_COLUMNS
* | [<---] E_T_AXIS_DATA_ROWS             TYPE        TY_ROWS
* | [<---] E_T_CELL_DATA                  TYPE        TY_CELLS
* | [<---] E_T_DATA                       TYPE        ZBPCE_BEX_DATA_TT
* | [EXC!] INVALID_INPUT
* | [EXC!] INVALID_VARIABLE_VALUES
* | [EXC!] NO_APPLICABLE_DATA
* | [EXC!] NO_AUTHORITY
* | [EXC!] ABORT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD BEX_READ.

    DATA:
      l_r_page          TYPE REF TO cl_rsr_www_page,
      l_r_request       TYPE REF TO cl_rsr_request,
      l_r_view          TYPE REF TO cl_rsr_www_view,
      l_r_data_provider TYPE REF TO cl_rsr_www_data_provider,
      l_r_data_set      TYPE REF TO cl_rsr_data_set,
      l_r_parameter     TYPE REF TO cl_rsr_parameter,
      l_pageno          TYPE rrxw3_pageno,
      l_return_code     TYPE rrxw3_return_code,
      l_stat_event_cnt  TYPE int4,
      l_t_messages      TYPE rrms_t_mesg.

    FIELD-SYMBOLS:
      <l_sx_data>       TYPE rsrds_sx_data_set_20a_flat1.

    TRY.
*     Create the parameter object
        CREATE OBJECT l_r_parameter EXPORTING i_t_query = i_t_parameter[].

*     Add command for instanciation
        CALL METHOD l_r_parameter->add( i_id = cl_rsr_www_page=>c_parameter_cmd i_value = cl_rsr_www_page=>c_cmd_set_data_provider ).
        CALL METHOD l_r_parameter->add( i_id = cl_rsr_www_data_provider=>c_parameter_data_provider_name i_value = 'DP' ).
        IF i_query IS NOT INITIAL.
          CALL METHOD l_r_parameter->add( i_id = cl_rsr_www_view=>c_parameter_query i_value = i_query ).
        ENDIF.
*      IF i_view_id IS NOT INITIAL.
*        CALL METHOD l_r_parameter->add( i_id = cl_rsr_www_data_provider=>c_parameter_data_provider_id i_value = i_view_id ).
*      ENDIF.

*     Create the data provider
        cl_rsr_www_page=>process_cmd(
          EXPORTING
            i_r_parameter           = l_r_parameter
          IMPORTING
            e_pageno                = l_pageno
            e_return_code           = l_return_code
            e_r_page                = l_r_page ).

        IF l_return_code = cl_rsr_www_page=>c_rc_abort.
          RAISE invalid_input.
        ENDIF.

*     Process the variables
        CALL METHOD l_r_parameter->add( i_id = cl_rsr_www_page=>c_parameter_cmd i_value = cl_rsr_www_page=>c_cmd_process_variables ).
        CALL METHOD l_r_parameter->add( i_id = cl_rsr_www_page=>c_parameter_subcmd i_value = cl_rsr_www_variable_screen=>c_cmd_var_submit ).

        cl_rsr_www_page=>process_cmd(
          EXPORTING
            i_pageno                = l_pageno
            i_r_parameter           = l_r_parameter
          IMPORTING
            e_return_code           = l_return_code ).

        APPEND LINES OF l_t_messages TO e_t_messages[].
        IF l_return_code = cl_rsr_www_page=>c_rc_abort.
          RAISE invalid_input.
        ENDIF.

*     Get the data provider
        l_r_page->get_data_provider(
           EXPORTING
             i_name = 'DP'
           IMPORTING
             e_r_ref = l_r_data_provider ).

        IF l_r_data_provider IS INITIAL.
          RAISE invalid_input.
        ENDIF.

        l_r_view ?= l_r_data_provider.
        IF l_r_view->n_not_assigned = rs_c_true.
          RAISE invalid_input.
        ELSE.
          l_r_request = l_r_view->n_r_request.
        ENDIF.

*     Variables submitted?
        l_r_request->variables_submit(
          EXCEPTIONS
            bad_value_combination = 1
            x_message             = 2
            user_not_authorized   = 3
            OTHERS                = 4 ).

        IF sy-subrc <> 0.
          RAISE invalid_variable_values.
        ENDIF.

*     Read the data
        CALL METHOD l_r_request->read_data
          EXCEPTIONS
            no_processing = 1.

*     Output available ?
        IF l_r_request->n_sx_output-no_data = rs_c_true.
          CALL METHOD l_r_request->free.
          RAISE no_applicable_data.
        ELSEIF l_r_request->n_sx_output-no_authorization = rs_c_true.
          CALL METHOD l_r_request->free.
          RAISE no_authority.
        ELSEIF l_r_request->n_sx_output-bad_value_combination = rs_c_true.
          CALL METHOD l_r_request->free.
          RAISE invalid_variable_values.
        ENDIF.

*     Prepare the output
        l_r_data_set = cl_rsr_data_set=>get( l_r_request ).
        CALL METHOD l_r_data_set->refresh( cl_rsr_data_set=>c_version_20a_flat1 ).

        DATA: l_t_temp_axis_data_columns TYPE rsrds_t_axis_data,
              l_s_temp_axis_data_columns TYPE rsrds_s_axis_data,
              l_s_axis_data_columns      TYPE rrx_x_axis_data,
              l_t_temp_axis_data_rows    TYPE rsrds_t_axis_data,
              l_s_temp_axis_data_rows    TYPE rsrds_s_axis_data,
              l_s_axis_data_rows         TYPE rrx_x_axis_data,
              l_t_temp_axis_data_slicer  TYPE rsrds_t_axis_data,
              l_s_temp_axis_data_slicer  TYPE rsrds_s_axis_data,
              l_s_axis_data_slicer       TYPE rrx_x_axis_data,
              l_t_temp_attr_data_rows    TYPE rsrds_t_attr_data,
              l_s_temp_attr_data_rows    TYPE rsrds_s_attr_data,
              l_s_attr_data_rows         TYPE rrx_x_attr_data,
              l_t_temp_attr_data_columns TYPE rsrds_t_attr_data,
              l_s_temp_attr_data_columns TYPE rsrds_s_attr_data,
              l_s_attr_data_columns      TYPE rrx_x_attr_data.

*     Set the output
        ASSIGN l_r_data_set->n_sx_version_20a_flat1 TO <l_sx_data>.

        e_t_cell_data[]         = <l_sx_data>-cell_data.

        LOOP AT <l_sx_data>-axis_data_columns INTO l_s_temp_axis_data_columns.
          MOVE-CORRESPONDING l_s_temp_axis_data_columns TO l_s_axis_data_columns.
          INSERT l_s_axis_data_columns INTO TABLE e_t_axis_data_columns.
        ENDLOOP.

        LOOP AT <l_sx_data>-axis_data_rows INTO l_s_temp_axis_data_rows.
          MOVE-CORRESPONDING l_s_temp_axis_data_rows TO l_s_axis_data_rows.
          INSERT l_s_axis_data_rows INTO TABLE e_t_axis_data_rows.
        ENDLOOP.

        DATA: ls_bex_data       TYPE zbpce_bex_data_st,
              lv_col_index      TYPE syst_tabix,
             lv_row_index      TYPE syst_tabix,
              lv_row_index_last TYPE syst_tabix,
              lv_cell_index     TYPE rrtcellordinal,
              ls_cell_data      TYPE LINE OF ty_cells,
              lv_column_count   TYPE i.

        lv_column_count = lines( e_t_axis_data_columns ) .
        lv_row_index_last = -1 .

        LOOP AT e_t_axis_data_rows INTO l_s_axis_data_rows .
          lv_row_index = l_s_axis_data_rows-tuple_ordinal .
          CLEAR ls_bex_data .
          ls_bex_data-query_id = i_query .
          ls_bex_data-recordno = lv_row_index .
          ls_bex_data-chanm = l_s_axis_data_rows-chanm .
          ls_bex_data-chavl = l_s_axis_data_rows-chavl .
          REPLACE '#' IN l_s_axis_data_rows-chavl WITH '' .
          INSERT ls_bex_data INTO TABLE e_t_data .

          IF lv_row_index_last <> lv_row_index .
            LOOP AT e_t_axis_data_columns INTO l_s_axis_data_columns .
              lv_col_index = sy-tabix .

              CLEAR ls_bex_data .
              ls_bex_data-query_id = i_query .
              ls_bex_data-recordno = lv_row_index .
              ls_bex_data-chanm = l_s_axis_data_columns-caption .
              TRANSLATE ls_bex_data-chanm TO UPPER CASE .
              ls_bex_data-chavl = '' .

              lv_cell_index = ( lv_row_index * lv_column_count ) + lv_col_index - 1 .
              CLEAR ls_cell_data .
              READ TABLE e_t_cell_data INTO ls_cell_data WITH TABLE KEY cell_ordinal = lv_cell_index .
              IF sy-subrc EQ 0 .
                ls_bex_data-value = ls_cell_data-value .
                INSERT ls_bex_data INTO TABLE e_t_data .
              ENDIF .
            ENDLOOP .
            lv_row_index_last = l_s_axis_data_rows-tuple_ordinal .
          ENDIF .
        ENDLOOP .

        SORT e_t_data BY recordno chanm .

*     Free the resources
        CALL METHOD l_r_data_set->free.
        CALL METHOD l_r_request->free.

      CATCH cx_root.
        RAISE abort.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_WOREF_BEX_READ->IF_RSPLFA_SRVTYPE_IMP_EXEC~EXECUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_rsplfa_srvtype_imp_exec~execute.

    FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_poc_al01.
    DATA:lt_data TYPE TABLE OF zbpce_pal_poc_al01.
    DATA: lt_charsel            TYPE rsplf_t_charsel,
          ls_charsel            LIKE LINE OF lt_charsel,
          lv_rfc_destination    TYPE rfc_dest,
          lv_fiscper_prior      TYPE rsfiscper,
          lv_fiscper            TYPE rsfiscper,
          lv_fiscper_next       TYPE rsfiscper,
          lv_year(4)            TYPE n,
          lv_month(2)           TYPE n,
          lv_aysayisi(3)        TYPE p DECIMALS 1,
          lv_kalan(2)           TYPE n,
          ls_comp_code          TYPE /bi0/pcomp_code,
          lv_tutar              TYPE f,
          lv_compcode           TYPE /bi0/oicomp_code,
          lv_version            TYPE /bic/oibpcversin,
          lv_project            TYPE /bi0/oiproject,
          ls_project            TYPE /bi0/pproject,
          lv_dbs                TYPE f,
          lv_dss                TYPE f,
          lv_kdv                TYPE f,
          lv_vade               TYPE f,
          lv_ay                 TYPE f,
          lv_yil                TYPE f,
          lv_ok                 TYPE i,
          gt_message            TYPE TABLE OF bapireturn,
          gs_message            TYPE bapireturn,
          lv_bakiye_degerlenmis TYPE f,
          lv_gecmis_bakiye      TYPE f,
          lv_cari_tutar_10      TYPE f,
          lv_bakiye             TYPE f,
          l_dummy               TYPE string,
          lt_result             TYPE TABLE OF zbpce_pal_poc_al01,
          ls_result             TYPE zbpce_pal_poc_al01,
          ls_wbs_elemnt         TYPE /bi0/pwbs_elemt.
    FIELD-SYMBOLS : <comp_code> TYPE any.
*    DATA: BEGIN OF ls_result .
*            INCLUDE TYPE zrspl_pal121 .
*            DATA: map_id TYPE zbpc_mt_map-zkural.
*    DATA: END OF ls_result .
*
*    DATA: lt_result LIKE HASHED TABLE OF ls_result WITH UNIQUE DEFAULT KEY .
*
*    DATA: BEGIN OF ls_result2 .
*            INCLUDE TYPE zrspl_pal121 .
*            DATA: map_id TYPE zbpc_mt_map-zkural.
*    DATA: END OF ls_result2 .
*
*    DATA: lt_result2 LIKE HASHED TABLE OF ls_result2 WITH UNIQUE DEFAULT KEY .
*
*    FIELD-SYMBOLS : <ls_pal121> TYPE zrspl_pal121.

    CALL METHOD cl_rsplfr_controller=>get_active_selection
      RECEIVING
        r_t_charsel = lt_charsel.

    CLEAR ls_charsel.
    READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN' .
    lv_version = ls_charsel-low.

*    SELECT SINGLE * INTO ls_version FROM /bic/ppvers WHERE objvers = 'A' AND /bic/pvers = lv_version .
*    CHECK ls_version-/bic/pfbit IS NOT INITIAL .
*    CHECK ls_version-/bic/ppbas IS NOT INITIAL .
*    lv_fiscper_prior = ls_version-/bic/pfbit .
*
    CLEAR ls_charsel.
    READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0COMP_CODE' .
    lv_compcode = ls_charsel-low.

**********************************************************************

    DATA: i_t_parameter TYPE ty_t_w3query,
          ls_variables  TYPE LINE OF ty_t_w3query,
          ls_bex        TYPE LINE OF ty_ht_bex_data,
          lv_index      TYPE rrttupleordinal,
          lv_recno      TYPE rrttupleordinal.

    FIELD-SYMBOLS: <field> TYPE any .
  LOOP AT c_th_data ASSIGNING <ls_data>.
    <ls_data>-0amount_oc = 0.
    <ls_data>-gom_poc  = 0.
     <ls_data>-per_poc = 0.
  ENDLOOP.
*    ls_variables-name  = 'VAR_NAME_1' .
*    ls_variables-value = 'PVERS_S' .
*    APPEND ls_variables TO i_t_parameter .
*
*    ls_variables-name  = 'VAR_NAME_2' .
*    ls_variables-value = 'COMP_CODE_SM' .
*    APPEND ls_variables TO i_t_parameter .
*
*    ls_variables-name  = 'VAR_SIGN_1' .
*    ls_variables-value = 'I' .
*    APPEND ls_variables TO i_t_parameter .
*
*    ls_variables-name  = 'VAR_OPERATOR_1' .
*    ls_variables-value = 'EQ' .
*    APPEND ls_variables TO i_t_parameter .
*
*    ls_variables-name  = 'VAR_VALUE_EXT_1' .
*    ls_variables-value = 0   . "lv_version .
*    APPEND ls_variables TO i_t_parameter .
*
*    ls_variables-name  = 'VAR_SIGN_2' .
*    ls_variables-value = 'I' .
*    APPEND ls_variables TO i_t_parameter .
*
*    ls_variables-name  = 'VAR_OPERATOR_2' .
*    ls_variables-value = 'EQ' .
*    APPEND ls_variables TO i_t_parameter .
*
*    ls_variables-name  = 'VAR_VALUE_EXT_2' .
*    ls_variables-value = 0. "lv_compcode .
*    APPEND ls_variables TO i_t_parameter .


**********************************************************************
***>> Paralel Process

    DATA: lt_bex_list      TYPE TABLE OF rsbolap_wsp_query_name,
          ls_bex_list      TYPE rsbolap_wsp_query_name,
          lv_process_count TYPE i,
          l_name           TYPE c LENGTH 30.

    ls_bex_list = 'BPCFIC01_Q_0002'.

    DATA: lt_bex TYPE zbpce_bex_data_tt .

    CLEAR lt_bex .
    CALL FUNCTION 'ZBPCE_BEX_READ'
      EXPORTING
        i_query       = ls_bex_list
        i_t_parameter = i_t_parameter
      IMPORTING
        e_t_data      = lt_bex.

    LOOP AT lt_bex INTO ls_bex .
      INSERT ls_bex INTO TABLE gt_bex .
    ENDLOOP .
*
    lv_recno = 0 .
    LOOP AT gt_bex INTO ls_bex USING KEY k2 WHERE query_id = 'BPCFIC01_Q_0002' .
      lv_recno = ls_bex-recordno + 1 .
    ENDLOOP .

*QUERY RESULTs*0FISCPER*0WBS_ELEMT__WBS2ST*0WBS_ELEMT__WBS_EL2*ACUM COST PLAN*CALCULATED REVENUE (GAM)
*COST*GOM*GROSS ANALYTICAL MARGIN*MARGIN*POC*USED COST##*USED REVENUE
    DATA: lv_fiscper1  TYPE /bi0/oifiscper,
          lv_fiscvarnt TYPE /bi0/oifiscvarnt,
          lv_comp_code TYPE /bi0/oicomp_code,
          lv_costelmnt TYPE /bi0/oicostelmnt,
          lv_co_area   TYPE /bi0/oico_area,
          lv_wbs_elemt TYPE /bi0/oiwbs_elemt,
          lv_bpcversin TYPE /bic/oibpcversin,
          lv_datatype  TYPE /bic/oidatatype,
          lv_obj_cur   TYPE /bi0/oicurrency,
          lv_amount_oc TYPE /bi0/oiamount_oc,
          lv_gom       TYPE /bi0/oiamount_oc,
          lv_gam       TYPE /bi0/oiamount_oc,
          lv_gom_poc   TYPE f,
          lv_per_poc   TYPE f.

    DO lv_recno TIMES .
      lv_index = sy-index - 1 .
    CLEAR: lv_fiscper1,lv_costelmnt , lv_wbs_elemt,lv_datatype,lv_amount_oc,
          lv_gom   ,  lv_gam , lv_gom_poc, lv_per_poc .
      LOOP AT gt_bex INTO ls_bex USING KEY k2 WHERE recordno = lv_index AND query_id = 'BPCFIC01_Q_0002' .
        IF ls_bex-chavl = '#' . ls_bex-chavl = '' . ENDIF .
        ASSIGN COMPONENT ls_bex-chanm OF STRUCTURE ls_result TO <field> .
        IF ls_bex-chanm = '0COMP_CODE'  .
          "    lv_comp_code =  ls_bex-chavl .
        ELSEIF ls_bex-chanm = '0FISCPER' .
          lv_fiscper1  = ls_bex-chavl .
        ELSEIF ls_bex-chanm = '0WBS_ELEMT__WBS_EL2' .
          lv_wbs_elemt = ls_bex-chavl .
        ELSEIF ls_bex-chanm = 'POC'.
          lv_per_poc = ls_bex-value .
        ELSEIF ls_bex-chanm = 'GOM'.
          lv_gom  =  ls_bex-value .
        ELSEIF ls_bex-chanm = 'GROSS ANALYTICAL MARGIN'.
          lv_gam  =  ls_bex-value .
        ENDIF.
      ENDLOOP .
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = lv_wbs_elemt.
      ls_result-0fiscper   = lv_fiscper1.
      check lv_fiscper1+4(3) <= '012'.
      check lv_fiscper1+4(3) ne '000'.
*and also check the bpcversin
      check lv_fiscper1 >= lv_version.
      ls_result-0fiscvarnt = 'Z4'.
      ls_result-0comp_code = lv_compcode.
      " ls_result-0costelmnt TYPE /bi0/oicostelmnt,
      ls_result-0co_area  = 'TE01'.
      ls_result-0wbs_elemt = lv_wbs_elemt.
      ls_result-bpcversin = lv_version.
      ls_result-0obj_cur  = ls_wbs_elemnt-obj_curr.
      ls_result-datatype   = 'GOM'.
      ls_result-0amount_oc = lv_gom.
      clear ls_result-per_poc.
      COLLECT ls_result INTO lt_result.
      CLEAR ls_result-0amount_oc.
      ls_result-datatype   = 'GAM'.
      ls_result-0amount_oc = lv_gam.
      CLEAR ls_result-per_poc.
      COLLECT ls_result INTO lt_result.
      CLEAR ls_result-0amount_oc.
      CLEAR ls_result-per_poc.
      ls_result-datatype   = 'POC'.
      ls_result-per_poc  = lv_per_poc.
      COLLECT ls_result INTO lt_result.
    ENDDO .

  CLEAR : ls_result.
    LOOP AT lt_result INTO ls_result .  "WHERE 0comp_code = lv_compcode .
      check ls_result-0fiscper+4(3) <= '012'.

**      CHECK ls_final2-0fiscper BETWEEN ls_version-/bic/ppbas AND ls_version-/bic/ppbit. .
      COLLECT ls_result INTO c_th_data .
  ENDLOOP .
*
*    SORT gt_message .
*    DELETE ADJACENT DUPLICATES FROM gt_message .
*
*    LOOP AT gt_message INTO gs_message .
*      MESSAGE ID 'ZBPCE' TYPE gs_message-type NUMBER gs_message-log_no WITH gs_message-message_v1 gs_message-message_v2 gs_message-message_v3 gs_message-message_v4 INTO l_dummy.
*      i_r_msg->add_msg( '1' ).
*    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_WOREF_BEX_READ->IF_RSPLFA_SRVTYPE_IMP_EXEC~FINISH_EXECUTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD IF_RSPLFA_SRVTYPE_IMP_EXEC~FINISH_EXECUTION.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_WOREF_BEX_READ->IF_RSPLFA_SRVTYPE_IMP_EXEC~INIT_EXECUTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_SRVTYPE_DEF                TYPE REF TO IF_RSPLFA_SRVTYPE_DEF
* | [--->] I_R_SRV                        TYPE REF TO IF_RSPLFA_SRV
* | [--->] I_R_INFOPROV_DESC              TYPE REF TO IF_RSPLFA_INFOPROV_DESC
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD IF_RSPLFA_SRVTYPE_IMP_EXEC~INIT_EXECUTION.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_WOREF_BEX_READ->PARALLEL_PROCESS_END
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_TASK                         TYPE        CLIKE
* | [<---] E_T_DATA                       TYPE        ZBPCE_BEX_DATA_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD PARALLEL_PROCESS_END.

   DATA: lt_bex TYPE zbpce_bex_data_tt .
    DATA: ls_bex TYPE LINE OF zbpce_bex_data_tt .
    CLEAR lt_bex .

    p_received_jobs = p_received_jobs + 1.

    RECEIVE RESULTS FROM FUNCTION 'ZBPCe_PARALLEL_BEX_READ'
      IMPORTING
        e_t_data               = lt_bex .

    LOOP AT lt_bex INTO ls_bex .
      COLLECT ls_bex INTO gt_bex .
    ENDLOOP .

  ENDMETHOD.
ENDCLASS.
FUNCTION ZBPCE_BEX_READ.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_QUERY) TYPE  RSBOLAP_WSP_QUERY_NAME
*"     REFERENCE(I_T_PARAMETER) TYPE  RRXW3TQUERY
*"  EXPORTING
*"     REFERENCE(E_T_DATA) TYPE  ZBPCE_BEX_DATA_TT
*"----------------------------------------------------------------------
*
*FUNCTION ZBPCE_BEX_READ
*  IMPORTING
*    VALUE(I_QUERY) TYPE ZBPC_MT_MAP-ZBEX OPTIONAL
*    VALUE(I_T_PARAMETER) TYPE RRXW3TQUERY OPTIONAL
*  EXPORTING
*    VALUE(E_T_DATA) TYPE ZBPC_BEX_DATA_TT.
  TRY .

      CALL METHOD ZCL_WOREF_BEX_READ=>bex_read
        EXPORTING
          i_query                 = i_query
          i_t_parameter           = i_t_parameter
        IMPORTING
          e_t_data                = e_t_data
        EXCEPTIONS
          invalid_input           = 1
          invalid_variable_values = 2
          no_applicable_data      = 3
          no_authority            = 4
          abort                   = 5
          OTHERS                  = 6.
      IF sy-subrc <> 0.
        " MESAJ VERDİRİLECEK
      ENDIF.

  ENDTRY .
ENDFUNCTION.

