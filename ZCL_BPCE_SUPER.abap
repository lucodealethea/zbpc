class ZCL_BPCE_SUPER definition
  public
  create public .

public section.

  types:
*"* public components of class ZCL_BPC_SUPER
*"* do not include other source files here!!!
* this is RSDRI_INFOPROV_READ based not AMDP with aDSO view directly
    begin of gst_dim_md_s,
        dim      type uj_dim_name,
        dim_md_t type string,
      end of gst_dim_md_s .
  types:
    gtt_dim_md_t type table of gst_dim_md_s .
  types:
    begin of gst_rates,
        currency   type uj_dim_member,
        r_account  type uj_dim_member,
        r_entity   type uj_dim_member,
        time       type uj_dim_member,
        version    type uj_dim_member,
        signeddata type uj_signeddata,
      end of gst_rates .
  types:
    gtt_rates type table of gst_rates .
  types:
    begin of gst_rates_final,
        curr       type uj_dim_member,
        ratecurr   type uj_dim_member,
        time       type uj_dim_member,
        signeddata type p length 16 decimals 2,
      end of gst_rates_final .
  types:
    gtt_rates_final type table of gst_rates_final .

  class-data:
    begin of gs_masterdata_map,
        dim_name      type uj_dim_name,
        id            type uj_dim_member,
        investment_group type char2,
        distribution_channel type c length 2,
        item      type c length 4,
        dvclass       type c length 3,
        credit_term  type c length 4,
      end of gs_masterdata_map .
  class-data:
    gt_masterdata_map       like table of gs_masterdata_map .
  class-data:
    gt_masterdata_map_final like table of gs_masterdata_map .
  class-data GV_VERSION type UJ_DIM_MEMBER .
    "  class-data GV_COMPANY type UJ_DIM_MEMBER .
  class-data GT_CV type UJK_T_CV .
  class-data GS_CV type UJK_S_CV .
  class-data GT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE .
  class-data GV_ENVIRONMENT type UJ_APPSET_ID .
  class-data GV_MODEL type UJ_APPL_ID .
  class-data GS_CONTEXT type RSDRI_S_RANGE .
  class-data GT_CONTEXT type RSDRI_T_RANGE .
  class-data GT_MESSAGE type UJ0_T_MESSAGE .
  class-data GS_MESSAGE type UJ0_S_MESSAGE .
  class-data GV_TOP_COORDINATE type I .
  class-data:
    begin of gs_hr_param,
        gross_begin     type uj_signeddata,
        gross_end       type uj_signeddata,
        agitavan        type uj_signeddata,
        agi             type uj_signeddata,
        multiplier      type uj_signeddata,
        encentive       type uj_signeddata,
        timestmp        type uj_signeddata,
        employerk       type uj_signeddata,
        unemployed      type uj_signeddata,
        worker          type uj_signeddata,
        unemployment    type uj_signeddata,
        tax15         type uj_signeddata,
        tax20         type uj_signeddata,
        tax27         type uj_signeddata,
        tax35         type uj_signeddata,
      end of gs_hr_param .
  class-data:
    gt_hr_param like table of gs_hr_param .
  class-data:
    begin of gs_hr,
        costcenter     type uj_dim_member,
        entrytype      type uj_dim_member,
        hr_acc         type uj_dim_member,
        hr_det         type uj_dim_member,
        inputcurrency  type uj_dim_member,
        measures       type uj_dim_member,
        personel       type uj_dim_member,
        reportcurrency type uj_dim_member,
        time           type uj_dim_member,
        version        type uj_dim_member,
        signeddata     type uj_signeddata,
      end of gs_hr .
  class-data:
    gt_hr like table of gs_hr .
  class-data:
    begin of gs_dim_hier,
        dim_name    type uj_dim_name,
        dim_member  type uj_dim_member,
        dim_members type uj_dim_member,
      end of gs_dim_hier .
  class-data:
    gt_dim_hier like table of gs_dim_hier .
  class-data:
    begin of gs_rate,
        r_type     type uj_dim_member,
        r_time     type uj_dim_member,
        r_curr     type uj_dim_member,
        signeddata type uj_signeddata,
      end of gs_rate .
  class-data:
    gt_rate like table of gs_rate .
  class-data:
    begin of gs_lcl_acc,
        id(32),
        parenth1(32),
        dimlist(32),
        ratetype(32),
        flag(1),
      end of gs_lcl_acc .
  class-data:
    gt_lcl_acc like table of gs_lcl_acc .
  class-data:
    begin of gs_currency,
        id        type uj_dim_member,
        entity    type uj_dim_member,
        reporting type uj_dim_member,
      end of gs_currency .
  class-data:
    gt_currency like table of gs_currency .
  class-data:
    gt_reportcurrency like table of gs_currency .
  class-data:
    begin of gs_rates_final,
        curr       type uj_dim_member,
        ratecurr   type uj_dim_member,
        time       type uj_dim_member,
        signeddata type uj_signeddata,
      end of gs_rates_final .
  class-data:
    gt_rates_final like table of gs_rates_final .
  constants GC_CHECK_KDV type STRING value ' 1234567890.' ##NO_TEXT.
  constants GC_CHECK_VADE type STRING value ' 1234567890' ##NO_TEXT.
  class-data:
    begin of gs_mapped_records,
        iobjname type uj_dim_name,
        name     type string,
        val      type char100,
        id       type uj_dim_member,
      end of gs_mapped_records .
  class-data:
    gt_mapped_records  like sorted table of gs_mapped_records with non-unique key iobjname name val .
  class-data:
    gt_target_attr_map like sorted table of gs_mapped_records with non-unique key iobjname name id .
  class-data GS_RATES type GST_RATES .
  class-data GT_RANGE type RSDRI_T_RANGE .
  class-data:
    gs_range like line of gt_range .
  class-data GT_SFC type RSDRI_T_SFC .
  class-data:
    gs_sfc like line of gt_sfc .
  class-data GT_SFK type RSDRI_T_SFK .
  class-data:
    gs_sfk like line of gt_sfk .
  data GT_CHARSEL type RSPLF_T_CHARSEL .
  data:
    gs_charsel like line of gt_charsel .
  data GV_COMPANY type UJ_DIM_MEMBER .

 constants N_C_VERSION type STRING value '4.1' ##NO_TEXT.
  class-data N_ENCODING type ABAP_ENCODING read-only .
  class-data N_OVERWRITE type CHAR1 read-only .
  class-data N_SKIP_HEADER type ABAP_BOOL read-only .
  class-data N_CONV_FIELDS type ABAP_BOOL read-only .
  class-data N_CONV_AMOUNTS type ABAP_BOOL read-only .
  class-data N_CHECK_DUPLICATES type CHAR1 read-only .
  class-data N_INFOPROV type RSINFOPROV read-only .
  class-data N_T_IOBJ_PRO type RSPLFA_T_IOBJ_PRO read-only .
  class-data N_TH_COB_PRO type RSD_TH_COB_PRO read-only .
  class-data N_T_IOBJ_CMP type RSD_T_IOBJ_CMP read-only .
  class-data N_T_IOBJNM type RSD_T_IOBJNM read-only .
  class-data N_T_CHARSEL type RSPLF_T_CHARSEL read-only .
  class-data N_PLAN_DATE type SYDATUM read-only .
  class-data N_GUI_USED type ABAP_BOOL read-only .
  class-data N_SRVTYPENM type RSPLF_SRVTYPENM read-only .
  class-data N_SRVNM type RSPLF_SRVNM read-only .
  class-data N_MESSAGE type CHAR255 .
  class-data N_MSG_DISPLAY type STRING value 'WEXA' ##NO_TEXT.
  class-data N_MSG_INDEX type I .
  class-data N_MSG_COUNT type I .
  class-data N_MSG_VISIBLE type I .
  class-data N_HANDLE type RSBBS_HANDLE .
  class-data N_BUTTON_UPLOAD_ENABLED type WDY_BOOLEAN .
  class-data N_BUTTON_SAVE_ENABLED type WDY_BOOLEAN .
  class-data N_BUTTON_UNDO_ENABLED type WDY_BOOLEAN .
  class-data N_FILE_BROWSE_ENABLED type WDY_BOOLEAN .
  class-data N_FILENAME type STRING .
  class-data N_CONTENT type XSTRING .
  class-data N_MIMETYPE type STRING .
  class-data N_SEQNM type RSPLS_SEQNM .
  class-data N_DATA_SEPARATOR type CHAR1 read-only value ';' ##NO_TEXT.
  class-data N_ESCAPE_CHARACTER type CHAR1 read-only value '"' ##NO_TEXT.
  class-data N_SEL_SCREEN type ABAP_BOOL .
  class-data N_ALV_FILLED type ABAP_BOOL .
  class-data N_ALV_DISPLAY type STRING value 'FD' ##NO_TEXT.
  class-data N_TEXT_INSTRUCTIONS type STRING .
  class-data N_TEXT_SUPPORT type STRING .
  class-data N_R_FILE type ref to DATA read-only .
  class-data N_R_T_FILE type ref to DATA read-only .
  class-data N_R_DATA type ref to DATA read-only .
  class-data N_R_T_DATA type ref to DATA read-only .
  class-data N_ROWS_FILE type I .
  class-data N_ROWS_DATA type I .
  class-data N_T_VARIABLES type RSRVARIABLES_T_PERS_VAR .
  class-data N_R_SEL_OPTIONS type ref to IF_WD_SELECT_OPTIONS .
  class-data N_T_FIELDS type IF_WD_SELECT_OPTIONS=>TT_SELECTION_SCREEN_ITEM .
  constants N_C_VARIABLE type STRING value 'VAR_' ##NO_TEXT.
  constants N_C_INPUT_FIELD type STRING value 'INPUT_' ##NO_TEXT.
  constants N_C_LABEL type STRING value 'LABEL_' ##NO_TEXT.
  constants N_C_DELTA type STRING value 'D' ##NO_TEXT.
  constants N_C_ENCODING_DEFAULT type ABAP_ENCODING value 'DEFAULT' ##NO_TEXT.
  constants N_C_MIME_TEXT type STRING value 'text/plain' ##NO_TEXT.
  constants N_C_MIME_XML type STRING value 'text/xml' ##NO_TEXT.
  constants N_C_MIME_CSV type STRING value 'application/vnd.ms-excel' ##NO_TEXT.
  constants N_C_MIME_CSV2 type STRING value 'text/comma-separated-values' ##NO_TEXT.
  constants N_C_MIME_CSV3 type STRING value 'application/csv' ##NO_TEXT.
  constants N_C_MIME_EXCEL type STRING value 'application/vnd.ms-excel' ##NO_TEXT.
  constants N_C_MIME_EXCEL2 type STRING value 'application/vnd.msexcel' ##NO_TEXT.
  constants N_C_MIME_EXCEL3 type STRING value 'application/excel' ##NO_TEXT.
  constants N_C_MIME_OCTET type STRING value 'application/octet-stream' ##NO_TEXT.
  constants N_C_CURR type DATATYPE_D value 'CURR' ##NO_TEXT.
  constants N_C_TYPE_CHARACTER type STRING value 'CN' ##NO_TEXT.
  constants N_C_TYPE_NUMERIC type STRING value 'PFI' ##NO_TEXT.
  constants N_C_DATA_DEFAULT type CHAR1 value ';' ##NO_TEXT.
  constants N_C_ESCAPE_DEFAULT type CHAR1 value '"' ##NO_TEXT.
  constants N_C_DUPLICATES_ERROR type CHAR1 value 'E' ##NO_TEXT.
  constants N_C_DUPLICATES_ADD type CHAR1 value 'A' ##NO_TEXT.
  constants N_C_DUPLICATES_WARN type CHAR1 value 'W' ##NO_TEXT.
  constants N_C_HEADER_NAME type CHAR1 value 'X' ##NO_TEXT.
  constants N_C_HEADER_TEXT type CHAR1 value 'D' ##NO_TEXT.
  constants N_C_DEFAULT_VARIANT type STRING value '!DEF_VARIANT' ##NO_TEXT.
  constants N_C_LOGICAL_FILENAME type FILEINTERN value 'ZRSPLF_FILE_UPLOAD' ##NO_TEXT.
  constants N_C_CHAKYF type STRING value '1KYF_*' ##NO_TEXT.
  constants N_C_QUAN type DATATYPE_D value 'QUAN' ##NO_TEXT.
  constants N_C_CHAVL_MAXLEN type ROLLNAME value 'RSCHAVL_MAXLEN' ##NO_TEXT.
  constants N_C_CHAVL_EXT type ROLLNAME value 'RSCHAVLEXT' ##NO_TEXT.
  constants N_C_VALUE type ROLLNAME value 'RSVALUE' ##NO_TEXT.
  constants N_C_MIME_CITRIX type STRING value 'application/octet-stream' ##NO_TEXT.
  class-methods _COLLECT_TABLE_KYF
    changing
      !CT_DATA type STANDARD TABLE .
  class-methods _COLLECT_TABLE
    importing
      !I_KPI type UJ_DIM_NAME
    changing
      !CT_DATA type STANDARD TABLE .
  class-methods _READ_CUBE_DATA
    importing
      !I_INFOPROV type RSINFOPROV
      !I_T_SFC type RSDRI_T_SFC
      !I_T_SFK type RSDRI_T_SFK
      !I_T_RANGE type RSDRI_T_RANGE
      !I_COLLECT type CHAR1 optional
      !I_KEYDATE type SY-DATUM optional
    exporting
      !E_T_DATA type STANDARD TABLE
    exceptions
      DATA_READ_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BPCE_SUPER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BPCE_SUPER=>_COLLECT_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_KPI                          TYPE        UJ_DIM_NAME
* | [<-->] CT_DATA                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _COLLECT_TABLE.

    data: lr_td           type ref to data,
          ls_td1          type ref to data,
          ls_td2          type ref to data,
          ls_td3          type ref to data,
          lr_output       type ref to data.


    field-symbols:
      <lt_td>               type standard table,
      <ls_td>               type any,
      <ls_dummy1>           type any,
      <ls_dummy2>           type any,
      <value>               type any,
      <ls_output>           type any,
      <lv_signeddata>       type any,
      <lv_signeddata_total> type any.

    create data lr_td like ct_data.
    assign lr_td->* to <lt_td>.

    check ct_data[] is not initial.
    <lt_td> = ct_data.
    refresh ct_data.

    create data ls_td1 like line of <lt_td>.
    create data ls_td2 like line of <lt_td>.
    create data ls_td3 like line of <lt_td>.
    create data lr_output like line of <lt_td>.
    assign lr_output->* to <ls_output>.
    assign ls_td1->* to <ls_dummy2>.
    assign ls_td2->* to <ls_dummy1>.
    assign ls_td3->* to <ls_td>.

    assign component i_kpi of structure <ls_dummy1> to <value>.
    assign component i_kpi of structure <ls_td> to <lv_signeddata>.
    assign component i_kpi of structure <ls_output> to <lv_signeddata_total>.

*    while sy-uname = 'PMATHIEU'.
*
*    endwhile.

    sort <lt_td>.
    loop at <lt_td> into <ls_td>.
      delete <lt_td>.
      <ls_dummy1> = <ls_td>.
      <value> = 0.
      if <ls_dummy2> <> <ls_dummy1>.
        <ls_dummy2> = <ls_dummy1>.
        append <ls_output> to ct_data.
        <ls_output> = <ls_td>.
      else.
        <lv_signeddata_total> = <lv_signeddata_total> + <lv_signeddata>.
      endif.

    endloop.

    append <ls_output> to ct_data.
    delete ct_data index 1.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BPCE_SUPER=>_COLLECT_TABLE_KYF
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_DATA                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _COLLECT_TABLE_KYF.
     data: lrt_collected             type ref to data,
          lrt_source                type ref to data,
          lrs_source                type ref to data,
          lrs_ct_data               type ref to data,
          lrs_ct_data_start_with_z  type ref to data,
          lrt_ct_data_start_with_z  type ref to data,
          lrs_before                type ref to data,
          lrs_current               type ref to data,
          lo_structure              type ref to cl_abap_structdescr,
          lo_structure_ct_data      type ref to cl_abap_structdescr,
          lt_components             type abap_component_tab,
          lt_components_ct_data     type abap_component_tab,
          ls_components             like line of lt_components,
          lt_components_non_numeric like lt_components,
          lv_non_numeric_type       type ref to cl_abap_structdescr,
          lv_ct_data_type           type ref to cl_abap_structdescr,
          lv_type                   type char1,
          lt_sort_string            like table of ls_components-name.

    field-symbols: <lt_source>     type standard table,
                   <ls_source>     type any,
                   <lt_collected>  type standard table,
                   <ls_before>     type any,
                   <ls_current>    type any,
                   <lv_field>      type any,
                   <cs_data>       type any,
                   <ls_components> like line of lt_components_ct_data,
                   <lt_data>       type standard table,
                   <ls_data>       type any.

    ">>>> get structure of ct_data.
    create data lrs_ct_data like line of ct_data.
    assign lrs_ct_data->* to <cs_data>.

    lo_structure_ct_data ?= cl_abap_typedescr=>describe_by_data( <cs_data> ).
    lt_components_ct_data = lo_structure_ct_data->get_components( ).

    loop at lt_components_ct_data assigning <ls_components> where name cp '0*'.
      shift <ls_components>-name by 1 places left.
      <ls_components>-name = 'ZZZZ' && <ls_components>-name.
    endloop.

    lv_ct_data_type = cl_abap_structdescr=>create( p_components = lt_components_ct_data ).
    create data lrs_ct_data_start_with_z type handle lv_ct_data_type.
    assign lrs_ct_data_start_with_z->* to <ls_data>.

    create data lrt_ct_data_start_with_z like table of <ls_data>.
    assign lrt_ct_data_start_with_z->* to <lt_data>.
    ">>>> assignments

    create data lrt_source like <lt_data>.
    create data lrs_source like line of <lt_data>.
    create data lrt_collected like <lt_data>.

    assign lrt_source->* to <lt_source>.
    assign lrs_source->* to <ls_source>.
    assign lrt_collected->* to <lt_collected>.

    <lt_source> = ct_data.
    clear ct_data.

    "<<<< assignments

    ">>>> get structure

    lo_structure ?= cl_abap_typedescr=>describe_by_data( <ls_source> ).
    lt_components = lo_structure->get_components( ).
    loop at lt_components into ls_components.
      assign component ls_components-name of structure <ls_source> to <lv_field>.
      describe field <lv_field> type lv_type.
      translate lv_type to upper case.
      case lv_type.
        when 'B' or 'S' or 'I' or '8' or 'P' or 'A' or 'E' or 'F'. "numeric data types
        when others.
          append ls_components to lt_components_non_numeric.
          append ls_components-name to lt_sort_string.
      endcase.

    endloop.

    lv_non_numeric_type = cl_abap_structdescr=>create( p_components = lt_components_non_numeric ).

    create data lrs_before type handle lv_non_numeric_type.
    create data lrs_current type handle lv_non_numeric_type.

    "<<<< End region get structure

    assign lrs_current->* to <ls_current>.
    assign lrs_before->* to <ls_before>.

    loop at lt_sort_string into ls_components-name.
      sort <lt_source> stable by (ls_components-name).
    endloop.

    loop at <lt_source> into <ls_source>.
      delete <lt_source>.

      move-corresponding <ls_source> to <ls_current>.
      if <ls_current> <> <ls_before>.
        append lines of <lt_collected> to ct_data.
        <ls_before> = <ls_current>.
        clear <lt_collected>.
        append <ls_source> to <lt_collected>.
      else.
        collect <ls_source> into <lt_collected>.
      endif.
    endloop.

    append lines of <lt_collected> to ct_data.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BPCE_SUPER=>_READ_CUBE_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_INFOPROV                     TYPE        RSINFOPROV
* | [--->] I_T_SFC                        TYPE        RSDRI_T_SFC
* | [--->] I_T_SFK                        TYPE        RSDRI_T_SFK
* | [--->] I_T_RANGE                      TYPE        RSDRI_T_RANGE
* | [--->] I_COLLECT                      TYPE        CHAR1(optional)
* | [--->] I_KEYDATE                      TYPE        SY-DATUM(optional)
* | [<---] E_T_DATA                       TYPE        STANDARD TABLE
* | [EXC!] DATA_READ_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _READ_CUBE_DATA.
     data : lv_eod(1),
           lv_firstcall(1) value 'X',
           lt_data         type ref to data,
           lv_keydate      type sy-datum,
           lt_sfc_h        type rsdri_th_sfc,
           lt_sfk_h        type rsdri_th_sfk,
           lv_counter      type i.

    field-symbols : <lt_data> type standard table.

    create data lt_data like e_t_data.
    assign lt_data->* to <lt_data>.


    lt_sfc_h[] = i_t_sfc[].
    lt_sfk_h[] = i_t_sfk[].
    if i_keydate is initial.
      lv_keydate = sy-datum.
    else.
      lv_keydate = i_keydate.
    endif.


    clear : lv_eod, <lt_data>, e_t_data.

    while lv_eod is initial.
      call function 'RSDRI_INFOPROV_READ'
        exporting
          i_infoprov             = i_infoprov
          i_th_sfc               = lt_sfc_h
          i_th_sfk               = lt_sfk_h
          i_t_range              = i_t_range
          i_rollup_only          = rs_c_false
          i_packagesize          = 100000
          i_reference_date       = lv_keydate
          i_authority_check      = rsdrc_c_authchk-none
        importing
          e_t_data               = <lt_data>
          e_end_of_data          = lv_eod
        changing
          c_first_call           = lv_firstcall
        exceptions
          illegal_input          = 1
          illegal_input_sfc      = 2
          illegal_input_sfk      = 3
          illegal_input_range    = 4
          illegal_input_tablesel = 5
          no_authorization       = 6
          illegal_download       = 7
          illegal_tablename      = 8
          trans_no_write_mode    = 9
          inherited_error        = 10
          x_message              = 11
          no_commit_free_read    = 12
          others                 = 13.

      if sy-subrc <> 0.
        raise data_read_error.
        exit.
      endif.
      add 1 to lv_counter.

      append lines of <lt_data> to e_t_data.
    endwhile.

* Run collection function
    if i_collect = 'X'.

      _collect_table_kyf(
        changing
          ct_data = e_t_data
      ).

    endif.

  endmethod.
ENDCLASS.
