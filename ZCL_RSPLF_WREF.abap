class ZCL_RSPLF_WREF definition
  public
  inheriting from ZCL_BPCE_SUPER
  create public .

*"* public components of class ZCL_RSPLF_WREF
*"* do not include other source files here!!!
 " types:
*    BEGIN OF gst_eco_param,
*        country   TYPE /bi0/oicountry,
*        xcostmthd TYPE /bic/oixcostmthd,
*        calmonth  TYPE /bi0/oicalmonth,
*        xvalue    TYPE /bic/oixvalue,
*      END OF gst_eco_param .
*  types:
*    gtt_eco_param TYPE TABLE OF gst_eco_param .
*  types:
*    BEGIN OF gst_data_extended,
*        country   TYPE /bi0/oicountry,
*        xcostmthd TYPE /bic/oixcostmthd.
*        INCLUDE TYPE zbpcn_pexp_ds01.
*      TYPES:END OF gst_data_extended .
*  types:
*    gtt_data_extended TYPE TABLE OF gst_data_extended .
*
*  data:
*    g_t_ccode_lc TYPE TABLE OF /bic/pxcompcode .
*  data G_S_CCODE_LC type /BIC/PXCOMPCODE .
  " G_T_DATA_CHARSEL type RSPLF_T_CHARSEL .
 "  G_S_DATA_CHARSEL type RSPLF_S_CHARSEL .
*  data G_V_FTYPE type /BIC/OIXFTYPE .
*  data G_V_XVERSION type /BIC/OIXVERSION .
*  data G_S_XVERSION type /BIC/PXVERSION .
*  data G_S_MESSAGE type GST_MESSAGE .
*  data G_T_MESSAGE type GTT_MESSAGE .
*  data:
*    BEGIN OF gs_exc_rates,
*        xitem         TYPE /bic/oixitem,
*        currency_from TYPE /bi0/oicurrency,
*        currency_to   TYPE /bi0/oicurrency,
*        calmonth      TYPE /bi0/oicalmonth,
*        xvalue        TYPE /bic/oixvalue,
*      END OF gs_exc_rates .
*  data:
*    gt_exc_rates LIKE TABLE OF gs_exc_rates .
*  data GS_ECO_PARAM type GST_ECO_PARAM .
*  data GT_ECO_PARAM type GTT_ECO_PARAM .
public section.

  interfaces IF_RSPLFA_SRVTYPE_IMP_EXEC_REF .

  types:
    tt_rfc_db_opt TYPE TABLE OF rfc_db_opt .
  types:
    tt_rfc_db_fld TYPE TABLE OF rfc_db_fld .
  types:
    tt_ddshselopt TYPE TABLE OF ddshselopt .
  types:
    BEGIN OF gst_message,
        msgty TYPE sy-msgty,
        msgv1 TYPE sy-msgv1,
        msgv2 TYPE sy-msgv2,
        msgv3 TYPE sy-msgv3,
        msgv4 TYPE sy-msgv4,
      END OF gst_message .
  types:
    gtt_message TYPE TABLE OF gst_message .

  methods CALC_CFWD
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods DRCOST_01
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods PER_CHECK
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods REFDATA_CALC_CFWD
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods REFDATA_DRCOST_01
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods REFDATA_PER_CHECK
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods REFDATA_RETRACTION_01
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods REFDATA_COPY_PER_AV
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods REFDATA_SWITCH_PER
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods REFDATA_TM_REVENUE
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods RETRACTION_01
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods SWITCH_PER
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods TM_REVENUE
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods TM_REVENUE_OLD_COND
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods _RFC_READ_TABLE
    importing
      !IV_DESTINATION type RSDLOGSYS
      !IV_QUERY_TABLE type DD02L-TABNAME
      !IV_STRUCTURE type ANY
      !IT_SELOPT type TT_DDSHSELOPT
      !IT_FIELDS type TT_RFC_DB_FLD
    exporting
      !ET_DATA type STANDARD TABLE .
  methods COPY_PER_AV
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods HIS_UPLOAD
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods REFDATA_HIS_UPLOAD
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods BET_CALC
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods REFDATA_BET_CALC
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods PER_COPY
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods REFDATA_PER_COPY
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
  methods UPDATE_CAR
    importing
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_TH_REF_DATA) type HASHED TABLE
      value(I_S_BLOCK_LINE) type ANY
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    changing
      !C_TH_DATA type HASHED TABLE .
  methods REFDATA_UPDATE_CAR
    importing
      value(I_T_DATA_CHARSEL) type RSPLF_T_CHARSEL
      value(I_T_DATA_NODE_SEL) type RSPLF_T_NODE optional
      value(I_R_PARAM_SET) type ref to IF_RSPLFA_PARAM_SET
      value(I_R_MSG) type ref to IF_RSPLFA_MSG
    exporting
      !E_TH_NO_REF_BLOCK_CHA type RSPLF_TH_IOBJ
      !E_T_REF_CHARSEL type RSPLF_T_CHARSEL
      !E_T_REF_NODE_SEL type RSPLF_T_NODE .
protected section.
*"* protected components of class ZCL_RSPLF_WREF
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_RSPLF_WREF
*"* do not include other source files here!!!

  data G_S_MESSAGE type GST_MESSAGE .
  data G_T_MESSAGE type GTT_MESSAGE .
  data G_V_FTYPE type /BIC/OIXFTYPE .
ENDCLASS.



CLASS ZCL_RSPLF_WREF IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->BET_CALC
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD bet_calc.
**********************************************************************
*Logic reads the customer exit variable-ZCXPM_BPCVERSIN- aktif period
* of the company code
* if data avaliable before- it is not working
*if data doesnt avaliable for that period, it is calculating ACTPLN
*** Change  :
**********************************************************************

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a08c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a08c..
  DATA: lt_charsel           TYPE rsplf_t_charsel,
        l_r_msg              TYPE REF TO cl_rsplfu_msg,
        ls_charsel           LIKE LINE OF lt_charsel,
        lv_compcode          TYPE /bi0/oicomp_code,
        lv_version           TYPE /bic/oibpcversin,
        l_dummy              TYPE c,
        ls_wbs_elemnt        TYPE /bi0/pwbs_elemt,
        ls_comp_code         TYPE /bi0/pcomp_code,
        lt_act               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_act               TYPE zbpce_pal_cck_a08c,
        lt_plan              TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_plan              TYPE zbpce_pal_cck_a08c,
        lt_actplan           TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_actplan           TYPE zbpce_pal_cck_a08c,
        lt_bet               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_bet               TYPE zbpce_pal_cck_a08c,

        ls_conditions        TYPE zbpce_pal_cck_a03c,
        ls_breakdown         TYPE zbpce_breakdown,
        lt_breakdown         TYPE TABLE OF zbpce_breakdown,
        lv_f_beg             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a03c-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a09c,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i,
        lv_lastday           TYPE d,
        lt_map               TYPE TABLE OF zwbs_map_01,
        ls_map               TYPE zwbs_map_01,
        ls_project           TYPE /bi0/mproject,
        lv_lastdaydats       TYPE dats,
        lv_famount           TYPE f.

*
  DATA: ls_costelemnt  TYPE /bi0/mcostelmnt,
        ls_costelemnt2 TYPE /bi0/mcostelmnt,
        lt_costelemnt  TYPE TABLE OF /bi0/mcostelmnt.
  DATA: ls_bpcfid90 TYPE /bic/abpcfid907,
        lt_bpcfid90 TYPE TABLE OF /bic/abpcfid907.

  SELECT comp_code AS comp_code, calmonth AS calmonth
       FROM /bic/abpcfid907
       INTO CORRESPONDING FIELDS OF TABLE  @lt_bpcfid90.

  SELECT costelmnt AS costelmnt, /bic/celmsubgr AS /bic/celmsubgr,
       /bic/celmgroup AS /bic/celmgroup,
       cstelmntyp AS cstelmntyp
        FROM /bi0/mcostelmnt
        INTO CORRESPONDING FIELDS OF TABLE  @lt_costelemnt
        WHERE   objvers = 'A'.

**********************************************************************
  DATA :
    lv_prevper TYPE /bic/oibpcversin,
    lv_year(4) TYPE n.

*
  l_r_msg ?= i_r_msg.
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.
  lv_prevper = lv_version .

  IF lv_prevper+4(3) NE '001'.
    lv_prevper = lv_prevper - 1.
  ELSE.
    lv_year = lv_year - 1.
    CONCATENATE lv_year '012' INTO lv_prevper.
  ENDIF.
  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0COMP_CODE'.
  lv_compcode = ls_charsel-low.
  DATA: kontrol TYPE i.
* control if data avaliable already in the version dont touch it.
  kontrol = 0.

  LOOP AT c_th_data ASSIGNING <ls_data>.
    IF <ls_data>-bpcaudit = 'BET' .
      <ls_data>-0amount_oc = 0.
      <ls_data>-0quantity = 0.
    ENDIF.
  ENDLOOP.

  "collect plan data
  LOOP AT c_th_data ASSIGNING <ls_data>.
    MOVE <ls_data> TO ls_plan.
    CHECK ls_plan-0fiscper >= lv_version  OR ls_plan-0fiscper EQ '0000000'.
    "  CHECK ls_plan-bpcaudit NE 'ACPLN'.
    ls_plan-bpcversin = lv_version.
    ls_plan-0fiscper =  '0000000'.
    ls_plan-bpcaudit = 'BET'.
    COLLECT ls_plan INTO lt_plan .
  ENDLOOP.


  "collect the actuals, convert them
  "dont get any CTG and REV (Lumps) from actuals
  LOOP AT i_th_ref_data ASSIGNING <ls_data>.
    MOVE <ls_data> TO ls_act.
    CHECK ls_act-0fiscper < lv_version  OR ls_act-0fiscper EQ '0000000'.    "DATE CHECK
    CLEAR ls_wbs_elemnt.
    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_act-0wbs_elemt.
    IF  ls_wbs_elemnt-ps_level NE 3.
      ls_act-0wbs_elemt = ls_wbs_elemnt-/bic/wbs_el3.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_act-0wbs_elemt.
    ENDIF.
    CHECK ls_wbs_elemnt-/bic/wbs_el3 IS NOT INITIAL.
    check ls_wbs_elemnt-statussys0 = '02'.
    CLEAR ls_costelemnt.
    READ TABLE lt_costelemnt INTO ls_costelemnt WITH  KEY  costelmnt = ls_act-0costelmnt.

    CLEAR ls_costelemnt2.
    READ TABLE lt_costelemnt INTO ls_costelemnt2 WITH  KEY  /bic/celmgroup = ls_costelemnt-/bic/celmgroup
                                                            cstelmntyp  = '21'.
    ls_act-0costelmnt = ls_costelemnt2-costelmnt.
    if ( ls_wbs_elemnt-/bic/ps_rkeyan eq 'Z00001' or ls_wbs_elemnt-/bic/ps_rkeyan eq 'Z99999' ) AND ls_costelemnt-/bic/celmgroup+0(4) eq 'GREV'.
   else.
    "  CHECK ls_plan-bpcaudit NE 'ACPLN'.
    ls_act-bpcversin = lv_version.
    ls_act-0infoprov = 'BPCFID01'.
    ls_act-0fiscper =  '0000000'.
    ls_act-0version =  '000'.
    ls_act-bpcaudit = 'BET'.
    COLLECT ls_act INTO c_th_data. "lt_act .
   endif.
    "COLLECT ls_act INTO c_th_data .

  ENDLOOP.
  loop at lt_plan into ls_plan.
    COLLECT ls_plan INTO c_th_data.
  endloop.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->CALC_CFWD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD calc_cfwd.
**********************************************************************

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a08c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a08c.
  DATA: lt_charsel           TYPE rsplf_t_charsel,
        ls_charsel           LIKE LINE OF lt_charsel,
        lv_compcode          TYPE /bi0/oicomp_code,
        lv_version           TYPE /bic/oibpcversin,
        lv_project           TYPE /bi0/oiproject,
        ls_project           TYPE /bi0/pproject,
        ls_wbs_elemnt        TYPE /bi0/pwbs_elemt,
        ls_comp_code         TYPE /bi0/pcomp_code,
        lt_hours             TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_hours             TYPE zbpce_pal_cck_a08c,
        lt_exp               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_exp               TYPE zbpce_pal_cck_a08c,
        lt_act               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_act               TYPE zbpce_pal_cck_a08c,
        lt_act_exp           TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_act_exp           TYPE zbpce_pal_cck_a08c,
        lt_plan              TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_plan              TYPE zbpce_pal_cck_a08c,
        ls_plan2              TYPE zbpce_pal_cck_a08c,
        lt_actplan           TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_actplan           TYPE zbpce_pal_cck_a08c,
        lt_plan_exp          TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_plan_exp          TYPE zbpce_pal_cck_a08c,
        lt_actplan_exp       TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_actplan_exp       TYPE zbpce_pal_cck_a08c,
        lt_bet               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_bet               TYPE zbpce_pal_cck_a08c,
        lt_total             TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total             TYPE zbpce_pal_cck_a08ct,
        lt_total_exp         TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total_exp         TYPE zbpce_pal_cck_a08ct,
        lt_total_act         TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total_act         TYPE zbpce_pal_cck_a08ct,
        lt_total_act_exp     TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total_act_exp     TYPE zbpce_pal_cck_a08ct,
        ls_conditions        TYPE zbpce_pal_cck_a03c,
        ls_breakdown         TYPE zbpce_breakdown,
        lt_breakdown         TYPE TABLE OF zbpce_breakdown,
        ls_breakdown_exp     TYPE zbpce_breakdown,
        lt_breakdown_exp     TYPE TABLE OF zbpce_breakdown,
        lv_f_beg             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a03c-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a08c,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i,
        lv_lastday           TYPE d,
        lt_map               TYPE TABLE OF zwbs_map_01,
        ls_map               TYPE zwbs_map_01,
        lv_lastdaydats       TYPE dats,
        lv_famount           TYPE f,
        lv_difamount         TYPE  decfloat34, " /bi0/oiquantity.
        lv_round             TYPE  /bi0/oiamount_oc,
        lv_roundq            TYPE  /bi0/oiquantity.

*
  DATA: ls_costelemnt  TYPE /bi0/mcostelmnt,
        ls_costelemnt2 TYPE /bi0/mcostelmnt,
        lt_costelemnt  TYPE TABLE OF /bi0/mcostelmnt.
  DATA :lv_act_prev_amount   TYPE /bi0/oiamount_oc,
        lv_act_prev_quantity TYPE /bi0/oiquantity.


  SELECT costelmnt AS costelmnt, /bic/celmsubgr AS /bic/celmsubgr,
         /bic/celmgroup AS /bic/celmgroup,
         cstelmntyp AS cstelmntyp
          FROM /bi0/mcostelmnt
          INTO CORRESPONDING FIELDS OF TABLE  @lt_costelemnt
          WHERE   objvers = 'A'.
  "  AND ( cstelmntyp = '21' OR cstelmntyp = '01' ).
  "     AND  /bic/celmtype =  'GEXP'.
  .

  SELECT inputtype AS inputtype, ps_prjtype AS ps_prjtype,
           costelmnt AS costelmnt
          FROM  zwbs_map_01
          INTO CORRESPONDING FIELDS OF TABLE  @lt_map.

**********************************************************************
*
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0PROJECT'.
  lv_project = ls_charsel-low.
* if it hours , WBS element, activity type, costcenter
* if it is expenses BPC Version, WBS, Cost Element, Fiscper

  LOOP AT i_th_ref_data ASSIGNING <ls_data>.
    "  CHECK <ls_data>-0fiscper <= lv_version.  "NAZ
    CHECK <ls_data>-0fiscper = lv_version.    "NAZ2

    MOVE <ls_data> TO ls_act.
    CLEAR ls_wbs_elemnt.
    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_act-0wbs_elemt.
    IF  ls_wbs_elemnt-ps_level NE 3.
      ls_act-0wbs_elemt = ls_wbs_elemnt-/bic/wbs_el3.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_act-0wbs_elemt.
    ENDIF.
    CHECK ls_wbs_elemnt-/bic/wbs_el3 IS NOT INITIAL.
    CHECK  ( ls_wbs_elemnt-/bic/ps_rkeyan = 'Z00001'  OR ls_wbs_elemnt-/bic/ps_rkeyan = 'Z99999' ).
    CLEAR ls_costelemnt.
    READ TABLE lt_costelemnt INTO ls_costelemnt WITH  KEY  costelmnt = ls_act-0costelmnt.

    CASE ls_costelemnt-/bic/celmgroup+0(4).
      WHEN 'GHRS'.
        CLEAR ls_costelemnt2.
        READ TABLE lt_costelemnt INTO ls_costelemnt2 WITH  KEY  /bic/celmgroup = ls_costelemnt-/bic/celmgroup
                                                                cstelmntyp  = '21'.
        ls_act-0costelmnt = ls_costelemnt2-costelmnt.
        COLLECT ls_act INTO lt_act.    "actuals all
        MOVE-CORRESPONDING ls_act TO ls_total_act.
        COLLECT ls_total_act INTO lt_total_act.
        MOVE-CORRESPONDING ls_act TO ls_breakdown.
        COLLECT ls_breakdown INTO lt_breakdown.
      WHEN 'GEXP'.
        CLEAR ls_costelemnt2.
        READ TABLE lt_costelemnt INTO ls_costelemnt2 WITH  KEY  /bic/celmgroup = ls_costelemnt-/bic/celmgroup
                                                                cstelmntyp  = '21'.
        ls_act-0costelmnt = ls_costelemnt2-costelmnt.
        ls_act-0unit = 'H'.
        COLLECT ls_act INTO lt_act_exp.    "actuals all
        MOVE-CORRESPONDING ls_act TO ls_total_act_exp.
        COLLECT ls_total_act_exp INTO lt_total_act_exp.
        MOVE-CORRESPONDING ls_act TO ls_breakdown_exp.
        COLLECT ls_breakdown_exp INTO lt_breakdown_exp.
    ENDCASE.
  ENDLOOP.

  LOOP AT c_th_data ASSIGNING <ls_data>.
    CLEAR ls_wbs_elemnt.
    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = <ls_data>-0wbs_elemt.

    CHECK  ( ls_wbs_elemnt-/bic/ps_rkeyan = 'Z00001'  OR ls_wbs_elemnt-/bic/ps_rkeyan = 'Z99999' ).
    IF  <ls_data>-bpcaudit NE 'FWD' AND  <ls_data>-bpcaudit NE 'CAR'.
      CLEAR ls_costelemnt.
      READ TABLE lt_costelemnt INTO ls_costelemnt WITH  KEY  costelmnt = <ls_data>-0costelmnt.
      CASE ls_costelemnt-/bic/celmsubgr+0(4).
        WHEN 'GHRS'.
          MOVE <ls_data> TO ls_plan.
          " CLEAR ls_plan-0costelmnt.
          CLEAR ls_plan-0comp_code.
         ls_plan-bpcaudit = 'M_INPUT'.
          IF <ls_data>-0fiscper = lv_version."NAZ2    <ls_data>-bpcaudit =  'ACPLN' . "OR
            "dcost will be collected
             ls_plan-0fiscper = lv_version.
            COLLECT ls_plan INTO lt_actplan.    "accumlated plan
          ELSE.
            MOVE <ls_data> TO ls_plan2.
            if  ls_plan2-bpcaudit ne 'DCOST'.
               ls_plan2-bpcaudit = 'M_INPUT'.
              endif.
            COLLECT  ls_plan2 INTO lt_plan.    "plan from now on in this version
            MOVE-CORRESPONDING ls_plan TO ls_total.
            COLLECT ls_total INTO lt_total. "total plan from now on
          ENDIF.
          MOVE-CORRESPONDING ls_plan TO ls_breakdown.
          COLLECT ls_breakdown INTO lt_breakdown.
        WHEN 'GEXP'.
          MOVE <ls_data> TO ls_plan.
          "  CLEAR ls_plan-0costelmnt.
          ls_plan-bpcaudit = 'M_INPUT'.
          IF <ls_data>-0fiscper = lv_version." NAZ  2 <ls_data>-bpcaudit =  'ACPLN' ."OR
            ls_plan-0fiscper = lv_version.
            COLLECT ls_plan INTO lt_actplan_exp.    "accumlated plan
          ELSE.
            COLLECT ls_plan INTO lt_plan_exp.    "plan from now on in this version
            MOVE-CORRESPONDING ls_plan TO ls_total_exp.
            COLLECT ls_total_exp INTO lt_total_exp. "total plan from now on
          ENDIF.
          MOVE-CORRESPONDING ls_plan TO ls_breakdown_exp.
          COLLECT ls_breakdown_exp INTO lt_breakdown_exp.
      ENDCASE.
    ELSE.
      <ls_data>-0amount_oc = 0.
      <ls_data>-0quantity = 0.      "clear data avaliable in FWD
    ENDIF.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_breakdown.
  DELETE ADJACENT DUPLICATES FROM lt_breakdown_exp.

  DATA: lv_percent TYPE decfloat34, "/bi0/oiamount_oc,
        lv_dif  TYPE  BW4_VOLSTAT_COSIZE,     "/bi0/oiquantity, " /bi0/oiamount_oc,
        lv_difq     TYPE /bi0/oiquantity.
  DATA: lv_nextper TYPE /bic/oibpcversin,
        lv_year(4) TYPE n,
        divif      TYPE i.
  divif = 0.

  LOOP AT lt_breakdown INTO ls_breakdown.
*check is an actual data is not avaliable in plan values should subtrc.
    CLEAR lv_round.
    CLEAR lv_roundq.
    CLEAR ls_plan.
    LOOP AT lt_plan INTO ls_plan WHERE 0wbs_elemt = ls_breakdown-0wbs_elemt
                                  AND  0acttype   = ls_breakdown-0acttype
                                  AND  0costcenter = ls_breakdown-0costcenter
                                  AND  0costelmnt = ls_breakdown-0costelmnt
                                  AND  0fiscper >= lv_version.     "NAZ
      "                                AND  0fiscper > lv_version.

      CLEAR ls_data.
      CLEAR ls_total_act. "total of the actuals without per.
      READ TABLE lt_total_act INTO ls_total_act WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                                          0acttype   = ls_breakdown-0acttype
                                                          0costcenter = ls_breakdown-0costcenter
                                                          0costelmnt  = ls_breakdown-0costelmnt.
      IF ls_plan-bpcaudit = 'DCOST'.
        CLEAR ls_total.   "total of the breakdown for the next periods
        READ TABLE lt_total INTO ls_total WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                     0acttype   = ls_breakdown-0acttype
                                     0costcenter = ls_breakdown-0costcenter
                                     0costelmnt  = ls_breakdown-0costelmnt
                                     0unit = ''.
        CLEAR ls_actplan.
        READ TABLE lt_actplan INTO ls_actplan WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                                            0acttype   = ls_breakdown-0acttype
                                                            0costcenter = ls_breakdown-0costcenter
                                                            0costelmnt  = ls_breakdown-0costelmnt
                                                            0unit = ''.
        lv_percent = ls_plan-0amount_oc / ls_total-0amount_oc.
        "find the difference
        lv_dif = ls_actplan-0amount_oc - ls_total_act-0amount_oc .  "amount to be distributed.
        IF lv_dif NE 0.
          CLEAR lv_difamount .
          lv_difamount = lv_dif  * lv_percent .
          ls_data-0amount_oc   = lv_difamount.

          IF lv_round IS INITIAL.
            lv_round = lv_dif .
          ENDIF.
          lv_round = lv_round - ls_data-0amount_oc .
        ENDIF.
        ls_data-0obj_cur    = ls_plan-0obj_cur.
        ls_data-0unit       = ''.
        divif = 1.
      ELSE.
        CLEAR ls_total.   "total of the breakdown for the next periods
        READ TABLE lt_total INTO ls_total WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                     0acttype   = ls_breakdown-0acttype
                                     0costcenter = ls_breakdown-0costcenter
                                     0costelmnt  = ls_breakdown-0costelmnt
                                     0unit = 'H'.
        CLEAR ls_actplan.
        READ TABLE lt_actplan INTO ls_actplan WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                                            0acttype   = ls_breakdown-0acttype
                                                            0costcenter = ls_breakdown-0costcenter
                                                            0costelmnt  = ls_breakdown-0costelmnt
                                                            0unit = 'H'.

*find the percentages of each breakdown for the period in lt_plan
        lv_percent = ls_plan-0quantity / ls_total-0quantity.
        "find the difference
        lv_dif  = ls_actplan-0quantity - ls_total_act-0quantity .    "amount to be distributed.
        IF lv_dif  NE 0.
          ls_data-0quantity   = ( lv_dif * ls_plan-0quantity ) / ls_total-0quantity .
          IF lv_roundq IS INITIAL.
            lv_roundq = lv_dif  .
          ENDIF.
          lv_roundq = lv_roundq - ls_data-0quantity  .
        ENDIF.
        ls_data-0obj_cur    = ''.  "ls_plan-0obj_cu'r.
        ls_data-0unit       = 'H'.
        divif = 0.
      ENDIF.

      IF lv_dif NE 0.
        ls_data-0fiscper  =  ls_plan-0fiscper.
        ls_data-0fiscvarnt = ls_plan-0fiscvarnt.
        ls_data-0acttype    = ls_plan-0acttype.
        ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
        ls_data-0costelmnt = ls_plan-0costelmnt.
        ls_data-0costcenter = ls_plan-0costcenter.
        ls_data-0costelmnt  = ls_plan-0costelmnt.
        ls_data-0co_area    = ls_plan-0co_area.
        ls_data-0infoprov   = ls_plan-0infoprov .
        ls_data-0project    = ls_plan-0project.
        ls_data-0version    = ls_plan-0version.
        ls_data-bpcversin   = ls_plan-bpcversin.
        ls_data-0wbs_elemt  = ls_plan-0wbs_elemt.
        ls_data-bpcaudit    = 'FWD'.
        COLLECT ls_data INTO c_th_data.
        ls_data-0amount_oc  = ls_data-0amount_oc  * -1.
        ls_data-0quantity  = ls_data-0quantity * -1.
        ls_data-0fiscper  = lv_version.
        ls_data-bpcaudit    = 'CAR'.
        COLLECT ls_data INTO c_th_data.

      ENDIF.
    ENDLOOP.
    IF lv_round IS NOT INITIAL.
      ls_data-0obj_cur    = ls_wbs_elemnt-obj_curr.
      ls_data-0unit       = ''.
      ls_data-0fiscper  =  ls_plan-0fiscper.
      ls_data-0fiscvarnt = ls_plan-0fiscvarnt.
      ls_data-0acttype    = ls_plan-0acttype.
      ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
      ls_data-0costelmnt = ls_plan-0costelmnt.
      ls_data-0costcenter = ls_plan-0costcenter.
      ls_data-0costelmnt  = ls_plan-0costelmnt.
      ls_data-0co_area    = ls_plan-0co_area.
      ls_data-0infoprov   = ls_plan-0infoprov .
      ls_data-0project    = ls_plan-0project.
      ls_data-0version    = ls_plan-0version.
      ls_data-0wbs_elemt  = ls_plan-0wbs_elemt.
      ls_data-0amount_oc  = lv_round.
      ls_data-0quantity   = 0.
      ls_data-bpcaudit    = 'FWD'.
      ls_data-bpcversin   = ls_plan-bpcversin.
      COLLECT ls_data INTO c_th_data.
      ls_data-0amount_oc = ls_data-0amount_oc  * -1.
      ls_data-0fiscper  = lv_version.
      ls_data-bpcaudit    = 'CAR'.
      COLLECT ls_data INTO c_th_data.
    ENDIF.
    IF lv_roundq IS NOT INITIAL.
      ls_data-0obj_cur    = ''.
      ls_data-0unit       = 'H'.
      ls_data-0fiscper  =  ls_plan-0fiscper.
      ls_data-0fiscvarnt = ls_plan-0fiscvarnt.
      ls_data-0acttype    = ls_plan-0acttype.
      ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
      ls_data-0costelmnt = ls_plan-0costelmnt.
      ls_data-0costcenter = ls_plan-0costcenter.
      ls_data-0costelmnt  = ls_plan-0costelmnt.
      ls_data-0co_area    = ls_plan-0co_area.
      ls_data-0infoprov   = ls_plan-0infoprov .
      ls_data-0project    = ls_plan-0project.
      ls_data-0version    = ls_plan-0version.
      ls_data-0wbs_elemt  = ls_plan-0wbs_elemt.
      ls_data-0amount_oc  = 0.
      ls_data-0quantity   = lv_roundq.
      ls_data-bpcaudit    = 'FWD'.
      ls_data-bpcversin   = ls_plan-bpcversin.
      COLLECT ls_data INTO c_th_data.
      ls_data-0quantity  = ls_data-0quantity  * -1.
      ls_data-0fiscper  = lv_version.
      ls_data-bpcaudit    = 'CAR'.
      COLLECT ls_data INTO c_th_data.
    ENDIF.

    IF ls_plan IS INITIAL.     "plan is not avaliable for this selection and check actuals
      CLEAR ls_wbs_elemnt.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_breakdown-0wbs_elemt.

      CLEAR ls_total_act. "total of the actuals without per.
      READ  TABLE lt_total_act INTO ls_total_act WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                                         0acttype   = ls_breakdown-0acttype
                                                         0costcenter = ls_breakdown-0costcenter
                                                         0costelmnt  = ls_breakdown-0costelmnt.

* check if there is accumulated plan already ?
      CLEAR:lv_act_prev_amount, lv_act_prev_quantity.
      CLEAR ls_actplan.
      READ TABLE lt_actplan INTO ls_actplan WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                                          0acttype   = ls_breakdown-0acttype
                                                          0costcenter = ls_breakdown-0costcenter
                                                          0costelmnt  = ls_breakdown-0costelmnt
                                                          0unit = ''.
      IF sy-subrc = 0.
        lv_act_prev_amount = ls_actplan-0amount_oc.
      ENDIF.

      CLEAR ls_actplan.
      READ TABLE lt_actplan INTO ls_actplan WITH KEY  0wbs_elemt = ls_breakdown-0wbs_elemt
                                                          0acttype   = ls_breakdown-0acttype
                                                          0costcenter = ls_breakdown-0costcenter
                                                          0costelmnt  = ls_breakdown-0costelmnt
                                                          0unit = 'H'.
      IF sy-subrc = 0.
        lv_act_prev_quantity = ls_actplan-0quantity.
      ENDIF.
      "this means this breakdown comes from actuals.%100 from next month.
      lv_nextper = lv_version + 1.
      IF lv_nextper+4(3) > 012.
        lv_year = lv_year + 1.
        CONCATENATE lv_year '001' INTO lv_nextper.
      ENDIF.
      ls_data-0fiscper    = lv_nextper.
      ls_data-0fiscvarnt  = 'Z4'.    "ls_plan-0fiscvarnt.'
      ls_data-0acttype    = ls_breakdown-0acttype.
      "
      ls_data-0costcenter = ls_breakdown-0costcenter.
      ls_data-0co_area    = 'TE01'.
      ls_data-0costelmnt  = ls_breakdown-0costelmnt ."ls_plan-0co_area.'
      ls_data-0infoprov   = 'BPCFID01' .
      ls_data-0wbs_elemt  = ls_breakdown-0wbs_elemt.
      CLEAR ls_wbs_elemnt.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt
                        WHERE objvers = 'A' AND wbs_elemt = ls_breakdown-0wbs_elemt.
      ls_data-0project    = ls_wbs_elemnt-project. "
      ls_data-0version    = '000'.
      ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
      ls_data-bpcaudit    = 'FWD'.
      ls_data-bpcversin   = lv_version.
      ls_data-0obj_cur    = ls_wbs_elemnt-obj_curr.
      ls_data-0amount_oc     = ( ls_total_act-0amount_oc -  lv_act_prev_amount ) * -1.
      ls_data-0quantity   = 0.
      ls_data-0unit       = ''.
      COLLECT ls_data INTO c_th_data.
      ls_data-0amount_oc  = ls_data-0amount_oc * -1.
      ls_data-0fiscper  = lv_version.
      ls_data-bpcaudit    = 'CAR'.
      COLLECT ls_data INTO c_th_data.
      ls_data-0obj_cur    = ''.
      ls_data-0unit       = 'H'.
      ls_data-0amount_oc   = 0.
      ls_data-bpcaudit    = 'FWD'.
      ls_data-0fiscper  = lv_nextper.
      ls_data-bpcversin   = lv_version.
      ls_data-0quantity   = ( ls_total_act-0quantity  -  lv_act_prev_quantity ) * -1.
      COLLECT ls_data INTO c_th_data.
      ls_data-0quantity  = ls_data-0quantity  * -1.
      ls_data-0fiscper  = lv_version.
      ls_data-bpcaudit    = 'CAR'.
      COLLECT ls_data INTO c_th_data.


    ENDIF.


  ENDLOOP.

  DATA: plan_found(1) TYPE n,
        act_found(1)  TYPE n.
*expenses will be fowardded one month after
  LOOP AT lt_breakdown_exp INTO ls_breakdown_exp.
*check is an actual data is not avaliable in plan values should subtrc.
    CLEAR: ls_data,act_found,plan_found.
    lv_nextper = lv_version + 1.
    IF lv_nextper+4(3) > 012.
      lv_year = lv_year + 1.
      CONCATENATE lv_year '001' INTO lv_nextper.
    ENDIF.
    ls_data-0fiscper  =  lv_nextper.

    CLEAR ls_total_act_exp. "total of the actuals without per.
    READ TABLE lt_total_act_exp INTO ls_total_act_exp WITH KEY  0wbs_elemt = ls_breakdown_exp-0wbs_elemt
                                                        0acttype   = ls_breakdown_exp-0acttype
                                                        0costcenter = ls_breakdown_exp-0costcenter
                                                        0costelmnt  = ls_breakdown_exp-0costelmnt.
    IF sy-subrc = 0.
      act_found = 1.
    ENDIF.

    CLEAR ls_actplan_exp.
    READ TABLE lt_actplan_exp INTO ls_actplan_exp WITH KEY  0wbs_elemt = ls_breakdown_exp-0wbs_elemt
                                                        0acttype   = ls_breakdown_exp-0acttype
                                                        0costcenter = ls_breakdown_exp-0costcenter
                                                         0costelmnt  = ls_breakdown_exp-0costelmnt.

    IF sy-subrc = 0.
      plan_found = 1.
    ENDIF.

    CLEAR lv_dif.
    lv_dif = ls_actplan_exp-0amount_oc - ls_total_act_exp-0amount_oc .

    IF act_found = 1 AND plan_found = 1.
      " lv_dif = ls_actplan_exp-0amount_oc - ls_total_act_exp-0amount_oc .   "amount to be distributed.
      ls_data-0amount_oc   = lv_dif .
      ls_data-0fiscvarnt = ls_actplan_exp-0fiscvarnt.
      ls_data-0acttype    = ls_actplan_exp-0acttype.

      ls_data-0costcenter = ls_actplan_exp-0costcenter.
      ls_data-0costelmnt  = ls_actplan_exp-0costelmnt.
      ls_data-0co_area    = ls_actplan_exp-0co_area.
      ls_data-0infoprov   = ls_actplan_exp-0infoprov .
      ls_data-0version    = ls_actplan_exp-0version.
      ls_data-0wbs_elemt  = ls_actplan_exp-0wbs_elemt.
      CLEAR ls_wbs_elemnt.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt
                        WHERE objvers = 'A' AND wbs_elemt = ls_actplan_exp-0wbs_elemt.
      ls_data-0project    = ls_wbs_elemnt-project. "
      ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
      ls_data-bpcaudit    = 'FWD'.
      ls_data-bpcversin   = ls_actplan_exp-bpcversin.
      ls_data-0obj_cur    = ls_actplan_exp-0obj_cur.
      ls_data-0unit       = ''.
      COLLECT ls_data INTO c_th_data.
      "this means this breakdown comes from actuals.%100 from next month.
    ENDIF.

    IF act_found = 1 AND plan_found = 0.

      ls_data-0fiscvarnt  = 'Z4'.    "ls_plan-0fiscvarnt.'
      ls_data-0acttype    = ls_breakdown_exp-0acttype.

      ls_data-0costcenter = ls_breakdown_exp-0costcenter.
      ls_data-0costelmnt  = ls_breakdown_exp-0costelmnt.  "  "ls_plan-0co_area.'
      ls_data-0infoprov   = 'BPCFID01' .
      ls_data-0wbs_elemt  = ls_breakdown_exp-0wbs_elemt.

      CLEAR ls_wbs_elemnt.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt
                        WHERE objvers = 'A' AND wbs_elemt = ls_breakdown_exp-0wbs_elemt.
      ls_data-0project    = ls_wbs_elemnt-project. "
      ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
      " ls_data-0project    = lv_project. "
      ls_data-0version    = '000'.

      ls_data-bpcaudit    = 'FWD'.
      ls_data-0co_area    = 'TE01'  ."'
      ls_data-bpcversin   = lv_version.
      ls_data-0obj_cur    = ls_total_act_exp-0obj_cur.
      ls_data-0unit       = ''.
      ls_data-0amount_oc     = lv_dif .
      COLLECT ls_data INTO c_th_data.


    ENDIF.

    IF act_found = 0 AND plan_found = 1.

      ls_data-0amount_oc  = lv_dif .
      ls_data-0fiscvarnt  = ls_actplan_exp-0fiscvarnt.
      ls_data-0acttype    = ls_actplan_exp-0acttype.

      ls_data-0costcenter = ls_actplan_exp-0costcenter.
      ls_data-0costelmnt  = ls_actplan_exp-0costelmnt.
      ls_data-0co_area    = ls_actplan_exp-0co_area.
      ls_data-0infoprov   = ls_actplan_exp-0infoprov .
      ls_data-0wbs_elemt  = ls_actplan_exp-0wbs_elemt.
      CLEAR ls_wbs_elemnt.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt
                        WHERE objvers = 'A' AND wbs_elemt = ls_actplan_exp-0wbs_elemt.
      ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
      "ls_data-0project    = ls_actplan_exp-0project.
      ls_data-0project    = ls_wbs_elemnt-project.
      ls_data-0version    = ls_actplan_exp-0version.
      ls_data-bpcaudit    = 'FWD'.
      ls_data-bpcversin   = lv_version.
      ls_data-0obj_cur    = ls_actplan_exp-0obj_cur.
      ls_data-0unit       = ''.
      COLLECT ls_data INTO c_th_data.
      "this means this breakdown comes from actuals.%100 from next month.

    ENDIF.
    ls_data-0fiscper  = lv_version.
    ls_data-0amount_oc = ls_data-0amount_oc * -1.
    ls_data-bpcaudit    = 'CAR'.
    COLLECT ls_data INTO c_th_data.


  ENDLOOP.



ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->COPY_PER_AV
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD copy_per_av.

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a08c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a08c.
  DATA: lt_charsel           TYPE rsplf_t_charsel,
        l_r_msg              TYPE REF TO cl_rsplfu_msg,
        ls_charsel           LIKE LINE OF lt_charsel,
        lv_compcode          TYPE /bi0/oicomp_code,
        lv_version           TYPE /bic/oibpcversin,
        l_dummy              TYPE c,
        ls_wbs_elemnt        TYPE /bi0/pwbs_elemt,
        ls_comp_code         TYPE /bi0/pcomp_code,
        lt_hours             TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_hours             TYPE zbpce_pal_cck_a08c,
        lt_exp               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_exp               TYPE zbpce_pal_cck_a08c,
        lt_act               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_act               TYPE zbpce_pal_cck_a08c,
        lt_plan              TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_plan              TYPE zbpce_pal_cck_a08c,
        lt_actplan           TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_actplan           TYPE zbpce_pal_cck_a08c,
        lt_bet               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_bet               TYPE zbpce_pal_cck_a08c,
        lt_total             TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total             TYPE zbpce_pal_cck_a08ct,
        lt_total_act         TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total_act         TYPE zbpce_pal_cck_a08ct,
        ls_conditions        TYPE zbpce_pal_cck_a03c,
        ls_breakdown         TYPE zbpce_breakdown,
        lt_breakdown         TYPE TABLE OF zbpce_breakdown,
        lv_f_beg             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a03c-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a08c,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i,
        lv_lastday           TYPE d,
        lt_map               TYPE TABLE OF zwbs_map_01,
        ls_map               TYPE zwbs_map_01,
        ls_project           TYPE /bi0/mproject,
        lv_lastdaydats       TYPE dats,
        lv_famount           TYPE f.

*
  DATA: ls_costelemnt TYPE /bi0/mcostelmnt,
        lt_costelemnt TYPE TABLE OF /bi0/mcostelmnt.
  DATA: ls_bpcfid90 TYPE /bic/abpcfid907,
        lt_bpcfid90 TYPE TABLE OF /bic/abpcfid907.

  SELECT comp_code AS comp_code, calmonth AS calmonth
       FROM /bic/abpcfid907
       INTO CORRESPONDING FIELDS OF TABLE  @lt_bpcfid90.

**********************************************************************
  DATA :
    lv_prevper TYPE /bic/oibpcversin,
    lv_year(4) TYPE n.

*
  l_r_msg ?= i_r_msg.
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.
  lv_prevper = lv_version .

  IF lv_prevper+4(3) NE '001'.
    lv_prevper = lv_prevper - 1.
  ELSE.
    lv_year = lv_year - 1.
    CONCATENATE lv_year '012' INTO lv_prevper.
  ENDIF.
  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0WBS_ELEMT__0COMP_CODE'.
  lv_compcode = ls_charsel-low.
  DATA: kontrol TYPE i.
* control if data avaliable already in the version dont touch it.
  kontrol = 0.

  LOOP AT c_th_data ASSIGNING <ls_data>.
    <ls_data>-0amount_oc = 0.
  ENDLOOP.

  "CHECK c_th_data IS INITIAL.

  IF  i_th_ref_data IS INITIAL.
    MESSAGE e007(zbpce) WITH lv_version lv_compcode INTO l_dummy.
    l_r_msg->add_msg( ).
  ENDIF.
*status of the Project should be checked.
  LOOP AT i_th_ref_data ASSIGNING <ls_data>.
    MOVE <ls_data> TO ls_plan.
    "   SELECT SINGLE * INTO ls_project  FROM /bi0/mproject WHERE objvers = 'A' AND project = ls_plan-0project.
    "   check ls_project-statussys0 =  '02'.
    "  CHECK ls_plan-bpcaudit NE 'ACPLN'.
    ls_plan-bpcversin = lv_version.
    IF ls_plan-0fiscper <  lv_version.
      IF ls_plan-0fiscper NE '0000000'.
        ls_plan-0fiscper = lv_prevper.
      ENDIF.
      "clear ls_plan-0fiscper.
      ls_plan-bpcaudit = 'ACPLN'.
      COLLECT ls_plan INTO c_th_data .
    ELSE.

      COLLECT ls_plan INTO c_th_data .

    ENDIF.
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->DRCOST_01
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD drcost_01.

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a06.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a06.
  DATA: lt_charsel    TYPE rsplf_t_charsel,
        ls_charsel    LIKE LINE OF lt_charsel,
        lv_compcode   TYPE /bi0/oicomp_code,
        lv_version    TYPE /bic/oibpcversin,
        lv_project    TYPE /bi0/oiproject,
        ls_project    TYPE /bi0/pproject,
        ls_wbs_elemnt TYPE /bi0/pwbs_elemt,
        lt_hours      TYPE TABLE OF zbpce_pal_cck_a06,
        ls_hours      TYPE zbpce_pal_cck_a06,
        ls_comp_code  TYPE /bi0/pcomp_code,
*        lt_conditions TYPE HASHED TABLE OF /BIC/AFICOA017
*                                 WITH UNIQUE  default KEY,
*        "  WITH NON-UNIQUE SORTED KEY A2 components ACTTYPe comp_code costcenter employee wbs_elemt validto,
*
*        ls_conditions TYPE /BIC/AFICOA017,
        BEGIN OF ls_conditions,
          co_area        TYPE /bi0/oico_area,
          comp_code      TYPE /bi0/oicomp_code,
          costcenter     TYPE /bi0/oicostcenter,
          acttype        TYPE /bi0/oiacttype,
          /bic/srvcstlvl TYPE /bic/oisrvcstlvl,
          employee       TYPE  /bi0/oiemployee,
          wbs_elemt      TYPE /bi0/oiwbs_elemt,
          validto        TYPE /bi0/oivalidto,
          validfrom      TYPE /bi0/oivalidfrom,
          amount         TYPE /bi0/oiamount_oc,
          quantity       TYPE /bi0/oiquantity,
          currency       TYPE /bi0/oicurrency,
          unit           TYPE /bi0/oiunit,
        END OF ls_conditions,
        lt_conditions        LIKE TABLE OF ls_conditions,
       " lt_zcoste_map_01     TYPE TABLE OF zcoste_map_01,
        lv_f_beg             TYPE zbpce_pal_cck_a06-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a06-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a06-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a06,
        lt_inflation_base    TYPE TABLE OF /bic/abpcfid027,
        ls_inflation_base    TYPE  /bic/abpcfid027,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i.

*          lv_costcntr          TYPE /bic/oipcostcntr,
*          lv_coarea            TYPE /bic/oipcoarea.
*
**********************************************************************
*
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.


 READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0COMP_CODE'.
 lv_compcode  = ls_charsel-low.
*
*INFLATION RATES LOOKUP  - get the rates of  Version "0". after the bpcversin

  SELECT fiscper AS fiscper , fiscvarnt AS fiscvarnt, version AS version, comp_code AS comp_code , co_area AS co_area,
           SUM( /bic/inf_rate ) AS /bic/inf_rate
           FROM /bic/abpcfid027
           INTO CORRESPONDING FIELDS OF TABLE  @lt_inflation_base
           WHERE version EQ '000'
             AND fiscvarnt EQ 'Z4'
             AND fiscper >= @lv_version
           GROUP BY comp_code ,co_area, fiscper, fiscvarnt, version.
  DELETE lt_inflation_base WHERE /bic/inf_rate  EQ 0.

*INFLATION RATES LOOKUP
* CALCULATE the rates to be used
  DATA period TYPE /bi0/oifiscper.
  DATA current_months TYPE /bi0/oifiscper.
  DATA previus_months TYPE /bi0/oifiscper.
  DATA amount TYPE f.
  DATA qtd_month TYPE i.
  DATA lv_month(3) TYPE n.
  DATA inf_rate_sum  TYPE f.
  DATA lv_year(4) TYPE n.
  DATA: lv_lastday           TYPE d.
 " lv_compcode =  'BE74'.
  period = lv_version.
*there wont be inflation in the beginning.
  "loop at lt_comp_code.
  qtd_month = 0.
  inf_rate_sum = 0.
  period = period + 1.
  DO.

    READ TABLE lt_inflation_base INTO ls_inflation_base WITH KEY fiscper = period
                                                            comp_code = lv_compcode.
    IF sy-subrc = 0.
      inf_rate_sum  = ls_inflation_base-/bic/inf_rate + inf_rate_sum.
    ENDIF.
    MOVE-CORRESPONDING ls_inflation_base TO ls_inflation.
    ls_inflation-fiscper = period.
    ls_inflation-/bic/inf_rate  = inf_rate_sum.
    COLLECT ls_inflation INTO lt_inflation.
    qtd_month = qtd_month + 1.
    lv_year = period+0(4).
    lv_month = period+4(3).
    lv_month = lv_month + 1.
    IF lv_month = 013.
      lv_month = 001.
      lv_year = lv_year + 1.
    ENDIF.
    CONCATENATE lv_year lv_month INTO period.
* 120 MONTHS = 10 YEARS AHEAD
    IF qtd_month = 120.
      EXIT.
    ENDIF.
  ENDDO.
  "endloop.
** CALCULATE the rates to be used

*CONDITIONS LOOKUP

  CLEAR:gs_range,gt_range,gs_sfc,gt_sfc,gs_sfk,gt_sfk.

  gs_sfc-chanm = '0CO_AREA'. gs_sfc-chaalias = 'CO_AREA '. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0PCOMP_CODE'. gs_sfc-chaalias = 'COMP_CODE'. APPEND gs_sfc TO gt_sfc. " 0PComp_code = Company Code to be billed
  gs_sfc-chanm = '0COSTCENTER'. gs_sfc-chaalias = 'COSTCENTER'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0ACTTYPE'. gs_sfc-chaalias = 'ACTTYPE'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = 'SRVCSTLVL'. gs_sfc-chaalias = '/BIC/SRVCSTLVL'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0EMPLOYEE'. gs_sfc-chaalias = 'EMPLOYEE'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0WBS_ELEMT'. gs_sfc-chaalias = 'WBS_ELEMT'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0VALIDTO'. gs_sfc-chaalias = 'VALIDTO'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0VALIDFROM'. gs_sfc-chaalias = 'VALIDFROM'. APPEND gs_sfc TO gt_sfc.
  "kyf alan donusumlerini yap
  gs_sfk-kyfnm = '0AMOUNT'. gs_sfk-kyfalias = 'AMOUNT'. gs_sfk-aggr = 'SUM'. APPEND gs_sfk TO gt_sfk.
  gs_sfk-kyfnm = '0QUANTITY'. gs_sfk-kyfalias = 'QUANTITY'. gs_sfk-aggr = 'SUM'. APPEND gs_sfk TO gt_sfk.

*
  CALL METHOD _read_cube_data
    EXPORTING
      i_infoprov = 'FICOV01'
      i_t_sfc    = gt_sfc
      i_t_sfk    = gt_sfk
      i_t_range  = gt_range
 "    i_collect  = 'X'
"     i_keydate  =
    IMPORTING
      e_t_data   = lt_conditions.
  SORT lt_conditions  BY comp_code acttype costcenter employee wbs_elemt validto DESCENDING.
*CONDITIONS LOOKUP

*REF DATA contains hours inputed
  LOOP AT i_th_ref_data ASSIGNING <ls_data>.

    IF <ls_data>-bpcaudit = 'M_INPUT'.
      COLLECT <ls_data> INTO lt_hours.
    ENDIF.

  ENDLOOP.
*REF DATA contains hours inputed
*before calculate the direct_cost- first delete

  LOOP AT c_th_data ASSIGNING <ls_data>.
    <ls_data>-0amount_oc = 0.
  ENDLOOP.
*Project only ?
  LOOP AT lt_hours INTO ls_hours.

    CALL FUNCTION 'Z_LAST_DAY_IN_FISCAL_PERIOD'
      EXPORTING
        i_fper    = ls_hours-0fiscper
        i_fvarnt  = 'Z4'
      IMPORTING
        e_lastday = lv_lastday.

    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_hours-0wbs_elemt.
*check company code is filled.
    CLEAR: ls_data, ls_conditions, ls_inflation, lv_found.

    IF ls_hours-0employee IS NOT INITIAL.
      LOOP AT lt_conditions INTO ls_conditions WHERE  acttype  = ls_hours-0acttype
                                                AND             costcenter = ''  "ls_hours-0costcenter
                                                AND             employee = ls_hours-0employee
                                                AND             comp_code = ls_wbs_elemnt-comp_code
                                                AND             wbs_elemt = ls_hours-0wbs_elemt
                                                AND             validto GE lv_lastday
                                                AND             validfrom  LE lv_lastday .
        lv_found = 1.
        EXIT.

      ENDLOOP.



      IF  lv_found = 0.

        LOOP AT lt_conditions INTO ls_conditions  WHERE  acttype  = ls_hours-0acttype
                                                   AND   costcenter = '' " ls_hours-0costcenter
                                                   AND   employee = ls_hours-0employee
                                                   AND   comp_code = ls_wbs_elemnt-comp_code
                                                   AND   wbs_elemt = ''
                                                   AND   validto GE  lv_lastday
                                                   AND   validfrom LE lv_lastday .

          lv_found = 1.
          EXIT.

        ENDLOOP.
      ENDIF.
*this selection has been removed at 29.9.2021 -Intercompany DC calculation
*      if lv_found = 0.
*        loop at  lt_conditions into ls_conditions where  acttype  = ls_hours-0ACTTYPE
*                                                  AND       costcenter = ''
*                                                  AND       employee = ls_hours-0employee
*                                                  AND       comp_code = ls_wbs_elemnt-COMP_CODE
*                                                  AND        WBS_ELEMT = ''
*                                                  and             validto ge  lv_lastday
*                                                  and             validfrom  le lv_lastday .
*
*          lv_found = 1.
*          exit.
*
*        endloop.
*      endif.
      .
    ENDIF.  "if employee is not initial
    IF ( ls_hours-0employee IS  INITIAL ) OR lv_found = 0.

      LOOP AT lt_conditions INTO ls_conditions WHERE  acttype  = ls_hours-0acttype
                                                 AND  costcenter = ls_hours-0costcenter
                                                 AND  employee = ''
                                                 AND  comp_code = ls_wbs_elemnt-comp_code
                                                 AND  wbs_elemt = ls_hours-0wbs_elemt
                                                 AND  validto GE  lv_lastday
                                                 AND  validfrom  LE lv_lastday .
        lv_found = 1.
        EXIT.

      ENDLOOP.


      IF lv_found = 0.
        LOOP AT lt_conditions INTO ls_conditions  WHERE  acttype  = ls_hours-0acttype
                                                    AND  costcenter = ls_hours-0costcenter
                                                    AND  employee = ''
                                                    AND  comp_code = ls_wbs_elemnt-comp_code
                                                    AND  wbs_elemt = ''
                                                    AND  validto GE  lv_lastday
                                                    AND  validfrom  LE lv_lastday .

          lv_found = 1.
          EXIT.

        ENDLOOP.
      ENDIF.
      IF lv_found = 0.

        LOOP AT  lt_conditions INTO ls_conditions WHERE  acttype  = ls_hours-0acttype
                                                   AND   costcenter = ''
                                                   AND   employee = ''
                                                   AND   comp_code = ls_wbs_elemnt-comp_code
                                                   AND   wbs_elemt = ''
                                                   AND   validto GE  lv_lastday
                                                   AND   validfrom  LE lv_lastday .

          lv_found = 1.
          EXIT.

        ENDLOOP.
      ENDIF.
    ENDIF.

    IF lv_found = 1.
* get the inflation rate
      CLEAR ls_inflation.
      READ TABLE lt_inflation INTO ls_inflation WITH KEY fiscper = ls_hours-0fiscper
                                                         fiscvarnt = ls_hours-0fiscvarnt
                                                         version = '000'
                                                         comp_code = ls_wbs_elemnt-comp_code. "ls_hours-0comp_code.

*get the compnay code currency
      CLEAR ls_comp_code.
      SELECT SINGLE * INTO ls_comp_code  FROM /bi0/mcomp_code WHERE objvers = 'A' AND comp_code = ls_wbs_elemnt-comp_code.

      ls_data-0fiscper  = ls_hours-0fiscper.
      ls_data-0fiscvarnt = ls_hours-0fiscvarnt.
      ls_data-0acttype    = ls_hours-0acttype.
      ls_data-0comp_code  =  ls_wbs_elemnt-comp_code..
      ls_data-0costcenter = ls_hours-0costcenter.
      ls_data-0costelmnt  = ls_hours-0costelmnt.
      ls_data-0co_area    = ls_hours-0co_area.
      ls_data-0employee   = ls_hours-0employee.
      ls_data-0infoprov   = ls_hours-0infoprov .
      ls_data-0project    = ls_hours-0project.
      ls_data-0version    = ls_hours-0version.
      ls_data-0wbs_elemt  = ls_hours-0wbs_elemt.
      ls_data-bpcaudit    = 'DCOST'.
      ls_data-bpcversin   = ls_hours-bpcversin.
      ls_data-0obj_cur   = ls_comp_code-currency.
      ls_data-0unit       = ''.
      ls_data-0amount_oc     = ls_hours-0quantity * ( ls_conditions-amount /  ls_conditions-quantity )
                                         * ( 1 + ls_inflation-/bic/inf_rate ) . " / 100 ).
      ls_data-0quantity   = 0.

      COLLECT ls_data INTO c_th_data.
    ENDIF.
  ENDLOOP.

** Message
*  CLEAR:g_s_message.
*  g_s_message-msgty = 'W'.
*  g_s_message-msgv1 = 'Errors:' ."&& ls_mat_plant-mat_plant && '|'.
*  g_s_message-msgv2 = 'check the fields:'." && ls_mat_plant-plant && '|'.
**        g_s_message-msgv3 =
*  g_s_message-msgv4 = ' .'.
*  COLLECT g_s_message INTO g_t_message.


*  SELECT ACTTYPE AS ACTTYPE, COMP_CODE AS COMP_CODE, COSTCENTER AS COSTCENTER, CO_AREA AS CO_AREA,
*              EMPLOYEE AS EMPLOYEE, WBS_ELEMT AS WBS_ELEMT, RELEASESTRAT AS RELEASESTRAT, /BIC/SRVCSTLVL AS /BIC/SRVCSTLVL,
*               VALIDTO AS VALIDTO, VALIDFROM AS VALIDFROM,
*              CURRENCY AS CURRENCY,
*           SUM( AMOUNT ) AS AMOUNT,
*           SUM( QUANTITY ) AS QUANTITY
*             FROM /BIC/AFICOA017
*             INTO CORRESPONDING FIELDS OF TABLE  @LT_CONDITIONS
*               GROUP BY ACTTYPE , COMP_CODE, COSTCENTER, CO_AREA, EMPLOYEE , WBS_ELEMT, RELEASESTRAT,  /BIC/SRVCSTLVL,VALIDTO,VALIDFROM, CURRENCY.
*  DELETE LT_CONDITIONS WHERE amount  EQ 0.
*
*  sort LT_CONDITIONS  by ACTTYPe comp_code costcenter employee wbs_elemt validto descending.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->HIS_UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD his_upload.

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_his_a07c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_his_a07c.
  DATA: lt_charsel     TYPE rsplf_t_charsel,
        ls_charsel     LIKE LINE OF lt_charsel,
        lv_compcode    TYPE /bi0/oicomp_code,
        lv_version     TYPE /bic/oibpcversin,
        lv_project     TYPE /bi0/oiproject,
        ls_project     TYPE /bi0/pproject,
        ls_wbs_elemnt  TYPE /bi0/pwbs_elemt,
        ls_comp_code   TYPE /bi0/pcomp_code,
        lt_plan        TYPE TABLE OF zbpce_pal_his_a07c,
        ls_plan        TYPE zbpce_pal_his_a07c,
        lt_act         TYPE TABLE OF zbpce_pal_his_a07c,
        ls_act         TYPE zbpce_pal_his_a07c,
        lt_actplan     TYPE TABLE OF zbpce_pal_his_a07c,
        ls_actplan     TYPE zbpce_pal_his_a07c,
        lv_found       TYPE i,
        lv_lastday     TYPE d,
        lt_map         TYPE TABLE OF zwbs_map_01,
        ls_map         TYPE zwbs_map_01,
        lv_lastdaydats TYPE dats,
        lv_famount     TYPE f,
        lv_difamount   TYPE  decfloat34, " /bi0/oiquantity.
        lv_round       TYPE  /bi0/oiamount_oc,
        lv_roundq      TYPE  /bi0/oiquantity.

*
  DATA: ls_costelemnt  TYPE /bi0/mcostelmnt,
        ls_costelemnt2 TYPE /bi0/mcostelmnt,
        lt_costelemnt  TYPE TABLE OF /bi0/mcostelmnt.
  DATA :lv_act_prev_amount   TYPE /bi0/oiamount_oc,
        lv_act_prev_quantity TYPE /bi0/oiquantity.


  SELECT costelmnt AS costelmnt, /bic/celmsubgr AS /bic/celmsubgr,
         /bic/celmgroup AS /bic/celmgroup,
         cstelmntyp AS cstelmntyp
          FROM /bi0/mcostelmnt
          INTO CORRESPONDING FIELDS OF TABLE  @lt_costelemnt
          WHERE   objvers = 'A'.
  "  AND ( cstelmntyp = '21' OR cstelmntyp = '01' ).
  "     AND  /bic/celmtype =  'GEXP'.
  .

  SELECT inputtype AS inputtype, ps_prjtype AS ps_prjtype,
           costelmnt AS costelmnt
          FROM  zwbs_map_01
          INTO CORRESPONDING FIELDS OF TABLE  @lt_map.

**********************************************************************
*
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0PROJECT'.
  lv_project = ls_charsel-low.
**REVENUES LUMP
*GREV0100
*GREV0200
*GREV0300
*GREV0400
*GREV0500
*REVENUES TM
*GREV0200

  LOOP AT i_th_ref_data ASSIGNING <ls_data>.
    "  CHECK <ls_data>-0fiscper <= lv_version.  "NAZ
    " CHECK <ls_data>-0fiscper < lv_version.    "NAZ

    MOVE <ls_data> TO ls_act.
    CHECK ls_act-datatype = 'AMT' OR ls_act-datatype = 'BET'.
    CHECK ls_act-ip_origin = 'PLA'.
    CLEAR ls_wbs_elemnt.
    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_act-0wbs_elemt.
    CHECK ls_wbs_elemnt-ps_level IS NOT INITIAL.
    ls_act-0project = ls_wbs_elemnt-project.
    IF  ls_wbs_elemnt-ps_level NE 3.
      ls_act-0wbs_elemt = ls_wbs_elemnt-/bic/wbs_el3.
      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_act-0wbs_elemt.
    ENDIF.
    "  CHECK ls_wbs_elemnt-/bic/wbs_el3 IS NOT INITIAL.
    "   CHECK  ( ls_wbs_elemnt-/bic/ps_rkeyan = 'Z00001'  OR ls_wbs_elemnt-/bic/ps_rkeyan = 'Z99999' ).
    CLEAR ls_costelemnt.
    READ TABLE lt_costelemnt INTO ls_costelemnt WITH  KEY  costelmnt = ls_act-0costelmnt.
    IF sy-subrc NE 0.
      CLEAR ls_costelemnt2.
      READ TABLE lt_costelemnt INTO ls_costelemnt WITH  KEY  /bic/celmgroup = ls_act-celmgroup
                                                              cstelmntyp  = '21'.
    ENDIF.

    CASE ls_costelemnt-/bic/celmgroup+0(4).
      WHEN 'GHRS'.
        CLEAR ls_costelemnt2.
        READ TABLE lt_costelemnt INTO ls_costelemnt2 WITH  KEY  /bic/celmgroup = ls_costelemnt-/bic/celmgroup
                                                                cstelmntyp  = '21'.
        ls_act-0costelmnt = ls_costelemnt2-costelmnt.
        COLLECT ls_act INTO lt_act.    "actuals all

      WHEN 'GEXP'.
        CLEAR ls_costelemnt2.
        READ TABLE lt_costelemnt INTO ls_costelemnt2 WITH  KEY  /bic/celmgroup = ls_costelemnt-/bic/celmgroup
                                                                cstelmntyp  = '21'.
        ls_act-0costelmnt = ls_costelemnt2-costelmnt.
        ls_act-0unit = 'H'.
        COLLECT ls_act INTO lt_act.   "actuals all

    ENDCASE.
  ENDLOOP.





*    IF act_found = 1 AND plan_found = 1.
*      " lv_dif = ls_actplan_exp-0amount_oc - ls_total_act_exp-0amount_oc .   "amount to be distributed.
*      ls_data-0amount_oc   = lv_dif .
*      ls_data-0fiscvarnt = ls_actplan_exp-0fiscvarnt.
*      ls_data-0acttype    = ls_actplan_exp-0acttype.
*
*      ls_data-0costcenter = ls_actplan_exp-0costcenter.
*      ls_data-0costelmnt  = ls_actplan_exp-0costelmnt.
*      ls_data-0co_area    = ls_actplan_exp-0co_area.
*      ls_data-0infoprov   = ls_actplan_exp-0infoprov .
*      ls_data-0version    = ls_actplan_exp-0version.
*      ls_data-0wbs_elemt  = ls_actplan_exp-0wbs_elemt.
*      CLEAR ls_wbs_elemnt.
*      SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt
*                        WHERE objvers = 'A' AND wbs_elemt = ls_actplan_exp-0wbs_elemt.
*      ls_data-0project    = ls_wbs_elemnt-project. "
*     ls_data-0comp_code  =  ls_wbs_elemnt-COMP_CODE..
*      ls_data-bpcaudit    = 'FWD'.
*      ls_data-bpcversin   = ls_actplan_exp-bpcversin.
*      ls_data-0obj_cur    = ls_actplan_exp-0obj_cur.
*      ls_data-0unit       = ''.
*      COLLECT ls_data INTO c_th_data.
*      "this means this breakdown comes from actuals.%100 from next month.
*    ENDIF.



ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~ADD_NEW_BLOCKS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_TS_EXISTING_BLOCKS           TYPE        SORTED TABLE
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TS_NEW_BLOCKS                TYPE        SORTED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~ADD_NEW_BLOCKS.

*   DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .
*
**
*    ls_charsel-sign = 'I'.
*    ls_charsel-opt = 'EQ'.
**
*   ls_charsel-iobjnm = '0INFOPROV'.
*    ls_charsel-low = 'FICOV01'.
*    APPEND ls_charsel TO e_t_ref_charsel.
**read ods data
*  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~EXECUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~EXECUTE.
 DATA:lr_rsplfa_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA:l_method TYPE string.

    lr_rsplfa_param_elem = i_r_param_set->get_param_elem( 'ZPR_WREF' ).
    lr_rsplfa_param_elem->get_value( IMPORTING e_value = g_v_ftype ).

"DRCOST_01
* g_v_type = Aggr.lvl ID & '_' & Step No (Sample:PCEE_ALC1_01)
    l_method = g_v_ftype.
    if g_v_ftype =  'UPDATE_CARH' or g_v_ftype = 'UPDATE_CARE'.
      l_method = 'UPDATE_CAR'.
    endif.
    CALL METHOD me->(l_method)
      EXPORTING
        i_r_param_set  = i_r_param_set
        i_th_ref_data  = i_th_ref_data
        i_s_block_line = i_s_block_line
        i_r_msg        = i_r_msg
      CHANGING
        c_th_data      = c_th_data.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~FINISH_EXECUTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~FINISH_EXECUTION.
   DATA:l_dummy TYPE c.

    LOOP AT g_t_message INTO g_s_message.
      CASE g_s_message-msgty.
        WHEN 'E'.
          MESSAGE e000(rspls) WITH g_s_message-msgv1
                                   g_s_message-msgv2
                                   g_s_message-msgv3
                                   g_s_message-msgv4 INTO l_dummy.
        WHEN 'W'.
          MESSAGE w000(rspls) WITH g_s_message-msgv1
                                   g_s_message-msgv2
                                   g_s_message-msgv3
                                   g_s_message-msgv4 INTO l_dummy.

        WHEN 'I'.
          MESSAGE i000(rspls) WITH g_s_message-msgv1
                                   g_s_message-msgv2
                                   g_s_message-msgv3
                                   g_s_message-msgv4 INTO l_dummy.
        WHEN 'S'.
          MESSAGE s000(rspls) WITH g_s_message-msgv1
                                   g_s_message-msgv2
                                   g_s_message-msgv3
                                   g_s_message-msgv4 INTO l_dummy.
      ENDCASE.
      i_r_msg->add_msg( '1' ).
    ENDLOOP.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~GET_REF_DATA_SEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~GET_REF_DATA_SEL.

 DATA:lr_rsplfa_param_elem TYPE REF TO if_rsplfa_param_elem.
    DATA:l_method TYPE string.
    lr_rsplfa_param_elem = i_r_param_set->get_param_elem( 'ZPR_WREF' ).
    lr_rsplfa_param_elem->get_value( IMPORTING e_value = g_v_ftype ).

* g_v_type = Aggr.lvl ID & '_' & Step No (Sample:PCEE_ALC1_01)
    l_method = 'REFDATA_' && g_v_ftype.
    CALL METHOD me->(l_method)
      EXPORTING
        i_t_data_charsel      = i_t_data_charsel
        i_t_data_node_sel     = i_t_data_node_sel
        i_r_param_set         = i_r_param_set
        i_r_msg               = i_r_msg
      IMPORTING
        e_th_no_ref_block_cha = e_th_no_ref_block_cha
        e_t_ref_charsel       = e_t_ref_charsel
        e_t_ref_node_sel      = e_t_ref_node_sel.



endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~INIT_EXECUTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_SRVTYPE_DEF                TYPE REF TO IF_RSPLFA_SRVTYPE_DEF
* | [--->] I_R_SRV                        TYPE REF TO IF_RSPLFA_SRV
* | [--->] I_R_INFOPROV_DESC              TYPE REF TO IF_RSPLFA_INFOPROV_DESC
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPLFA_SRVTYPE_IMP_EXEC_REF~INIT_EXECUTION.

  CLEAR:g_t_message.

"N_SRVNM = i_r_srv.
*DATA:
*ld_R_SRVNM TYPE RSPLF_S_SRV_ADMIN_INFO.
*
*
"DATA: lo_SRV TYPE REF TO IF_RSPLFA_SRV .

"n_seqnm = i_r_srv->P_R_VAR_CONT->N_SEQNM."   ->get_name().

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->PER_CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD per_check.

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_gen_al01.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a08c.
  DATA: lt_charsel     TYPE rsplf_t_charsel,
        l_r_msg        TYPE REF TO cl_rsplfu_msg,
        ls_charsel     LIKE LINE OF lt_charsel,
        lv_compcode    TYPE /bi0/oicomp_code,
        lv_version     TYPE /bic/oibpcversin,
        l_dummy        TYPE c,
        ls_wbs_elemnt  TYPE /bi0/pwbs_elemt,
        ls_comp_code   TYPE /bi0/pcomp_code,
        ls_data        TYPE zbpce_pal_cck_a08c,
        lt_inflation   TYPE TABLE OF /bic/abpcfid027,
        ls_inflation   TYPE  /bic/abpcfid027,
        lv_found       TYPE i,
        lv_lastday     TYPE d,
        lt_map         TYPE TABLE OF zwbs_map_01,
        ls_map         TYPE zwbs_map_01,
        lv_lastdaydats TYPE dats,
        lv_famount     TYPE f,
        ls_bpcfid90    TYPE /bic/abpcfid907,
        lt_bpcfid90    TYPE TABLE OF  /bic/abpcfid907.

  DATA: diff01      TYPE i,
        firstper    TYPE /bi0/oicalmonth,
        secondper   TYPE /bi0/oicalmonth,
        i_date_from TYPE dbervon,
        i_date_to   TYPE dbervon,
        e_months    TYPE vtbbewe-atage.

  DATA: ls_costelemnt TYPE /bi0/mcostelmnt,
        lt_costelemnt TYPE TABLE OF /bi0/mcostelmnt.

  SELECT comp_code AS comp_code, calmonth AS calmonth
          FROM /bic/abpcfid907
          INTO CORRESPONDING FIELDS OF TABLE  @lt_bpcfid90.
**********************************************************************
*
  l_r_msg ?= i_r_msg.
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.


  LOOP AT c_th_data ASSIGNING <ls_data>.
    CLEAR:  firstper, secondper.
    CLEAR ls_bpcfid90.
    READ TABLE  lt_bpcfid90 INTO ls_bpcfid90
                                   WITH KEY comp_code = <ls_data>-comp_code .
    CLEAR diff01.

    firstper = <ls_data>-1kyf_0calmonth.
    secondper = ls_bpcfid90-calmonth.
    IF firstper > secondper.
      CONCATENATE secondper '01' INTO i_date_from.
      CONCATENATE firstper '01' INTO i_date_to.
    ELSE.
      CONCATENATE firstper '01' INTO i_date_from.
      CONCATENATE  secondper '01' INTO i_date_to.
    ENDIF.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = i_date_from
        i_date_to   = i_date_to
      IMPORTING
        e_months    = e_months.
    diff01 = e_months.

    CHECK diff01 NE 0.
    IF diff01 NE 1 .
      <ls_data>-1kyf_0calmonth = ls_bpcfid90-calmonth.
      MESSAGE E008(zbpce) WITH ls_bpcfid90-comp_code INTO l_dummy.
      l_r_msg->add_msg( ). "&1 You can only change one period before or after.
      CONTINUE.
    ENDIF.


  ENDLOOP.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->PER_COPY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD per_copy.
**********************************************************************
*Logic reads the customer exit variable-ZCXPM_BPCVERSIN- active period
* of the company code
* if data avaliable before- it is not working
*if data doesnt avaliable for that period, it is calculating ACTPLN
*** Change  :
**********************************************************************

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a09c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a09c.
  DATA: lt_charsel           TYPE rsplf_t_charsel,
        l_r_msg              TYPE REF TO cl_rsplfu_msg,
        ls_charsel           LIKE LINE OF lt_charsel,
        lv_compcode          TYPE /bi0/oicomp_code,
        lv_version           TYPE /bic/oibpcversin,
        l_dummy              TYPE c,
        ls_wbs_elemnt        TYPE /bi0/pwbs_elemt,
        ls_comp_code         TYPE /bi0/pcomp_code,
        lt_hours             TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_hours             TYPE zbpce_pal_cck_a09c,
        lt_exp               TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_exp               TYPE zbpce_pal_cck_a09c,
        lt_act               TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_act               TYPE zbpce_pal_cck_a09c,
        lt_plan              TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_plan              TYPE zbpce_pal_cck_a09c,
        lt_actplan           TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_actplan           TYPE zbpce_pal_cck_a09c,
        lt_bet               TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_bet               TYPE zbpce_pal_cck_a09c,

        ls_conditions        TYPE zbpce_pal_cck_a03c,
        ls_breakdown         TYPE zbpce_breakdown,
        lt_breakdown         TYPE TABLE OF zbpce_breakdown,
        lv_f_beg             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a03c-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a09c,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i,
        lv_lastday           TYPE d,
        lt_map               TYPE TABLE OF zwbs_map_01,
        ls_map               TYPE zwbs_map_01,
        ls_project           TYPE /bi0/mproject,
        lv_lastdaydats       TYPE dats,
        lv_famount           TYPE f.

*
  DATA: ls_costelemnt TYPE /bi0/mcostelmnt,
        lt_costelemnt TYPE TABLE OF /bi0/mcostelmnt.
  DATA: ls_bpcfid90 TYPE /bic/abpcfid907,
        lt_bpcfid90 TYPE TABLE OF /bic/abpcfid907.

  SELECT comp_code AS comp_code, calmonth AS calmonth
       FROM /bic/abpcfid907
       INTO CORRESPONDING FIELDS OF TABLE  @lt_bpcfid90.

**********************************************************************
  DATA :
    lv_prevper TYPE /bic/oibpcversin,
    lv_year(4) TYPE n.

*
  l_r_msg ?= i_r_msg.
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.
  lv_prevper = lv_version .


  LOOP AT c_th_data ASSIGNING <ls_data>.
    <ls_data>-0amount_oc = 0.
    <ls_data>-0quantity = 0.
  ENDLOOP.

  CHECK i_th_ref_data IS NOT INITIAL.
  LOOP AT i_th_ref_data ASSIGNING <ls_data>.
    MOVE <ls_data> TO ls_plan.
    ls_plan-bpcversin = lv_version.
    CHECK  ls_plan-0fiscper >= lv_version   or  ls_plan-0fiscper eq '0000000'.
    COLLECT ls_plan INTO c_th_data .
*
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_BET_CALC
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REFDATA_BET_CALC.
DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
 e_t_ref_charsel = i_t_data_charsel.
*read ods data





*
  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .
  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCVERSIN'.
*
**
    ls_charsel-sign = 'I'.
    ls_charsel-opt = 'EQ'.
*
    ls_charsel-iobjnm = '0INFOPROV'.
    ls_charsel-low = 'FICAA0'.
    APPEND ls_charsel TO e_t_ref_charsel.
**
*   ls_charsel-sign = 'I'.
*    ls_charsel-opt = 'EQ'.
**
*    ls_charsel-iobjnm = '0PRO'.
*    ls_charsel-low = 'FICAA0'.
*    APPEND ls_charsel TO e_t_ref_charsel.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_CALC_CFWD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REFDATA_CALC_CFWD.
DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
 e_t_ref_charsel = i_t_data_charsel.
*read ods data





*
  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .
  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCVERSIN'.
*
**
    ls_charsel-sign = 'I'.
    ls_charsel-opt = 'EQ'.
*
    ls_charsel-iobjnm = '0INFOPROV'.
    ls_charsel-low = 'FICAA0'.
    APPEND ls_charsel TO e_t_ref_charsel.
**
*   ls_charsel-sign = 'I'.
*    ls_charsel-opt = 'EQ'.
**
*    ls_charsel-iobjnm = '0PRO'.
*    ls_charsel-low = 'FICAA0'.
*    APPEND ls_charsel TO e_t_ref_charsel.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_COPY_PER_AV
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD REFDATA_COPY_PER_AV.
  DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
  DATA :lv_period  TYPE /bic/oibpcversin,
        lv_prevper TYPE /bic/oibpcversin,
        lv_year(4) TYPE n.

  e_t_ref_charsel = i_t_data_charsel.
*read ods data
  read table  e_t_ref_charsel INTO ls_charsel

                              with key iobjnm = 'BPCVERSIN'.


  lv_period = ls_charsel-low.

  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCVERSIN' .
  lv_prevper = lv_period .


  IF lv_prevper+4(3) NE '001'.
    lv_prevper = lv_period - 1.
  ELSE.
    lv_year = lv_year - 1.
    CONCATENATE lv_year '012' INTO lv_prevper.
  ENDIF.

*
  ls_charsel-sign = 'I'.
  ls_charsel-opt = 'EQ'.
*
  ls_charsel-iobjnm = '0INFOPROV'.
  ls_charsel-low = 'BPCFID01'.
  APPEND ls_charsel TO e_t_ref_charsel.
  "bir önceki periodun verisini kopyala

*  ls_charsel-iobjnm = '0FISCVARNT'.
*  ls_charsel-low = 'Z4'.
*  APPEND ls_charsel TO e_t_ref_charsel.


  ls_charsel-iobjnm = 'BPCVERSIN'.
  ls_charsel-low = lv_prevper.
  APPEND ls_charsel TO e_t_ref_charsel.



ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_DRCOST_01
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REFDATA_DRCOST_01.
DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
 e_t_ref_charsel = i_t_data_charsel.
*read ods data

 DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .
 DELETE e_t_ref_charsel WHERE iobjnm = '0OBJ_CURR'.

*
    ls_charsel-sign = 'I'.
    ls_charsel-opt = 'EQ'.
*
    ls_charsel-iobjnm = '0INFOPROV'.
    ls_charsel-low = 'BPCFID01'.
    APPEND ls_charsel TO e_t_ref_charsel.

    ls_charsel-iobjnm = 'BPCAUDIT'.
    ls_charsel-low = 'M_INPUT'.
    APPEND ls_charsel TO e_t_ref_charsel.

    ls_charsel-iobjnm = '0FISCVARNT'.
    ls_charsel-low = 'Z4'.
    APPEND ls_charsel TO e_t_ref_charsel.


    ls_charsel-iobjnm = '0UNIT'.
    ls_charsel-low = 'H'.
    APPEND ls_charsel TO e_t_ref_charsel.
*



endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_HIS_UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REFDATA_HIS_UPLOAD.
DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
 e_t_ref_charsel = i_t_data_charsel.

 "read ods data
*
  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .

  DELETE e_t_ref_charsel WHERE iobjnm = '0FISCVARNT'.
  DELETE e_t_ref_charsel WHERE iobjnm = '0INFOPROV'.
*
**0CURTYPE
    ls_charsel-sign = 'I'.
    ls_charsel-opt = 'EQ'.
*
    ls_charsel-iobjnm = '0INFOPROV'.
    ls_charsel-low = 'ZDIPC007'.
    APPEND ls_charsel TO e_t_ref_charsel.
**
       ls_charsel-opt = 'EQ'.
*
    ls_charsel-iobjnm = '0CURTYPE'.
    ls_charsel-low = '10'.
    APPEND ls_charsel TO e_t_ref_charsel.
*   ls_charsel-sign = 'I'.
*    ls_charsel-opt = 'EQ'.
**
*    ls_charsel-iobjnm = '0PRO'.
*    ls_charsel-low = 'FICAA0'.
*    APPEND ls_charsel TO e_t_ref_charsel.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_PER_CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD REFDATA_PER_CHECK.
  DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
  DATA :lv_period  TYPE /bic/oibpcversin,
        lv_prevper TYPE /bic/oibpcversin,
        lv_year(4) TYPE n.

  e_t_ref_charsel = i_t_data_charsel.
*read ods data




ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_PER_COPY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD REFDATA_PER_COPY.
  DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
  DATA :lv_period  TYPE /bic/oibpcversin,
        lv_prevper TYPE /bic/oibpcversin,
        lv_year(4) TYPE n.

  e_t_ref_charsel = i_t_data_charsel.
*read ods data
  read table  e_t_ref_charsel INTO ls_charsel

                              with key iobjnm = 'BPCVERSIN'.


  lv_period = ls_charsel-low.

  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCVERSIN' .
  lv_prevper = lv_period .


  IF lv_prevper+4(3) NE '001'.
    lv_prevper = lv_period - 1.
  ELSE.
    lv_year = lv_year - 1.
    CONCATENATE lv_year '012' INTO lv_prevper.
  ENDIF.

*
  ls_charsel-sign = 'I'.
  ls_charsel-opt = 'EQ'.
*
  ls_charsel-iobjnm = '0INFOPROV'.
  ls_charsel-low = 'BPCFID01'.
  APPEND ls_charsel TO e_t_ref_charsel.
  "bir önceki periodun verisini kopyala

*  ls_charsel-iobjnm = '0FISCVARNT'.
*  ls_charsel-low = 'Z4'.
*  APPEND ls_charsel TO e_t_ref_charsel.


  ls_charsel-iobjnm = 'BPCVERSIN'.
  ls_charsel-low = lv_prevper.
  APPEND ls_charsel TO e_t_ref_charsel.



ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_RETRACTION_01
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REFDATA_RETRACTION_01.
*BPCFID02--> inflation rates
*BPCFID01--> man hours
  DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.

  e_t_ref_charsel = i_t_data_charsel.






endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_SWITCH_PER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD refdata_switch_per.
  DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
  DATA :lv_period  TYPE /bic/oibpcversin,
        lv_prevper TYPE /bic/oibpcversin,
        lv_year(4) TYPE n.

  e_t_ref_charsel = i_t_data_charsel.
*read ods data
  read table  e_t_ref_charsel INTO ls_charsel

                              with key iobjnm = 'BPCVERSIN'.


  lv_period = ls_charsel-low.

  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCVERSIN' .
  lv_prevper = lv_period .


  IF lv_prevper+4(3) NE '001'.
    lv_prevper = lv_period - 1.
  ELSE.
    lv_year = lv_year - 1.
    CONCATENATE lv_year '012' INTO lv_prevper.
  ENDIF.

*
  ls_charsel-sign = 'I'.
  ls_charsel-opt = 'EQ'.
*
  ls_charsel-iobjnm = '0INFOPROV'.
  ls_charsel-low = 'BPCFID01'.
  APPEND ls_charsel TO e_t_ref_charsel.
  "bir önceki periodun verisini kopyala

*  ls_charsel-iobjnm = '0FISCVARNT'.
*  ls_charsel-low = 'Z4'.
*  APPEND ls_charsel TO e_t_ref_charsel.


  ls_charsel-iobjnm = 'BPCVERSIN'.
  ls_charsel-low = lv_prevper.
  APPEND ls_charsel TO e_t_ref_charsel.



ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_TM_REVENUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REFDATA_TM_REVENUE.
*BPCFID02--> inflation rates
*BPCFID01--> man hours
  DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.

  e_t_ref_charsel = i_t_data_charsel.

  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .
   DELETE e_t_ref_charsel WHERE iobjnm = '0OBJ_CURR'.

***HRS

  ls_charsel-sign = 'I'.
  ls_charsel-opt = 'EQ'.
*
  ls_charsel-iobjnm = '0INFOPROV'.
  ls_charsel-low = 'BPCFID01'.
  APPEND ls_charsel TO e_t_ref_charsel.
*
  ls_charsel-iobjnm = 'BPCAUDIT'.
  ls_charsel-low = 'M_INPUT'.
  APPEND ls_charsel TO e_t_ref_charsel.
*
  ls_charsel-iobjnm = '0FISCVARNT'.
  ls_charsel-low = 'Z4'.
  APPEND ls_charsel TO e_t_ref_charsel.

  ls_charsel-iobjnm = '0COSTELMNT'.
  ls_charsel-low = 'S218000021'.
  APPEND ls_charsel TO e_t_ref_charsel.


  ls_charsel-iobjnm = '0UNIT'.
  ls_charsel-low = 'H'.
  APPEND ls_charsel TO e_t_ref_charsel.

*
****EXP
  DATA: ls_costelemnt TYPE /BI0/MCOSTELMNT,
        lt_costelemnt type table of /BI0/MCOSTELMNT.


  SELECT COSTELMNT AS COSTELMNT
          FROM /BI0/MCOSTELMNT
          INTO CORRESPONDING FIELDS OF TABLE  @LT_costelemnt
          WHERE   objvers = 'A'
          AND CSTELMNTYP = '21'
          AND  /BIC/CELMTYPE =  'GEXP'.
  ls_charsel-iobjnm = '0UNIT'.
  ls_charsel-low = ''.
  APPEND ls_charsel TO e_t_ref_charsel.
  LOOP AT LT_costelemnt into ls_costelemnt.

    ls_charsel-iobjnm = '0COSTELMNT'.
    ls_charsel-low = ls_costelemnt-COSTELMNT.
    APPEND ls_charsel TO e_t_ref_charsel.
  endloop.




endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->REFDATA_UPDATE_CAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_DATA_CHARSEL               TYPE        RSPLF_T_CHARSEL
* | [--->] I_T_DATA_NODE_SEL              TYPE        RSPLF_T_NODE(optional)
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<---] E_TH_NO_REF_BLOCK_CHA          TYPE        RSPLF_TH_IOBJ
* | [<---] E_T_REF_CHARSEL                TYPE        RSPLF_T_CHARSEL
* | [<---] E_T_REF_NODE_SEL               TYPE        RSPLF_T_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REFDATA_UPDATE_CAR.
DATA : ls_charsel TYPE LINE OF rsplf_t_charsel.
 e_t_ref_charsel = i_t_data_charsel.
*read ods data




*
**
*  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCAUDIT' .
*  DELETE e_t_ref_charsel WHERE iobjnm = 'BPCVERSIN'.
**
***
*    ls_charsel-sign = 'I'.
*    ls_charsel-opt = 'EQ'.
**
*    ls_charsel-iobjnm = '0INFOPROV'.
*    ls_charsel-low = 'FICAA0'.
*    APPEND ls_charsel TO e_t_ref_charsel.
**
*   ls_charsel-sign = 'I'.
*    ls_charsel-opt = 'EQ'.
**
*    ls_charsel-iobjnm = '0PRO'.
*    ls_charsel-low = 'FICAA0'.
*    APPEND ls_charsel TO e_t_ref_charsel.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->RETRACTION_01
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD retraction_01.

**********************************************************************

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a06.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a06,
       l_r_msg TYPE REF TO cl_rsplfu_msg.
  DATA: lt_charsel           TYPE rsplf_t_charsel,
        ls_charsel           LIKE LINE OF lt_charsel,
        lv_compcode          TYPE /bi0/oicomp_code,
        lv_version           TYPE /bic/oibpcversin,
        lv_project           TYPE /bi0/oiproject,
        ls_project           TYPE /bi0/pproject,
        ls_wbs_elemnt        TYPE /bi0/pwbs_elemt,
        lt_hours             TYPE TABLE OF zbpce_pal_cck_a06,
        ls_hours             TYPE zbpce_pal_cck_a06,
*        lt_conditions        TYPE HASHED TABLE OF /BIC/AFICOA017
*                                 WITH UNIQUE  default KEY,
*        "  WITH NON-UNIQUE SORTED KEY A2 components ACTTYPe comp_code costcenter employee wbs_elemt validto,
*
*        ls_conditions        TYPE /BIC/AFICOA017,
        lv_f_beg             TYPE zbpce_pal_cck_a06-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a06-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a06-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a06,
        lt_inflation_base    TYPE TABLE OF /bic/abpcfid027,
        ls_inflation_base    TYPE  /bic/abpcfid027,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lt_active            TYPE TABLE OF /bic/abpcfid907,
        ls_active            TYPE  /bic/abpcfid907,
        lv_found             TYPE i,
        lv_cfisper           TYPE /bi0/oifiscper,
 "       lt_zcoste_map_01     TYPE TABLE OF zcoste_map_01,
  "      ls_zcoste_map_01     TYPE zcoste_map_01,
        et_error             TYPE bapirettab,
        es_error             type bapiret2.

  DATA :lv_return TYPE TABLE OF bapiret2.

*
**********************************************************************
l_r_msg ?= i_r_msg.
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  " READ TABLE lt_charsel INTO ls_charsel with key iobjnm = 'BPCVERSIN'.
  "  lv_version = ls_charsel-low.
  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0COMP_CODE'.
  lv_compcode  = ls_charsel-low.
  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0PROJECT'.
  lv_project  = ls_charsel-low.

  IF lv_project IS INITIAL.
    CALL FUNCTION 'Z_TBPC_RETRACTION'
      DESTINATION 'TD4CLNT200'
      EXPORTING
        "  P_PROJ           =
        p_compcode = lv_compcode
      IMPORTING
        et_error   = et_error.
  ELSE.
    CALL FUNCTION 'Z_TBPC_RETRACTION'
      DESTINATION 'TD4CLNT200'
      EXPORTING
        p_proj     = lv_project
        p_compcode = lv_compcode
      IMPORTING
        et_error   = et_error.
  ENDIF.

IF LINES( et_error ) > 0.
DATA:l_dummy TYPE c.

    LOOP AT et_error INTO es_error.
      CASE es_error-type.
        WHEN 'E'.
          MESSAGE e000(rspls) WITH es_error-MESSAGE_V1
                                   es_error-MESSAGE_V2
                                   es_error-MESSAGE_V3
                                   es_error-MESSAGE_V4 INTO l_dummy.
*        WHEN 'W'.
*          MESSAGE w000(rspls) WITH es_error-MESSAGE_V1
*                                   es_error-MESSAGE_V2
*                                   es_error-MESSAGE_V3
*                                   es_error-MESSAGE_V4 INTO l_dummy.

        WHEN 'I'.
          MESSAGE i000(rspls) WITH es_error-MESSAGE_V1
                                   es_error-MESSAGE_V2
                                   es_error-MESSAGE_V3
                                   es_error-MESSAGE_V4 INTO l_dummy.
        WHEN 'S'.
          MESSAGE s000(rspls) WITH es_error-MESSAGE_V1
                                   es_error-MESSAGE_V2
                                   es_error-MESSAGE_V3
                                   es_error-MESSAGE_V4 INTO l_dummy.
      ENDCASE.
      i_r_msg->add_msg( '1' ).
    ENDLOOP.
  ENDIF.

*  CLEAR:g_s_message.
*  g_s_message-msgty = 'W'.
*  g_s_message-msgv1 = 'Malzeme:' ."&& ls_mat_plant-mat_plant && '|'.
*  g_s_message-msgv2 = 'Üretim Yeri:'." && ls_mat_plant-plant && '|'.
**        g_s_message-msgv3 =
*  g_s_message-msgv4 = ' için tanımlı değil!'.
*COLLECT g_s_message INTO g_t_message.

*
***INFLATION RATES LOOKUP
*** CALCULATE the rates to be used
*  DATA PERIOD TYPE /BI0/OIfiscper.
*  DATA CURRENT_MONTHS TYPE /BI0/OIfiscper.
*  DATA PREVIUS_MONTHS TYPE /BI0/OIfiscper.
*  DATA AMOUNT TYPE F.
*  DATA QTD_MONTH TYPE I.
*  DATA lv_month(3) TYPE n.
*  DATA inf_rate_sum  type F.
*  DATA lv_YEAR(4) TYPE n.
*  DATA: lv_lastday           type d.
*  lv_compcode =  'BE74'.
*  PERIOD = lv_version.
**there wont be inflation in the beginning.
*
*** CALCULATE the rates to be used
*
**CONDITIONS LOOKUP
*
*
*  SELECT source_account AS source_account , target_account AS target_account, sign AS sign
*              FROM zcoste_map_01
*          INTO CORRESPONDING FIELDS OF TABLE  @lt_zcoste_map_01.
*
**  LOOP AT c_th_data ASSIGNING <ls_data>.
**    <ls_data>-0amount_oc = 0.
**  ENDLOOP.
**Project only ?
*  loop at c_th_data into <ls_data>.
*
*
*    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /BI0/PWBS_ELEMT WHERE objvers = 'A' AND WBS_ELEMT = <ls_data>-0WBS_ELEMT.
**check company code is filled.
*    clear: ls_data, ls_conditions, ls_inflation, lv_found.
*    IF sy-subrc NE 0.  "if wbs element doesnt exist in S4.?
*    ELSE.
*      CLEAR: ls_active, lv_cfisper.
*      READ TABLE lt_active INTO ls_active WITH KEY  comp_code = ls_wbs_elemnt-COMP_CODE.
*      CONCATENATE ls_active-calmonth+0(4) '0' ls_active-calmonth+4(2) INTO lv_cfisper.
*      CHECK lv_cfisper = <ls_data>-0fiscper.
*      CLEAR ls_ZCOSTE_MAP_01.
*      READ TABLE lt_ZCOSTE_MAP_01 INTO ls_ZCOSTE_MAP_01 WITH KEY source_account = <ls_data>-0COSTELMNT.
*      IF sy-subrc = 0.
*        gs_zbpc_a01-/erp/gl_acct = ls_ZCOSTE_MAP_01-target_account.
*      ELSE.
*        gs_zbpc_a01-/erp/gl_acct = ls_source-/erp/costelmt.
*      ENDIF.
*      gs_zbpc_a01-/erp/chrtacct = 'OCOA'.
*      gs_zbpc_a01-/erp/co_area = ls_source-/erp/co_area.
*      gs_zbpc_a01-/erp/compcode = lv_bukr.
*      gs_zbpc_a01-/erp/lcurr =  ls_source-/erp/lcurr.
*      IF ls_source-/erp/lcurr IS INITIAL.
*        gs_zbpc_a01-/erp/lcurr =  'EUR'.
*      ENDIF.
*
*      gs_zbpc_a01-fiscvarnt = ls_source-fiscvarnt.
*
*      IF sy-subrc = 0.
*        "should be get from BPCFID01- aktive version's year concatenate with period'001'
*        CONCATENATE ls_active-calmonth+0(4) '001'  INTO gs_zbpc_a01-fiscper.
*        " gs_zbpc_a01-fiscper = ls_source-fiscper.
*      ELSE.
*        "throw a message
*      ENDIF.
*      IF ls_ZCOSTE_MAP_01-sign = '-'.
*        gs_zbpc_a01-/erp/amount_l = ls_source-/erp/amount  * -1."amount in company code currency
*      ELSE.
*        gs_zbpc_a01-/erp/amount_l = ls_source-/erp/amount."amount in company code currency
*      ENDIF.
*      gs_zbpc_a01-/erp/wbselmt = ls_source-/erp/wbselmt.
*      gs_zbpc_a01-/erp/project = ls_source-/erp/project.
*      gs_zbpc_a01-unit = 'H'.
*      gs_zbpc_a01-/erp/quanty = ls_source-/erp/quanty.
*
*      COLLECT gs_zbpc_a01 INTO gt_zbpc_a01.
*    ENDIF.
*  ENDLOOP.
*
*
*
*
** Message


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->SWITCH_PER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD switch_per.
**********************************************************************
*Logic reads the customer exit variable-ZCXPM_BPCVERSIN- aktif period
* of the company code
* if data avaliable before- it is not working
*if data doesnt avaliable for that period, it is calculating ACTPLN
*** Change  :
**********************************************************************

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a09c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a09c.
  DATA: lt_charsel           TYPE rsplf_t_charsel,
        l_r_msg              TYPE REF TO cl_rsplfu_msg,
        ls_charsel           LIKE LINE OF lt_charsel,
        lv_compcode          TYPE /bi0/oicomp_code,
        lv_version           TYPE /bic/oibpcversin,
        l_dummy              TYPE c,
        ls_wbs_elemnt        TYPE /bi0/pwbs_elemt,
        ls_comp_code         TYPE /bi0/pcomp_code,
        lt_hours             TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_hours             TYPE zbpce_pal_cck_a09c,
        lt_exp               TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_exp               TYPE zbpce_pal_cck_a09c,
        lt_act               TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_act               TYPE zbpce_pal_cck_a09c,
        lt_plan              TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_plan              TYPE zbpce_pal_cck_a09c,
        lt_actplan           TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_actplan           TYPE zbpce_pal_cck_a09c,
        lt_bet               TYPE TABLE OF zbpce_pal_cck_a09c,
        ls_bet               TYPE zbpce_pal_cck_a09c,

        ls_conditions        TYPE zbpce_pal_cck_a03c,
        ls_breakdown         TYPE zbpce_breakdown,
        lt_breakdown         TYPE TABLE OF zbpce_breakdown,
        lv_f_beg             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a03c-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a09c,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i,
        lv_lastday           TYPE d,
        lt_map               TYPE TABLE OF zwbs_map_01,
        ls_map               TYPE zwbs_map_01,
        ls_project           TYPE /bi0/mproject,
        lv_lastdaydats       TYPE dats,
        lv_famount           TYPE f.

*
  DATA: ls_costelemnt TYPE /bi0/mcostelmnt,
        lt_costelemnt TYPE TABLE OF /bi0/mcostelmnt.
  DATA: ls_bpcfid90 TYPE /bic/abpcfid907,
        lt_bpcfid90 TYPE TABLE OF /bic/abpcfid907.

  SELECT comp_code AS comp_code, calmonth AS calmonth
       FROM /bic/abpcfid907
       INTO CORRESPONDING FIELDS OF TABLE  @lt_bpcfid90.

**********************************************************************
  DATA :
    lv_prevper TYPE /bic/oibpcversin,
    lv_year(4) TYPE n.

*
  l_r_msg ?= i_r_msg.
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.
  lv_prevper = lv_version .

  IF lv_prevper+4(3) NE '001'.
    lv_prevper = lv_prevper - 1.
  ELSE.
    lv_year = lv_year - 1.
    CONCATENATE lv_year '012' INTO lv_prevper.
  ENDIF.
  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0WBS_ELEMT__0COMP_CODE'.
  lv_compcode = ls_charsel-low.
  DATA: kontrol TYPE i.
* control if data avaliable already in the version dont touch it.
  kontrol = 0.
  IF  c_th_data IS NOT INITIAL.
    kontrol =  1.
    MESSAGE w009(zbpce) WITH lv_version lv_compcode INTO l_dummy.
    l_r_msg->add_msg( ).

    LOOP AT c_th_data ASSIGNING <ls_data>.
      IF <ls_data>-bpcaudit = 'ACPLN' OR <ls_data>-bpcaudit = 'PFWD' OR <ls_data>-bpcaudit = 'FWD'.
        <ls_data>-0amount_oc = 0.
        <ls_data>-0quantity = 0.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "CHECK c_th_data IS INITIAL.

*  IF  i_th_ref_data IS INITIAL.
*    MESSAGE e007(zbpce) WITH lv_version lv_compcode INTO l_dummy.
*    l_r_msg->add_msg( ).
*  ENDIF.
*status of the Project should be checked.
  CHECK i_th_ref_data IS NOT INITIAL.
  LOOP AT i_th_ref_data ASSIGNING <ls_data>.
    MOVE <ls_data> TO ls_plan.
    SELECT SINGLE * INTO ls_project  FROM /bi0/mproject WHERE objvers = 'A' AND project = ls_plan-0project.
    CHECK ls_project-statussys0 =  '02'.
    "  CHECK ls_plan-bpcaudit NE 'ACPLN'.
    ls_plan-bpcversin = lv_version.
*    IF ls_plan-0fiscper <  lv_version   AND ls_plan-0fiscper NE '0000000'.
    "  IF ls_plan-0fiscper NE '0000000'.
*      ls_plan-0fiscper = lv_prevper.
    "   ENDIF.
*      "clear ls_plan-0fiscper.
*      IF ls_plan-bpcaudit NE 'LSRT_INPUT' AND ls_plan-bpcaudit NE 'LSR_INPUT' AND ls_plan-bpcaudit NE 'TMR_INPUT'
*             AND ls_plan-0fiscper NE '0000000' and ls_plan-bpcaudit NE 'FWD' and ls_plan-bpcaudit NE 'PFWD'.
*        ls_plan-bpcaudit = 'ACPLN'.
*        COLLECT ls_plan INTO c_th_data .
*      ENDIF.

*    ELSE.
    IF ls_plan-0fiscper >=  lv_version   OR ls_plan-0fiscper EQ '0000000'.
      IF kontrol = 0.
        IF  ls_plan-bpcaudit NE 'FWD' AND ls_plan-bpcaudit NE 'PFWD'.
          COLLECT ls_plan INTO c_th_data .
        ENDIF.
      ENDIF.

      IF  ls_plan-bpcaudit = 'FWD' OR ls_plan-bpcaudit = 'PFWD'.
        ls_plan-bpcaudit = 'PFWD'.
        COLLECT ls_plan INTO c_th_data .
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->TM_REVENUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD tm_revenue.
**********************************************************************
***A907	WBS element/Generic Material
***A905	WBS element/Agent
***A906	WBS Element/Activity Type
***A902	Service Package / Generic Material
***A904	Service Package/ Activity Type
***A903	Service Package
*** Date    : 31.08.2021
*** Change  :
*
  "Incomes for T&M ((Number of planned hours of that period * selling price of the last actual period)
  " + ( Amount of expenses  + (Amount of expenses * mark-up of the last actual period /1000)
  "wbs-genericmaterial
  "wbs-agent
  "wbs-activity type
  "servicepac-material
  "service pac-activity type
  "service pack
**********************************************************************

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a03c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a03c.
  DATA: lt_charsel    TYPE rsplf_t_charsel,
        ls_charsel    LIKE LINE OF lt_charsel,
        lv_compcode   TYPE /bi0/oicomp_code,
        lv_version    TYPE /bic/oibpcversin,
        lv_project    TYPE /bi0/oiproject,
        ls_project    TYPE /bi0/pproject,
        ls_wbs_elemnt TYPE /bi0/pwbs_elemt,
        ls_comp_code  TYPE /bi0/pcomp_code,
        lt_hours      TYPE TABLE OF zbpce_pal_cck_a03c,
        ls_hours      TYPE zbpce_pal_cck_a03c,
        lt_exp        TYPE TABLE OF zbpce_pal_cck_a03c,
        ls_exp        TYPE zbpce_pal_cck_a03c,
*        lt_price             TYPE TABLE OF /BIC/ASDA017,
*        ls_price             TYPE /BIC/ASDA017,
        BEGIN OF ls_price,
          acttype        TYPE /bi0/oiacttype,
          co_area        TYPE /bi0/oico_area,
          employee       TYPE  /bi0/oiemployee,
          knart          TYPE  /bi0/oiknart,
          knstep         TYPE  /bi0/oiknstep,
          wbs_elemt      TYPE /bi0/oiwbs_elemt,
          materialnr     TYPE char40,
          "   CONDITIONUSA TYPE CHAR1,
          /bic/sd_svcpac TYPE  /bic/oisd_svcpac,
          validfrom      TYPE /bi0/oivalidfrom,
          validto        TYPE /bi0/oivalidto,
          amount         TYPE /bi0/oiamount_oc,
          quantity       TYPE /bi0/oiquantity,
          currency       TYPE /bi0/oicurrency,
          unit           TYPE /bi0/oiunit,
        END OF ls_price,
        lt_price LIKE TABLE OF ls_price,
        BEGIN OF ls_markup,
          acttype        TYPE /bi0/oiacttype,
          co_area        TYPE /bi0/oico_area,
          employee       TYPE /bi0/oiemployee,
          knart          TYPE /bi0/oiknart,
          knstep         TYPE /bi0/oiknstep,
          wbs_elemt      TYPE /bi0/oiwbs_elemt,
          materialnr     TYPE char40,
          "  CONDITIONUSA AS CONDITIONUSA,
          /bic/sd_svcpac TYPE /bic/oisd_svcpac,
          validfrom      TYPE /bi0/oivalidfrom,
          validto        TYPE /bi0/oivalidto,
          /bic/mrkprt    TYPE /bic/oimrkprt,
        END OF ls_markup,
        lt_markup            LIKE TABLE OF ls_markup,
        ls_conditions        TYPE zbpce_pal_cck_a03c,
        lv_f_beg             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a03c-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a03c,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i,
        lv_lastday           TYPE d,
        lt_map               TYPE TABLE OF zwbs_map_01,
        ls_map               TYPE zwbs_map_01,
        lv_lastdaydats       TYPE dats,
        lv_famount           TYPE f.

  "DATA: lv_lastday           type d.
  DATA: ls_costelemnt TYPE /bi0/mcostelmnt,
        lt_costelemnt TYPE TABLE OF /bi0/mcostelmnt.


  SELECT costelmnt AS costelmnt, /bic/celmsubgr AS /bic/celmsubgr
          FROM /bi0/mcostelmnt
          INTO CORRESPONDING FIELDS OF TABLE  @lt_costelemnt
          WHERE   objvers = 'A'
          AND cstelmntyp = '21'
          AND  /bic/celmtype =  'GEXP'.
  .

  SELECT inputtype AS inputtype, ps_prjtype AS ps_prjtype,
           costelmnt AS costelmnt
          FROM  zwbs_map_01
          INTO CORRESPONDING FIELDS OF TABLE  @lt_map.

**********************************************************************
*
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.


  LOOP AT i_th_ref_data ASSIGNING <ls_data>.

    IF <ls_data>-0costelmnt = 'S218000021'.
      IF  <ls_data>-bpcaudit = 'M_INPUT'    AND <ls_data>-0unit = 'H'.
        "AND <ls_data>-bpcversin = lv_version. " AND  <ls_data>-0obj_cur = ''.
        COLLECT <ls_data> INTO lt_hours.    "hours inputted
      ENDIF.
    ELSE.
      COLLECT <ls_data> INTO lt_exp.    "exp inputted
    ENDIF.
  ENDLOOP.

*before calculation first clear

  LOOP AT c_th_data ASSIGNING <ls_data>.
    <ls_data>-0amount_oc = 0.
  ENDLOOP.

*get the prices
*  SELECT ACTTYPE AS ACTTYPE, CO_AREA AS CO_AREA, EMPLOYEE AS EMPLOYEE,KNART AS KNART, KNSTEP AS KNSTEP,WBS_ELEMT AS WBS_ELEMT,
*                  MATERIALNR as MATERIALNR, CONDITIONUSA AS CONDITIONUSA, /BIC/SD_SVCPAC AS /BIC/SD_SVCPAC,CURRENCY AS CURRENCY,
*                  VALIDFROM as VALIDFROM, VALIDTO AS VALIDTO,
*           SUM( AMOUNT ) AS AMOUNT,
*           SUM( QUANTITY ) AS QUANTITY
*             FROM /BIC/ASDA017
*             INTO CORRESPONDING FIELDS OF TABLE  @LT_PRICE
*               GROUP BY ACTTYPE ,CO_AREA , EMPLOYEE, KNART, KNSTEP , WBS_ELEMT, MATERIALNR ,
*                                     CONDITIONUSA,/BIC/SD_SVCPAC,VALIDFROM,VALIDTO,CURRENCY.
  CLEAR:gs_range,gt_range,gs_sfc,gt_sfc,gs_sfk,gt_sfk.

  gs_sfc-chanm = '0CO_AREA'. gs_sfc-chaalias = 'CO_AREA '. APPEND gs_sfc TO gt_sfc.
  " gs_sfc-chanm = '0COSTCENTER'. gs_sfc-chaalias = 'COSTCENTER'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0ACTTYPE'. gs_sfc-chaalias = 'ACTTYPE'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0KNART'. gs_sfc-chaalias = 'KNART'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0KNSTEP'. gs_sfc-chaalias = 'KNSTEP'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0MATERIAL'. gs_sfc-chaalias = 'MATERIALNR'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = 'SD_SVCPAC'. gs_sfc-chaalias = '/BIC/SD_SVCPAC'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0EMPLOYEE'. gs_sfc-chaalias = 'EMPLOYEE'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0WBS_ELEMT'. gs_sfc-chaalias = 'WBS_ELEMT'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0VALIDTO'. gs_sfc-chaalias = 'VALIDTO'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0VALIDFROM'. gs_sfc-chaalias = 'VALIDFROM'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0CURRENCY'. gs_sfc-chaalias = 'CURRENCY'. APPEND gs_sfc TO gt_sfc.
  "kyf alan donusumlerini yap
  gs_sfk-kyfnm = '0AMOUNT'. gs_sfk-kyfalias = 'AMOUNT'. gs_sfk-aggr = 'SUM'. APPEND gs_sfk TO gt_sfk.
  gs_sfk-kyfnm = '0QUANTITY'. gs_sfk-kyfalias = 'QUANTITY'. gs_sfk-aggr = 'SUM'. APPEND gs_sfk TO gt_sfk.

*
  CALL METHOD _read_cube_data
    EXPORTING
      i_infoprov = 'SDV01'
      i_t_sfc    = gt_sfc
      i_t_sfk    = gt_sfk
      i_t_range  = gt_range
 "    i_collect  = 'X'
"     i_keydate  =
    IMPORTING
      e_t_data   = lt_price.
  DELETE lt_price WHERE amount  EQ 0.
  SORT lt_price BY acttype co_area  employee knart knstep wbs_elemt materialnr  /bic/sd_svcpac
                           ASCENDING validfrom validto DESCENDING.
*PRICE LOOKUP

*get the markups

  CLEAR:gs_range,gt_range,gs_sfc,gt_sfc,gs_sfk,gt_sfk.

  gs_sfc-chanm = '0CO_AREA'. gs_sfc-chaalias = 'CO_AREA '. APPEND gs_sfc TO gt_sfc.
  " gs_sfc-chanm = '0COSTCENTER'. gs_sfc-chaalias = 'COSTCENTER'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0ACTTYPE'. gs_sfc-chaalias = 'ACTTYPE'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0KNART'. gs_sfc-chaalias = 'KNART'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0KNSTEP'. gs_sfc-chaalias = 'KNSTEP'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0MATERIAL'. gs_sfc-chaalias = 'MATERIALNR'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = 'SD_SVCPAC'. gs_sfc-chaalias = '/BIC/SD_SVCPAC'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0EMPLOYEE'. gs_sfc-chaalias = 'EMPLOYEE'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0WBS_ELEMT'. gs_sfc-chaalias = 'WBS_ELEMT'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0VALIDTO'. gs_sfc-chaalias = 'VALIDTO'. APPEND gs_sfc TO gt_sfc.
  gs_sfc-chanm = '0VALIDFROM'. gs_sfc-chaalias = 'VALIDFROM'. APPEND gs_sfc TO gt_sfc.
  "kyf alan donusumlerini yap
  gs_sfk-kyfnm = 'MRKPRT '. gs_sfk-kyfalias = '/BIC/MRKPRT '. gs_sfk-aggr = 'SUM'. APPEND gs_sfk TO gt_sfk.
  " gs_sfk-kyfnm = '0QUANTITY'. gs_sfk-kyfalias = 'QUANTITY'. gs_sfk-aggr = 'SUM'. APPEND gs_sfk TO gt_sfk.

*
  CALL METHOD _read_cube_data
    EXPORTING
      i_infoprov = 'SDV02'
      i_t_sfc    = gt_sfc
      i_t_sfk    = gt_sfk
      i_t_range  = gt_range
 "    i_collect  = 'X'
"     i_keydate  =
    IMPORTING
      e_t_data   = lt_markup.

*  SELECT ACTTYPE AS ACTTYPE, CO_AREA AS CO_AREA, EMPLOYEE AS EMPLOYEE,KNART AS KNART, KNSTEP AS KNSTEP,WBS_ELEMT AS WBS_ELEMT,
*                  MATERIALNR as MATERIALNR, CONDITIONUSA AS CONDITIONUSA, /BIC/SD_SVCPAC AS /BIC/SD_SVCPAC,
*    VALIDFROM as VALIDFROM, VALIDTO AS VALIDTO,
*           SUM( /BIC/MRKPRT ) AS /BIC/MRKPRT
*            FROM /BIC/ASDA027
*             INTO CORRESPONDING FIELDS OF TABLE  @LT_MARKUP
*               GROUP BY ACTTYPE ,CO_AREA , EMPLOYEE, KNART, KNSTEP , WBS_ELEMT, MATERIALNR ,CONDITIONUSA,/BIC/SD_SVCPAC,
*               VALIDFROM, VALIDTO.
  " DELETE LT_MARKUP WHERE /BIC/MRKPRT  EQ 0.
  SORT lt_markup BY acttype co_area  employee knart knstep wbs_elemt materialnr  /bic/sd_svcpac
                          ASCENDING validfrom validto DESCENDING.

*MARKUP LOOKUP

**0wbc_elemnt--> result_key_analysis*
*If it is blank --> TM
*Get the attributes of WBS element ( service package)

  LOOP AT lt_hours INTO ls_hours.

    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_hours-0wbs_elemt.
* check if it is TM project.
    CHECK ls_wbs_elemnt-/bic/ps_rkeyan = ''.
*get the compnay code currency
    CLEAR ls_comp_code.
    SELECT SINGLE * INTO ls_comp_code  FROM /bi0/mcomp_code WHERE objvers = 'A' AND comp_code = ls_wbs_elemnt-comp_code.
*get the result costelement.
    CLEAR ls_map.
    READ TABLE lt_map INTO ls_map WITH  KEY  inputtype = 'HRS'
                                             ps_prjtype = ls_wbs_elemnt-ps_prjtype.
    IF sy-subrc NE 0.
      "write a message
    ELSE.

* check if it is TM project.
      CLEAR: ls_data, ls_price, ls_markup, lv_found.
      "wbs-genericmaterial
      "wbs-agent
      "wbs-activity type
      "servicepac-material
      "servicepac-agent
      "service pac-activity type
      "service pack
      CLEAR ls_costelemnt.
      READ TABLE lt_costelemnt INTO ls_costelemnt WITH  KEY  costelmnt = ls_hours-0costelmnt.
*read prices with an order.
      CALL FUNCTION 'Z_LAST_DAY_IN_FISCAL_PERIOD'
        EXPORTING
          i_fper    = ls_hours-0fiscper
          i_fvarnt  = 'Z4'
        IMPORTING
          e_lastday = lv_lastday.

      IF ls_costelemnt-/bic/celmsubgr IS NOT INITIAL AND ls_hours-0wbs_elemt IS NOT INITIAL AND ls_costelemnt-/bic/celmsubgr IS NOT INITIAL.

        LOOP AT lt_price INTO ls_price WHERE  wbs_elemt = ls_hours-0wbs_elemt
                                        AND   materialnr = ls_costelemnt-/bic/celmsubgr
                                        AND   validto GE lv_lastday
                                        AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.

      IF lv_found IS INITIAL AND ls_hours-0wbs_elemt IS NOT INITIAL AND ls_hours-0employee IS NOT INITIAL.
        LOOP AT lt_price INTO ls_price WHERE   wbs_elemt = ls_hours-0wbs_elemt
                                         AND   employee = ls_hours-0employee
                                         AND   validto GE lv_lastday
                                         AND   validfrom  LE lv_lastday.

          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found IS INITIAL AND ls_hours-0wbs_elemt IS NOT INITIAL AND ls_hours-0acttype IS NOT INITIAL.
        LOOP AT lt_price INTO ls_price WHERE   wbs_elemt = ls_hours-0wbs_elemt
                                        AND    acttype = ls_hours-0acttype
                                        AND     employee = ''
                                        AND   validto GE lv_lastday
                                        AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.
      "  check   ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial.
      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL AND ls_costelemnt-/bic/celmsubgr IS NOT INITIAL.
        LOOP AT lt_price INTO ls_price WHERE  /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                         AND  materialnr = ls_costelemnt-/bic/celmsubgr
                                         AND   validto GE lv_lastday
                                         AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.

      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL AND ls_hours-0employee IS NOT INITIAL.
        LOOP AT lt_price INTO ls_price  WHERE  /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                         AND   employee = ls_hours-0employee
                                         AND   validto GE lv_lastday
                                         AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL AND ls_hours-0acttype IS NOT INITIAL.
        LOOP AT lt_price INTO ls_price  WHERE  /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                         AND   acttype = ls_hours-0acttype
                                         AND   wbs_elemt = ''
                                         AND   validto GE lv_lastday
                                         AND   validfrom  LE lv_lastday.

          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL .
        LOOP AT lt_price INTO ls_price  WHERE  /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                         AND   wbs_elemt = ''
                                         AND   validto GE lv_lastday
                                         AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found = 1.
        ls_data-0fiscper  = ls_hours-0fiscper.
        ls_data-0fiscvarnt = ls_hours-0fiscvarnt.
        ls_data-0acttype    = ls_hours-0acttype.
        ls_data-0comp_code  = ls_wbs_elemnt-comp_code.
        ls_data-0costcenter = ls_hours-0costcenter.
        ls_data-0costelmnt  =  ls_map-costelmnt   .
        ls_data-0co_area    = ls_hours-0co_area.
        ls_data-0employee   = ls_hours-0employee.
        ls_data-0infoprov   = ls_hours-0infoprov .
        ls_data-0project    = ls_hours-0project.
        ls_data-0version    = ls_hours-0version.
        ls_data-0wbs_elemt  = ls_hours-0wbs_elemt.
        ls_data-celmsrc     = 'S218000021'. " source subgroup
        ls_data-bpcaudit    = 'TMREV'.
        ls_data-bpcversin   = ls_hours-bpcversin.
        ls_data-0obj_cur   = ls_comp_code-currency.
        IF ls_price-currency = ls_comp_code-currency.
          ls_data-0obj_cur   = ls_price-currency.
          lv_famount = ls_price-amount.
        ELSE.
*          CALL FUNCTION 'Z_LAST_DAY_IN_FISCAL_PERIOD'
*            EXPORTING
*              I_FPER    = ls_hours-0fiscper
*              I_FVARNT  = 'Z4'
*            IMPORTING
*              E_LASTDAY = lv_lastday.
          CLEAR:lv_lastdaydats,lv_famount.
          lv_lastdaydats = lv_lastday.
          lv_famount = ls_price-amount.
          CALL FUNCTION 'Z_CONVERT_AMOUNT_FROM_CURRENCY'
            EXPORTING
              kurst  = 'M'
              fcurr  = ls_price-currency
              tcurr  = ls_comp_code-currency
              datum  = lv_lastdaydats
              rate   = lv_famount
            IMPORTING
              amount = lv_famount.
        ENDIF.
        ls_data-0unit       = ''.
        IF ls_price-quantity  = 0. ls_price-quantity = 1. ENDIF.
        ls_data-0amount_oc     = ls_hours-0quantity *  ( lv_famount / ls_price-quantity ) .
        ls_data-0quantity   = 0.

        COLLECT ls_data INTO c_th_data.
      ENDIF.
    ENDIF.  "if we coulndt find wbs elemnt type
  ENDLOOP.

  LOOP AT lt_exp INTO ls_exp.

    CLEAR lv_lastday.
    CALL FUNCTION 'Z_LAST_DAY_IN_FISCAL_PERIOD'
      EXPORTING
        i_fper    = ls_exp-0fiscper
        i_fvarnt  = 'Z4'
      IMPORTING
        e_lastday = lv_lastday.

    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /bi0/pwbs_elemt WHERE objvers = 'A' AND wbs_elemt = ls_exp-0wbs_elemt.
* check if it is TM project.
    CHECK ls_wbs_elemnt-/bic/ps_rkeyan = ''.
*get the compnay code currency
    SELECT SINGLE * INTO ls_comp_code  FROM /bi0/mcomp_code WHERE objvers = 'A' AND comp_code = ls_wbs_elemnt-comp_code.
* check if it is TM project.
*get the result costelement.
    READ TABLE lt_map INTO ls_map WITH  KEY  inputtype = 'EXP'
                                             ps_prjtype = ls_wbs_elemnt-ps_prjtype.
    IF sy-subrc NE 0.
      "write a message
    ELSE.
      CLEAR: ls_data, ls_price, ls_markup, lv_found.
* find the correct material
      CLEAR ls_costelemnt.
      READ TABLE lt_costelemnt INTO ls_costelemnt WITH  KEY  costelmnt = ls_exp-0costelmnt.

      IF ls_costelemnt-/bic/celmsubgr IS NOT INITIAL AND ls_exp-0wbs_elemt IS NOT INITIAL AND ls_costelemnt-/bic/celmsubgr IS NOT INITIAL.
        LOOP AT  lt_markup INTO ls_markup WHERE  wbs_elemt = ls_exp-0wbs_elemt
                                            AND  materialnr = ls_costelemnt-/bic/celmsubgr
                                            AND   validto GE lv_lastday
                                            AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.

      IF lv_found IS INITIAL AND ls_hours-0wbs_elemt IS NOT INITIAL AND ls_exp-0employee IS NOT INITIAL.
        LOOP AT  lt_markup INTO ls_markup WHERE  wbs_elemt = ls_exp-0wbs_elemt
                                            AND  employee = ls_exp-0employee
                                            AND   validto GE lv_lastday
                                            AND   validfrom  LE lv_lastday.

          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found IS INITIAL AND ls_hours-0wbs_elemt IS NOT INITIAL AND ls_exp-0acttype IS NOT INITIAL.
        LOOP AT  lt_markup INTO ls_markup WHERE  wbs_elemt = ls_exp-0wbs_elemt
                                           AND  acttype = ls_exp-0acttype
                                           AND   validto GE lv_lastday
                                           AND   validfrom  LE lv_lastday.

          lv_found = 1.
        ENDLOOP.
      ENDIF.
      "  check   ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial.
      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL AND ls_costelemnt-/bic/celmsubgr IS NOT INITIAL.
        LOOP AT  lt_markup INTO ls_markup WHERE /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                           AND   materialnr = ls_costelemnt-/bic/celmsubgr
                                           AND   validto GE lv_lastday
                                           AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL AND ls_hours-0employee IS NOT INITIAL.
        LOOP AT  lt_markup INTO ls_markup WHERE /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                            AND   employee = ls_exp-0employee
                                            AND   validto GE lv_lastday
                                            AND   validfrom  LE lv_lastday.
          .
          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL AND ls_exp-0acttype IS NOT INITIAL.
        LOOP AT  lt_markup INTO ls_markup WHERE /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                           AND  acttype = ls_exp-0acttype
                                           AND   wbs_elemt = ''
                                           AND   validto GE lv_lastday
                                           AND   validfrom  LE lv_lastday.

          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found IS INITIAL AND ls_wbs_elemnt-/bic/sd_svcpac IS NOT INITIAL .
        LOOP AT  lt_markup INTO ls_markup WHERE /bic/sd_svcpac = ls_wbs_elemnt-/bic/sd_svcpac
                                           AND   wbs_elemt = ''
                                           AND   validto GE lv_lastday
                                           AND   validfrom  LE lv_lastday.
          lv_found = 1.
        ENDLOOP.
      ENDIF.
      IF lv_found = 1.
        ls_data-0fiscper  = ls_exp-0fiscper.
        ls_data-0fiscvarnt = ls_exp-0fiscvarnt.
        ls_data-0acttype    = ls_exp-0acttype.
        ls_data-0comp_code  = ls_wbs_elemnt-comp_code.
        ls_data-0costcenter = ls_exp-0costcenter.
        ls_data-0costelmnt  = ls_map-costelmnt.
        ls_data-0co_area    = ls_exp-0co_area.
        ls_data-0employee   = ls_exp-0employee.
        ls_data-0infoprov   = ls_exp-0infoprov .
        ls_data-0project    = ls_exp-0project.
        ls_data-0version    = ls_exp-0version.
        ls_data-0wbs_elemt  = ls_exp-0wbs_elemt.
        ls_data-celmsrc     = ls_exp-0costelmnt.
        ls_data-bpcaudit    = 'TMREV'.
        ls_data-bpcversin   = ls_exp-bpcversin.
        ls_data-0obj_cur   = ls_comp_code-currency.
        ls_data-0unit       = ''.
        ls_data-0amount_oc     = ls_exp-0amount_oc +  ( ls_markup-/bic/mrkprt * ls_exp-0amount_oc / 1000 ) .
        ls_data-0quantity   = 0.

        COLLECT ls_data INTO c_th_data.
      ENDIF.
    ENDIF.   "if ew coulndt find result cost element
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->TM_REVENUE_OLD_COND
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method TM_REVENUE_OLD_COND.
***********************************************************************
****A907  WBS element/Generic Material
****A905  WBS element/Agent
****A906  WBS Element/Activity Type
****A902  Service Package / Generic Material
****A904  Service Package/ Activity Type
****A903  Service Package
**** Date    : 31.08.2021
**** Change  :
**
*  "Incomes for T&M ((Number of planned hours of that period * selling price of the last actual period)
*  " + ( Amount of expenses  + (Amount of expenses * mark-up of the last actual period /1000)
*  "wbs-genericmaterial
*  "wbs-agent
*  "wbs-activity type
*  "servicepac-material
*  "service pac-activity type
*  "service pack
***********************************************************************
*
*  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a03C.
*  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a03C.
*  DATA: lt_charsel           TYPE rsplf_t_charsel,
*        ls_charsel           LIKE LINE OF lt_charsel,
*        lv_compcode          TYPE /BI0/OIcomp_code,
*        lv_version           TYPE /BIC/OIBPCVERSIN,
*        lv_project           TYPE /BI0/OIPROJECT,
*        ls_project           TYPE /BI0/PPROJECT,
*        ls_wbs_elemnt        TYPE /BI0/PWBS_ELEMT,
*        ls_comp_code         TYPE /BI0/PCOMP_CODE,
*        lt_hours             TYPE TABLE OF zbpce_pal_cck_a03C,
*        ls_hours             TYPE zbpce_pal_cck_a03C,
*        lt_exp               TYPE TABLE OF zbpce_pal_cck_a03C,
*        ls_exp               TYPE zbpce_pal_cck_a03C,
*       " lt_price             TYPE TABLE OF /BIC/ASDA017,
*       " ls_price             TYPE /BIC/ASDA017,
*        ls_conditions        TYPE zbpce_pal_cck_a03C,
*        lt_markup            TYPE TABLE OF /BIC/ASDA027,
*        ls_markup            TYPE /BIC/ASDA027,
*        lv_f_beg             TYPE zbpce_pal_cck_a03C-0fiscper,
*        lv_f_end             TYPE zbpce_pal_cck_a03C-0fiscper,
*        lv_last_closed_month TYPE i,
*        lv_fiscper           TYPE zbpce_pal_cck_a03C-0fiscper,
*        ls_data              TYPE zbpce_pal_cck_a03C,
*        lt_inflation         TYPE TABLE OF /BIC/ABPCFID027,
*        ls_inflation         TYPE  /BIC/ABPCFID027,
*        lv_found             type i,
*        lv_lastday           type d,
*        lt_map               TYPE TABLE OF ZWBS_MAP_01,
*        ls_map               TYPE ZWBS_MAP_01,
*        lv_lastdaydats       TYPE dats,
*        lv_famount           TYPE f.
*
**
*  DATA: ls_costelemnt TYPE /BI0/MCOSTELMNT,
*        lt_costelemnt type table of /BI0/MCOSTELMNT.
*
*
*  SELECT COSTELMNT AS COSTELMNT, /BIC/CELMSUBGR AS /BIC/CELMSUBGR
*          FROM /BI0/MCOSTELMNT
*          INTO CORRESPONDING FIELDS OF TABLE  @LT_costelemnt
*          WHERE   objvers = 'A'
*          AND CSTELMNTYP = '21'
*          AND  /BIC/CELMTYPE =  'GEXP'.
*  .
*
*  SELECT INPUTTYPE AS INPUTTYPE, PS_PRJTYPE AS PS_PRJTYPE,
*           COSTELMNT as COSTELMNT
*          FROM  ZWBS_MAP_01
*          INTO CORRESPONDING FIELDS OF TABLE  @LT_MAP.
*
***********************************************************************
**
*  CALL METHOD cl_rsplfr_controller=>get_active_selection
*    RECEIVING
*      r_t_charsel = lt_charsel.
*
*  clear ls_charsel.
*  read table lt_charsel into ls_charsel with key iobjnm = 'BPCVERSIN'.
*  lv_version = ls_charsel-low.
*
*
*  LOOP AT i_th_ref_data ASSIGNING <ls_data>.
*
*    if <ls_data>-0COSTELMNT = 'S218000021'.
*      if  <ls_data>-BPCAUDIT = 'M_INPUT'    AND <ls_data>-0UNIT = 'H'
*       AND <ls_data>-bpcversin = lv_version AND  <ls_data>-0obj_cur = ''.
*        COLLECT <ls_data> INTO lt_hours.    "hours inputted
*      endif.
*    else.
*      COLLECT <ls_data> INTO lt_exp.    "exp inputted
*    endif.
*  ENDLOOP.
*
**before calculation first clear
*
*  LOOP AT c_th_data ASSIGNING <ls_data>.
*    <ls_data>-0amount_oc = 0.
*  ENDLOOP.
*
**get the prices
*  SELECT ACTTYPE AS ACTTYPE, CO_AREA AS CO_AREA, EMPLOYEE AS EMPLOYEE,KNART AS KNART, KNSTEP AS KNSTEP,WBS_ELEMT AS WBS_ELEMT,
*                  MATERIALNR as MATERIALNR, CONDITIONUSA AS CONDITIONUSA, /BIC/SD_SVCPAC AS /BIC/SD_SVCPAC,CURRENCY AS CURRENCY,
*           SUM( AMOUNT ) AS AMOUNT,
*           SUM( QUANTITY ) AS QUANTITY
*             FROM /BIC/ASDA017
*             INTO CORRESPONDING FIELDS OF TABLE  @LT_PRICE
*               GROUP BY ACTTYPE ,CO_AREA , EMPLOYEE, KNART, KNSTEP , WBS_ELEMT, MATERIALNR ,CONDITIONUSA,/BIC/SD_SVCPAC,CURRENCY.
*  DELETE LT_PRICE WHERE amount  EQ 0.
**PRICE LOOKUP
*
**get the markups
*  SELECT ACTTYPE AS ACTTYPE, CO_AREA AS CO_AREA, EMPLOYEE AS EMPLOYEE,KNART AS KNART, KNSTEP AS KNSTEP,WBS_ELEMT AS WBS_ELEMT,
*                  MATERIALNR as MATERIALNR, CONDITIONUSA AS CONDITIONUSA, /BIC/SD_SVCPAC AS /BIC/SD_SVCPAC,
*           SUM( /BIC/MRKPRT ) AS /BIC/MRKPRT
*            FROM /BIC/ASDA027
*             INTO CORRESPONDING FIELDS OF TABLE  @LT_MARKUP
*               GROUP BY ACTTYPE ,CO_AREA , EMPLOYEE, KNART, KNSTEP , WBS_ELEMT, MATERIALNR ,CONDITIONUSA,/BIC/SD_SVCPAC.
*  DELETE LT_PRICE WHERE amount  EQ 0.
**MARKUP LOOKUP
*
***0wbc_elemnt--> result_key_analysis*
**If it is blank --> TM
**Get the attributes of WBS element ( service package)
*
*  loop at lt_hours into ls_hours.
*
*    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /BI0/PWBS_ELEMT WHERE objvers = 'A' AND WBS_ELEMT = ls_hours-0WBS_ELEMT.
** check if it is TM project.
*    check ls_wbs_elemnt-/BIC/PS_RKEYAN = ''.
**get the compnay code currency
*    SELECT SINGLE * INTO ls_comp_code  FROM /BI0/MCOMP_CODE WHERE objvers = 'A' AND COMP_CODE = ls_wbs_elemnt-COMP_CODE.
**get the result costelement.
*    read table lt_map into ls_map with  key  INPUTTYPE = 'HRS'
*                                             PS_PRJTYPE = ls_wbs_elemnt-PS_PRJTYPE.
*    if sy-subrc ne 0.
*      "write a message
*    else.
*
** check if it is TM project.
*      clear: ls_data, ls_price, ls_markup, lv_found.
*      "wbs-genericmaterial
*      "wbs-agent
*      "wbs-activity type
*      "servicepac-material
*      "servicepac-agent
*      "service pac-activity type
*      "service pack
*      clear ls_costelemnt.
*      read table LT_costelemnt into ls_costelemnt with  key  COSTELMNT = ls_hours-0COSTELMNT.
**read prices with an order.
*
*      if ls_costelemnt-/BIC/CELMSUBGR is not initial and ls_hours-0WBS_ELEMT is not initial and ls_costelemnt-/BIC/CELMSUBGR is not initial.
*        read table lt_price into ls_price with  key  WBS_ELEMT = ls_hours-0WBS_ELEMT
*                                                     MATERIALNR = ls_costelemnt-/BIC/CELMSUBGR.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*
*      if lv_found is initial and ls_hours-0WBS_ELEMT is not initial and ls_hours-0employee is not initial.
*        read table lt_price into ls_price with  key  WBS_ELEMT = ls_hours-0WBS_ELEMT
*                                                  employee = ls_hours-0employee.
*
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found is initial and ls_hours-0WBS_ELEMT is not initial and ls_hours-0ACTTYPE is not initial.
*        read table lt_price into ls_price with  key  WBS_ELEMT = ls_hours-0WBS_ELEMT
*                                                     acttype = ls_hours-0ACTTYPE
*                                                     employee = ''.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      "  check   ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial.
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial and ls_costelemnt-/BIC/CELMSUBGR is not initial.
*        read table lt_price into ls_price with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                      MATERIALNR = ls_costelemnt-/BIC/CELMSUBGR.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial and ls_hours-0employee is not initial.
*        read table lt_price into ls_price with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                      Employee = ls_hours-0employee.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial and ls_hours-0ACTTYPE is not initial.
*        read table lt_price into ls_price with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                    acttype = ls_hours-0ACTTYPE
*                                                    WBS_ELEMT = ''.
*
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial .
*        read table lt_price into ls_price with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                     WBS_ELEMT = ''.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found = 1.
*        ls_data-0fiscper  = ls_hours-0fiscper.
*        ls_data-0fiscvarnt = ls_hours-0fiscvarnt.
*        ls_data-0acttype    = ls_hours-0acttype.
*        ls_data-0comp_code  = ls_wbs_elemnt-COMP_CODE.
*        ls_data-0costcenter = ls_hours-0COSTCENTER.
*        ls_data-0costelmnt  =  ls_map-COSTELMNT   .
*        ls_data-0co_area    = ls_hours-0co_area.
*        ls_data-0employee   = ls_hours-0EMPLOYEE.
*        ls_data-0infoprov   = ls_hours-0infoprov .
*        ls_data-0PROJECT    = ls_hours-0PROJECT.
*        ls_data-0VERSION    = ls_hours-0VERSION.
*        ls_data-0WBS_ELEMT  = ls_hours-0WBS_ELEMT.
*        ls_data-CELMSRC     = 'S218000021'. " source subgroup
*        ls_data-BPCAUDIT    = 'TMREV'.
*        ls_data-BPCVERSIN   = ls_hours-BPCVERSIN.
*        ls_data-0obj_cur    = ls_comp_code-CURRENCY.
*        if ls_price-CURRENCY = ls_comp_code-CURRENCY.
*          ls_data-0obj_cur  = ls_price-CURRENCY.
*          lv_famount = ls_price-AMOUNT.
*        else.
*          CALL FUNCTION 'Z_LAST_DAY_IN_FISCAL_PERIOD'
*            EXPORTING
*              I_FPER    = ls_hours-0fiscper
*              I_FVARNT  = 'Z4'
*            IMPORTING
*              E_LASTDAY = lv_lastday.
*          CLEAR:lv_lastdaydats,lv_famount.
*          lv_lastdaydats = lv_lastday.
*          lv_famount = ls_price-amount.
*          CALL FUNCTION 'Z_CONVERT_AMOUNT_FROM_CURRENCY'
*            EXPORTING
*              KURST  = 'M'
*              FCURR  = ls_price-CURRENCY
*              TCURR  = ls_comp_code-CURRENCY
*              DATUM  = lv_lastdaydats
*              RATE   = lv_famount
*            IMPORTING
*              AMOUNT = lv_famount.
*        endif.
*        ls_data-0unit       = ''.
*        if ls_price-QUANTITY  = 0. ls_price-QUANTITY = 1. endif.
*        ls_data-0AMOUNT_oc     = ls_hours-0QUANTITY *  ( lv_famount / ls_price-QUANTITY ) .
*        ls_data-0QUANTITY   = 0.
*
*        COLLECT ls_data INTO c_th_data.
*      endif.
*    endif.  "if we coulndt find wbs elemnt type
*  endloop.
*
*  loop at lt_exp into ls_exp.
*
*    SELECT SINGLE * INTO ls_wbs_elemnt  FROM /BI0/PWBS_ELEMT WHERE objvers = 'A' AND WBS_ELEMT = ls_exp-0WBS_ELEMT.
** check if it is TM project.
*    check ls_wbs_elemnt-/BIC/PS_RKEYAN = ''.
**get the compnay code currency
*    SELECT SINGLE * INTO ls_comp_code  FROM /BI0/MCOMP_CODE WHERE objvers = 'A' AND COMP_CODE = ls_wbs_elemnt-COMP_CODE.
** check if it is TM project.
**get the result costelement.
*    read table lt_map into ls_map with  key  INPUTTYPE = 'EXP'
*                                             PS_PRJTYPE = ls_wbs_elemnt-PS_PRJTYPE.
*    if sy-subrc ne 0.
*      "write a message
*    else.
*      clear: ls_data, ls_price, ls_markup, lv_found.
** find the correct material
*      clear ls_costelemnt.
*      read table LT_costelemnt into ls_costelemnt with  key  COSTELMNT = ls_exp-0COSTELMNT.
*
*      if ls_costelemnt-/BIC/CELMSUBGR is not initial and ls_exp-0WBS_ELEMT is not initial and ls_costelemnt-/BIC/CELMSUBGR is not initial.
*        read table lt_markup into ls_markup with  key  WBS_ELEMT = ls_exp-0WBS_ELEMT
*                                                     MATERIALNR = ls_costelemnt-/BIC/CELMSUBGR.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*
*      if lv_found is initial and ls_hours-0WBS_ELEMT is not initial and ls_exp-0employee is not initial.
*        read table lt_markup into ls_markup with  key  WBS_ELEMT = ls_exp-0WBS_ELEMT
*                                                  employee = ls_exp-0employee.
*
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found is initial and ls_hours-0WBS_ELEMT is not initial and ls_exp-0ACTTYPE is not initial.
*        read table lt_markup into ls_markup with  key  WBS_ELEMT = ls_exp-0WBS_ELEMT
*                                                     acttype = ls_exp-0ACTTYPE.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      "  check   ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial.
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial and ls_costelemnt-/BIC/CELMSUBGR is not initial.
*        read table lt_markup into ls_markup with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                      MATERIALNR = ls_costelemnt-/BIC/CELMSUBGR.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial and ls_hours-0employee is not initial.
*        read table lt_markup into ls_markup with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                      Employee = ls_exp-0employee.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial and ls_exp-0ACTTYPE is not initial.
*        read table lt_markup into ls_markup with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                    acttype = ls_exp-0ACTTYPE
*                                                    WBS_ELEMT = ''.
*
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found is initial and ls_wbs_elemnt-/BIC/SD_SVCPAC is not initial .
*        read table lt_markup into ls_markup with  key  /BIC/SD_SVCPAC = ls_wbs_elemnt-/BIC/SD_SVCPAC
*                                                     WBS_ELEMT = ''.
*        if sy-subrc = 0.
*          lv_found = 1.
*        endif.
*      endif.
*      if lv_found = 1.
*        ls_data-0fiscper  = ls_exp-0fiscper.
*        ls_data-0fiscvarnt = ls_exp-0fiscvarnt.
*        ls_data-0acttype    = ls_exp-0acttype.
*        ls_data-0comp_code  = ls_wbs_elemnt-COMP_CODE.
*        ls_data-0costcenter = ls_exp-0COSTCENTER.
*        ls_data-0costelmnt  = ls_map-COSTELMNT.
*        ls_data-0co_area    = ls_exp-0co_area.
*        ls_data-0employee   = ls_exp-0EMPLOYEE.
*        ls_data-0infoprov   = ls_exp-0infoprov .
*        ls_data-0PROJECT    = ls_exp-0PROJECT.
*        ls_data-0VERSION    = ls_exp-0VERSION.
*        ls_data-0WBS_ELEMT  = ls_exp-0WBS_ELEMT.
*        ls_data-CELMSRC     = ls_exp-0COSTELMNT.
*        ls_data-BPCAUDIT    = 'TMREV'.
*        ls_data-BPCVERSIN   = ls_exp-BPCVERSIN.
*        ls_data-0obj_cur    = ls_comp_code-CURRENCY.
*        ls_data-0unit       = ''.
*        ls_data-0AMOUNT_oc     = ls_exp-0AMOUNT_oc +  ( ls_markup-/BIC/MRKPRT * ls_exp-0AMOUNT_oc / 1000 ) .
*        ls_data-0QUANTITY   = 0.
*
*        COLLECT ls_data INTO c_th_data.
*      endif.
*    endif.   "if ew coulndt find result cost element
*  endloop.
*
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->UPDATE_CAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_R_PARAM_SET                  TYPE REF TO IF_RSPLFA_PARAM_SET
* | [--->] I_TH_REF_DATA                  TYPE        HASHED TABLE
* | [--->] I_S_BLOCK_LINE                 TYPE        ANY
* | [--->] I_R_MSG                        TYPE REF TO IF_RSPLFA_MSG
* | [<-->] C_TH_DATA                      TYPE        HASHED TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD update_car.

  FIELD-SYMBOLS : <ls_data> TYPE zbpce_pal_cck_a08c.
  DATA:lt_data TYPE TABLE OF zbpce_pal_cck_a08c.
  DATA: lt_charsel           TYPE rsplf_t_charsel,
        ls_charsel           LIKE LINE OF lt_charsel,
        lv_compcode          TYPE /bi0/oicomp_code,
        lv_version           TYPE /bic/oibpcversin,
        lv_project           TYPE /bi0/oiproject,
        ls_project           TYPE /bi0/pproject,
        ls_wbs_elemnt        TYPE /bi0/pwbs_elemt,
        ls_comp_code         TYPE /bi0/pcomp_code,
        lt_hours             TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_hours             TYPE zbpce_pal_cck_a08c,
        lt_exp               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_exp               TYPE zbpce_pal_cck_a08c,
        lt_act               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_act               TYPE zbpce_pal_cck_a08c,
        lt_act_exp           TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_act_exp           TYPE zbpce_pal_cck_a08c,
        lt_plan              TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_plan              TYPE zbpce_pal_cck_a08c,
        ls_plan2             TYPE zbpce_pal_cck_a08c,
        lt_actplan           TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_actplan           TYPE zbpce_pal_cck_a08c,
        lt_plan_exp          TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_plan_exp          TYPE zbpce_pal_cck_a08c,
        lt_actplan_exp       TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_actplan_exp       TYPE zbpce_pal_cck_a08c,
        lt_bet               TYPE TABLE OF zbpce_pal_cck_a08c,
        ls_bet               TYPE zbpce_pal_cck_a08c,
        lt_total             TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total             TYPE zbpce_pal_cck_a08ct,
        lt_total_exp         TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total_exp         TYPE zbpce_pal_cck_a08ct,
        lt_total_act         TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total_act         TYPE zbpce_pal_cck_a08ct,
        lt_total_act_exp     TYPE TABLE OF zbpce_pal_cck_a08ct,
        ls_total_act_exp     TYPE zbpce_pal_cck_a08ct,
        ls_conditions        TYPE zbpce_pal_cck_a03c,
        ls_breakdown         TYPE zbpce_breakdown,
        lt_breakdown         TYPE TABLE OF zbpce_breakdown,
        ls_breakdown_exp     TYPE zbpce_breakdown,
        lt_breakdown_exp     TYPE TABLE OF zbpce_breakdown,
        lv_f_beg             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_f_end             TYPE zbpce_pal_cck_a03c-0fiscper,
        lv_last_closed_month TYPE i,
        lv_fiscper           TYPE zbpce_pal_cck_a03c-0fiscper,
        ls_data              TYPE zbpce_pal_cck_a08c,
        lt_inflation         TYPE TABLE OF /bic/abpcfid027,
        ls_inflation         TYPE  /bic/abpcfid027,
        lv_found             TYPE i,
        lv_lastday           TYPE d,
        lt_map               TYPE TABLE OF zwbs_map_01,
        ls_map               TYPE zwbs_map_01,
        lv_lastdaydats       TYPE dats,
        lv_famount           TYPE f,
        lv_difamount         TYPE  decfloat34, " /bi0/oiquantity.
        lv_round             TYPE  /bi0/oiamount_oc,
        lv_roundq            TYPE  /bi0/oiquantity.

*
  DATA: ls_costelemnt  TYPE /bi0/mcostelmnt,
        ls_costelemnt2 TYPE /bi0/mcostelmnt,
        lt_costelemnt  TYPE TABLE OF /bi0/mcostelmnt.
  DATA :lv_act_prev_amount   TYPE /bi0/oiamount_oc,
        lv_act_prev_quantity TYPE /bi0/oiquantity.


**********************************************************************
*
  CALL METHOD cl_rsplfr_controller=>get_active_selection
    RECEIVING
      r_t_charsel = lt_charsel.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = 'BPCVERSIN'.
  lv_version = ls_charsel-low.

  CLEAR ls_charsel.
  READ TABLE lt_charsel INTO ls_charsel WITH KEY iobjnm = '0PROJECT'.
  lv_project = ls_charsel-low.
* if it hours , WBS element, activity type, costcenter
* if it is expenses BPC Version, WBS, Cost Element, Fiscper
  DATA: check TYPE i.
  check = 0.

"get the planning seq name, to understand which of the keyfigures should be updated
"l_r_srv = cl_rsplfs_srv=>factory( i_srvnm = i_srvnm i_objvers = i_objvers ).
 DATA(l_r_application) = cl_rspls_plan_application=>get( ).
"check if CAR avaliable
  LOOP AT c_th_data ASSIGNING <ls_data>.
    CLEAR ls_plan.
    MOVE <ls_data> TO ls_plan.
    IF ls_plan-bpcaudit = 'CAR'.
      check = 1.
    ENDIF.
  ENDLOOP.
*if it avaliable update it with the PFWD.
  IF check = 1.
    LOOP AT c_th_data ASSIGNING <ls_data>.
      CLEAR ls_plan.
      MOVE <ls_data> TO ls_plan.
      IF ls_plan-bpcaudit = 'PFWD' and ls_plan-0fiscper = lv_version.
        ls_plan-0fiscper = lv_version.
        ls_plan-bpcaudit = 'CAR'.
        COLLECT ls_plan INTO c_th_data.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSPLF_WREF->_RFC_READ_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DESTINATION                 TYPE        RSDLOGSYS
* | [--->] IV_QUERY_TABLE                 TYPE        DD02L-TABNAME
* | [--->] IV_STRUCTURE                   TYPE        ANY
* | [--->] IT_SELOPT                      TYPE        TT_DDSHSELOPT
* | [--->] IT_FIELDS                      TYPE        TT_RFC_DB_FLD
* | [<---] ET_DATA                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _RFC_READ_TABLE.
        FIELD-SYMBOLS:<lt_data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS:<ls_data> TYPE any.
    FIELD-SYMBOLS:<any> TYPE any.
    DATA:lt_data TYPE TABLE OF tab512.
    DATA:lt_split TYPE TABLE OF string.
    DATA:lt_component TYPE abap_compdescr_tab.
    DATA:l_index TYPE sy-tabix.
    DATA:lobj_structdescr TYPE REF TO cl_abap_structdescr.
    DATA:lt_refdata TYPE REF TO data.
    DATA:ls_refdata TYPE REF TO data.
    DATA:lt_options TYPE TABLE OF rfc_db_opt.
    DATA:ls_options TYPE rfc_db_opt.
    DATA:lv_where_clause TYPE string.
    DATA:lv_where_clause2 TYPE string.
    DATA:lt_selopt TYPE TABLE OF ddshselopt.
    TRY.
        lobj_structdescr ?= cl_abap_structdescr=>describe_by_data( iv_structure ).
      CATCH cx_root.
    ENDTRY.

    lt_component = lobj_structdescr->components.

    CHECK lt_component IS NOT INITIAL.

    lt_selopt = it_selopt.
    CALL FUNCTION 'F4_CONV_SELOPT_TO_WHERECLAUSE'
      IMPORTING
        where_clause = lv_where_clause
      TABLES
        selopt_tab   = lt_selopt.

*    CALL METHOD _convert_where_to_rfc_options
*      EXPORTING
*        iv_where_clause = lv_where_clause
*      IMPORTING
*        et_rfc_db_opt   = lt_options.

    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION iv_destination
      EXPORTING
        query_table = iv_query_table
        delimiter   = ','
      TABLES
        data        = lt_data
        options     = lt_options
        fields      = it_fields.

    CHECK lt_data IS NOT INITIAL.

    CREATE DATA lt_refdata LIKE TABLE OF iv_structure.
    ASSIGN lt_refdata->* TO <lt_data>.
    CHECK sy-subrc = 0 AND <lt_data> IS ASSIGNED.

    CREATE DATA ls_refdata LIKE iv_structure.
    ASSIGN ls_refdata->* TO <ls_data>.
    CHECK sy-subrc = 0 AND <ls_data> IS ASSIGNED.

    LOOP AT lt_data INTO DATA(l_data).
      CLEAR:lt_split.
      SPLIT l_data AT ',' INTO TABLE lt_split.

      CLEAR:<ls_data>.
      LOOP AT lt_component INTO DATA(ls_component).
        l_index = sy-tabix.
        ASSIGN COMPONENT ls_component-name OF STRUCTURE <ls_data> TO <any>.
        CHECK sy-subrc = 0 AND <any> IS ASSIGNED.

        READ TABLE lt_split INTO DATA(ls_split) INDEX l_index.
        CHECK sy-subrc = 0.
        <any> = ls_split.
      ENDLOOP.
      APPEND <ls_data> TO <lt_data>.
    ENDLOOP.

    CHECK <lt_data> IS NOT INITIAL.

    et_data = <lt_data>.
  endmethod.
ENDCLASS.
FUNCTION Z_LAST_DAY_IN_FISCAL_PERIOD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_FPER) TYPE  /BI0/OIFISCPER
*"     REFERENCE(I_FVARNT) TYPE  /BI0/OIFISCVARNT
*"  EXPORTING
*"     REFERENCE(E_LASTDAY) TYPE  D
*"----------------------------------------------------------------------

data: lv_fyear type /BI0/OIFISCyear,
      lv_fper3 type /BI0/OIFISCper3.

lv_fyear = i_fper+0(4).
lv_fper3 = i_fper+4(3).

CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
  EXPORTING
    i_gjahr              = lv_fyear
*   I_MONMIT             = 00
    i_periv              = I_FVARNT
    i_poper              = lv_fper3
 IMPORTING
   E_DATE               = e_lastday

          .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDFUNCTION.
