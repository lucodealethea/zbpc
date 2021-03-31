CLASS zcl_bpc_sga_hours_based_alloc1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES if_badi_interface .
  INTERFACES if_uj_custom_logic .
PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    BEGIN OF t_s_dim_list,
          dimension TYPE uja_dimension-dimension,
*          property(20) type c,
          property TYPE uj_dim_member,
          kind(1) TYPE c,
      END OF t_s_dim_list .
  TYPES:
    t_th_dim_list TYPE HASHED TABLE OF t_s_dim_list WITH UNIQUE KEY dimension property INITIAL SIZE 1 .

  METHODS process_parameters
    IMPORTING
      !it_param TYPE ujk_t_script_logic_hashtable
      !it_cv TYPE ujk_t_cv
      !i_user TYPE uj_user_id
    EXPORTING
      !et_lgx TYPE ujk_t_script_logic_scripttable
      !e_chng_list TYPE t_th_dim_list
      !e_docname TYPE uj_docname
      !e_subrc TYPE sy-subrc
    RAISING
      cx_uj_custom_logic .
ENDCLASS.



CLASS zcl_bpc_sga_hours_based_alloc1 IMPLEMENTATION.


METHOD if_uj_custom_logic~cleanup.
* The INIT method can be used for PRE processing.
* The CLEANUP method can be used for POST processing.
* CLEANUP cleans what INIT populated.

* Example can be a script with XDIM_MAXMEMBERS.
* In that case EXECUTE will be called multiple times.
* If it reads Master Data it will be more efficient reading MD in INIT.
*  Then CLEANUP will remove data from those internal tables.
*DATA: t_debug_flag_cleanup VALUE 'X'.
*    IF sy-uname = 'BB5827'.
*      DO. IF t_debug_flag_cleanup = ' '. EXIT. ENDIF.
*      ENDDO.
*    ENDIF.
*BREAK BB5827.
ENDMETHOD.


METHOD if_uj_custom_logic~execute.

TYPES: BEGIN OF ty_obs,
obs TYPE uj_dim_member,
END OF ty_obs.

TYPES: tty_obs TYPE STANDARD TABLE OF ty_obs.

TYPES: BEGIN OF ty_obs_to_create,
n_account TYPE uj_dim_member,
obs TYPE uj_dim_member,
END OF ty_obs_to_create.

TYPES: BEGIN OF ty_dist_nacc,
n_account TYPE uj_dim_member,
ctr TYPE uj_signeddata,
END OF ty_dist_nacc.

TYPES: BEGIN OF ty_dist_nacc_obs,
n_account    TYPE uj_dim_member,
obs          TYPE uj_dim_member,
calc         TYPE char1,
created_in_ds TYPE char1,
bus_area  TYPE uj_dim_member,
parenth1  TYPE uj_dim_member,
parenth4  TYPE uj_dim_member,
parenth5  TYPE uj_dim_member,
ctr TYPE uj_signeddata,
END OF ty_dist_nacc_obs.

TYPES: tty_dist_nacc         TYPE HASHED TABLE OF ty_dist_nacc WITH UNIQUE KEY n_account.
TYPES: tty_dist_nacc_obs     TYPE HASHED TABLE OF ty_dist_nacc_obs
                             WITH UNIQUE KEY obs n_account calc bus_area created_in_ds parenth1 parenth4 parenth5 .
TYPES: tty_obs_to_create     TYPE STANDARD TABLE OF ty_obs_to_create WITH KEY n_account obs.

TYPES: BEGIN OF ty_fa,
functional_area TYPE uj_dim_member,
END OF ty_fa.

TYPES: BEGIN OF ty_figures,
figures TYPE uj_dim_member,
END OF ty_figures.

TYPES: BEGIN OF l_t_unfold,
    parent_lv1 TYPE uj_dim_member,
    parent_lv2 TYPE uj_dim_member,
    parent_lv3 TYPE uj_dim_member,
    parent_lv4 TYPE uj_dim_member,
    parent_lv5 TYPE uj_dim_member,
    parent_lv6 TYPE uj_dim_member,
    parent_lv7 TYPE uj_dim_member,
    parent_lv8 TYPE uj_dim_member,
    parent_lv9 TYPE uj_dim_member,
    seqnr TYPE i,
END OF l_t_unfold.

TYPES: BEGIN OF g_t_fahier.
  INCLUDE TYPE uja_s_mbr_node.
  INCLUDE TYPE l_t_unfold.
TYPES: END OF g_t_fahier.

DATA: gst_fahier TYPE g_t_fahier,
      gtt_fahier TYPE STANDARD TABLE OF g_t_fahier WITH KEY member.


DATA:       ls_fa_basealloc_2              TYPE uj_dim_member,
            ls_figures_basealloc           TYPE uj_dim_member,
            ls_dist_skf_n_account          TYPE ty_dist_nacc,
            lt_dist_skf_n_account          TYPE tty_dist_nacc,
            ls_dist_skf_obs_n_account      TYPE ty_dist_nacc_obs,
            lt_dist_skf_obs_n_account      TYPE tty_dist_nacc_obs,
            ls_obs_to_create               TYPE ty_obs_to_create,
            lt_obs_to_create               TYPE tty_obs_to_create,
            ls_adjust_sg43                 TYPE uj_dim_member.

DATA:        l_user              TYPE uj_user_id,
             ls_user             TYPE uj0_s_user,
             lt_th_lgx           TYPE ujk_t_script_logic_scripttable,
             lt_th_dim           TYPE t_th_dim_list,
             l_docname           TYPE uj_docname,
             l_subrc             TYPE sy-subrc,
             lo_old_context      TYPE REF TO if_uj_context,
             lo_user             TYPE REF TO cl_uje_user.

DATA:       wa_param_figures_basealloc   TYPE ujk_s_script_logic_hashentry,
            wa_param_fa_basealloc_2      TYPE ujk_s_script_logic_hashentry,
            wa_param_adjust_sg43         TYPE ujk_s_script_logic_hashentry.

DATA:       wa_param_fa_top_node           TYPE ujk_s_script_logic_hashentry,
            wa_param_fa_node_defines_smart TYPE ujk_s_script_logic_hashentry,
            wa_param_o_smart               TYPE ujk_s_script_logic_hashentry,
            wa_param_s_smart               TYPE ujk_s_script_logic_hashentry,
            wa_param_bu_line_t             TYPE ujk_s_script_logic_hashentry,
            wa_param_datasrc_t             TYPE ujk_s_script_logic_hashentry,
            wa_param_n_account_t           TYPE ujk_s_script_logic_hashentry,
            wa_param_n_account_t2          TYPE ujk_s_script_logic_hashentry,
            wa_param_partner_t             TYPE ujk_s_script_logic_hashentry,
            wa_param_product_t             TYPE ujk_s_script_logic_hashentry,
            wa_param_version_t             TYPE ujk_s_script_logic_hashentry,
            wa_param_figures_t             TYPE ujk_s_script_logic_hashentry.

DATA:       wa_param_obs_gb41_input      TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_ae42_input      TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_sg43_input_1    TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_gb41_what       TYPE ujk_s_script_logic_hashentry,
            wa_param_node_gb41_d         TYPE ujk_s_script_logic_hashentry,
            wa_param_node_ae42_d         TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_ae42_what       TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_sg43_what_1     TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_sg43_what_2     TYPE ujk_s_script_logic_hashentry.

DATA:
            wa_param_obs_sg43_addhie_i   TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_sg43_adjust_i   TYPE ujk_s_script_logic_hashentry,
            wa_param_obs_sg43_adjust_i2  TYPE ujk_s_script_logic_hashentry,
            wa_param_node_sg43_n_1       TYPE ujk_s_script_logic_hashentry,
            wa_param_node_sg43_n_2       TYPE ujk_s_script_logic_hashentry,
            wa_param_node_sg43_d_1       TYPE ujk_s_script_logic_hashentry.

DATA:       lt_split_obs_gb41_input      TYPE tty_obs,
            lt_split_obs_ae42_input      TYPE tty_obs,
            lt_split_obs_sg43_input_1    TYPE tty_obs.

DATA:       orig_n_account               TYPE uj_dim_member,
            orig_obs                     TYPE uj_dim_member,
            orig_partner                 TYPE uj_dim_member,
            skf_fa_perc                  TYPE /b28/oisdata,
            orig_amount                  TYPE /b28/oisdata.


DATA:  t_debug_flag_start VALUE 'X'.

DATA:  ld_log TYPE string.

DATA:  loop_tabix TYPE sy-tabix,
       lt_selection_dim TYPE uj0_t_sel,
       lx_selection_dim TYPE uj0_s_sel,
       lx_cv TYPE ujk_s_cv,
       lt_member TYPE uja_t_dim_member,
       lx_member TYPE uj_dim_member,
       ls_timeselection TYPE uj_dim_member,
       lt_timeselection   TYPE TABLE OF uj_dim_member,
       l_versionselection TYPE uj_dim_member,
       l_bu_lineselection TYPE uj_dim_member,
       l_datasrcselection   TYPE uj_dim_member,
       l_currencyselection  TYPE uj_dim_member,
       l_faselection        TYPE  uj_dim_member,
       l_partnerselection TYPE  uj_dim_member,
       l_productselection TYPE  uj_dim_member,
       l_smartselection  TYPE  uj_dim_member,
       current_month   TYPE uj_dim_member,
       previous_month   TYPE uj_dim_member,
       cmth_im TYPE n LENGTH 2,
       cmth_iy TYPE n LENGTH 4,
       v_where TYPE string.

DATA: gdo_obsdata       TYPE REF TO data,
      gdo_fadata        TYPE REF TO data,
      lo_obsstruct      TYPE REF TO cl_abap_structdescr,
      lo_fastruct       TYPE REF TO cl_abap_structdescr,
      lt_obscomp        TYPE cl_abap_structdescr=>component_table, "=TYPE abap_component_tab.
      ref_obj_obsstruc  TYPE REF TO cl_abap_structdescr,
      ref_obj_obstable  TYPE REF TO cl_abap_tabledescr,
      gso_obshandle     TYPE REF TO data,
      gdo_obshandle TYPE REF TO data.

DATA: ls_rec                TYPE REF TO data,
      ls_rec2               TYPE REF TO data,
      ls_rec3               TYPE REF TO data,
      lt_databup            TYPE REF TO data,
      lt_dataytd            TYPE REF TO data,

      lt_hours_skf          TYPE REF TO data,
      lt_perc_skf           TYPE REF TO data,
      lt_amount_sga_by_fa   TYPE REF TO data,
      lt_amount_sga_by_fab  TYPE REF TO data,
      lt_amount_sga         TYPE REF TO data.

DATA:  l_fa_node_def   TYPE uj_dim_member,
       l_fa_top_node   TYPE uj_dim_member,
       l_versiontarget TYPE uj_dim_member,
       l_bu_linetarget TYPE uj_dim_member,
       l_datasrctarget   TYPE uj_dim_member,
       l_currencytarget  TYPE uj_dim_member,
       l_n_accounttarget  TYPE uj_dim_member,
       l_n_accounttarget2 TYPE uj_dim_member,
       l_fatarget        TYPE  uj_dim_member,
       l_partnertarget TYPE  uj_dim_member,
       l_producttarget TYPE  uj_dim_member,
       l_figurestarget TYPE  uj_dim_member,
       l_s_smarttarget  TYPE  uj_dim_member,
       l_o_smarttarget  TYPE  uj_dim_member.

DATA: lt_comp TYPE cl_abap_structdescr=>component_table, "=TYPE abap_component_tab.
      ls_comp LIKE LINE OF lt_comp.

DATA: lt_facomp             TYPE cl_abap_structdescr=>component_table, "=TYPE abap_component_tab.
      ls_facomp             LIKE LINE OF lt_comp,
      ref_obj_fastruc       TYPE REF TO cl_abap_structdescr,
      ref_obj_fatable       TYPE REF TO cl_abap_tabledescr,
      gso_fahandle TYPE REF TO data,
      gdo_fahandle TYPE REF TO data,
      lt_fa_mbr_name TYPE uja_t_dim_member,
      ls_fa_mbr_name TYPE uj_dim_member,
      lt_fa_mbr_node TYPE uja_t_mbr_node,
      ls_fa_mbr_node TYPE uja_s_mbr_node.

DATA: l_lines_f TYPE i,
      ls_dfies   TYPE dfies,
      lt_dfies   TYPE TABLE OF dfies.


FIELD-SYMBOLS:  <dist_skf_obs_n_account> TYPE ty_dist_nacc_obs,
                <gs_obsstruc>            TYPE any,
                <gs_fastruc>             TYPE any,
                <fs_obsstruct>           TYPE any,

                <ls_fa_mbr_node>         TYPE uja_s_mbr_node,

                 <ls_rec>                TYPE any,
                 <ls_rec_sga>            TYPE any,
                 <ls_data>               TYPE any,
                                  <ls_data2>               TYPE any,
                 <ls_hours_perc>         TYPE any,

                 <l_time>                TYPE any, "Dimension
                 <l_per_qtd_ytd>         TYPE any, "Dimension
                 <l_figures>             TYPE any, "Dimension
                 <l_functional_area>     TYPE any, "Dimension
                 <l_newobs>              TYPE any, "Dimension
                 <l_new_n_acc>           TYPE any, "Dimension
                 <l_obs>                 TYPE any, "Dimension
                 <l_obs_input>           TYPE any, "Dimension
                 <l_bus_area>            TYPE any, "Dimension
                 <l_obs_id>              TYPE any, "Dimension
                 <l_alloc_rule>          TYPE any,
                 <l_n_account>           TYPE any,
                 <l_measures>            TYPE any,
                 <l_currency>            TYPE any,
                 <l_datasrc>             TYPE any,
                 <l_partner>             TYPE any,
                 <l_product>             TYPE any,
                 <l_smart>               TYPE any,
                 <l_signed_data>         TYPE any,
                 <l_version>             TYPE any,
                 <l_bu_line>             TYPE any,
                 <l_parenth1>            TYPE any,
                 <l_parenth4>            TYPE any,
                 <l_parenth5>            TYPE any,
                 <l_calc>                TYPE any,
                 <fs_obsdata>            TYPE table,

                 <lt_databup>            TYPE STANDARD TABLE,
                 <lt_dataytd>            TYPE STANDARD TABLE,
                 <lt_hours_skf>          TYPE STANDARD TABLE,
                 <lt_perc_skf>           TYPE STANDARD TABLE,
                 <lt_amount_sga_by_fa>   TYPE STANDARD TABLE,
                 <lt_amount_sga_by_fab>  TYPE STANDARD TABLE,
                 <lt_amount_sga>         TYPE STANDARD TABLE,

                 <l_percentage>          TYPE any,
                 <l_perc_skf>            TYPE any,
                 <l_perc_fa>             TYPE any,
                 <l_adjusted_data>       TYPE any,
                 <l_corr_data>           TYPE any,
                 <l_signedtotal>         TYPE any,
                 <l_signeddata>          TYPE any,

                 <fs_fastruct>           TYPE any,
                 <fs_fadata>             TYPE table,
                 <lev1>                  TYPE any,
                    <lev2>                 TYPE any,
                        <lev3>                 TYPE any,
                            <lev4>                 TYPE any,
                                <lev5>                 TYPE any,
                                    <lev6>                 TYPE any,
                                         <lev7>                 TYPE any,
                                              <lev8>                 TYPE any,
                                                  <lev9>                 TYPE any.


DATA: wa_signeddata TYPE /b28/oisdata.

DATA: l_ctr             TYPE i,
      l_ctrdo           TYPE i,
      lv_maxlevel       TYPE rstlevel,
      do_loop           TYPE rstlevel,
      w_pred_fa         TYPE char4,
      moff              TYPE i,
      mlen              TYPE i,
      lt_hier_name      TYPE uja_t_hier_name,
      ls_hier_name      TYPE uj_hier_name,
      l_lines           TYPE i,
      l_lines_on        TYPE numc10,
      wa_strg           TYPE string.

DATA: v_key_denom_1     TYPE uj_dim_member,
      v_numer_1         TYPE uj_dim_member,
      v_numer_2         TYPE uj_dim_member,
      v_sub_tot_obs_41      TYPE uj_dim_member,
      v_sub_tot_obs_42      TYPE uj_dim_member,
      v_sub_tot_obs_43      TYPE uj_dim_member,
      v_sub_tot_nacc_43     TYPE uj_dim_member.

CONSTANTS: c_sep            TYPE char1 VALUE ',',
           c_sq             TYPE char1 VALUE '''',
           c_wrg_total      TYPE char6 VALUE '_TOTAL',
           c_wrg_q          TYPE char2 VALUE '_Q',
           c_sep_sp         TYPE char1 VALUE ' ',


           w_pred_obsh1        TYPE char3 VALUE 'H4%',
           w_pred_obsh2        TYPE char3 VALUE 'H5%',
           c_input_suf         TYPE char2 VALUE '.I',
           c_prefix_41         TYPE char2 VALUE '41',
           c_prefix_42         TYPE char2 VALUE '42',
           c_prefix_43         TYPE char2 VALUE '43',
           c_prefix_h4         TYPE char2 VALUE 'H4',
           c_zz                TYPE uj_dim_member VALUE 'ZZZZZZ',
           c_99                TYPE uj_dim_member VALUE '999999',
           c_43_4301           TYPE uj_dim_member VALUE '43_4301',
           c_selector_hours_n1 TYPE uj_dim_member VALUE 'HOURS_N1',
           c_selector_hours_n2 TYPE uj_dim_member VALUE 'HOURS_N2',
           c_selector_n2       TYPE uj_dim_member VALUE 'N2',
           c_selector_hours    TYPE uj_dim_member VALUE 'HOURS',
           c_selector_woff     TYPE uj_dim_member VALUE 'WRITEOFF',
           c_selector_diff     TYPE uj_dim_member VALUE 'DIFFERENCE',
           c_selector_perc     TYPE uj_dim_member VALUE 'PERCENTAGE',
           c_selector_amt      TYPE uj_dim_member VALUE 'AMOUNT',
           c_periodic          TYPE uj_dim_member VALUE 'PERIODIC',
           c_added_0           TYPE uj_dim_member VALUE 'HOURS_ADDED_0',
           c_added_1           TYPE uj_dim_member VALUE 'HOURS_ADDED',
           c_numerator_1       TYPE uj_dim_member VALUE 'HOURS_N1',
           c_denominator_1     TYPE uj_dim_member VALUE 'HOURS_D1',
           c_numerator_2       TYPE uj_dim_member VALUE 'HOURS_N2'. "qualify figures for OBS H4_43_4306

DEFINE mc_selection_dim.
CLEAR &1.
&1-sign = 'I'.
&1-option = 'EQ'.
&1-low = &2.
&1-dimension = &3.
APPEND &1 TO &4.
END-OF-DEFINITION.

* START

    IF sy-uname = 'BB5827'.
      DO. IF t_debug_flag_start = ' '. EXIT. ENDIF.
      ENDDO.
      BREAK bb5827.
    ENDIF.
*&---------------------------------------------------------------------*
*& * Get user-context metadata
*&---------------------------------------------------------------------*
  lo_old_context = cl_uj_context=>get_cur_context( ).
  CALL METHOD lo_old_context->get_user_obj
    RECEIVING
      ro_user = lo_user.
  l_user = lo_user->d_obj_id.
  ls_user-user_id = l_user.
  ls_user-langu = sy-langu.

  me->process_parameters( EXPORTING it_param = it_param
                                    it_cv = it_cv
                                    i_user = l_user
                          IMPORTING et_lgx = lt_th_lgx
                                    e_chng_list = lt_th_dim
                                    e_docname = l_docname
                                    e_subrc = l_subrc ).
IF l_subrc = 0.
*& DIMSET MEMBERS FROM SCRIPT AND/OR DATA MANAGER PROMPTS(VARIABLES $)
*&---------------------------------------------------------------------*
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_GB41_INPUT' INTO wa_param_obs_gb41_input.
SPLIT wa_param_obs_gb41_input-hashvalue AT c_sep INTO TABLE lt_split_obs_gb41_input.

READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_AE42_INPUT' INTO wa_param_obs_ae42_input.
SPLIT wa_param_obs_ae42_input-hashvalue AT c_sep INTO TABLE lt_split_obs_ae42_input.

READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_SG43_INPUT_1' INTO wa_param_obs_sg43_input_1.
SPLIT wa_param_obs_sg43_input_1-hashvalue AT c_sep INTO TABLE lt_split_obs_sg43_input_1.


READ TABLE it_param WITH TABLE KEY hashkey = 'BASEALLOC_2' INTO wa_param_fa_basealloc_2.
ls_fa_basealloc_2 = wa_param_fa_basealloc_2-hashvalue.

READ TABLE it_param WITH TABLE KEY hashkey = 'BASEALLOC' INTO wa_param_figures_basealloc.
ls_figures_basealloc = wa_param_figures_basealloc-hashvalue.
* OBS_SG43_INPUT_1 = OBS_SG43_ADJUST_I ?
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_SG43_ADJUST_I' INTO wa_param_adjust_sg43.
ls_adjust_sg43 = wa_param_adjust_sg43-hashvalue.

READ TABLE it_param WITH TABLE KEY hashkey = 'TOP_FA_NODE'           INTO wa_param_fa_top_node.
l_fa_top_node = wa_param_fa_top_node-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'FA_NODE_DEFINES_SMART' INTO wa_param_fa_node_defines_smart. "eg.FS000
l_fa_node_def = wa_param_fa_node_defines_smart-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'O_SMART'               INTO wa_param_o_smart.
l_o_smarttarget = wa_param_o_smart-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'S_SMART'               INTO wa_param_s_smart.
l_s_smarttarget = wa_param_s_smart-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'BU_LINE_T'             INTO wa_param_bu_line_t.
l_bu_linetarget = wa_param_bu_line_t-hashvalue..
READ TABLE it_param WITH TABLE KEY hashkey = 'DATASRC_T'             INTO wa_param_datasrc_t.
l_datasrctarget = wa_param_datasrc_t-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'N_ACCOUNT_T'           INTO wa_param_n_account_t.
l_n_accounttarget = wa_param_n_account_t-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'N_ACCOUNT_T2'          INTO wa_param_n_account_t2.
l_n_accounttarget2 = wa_param_n_account_t2-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'PARTNER_T'             INTO wa_param_partner_t.
l_partnertarget = wa_param_partner_t-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'PRODUCT_T'             INTO wa_param_product_t.
l_producttarget = wa_param_product_t-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'VERSION_T'         INTO wa_param_version_t.
l_versiontarget = wa_param_version_t-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'FIGURES_T'         INTO wa_param_figures_t.
l_figurestarget = wa_param_figures_t-hashvalue.
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_GB41_WHAT'   INTO wa_param_obs_gb41_what.
READ TABLE it_param WITH TABLE KEY hashkey = 'NODE_GB41_D'     INTO wa_param_node_gb41_d.
READ TABLE it_param WITH TABLE KEY hashkey = 'NODE_AE42_D'     INTO wa_param_node_ae42_d.

READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_AE42_WHAT'   INTO wa_param_obs_ae42_what.
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_SG43_WHAT_1' INTO wa_param_obs_sg43_what_1.
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_SG43_WHAT_2' INTO wa_param_obs_sg43_what_2.

READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_SG43_ADDHIE_I' INTO wa_param_obs_sg43_addhie_i.
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_SG43_ADJUST_I' INTO wa_param_obs_sg43_adjust_i.
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_SG43_ADJUST_I2' INTO wa_param_obs_sg43_adjust_i2.
READ TABLE it_param WITH TABLE KEY hashkey = 'NODE_SG43_N_1'     INTO wa_param_node_sg43_n_1.
READ TABLE it_param WITH TABLE KEY hashkey = 'NODE_SG43_N_2'     INTO wa_param_node_sg43_n_2.
READ TABLE it_param WITH TABLE KEY hashkey = 'NODE_SG43_D_1'     INTO wa_param_node_sg43_d_1.

*v_key_denom_1 H4_43_430X_TO_430Y_&_H4_43_4301
*CONCATENATE wa_param_node_sg43_d_1-hashvalue '' INTO v_key_denom_1 SEPARATED BY '_'.
CONCATENATE wa_param_node_sg43_d_1-hashvalue wa_param_obs_sg43_what_1-hashvalue INTO v_key_denom_1 SEPARATED BY '_&_'.
*H4_4302_4305_&_H4_4306
CONCATENATE wa_param_node_sg43_n_1-hashvalue wa_param_node_sg43_n_2-hashvalue INTO v_numer_1 SEPARATED BY '_&_'.
*H4_4302_4305_+_43.S090
CONCATENATE wa_param_node_sg43_n_1-hashvalue wa_param_obs_sg43_adjust_i-hashvalue INTO v_numer_2 SEPARATED BY '_&_'.

*&---------------------------------------------------------------------*
*& GET INPUT MEMBERS FROM DATA MANAGER PROMPTS (VARIABLES %)
*&---------------------------------------------------------------------*
CLEAR: lt_selection_dim, lt_timeselection.
LOOP AT it_cv INTO lx_cv WHERE user_specified = 'X'.
lt_member = lx_cv-member.
LOOP AT lt_member INTO lx_member.
mc_selection_dim lx_selection_dim
lx_member lx_cv-dimension lt_selection_dim.
IF lx_cv-dimension = 'TIME'.
* avoid 20xx_Qx ex. 202_Q1 or avoid 20xx_TOTAL
FIND c_wrg_total IN lx_member.
CHECK sy-subrc <> 0.
FIND c_wrg_q IN lx_member.
IF sy-subrc <> 0.
ls_timeselection = lx_member.
APPEND ls_timeselection TO lt_timeselection.
ENDIF.
ELSEIF lx_cv-dimension = 'VERSION'.
l_versionselection = lx_member.
ELSEIF lx_cv-dimension = 'BU_LINE'.
l_bu_lineselection = lx_member.
ELSEIF lx_cv-dimension = 'DATASRC'.
l_datasrcselection = lx_member.
ELSEIF lx_cv-dimension = 'CURRENCY'.
l_currencyselection = lx_member.
ELSEIF lx_cv-dimension = 'PARTNER'.
l_partnerselection = lx_member.
ELSEIF lx_cv-dimension = 'PRODUCT'.
l_productselection = lx_member.
ELSEIF lx_cv-dimension = 'SMART'.
l_smartselection = lx_member.


ENDIF.

ENDLOOP.
ENDLOOP.

*&---------------------------------------------------------------------*
*& * Get the table name/BPC master_data for OBS
*&---------------------------------------------------------------------*
SELECT appset_id, dimension, tech_name, data_table
  INTO TABLE @DATA(obs_info)
  FROM uja_dimension
  WHERE appset_id = @i_appset_id AND dimension = 'OBS'.

DATA(wa_obs_info) = obs_info[ 1 ].
* create dynamically internal table to store mdata
CREATE DATA gdo_obsdata TYPE (wa_obs_info-data_table).
ASSIGN gdo_obsdata->* TO <gs_obsstruc>.
CHECK ( <gs_obsstruc> IS ASSIGNED ).

lo_obsstruct ?= cl_abap_typedescr=>describe_by_data( <gs_obsstruc> ).
lt_obscomp = lo_obsstruct->get_components( ).
ref_obj_obsstruc = cl_abap_structdescr=>get( p_components = lt_obscomp p_strict = ' ' ).
ref_obj_obstable ?= cl_abap_tabledescr=>create( ref_obj_obsstruc ).
CREATE DATA gso_obshandle TYPE HANDLE ref_obj_obsstruc.
ASSIGN gso_obshandle->* TO <fs_obsstruct>.

CREATE DATA gdo_obshandle TYPE HANDLE ref_obj_obstable.
ASSIGN gdo_obshandle->* TO <fs_obsdata>.

*&---------------------------------------------------------------------*
*& * Get RED master data for OBS by Company Code: ! maintenance required!
*&---------------------------------------------------------------------*

SELECT * FROM (wa_obs_info-data_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_obsdata>
WHERE ( /cpmb/ijp87ue = 'GB41' OR /cpmb/ijp87ue = 'AE42' OR /cpmb/ijp87ue = 'SG43' ).

* get the table name for FUNCTIONAL_AREA BPC master_data
SELECT appset_id, dimension, tech_name, data_table
  INTO TABLE @DATA(fa_info)
  FROM uja_dimension
  WHERE appset_id = @i_appset_id AND dimension = 'FUNCTIONAL_AREA'.

DATA(wa_fa_info) = fa_info[ 1 ].

CREATE DATA gdo_fadata TYPE (wa_fa_info-data_table).
ASSIGN gdo_fadata->* TO <gs_fastruc>.
CHECK ( <gs_fastruc> IS ASSIGNED ).

lo_fastruct ?= cl_abap_typedescr=>describe_by_data( <gs_fastruc> ).
lt_facomp = lo_fastruct->get_components( ).

CLEAR: l_lines_f.
REFRESH: lt_dfies.

DESCRIBE TABLE lt_facomp LINES l_lines_f.
ADD: 1 TO l_lines_f.

* we use any table that contains field with data type uj_member
CALL FUNCTION 'DDIF_FIELDINFO_GET'
  EXPORTING
    tabname              = 'UJA_USER_CV'
    fieldname            = 'MEMBER'
    langu                = sy-langu
 TABLES
   dfies_tab            = lt_dfies
*   FIXED_VALUES         =
 EXCEPTIONS
   not_found            = 1
   internal_error       = 2
   OTHERS               = 3.

READ TABLE lt_dfies INDEX 1 INTO ls_dfies.

IF sy-subrc <> 0.

ELSE.
 ls_facomp-name = 'PARENT_LV9'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_facomp-name = 'PARENT_LV8'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_facomp-name = 'PARENT_LV7'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_facomp-name = 'PARENT_LV6'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

  ls_facomp-name = 'PARENT_LV5'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
  ADD 1 TO l_lines_f.

 ls_facomp-name = 'PARENT_LV4'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_facomp-name = 'PARENT_LV3'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_facomp-name = 'PARENT_LV2'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_facomp-name = 'PARENT_LV1'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.
ENDIF.

ref_obj_fastruc = cl_abap_structdescr=>get( p_components = lt_facomp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_obj_fatable ?= cl_abap_tabledescr=>create( ref_obj_fastruc ).

CREATE DATA gso_fahandle TYPE HANDLE ref_obj_fastruc.
ASSIGN gso_fahandle->* TO <fs_fastruct>.

CREATE DATA gdo_fahandle TYPE HANDLE ref_obj_fatable.
ASSIGN gdo_fahandle->* TO <fs_fadata>.

*Functional_area hierarchy thru fa top node as a parameter in Logic Script
TRY.
CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'FUNCTIONAL_AREA'
    i_parent_mbr = l_fa_top_node
    i_type       = 'ALL'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_fa_mbr_name
    et_mbr_node  = lt_fa_mbr_node
    .
 CATCH cx_uj_static_check .
ENDTRY.

* flattening FUNCTIONAL_AREA hierarchy
LOOP AT lt_fa_mbr_node INTO ls_fa_mbr_node.
CLEAR: gst_fahier, l_ctrdo.
MOVE-CORRESPONDING ls_fa_mbr_node TO gst_fahier.
ADD 1 TO l_ctr.
gst_fahier-seqnr = l_ctr.

IF ls_fa_mbr_node-tlevel > 2.
lv_maxlevel = ls_fa_mbr_node-tlevel.
SUBTRACT 2 FROM lv_maxlevel.
do_loop = lv_maxlevel.
CHECK do_loop GT 0.
DO do_loop TIMES.
  ADD 1 TO l_ctrdo.
READ TABLE lt_fa_mbr_node ASSIGNING <ls_fa_mbr_node>
WITH KEY member = ls_fa_mbr_node-parent.
IF sy-subrc = 0.
CASE l_ctrdo.
WHEN '1'.
gst_fahier-parent_lv1 = <ls_fa_mbr_node>-parent.
WHEN '2'.
gst_fahier-parent_lv2 = <ls_fa_mbr_node>-parent.
WHEN '3'.
gst_fahier-parent_lv3 = <ls_fa_mbr_node>-parent.
WHEN '4'.
gst_fahier-parent_lv4 = <ls_fa_mbr_node>-parent.
WHEN '5'.
gst_fahier-parent_lv5 = <ls_fa_mbr_node>-parent.
WHEN '6'.
gst_fahier-parent_lv6 = <ls_fa_mbr_node>-parent.
WHEN '7'.
gst_fahier-parent_lv7 = <ls_fa_mbr_node>-parent.
WHEN '8'.
gst_fahier-parent_lv8 = <ls_fa_mbr_node>-parent.
WHEN '9'.
gst_fahier-parent_lv9 = <ls_fa_mbr_node>-parent.
ENDCASE.
ENDIF.
ls_fa_mbr_node-parent = <ls_fa_mbr_node>-parent.
ENDDO.
ENDIF.
APPEND gst_fahier TO gtt_fahier.
ENDLOOP.

* get the H name for FA hierarchy
* predicate LIKE '%H4%' LIKE '%H7%' should not be hardcoded
CLEAR: ls_hier_name, w_pred_fa, moff, mlen.
READ TABLE gtt_fahier INTO gst_fahier INDEX 1.
FIND REGEX 'H.' IN gst_fahier-hier_name MATCH OFFSET moff MATCH LENGTH mlen.
CONCATENATE '%' gst_fahier-hier_name+moff(mlen) '%' INTO w_pred_fa.

SELECT * FROM (wa_fa_info-data_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_fadata>
WHERE /cpmb/hir LIKE w_pred_fa. "'%H7%'

* Mixing Master data and Flattened Hierarchy
*FUNCTIONAL_AREA
LOOP AT <fs_fadata> ASSIGNING <fs_fastruct>.
ASSIGN COMPONENT:
'/CPMB/IJDO575' OF STRUCTURE <fs_fastruct> TO <l_functional_area>,
'PARENT_LV1'    OF STRUCTURE <fs_fastruct> TO <lev1>,
'PARENT_LV2'    OF STRUCTURE <fs_fastruct> TO <lev2>,
'PARENT_LV3'    OF STRUCTURE <fs_fastruct> TO <lev3>,
'PARENT_LV4'    OF STRUCTURE <fs_fastruct> TO <lev4>,
'PARENT_LV5'    OF STRUCTURE <fs_fastruct> TO <lev5>,
'PARENT_LV6'    OF STRUCTURE <fs_fastruct> TO <lev6>,
'PARENT_LV7'    OF STRUCTURE <fs_fastruct> TO <lev7>,
'PARENT_LV8'    OF STRUCTURE <fs_fastruct> TO <lev8>,
'PARENT_LV9'    OF STRUCTURE <fs_fastruct> TO <lev9>.
READ TABLE gtt_fahier INTO gst_fahier WITH TABLE KEY ('MEMBER') = <l_functional_area>.
IF sy-subrc = 0.
  <lev1> = gst_fahier-parent.
  <lev2> = gst_fahier-parent_lv1.
  <lev3> = gst_fahier-parent_lv2.
  <lev4> = gst_fahier-parent_lv3.
  <lev5> = gst_fahier-parent_lv4.
  <lev6> = gst_fahier-parent_lv5.
  <lev7> = gst_fahier-parent_lv6.
  <lev8> = gst_fahier-parent_lv7.
  <lev9> = gst_fahier-parent_lv8.
ENDIF.
ENDLOOP.

*&---------------------------------------------------------------------*
*& GET current month as the max selected and derive previous_month
*& XXXX_TOTAL could have been used but YTD has to be flexible
*&---------------------------------------------------------------------*
CLEAR: current_month, previous_month,cmth_iy,cmth_im.
SORT lt_timeselection DESCENDING.
READ TABLE lt_timeselection INDEX 1 INTO current_month.
cmth_iy = current_month(4).
cmth_im = current_month+5(2).
IF cmth_im = 1.
SUBTRACT: 1 FROM cmth_iy. MOVE 12 TO cmth_im.
CONCATENATE cmth_iy '_' cmth_im INTO previous_month.
*  previous_month = current_month.
ELSE.
  SUBTRACT: 1 FROM cmth_im, 0 FROM cmth_iy.
CONCATENATE cmth_iy '_' cmth_im INTO previous_month.
ENDIF.

*&---------------------------------------------------------------------*
*& Substitute Query DATASET with YTD Values month defined by DM TimeSet
*&---------------------------------------------------------------------*
*Creating the incoming data Line structure ( requires QUERY = ON )
CREATE DATA ls_rec LIKE LINE OF ct_data.
ASSIGN ls_rec->* TO <ls_data>.
CREATE DATA ls_rec2 LIKE LINE OF ct_data.
ASSIGN ls_rec2->* TO <ls_rec>.
CREATE DATA ls_rec3 LIKE LINE OF ct_data.
ASSIGN ls_rec3->* TO <ls_rec_sga>.
*Creating internal table based on Line structure
CREATE DATA lt_databup LIKE ct_data.
ASSIGN lt_databup->* TO <lt_databup>.
*Creating internal table based on Line structure
CREATE DATA lt_dataytd LIKE ct_data.
ASSIGN lt_dataytd->* TO <lt_dataytd>.
*Creating internal table based on Line structure
CREATE DATA lt_hours_skf LIKE ct_data.
ASSIGN lt_hours_skf->* TO <lt_hours_skf>.
*Creating internal table based on Line structure
CREATE DATA lt_amount_sga_by_fa LIKE ct_data.
ASSIGN lt_amount_sga_by_fa->* TO <lt_amount_sga_by_fa>.
*Creating internal table based on Line structure
CREATE DATA lt_amount_sga LIKE ct_data.
ASSIGN lt_amount_sga->* TO <lt_amount_sga>.
*Creating internal table based on Line structure
CREATE DATA lt_perc_skf LIKE ct_data.
ASSIGN lt_perc_skf->* TO <lt_perc_skf>.
*Creating internal table based on Line structure
CREATE DATA lt_amount_sga_by_fab LIKE ct_data.
ASSIGN lt_amount_sga_by_fab->* TO <lt_amount_sga_by_fab>.

* Group by ct_data on current month: using MEASURES
ASSIGN COMPONENT:
'N_ACCOUNT'  OF STRUCTURE <ls_data> TO <l_n_account>,
'OBS'        OF STRUCTURE <ls_data> TO <l_obs>,
'TIME'       OF STRUCTURE <ls_data> TO <l_time>,
'MEASURES'   OF STRUCTURE <ls_data> TO <l_per_qtd_ytd>.

LOOP AT ct_data INTO <ls_data>.
  <l_time> = current_month.
  <l_per_qtd_ytd> = 'YTD'.
  COLLECT <ls_data> INTO <lt_dataytd>.
  IF <l_n_account>(4) = 'SKF_'.
  ls_dist_skf_n_account-n_account = <l_n_account>.
  ls_dist_skf_n_account-ctr = 1.
  COLLECT ls_dist_skf_n_account INTO lt_dist_skf_n_account.
  ls_dist_skf_obs_n_account-obs = <l_obs>.
  ls_dist_skf_obs_n_account-n_account = <l_n_account>.
    ASSIGN COMPONENT:
        '/CPMB/IJP6PPJ' OF STRUCTURE <fs_obsstruct> TO <l_bus_area>,
        '/CPMB/CALC'    OF STRUCTURE <fs_obsstruct> TO <l_calc>,
        'PARENTH1'      OF STRUCTURE <fs_obsstruct> TO <l_parenth1>,
        'PARENTH4'      OF STRUCTURE <fs_obsstruct> TO <l_parenth4>,
        'PARENTH5'      OF STRUCTURE <fs_obsstruct> TO <l_parenth5>.
    READ TABLE  <fs_obsdata> INTO <fs_obsstruct> WITH KEY
        ('/CPMB/IJDP3S6') = <l_obs>.
        IF sy-subrc = 0.
        ls_dist_skf_obs_n_account-bus_area = <l_bus_area>.
        ls_dist_skf_obs_n_account-calc     = <l_calc>.
        ls_dist_skf_obs_n_account-parenth1 = <l_parenth1>.
        ls_dist_skf_obs_n_account-parenth4 = <l_parenth4>.
        ls_dist_skf_obs_n_account-parenth5 = <l_parenth5>.
          ENDIF.
  ls_dist_skf_obs_n_account-ctr = 1.

  COLLECT ls_dist_skf_obs_n_account INTO lt_dist_skf_obs_n_account.
  ENDIF.
ENDLOOP.
UNASSIGN: <l_n_account>,<l_obs>,<l_time>,<l_per_qtd_ytd>,<l_bus_area>,<l_calc>,<l_parenth1>,<l_parenth4>,<l_parenth5>.
*&---------------------------------------------------------------------*
*& Cleanse DATASET with HOURS figures on N2 n_account tuple not required
*& Except for 43.S090 stored in VAR wa_param_obs_sg43_adjust_i-hashvalue
*&---------------------------------------------------------------------*
CLEAR wa_strg.
CONCATENATE 'FIGURES EQ ''' c_selector_hours  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'AND N_ACCOUNT EQ ''' c_selector_n2  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'AND OBS NE ''' wa_param_obs_sg43_adjust_i-hashvalue  '''' INTO wa_strg.
DELETE <lt_dataytd> WHERE (wa_strg).

UNASSIGN:<l_bu_line>,<l_currency>,<l_datasrc>,<l_figures>,<l_functional_area>,<l_n_account>,<l_obs>,<l_partner>,
         <l_product>,<l_smart>,<l_time>,<l_version>,<l_per_qtd_ytd>,<l_signed_data>.

REFRESH: ct_data.
ct_data[] = <lt_dataytd>[].

* using version to store PARENTH4 / bu_line to store PARENTH5

REFRESH: <lt_hours_skf>, <lt_amount_sga_by_fa>, <lt_amount_sga>.

CLEAR: <ls_data>, <fs_obsstruct>.
* back_up point CT_DATA or <lt_dataytd>
BREAK bb5827.

LOOP AT ct_data INTO <ls_data>.
ASSIGN COMPONENT:
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'MEASURES'          OF STRUCTURE <ls_data> TO <l_measures>,
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'FUNCTIONAL_AREA'   OF STRUCTURE <ls_data> TO <l_functional_area>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_version>,
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_bu_line>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
* get the hours for SKF in driver table <lt_hours_skf> , the amount into another <lt_amount_sga>

CASE <l_figures>.
WHEN 'HOURS'.

 CASE <l_n_account>(6).

 WHEN 'N2'. "on  43.S090 - collapse on 43.S090 / H4_43_4306
         CHECK <l_functional_area> <> ls_fa_basealloc_2. "<>SGA999
         CLEAR: orig_obs, orig_n_account.
          orig_obs =       <l_obs>.
          orig_n_account = <l_n_account>.
         <l_version>     = wa_param_node_sg43_n_2-hashvalue.         "H4_43_4306
         <l_bu_line>     = l_bu_linetarget. "B101 ( exists B102 for 43.S090)
         <l_partner>     = orig_obs.
         <l_obs>         = wa_param_node_sg43_n_2-hashvalue+3(7).
         CONCATENATE 'SKF_' wa_param_node_sg43_n_2-hashvalue+6(4) INTO <l_n_account>.
         COLLECT <ls_data> INTO <lt_hours_skf>.

 WHEN 'SKF_41'.
      CHECK <l_n_account> <> 'N2' AND <l_functional_area> = ls_fa_basealloc_2.
* OBS_GB41_WHAT hours excluded   ? ie. 41_4101 (Support) YES with check
    CHECK  <l_obs> <> wa_param_obs_gb41_what-hashvalue.
    READ TABLE  <fs_obsdata> INTO <fs_obsstruct> WITH KEY
   ('/CPMB/IJP6PPJ') = <l_n_account>+4(4)
   ('/CPMB/IJPCTQT') = 'HOURS_N1'
   ('/CPMB/CALC') = 'Y'.
        IF sy-subrc = 0 AND NOT <l_parenth4> IS ASSIGNED.
         ASSIGN COMPONENT: 'PARENTH4'      OF STRUCTURE <fs_obsstruct> TO <l_parenth4>.
         <l_version> = <l_parenth4>.
         UNASSIGN: <l_parenth4>.
        ENDIF.
         <l_bu_line> = wa_param_obs_gb41_what-hashvalue.
*         <l_obs> = <l_obs_id>.

         CONCATENATE <l_obs>(2) '_' <l_n_account>+4(2) 'XX' INTO <l_version>.
         CONCATENATE <l_obs>(2) <l_n_account>+4(4) INTO <l_obs> SEPARATED BY '_'.
         COLLECT <ls_data> INTO <lt_hours_skf>.

         CONCATENATE <l_n_account>(6) 'XX' INTO <l_n_account>.
         CONCATENATE <l_n_account>+4(2) <l_n_account>+4(2) INTO <l_obs> SEPARATED BY '_'.
         v_sub_tot_obs_41 = <l_obs>.
         COLLECT <ls_data> INTO <lt_hours_skf>.

 WHEN 'SKF_42'.

      CHECK <l_n_account> <> 'N2' AND <l_functional_area> = ls_fa_basealloc_2.
* OBS_AE42_WHAT hours excluded   ? ie. 42_4201 (Support) YES with check
*    CHECK  <l_obs> <> wa_param_obs_ae42_what-hashvalue. " only OBS 42_4201 but SKF_4202,3..
     READ TABLE  <fs_obsdata> INTO <fs_obsstruct> WITH KEY
   ('/CPMB/IJP6PPJ') = <l_n_account>+4(4)
   ('/CPMB/IJPCTQT') = 'HOURS_N1'
   ('/CPMB/CALC') = 'Y'.
        IF sy-subrc = 0 AND NOT <l_parenth4> IS ASSIGNED.
         ASSIGN COMPONENT: 'PARENTH4'      OF STRUCTURE <fs_obsstruct> TO <l_parenth4>.
         <l_version> = <l_parenth4>.
         UNASSIGN: <l_parenth4>.
        ENDIF.
         <l_bu_line> = wa_param_obs_ae42_what-hashvalue.
*         <l_obs> = <l_obs_id>.

         CONCATENATE <l_obs>(2) '_' <l_n_account>+4(2) 'XX' INTO <l_version>.
         CONCATENATE <l_obs>(2) <l_n_account>+4(4) INTO <l_obs> SEPARATED BY '_'.
         COLLECT <ls_data> INTO <lt_hours_skf>.

         CONCATENATE <l_n_account>(6) 'XX' INTO <l_n_account>.
* sub_total obs example SKF_4302, obs = 43_43
         CONCATENATE <l_n_account>+4(2) <l_n_account>+4(2) INTO <l_obs> SEPARATED BY '_'.
         v_sub_tot_obs_42 = <l_obs>.
         COLLECT <ls_data> INTO <lt_hours_skf>.

 WHEN 'SKF_43'.

    ls_dist_skf_n_account-n_account = <l_n_account>.
    ls_dist_skf_n_account-ctr       = 0.
    COLLECT ls_dist_skf_n_account INTO lt_dist_skf_n_account.
* 43.S090 43.S060 not used in HOURS
    CHECK  <l_obs> <> wa_param_obs_sg43_adjust_i-hashvalue AND <l_obs> <> wa_param_obs_sg43_adjust_i2-hashvalue.
CLEAR: orig_obs, orig_n_account.
          orig_obs =       <l_obs>.
          orig_n_account = <l_n_account>.
          IF <l_obs> NE  wa_param_obs_sg43_what_1-hashvalue.
         <l_bu_line> = wa_param_obs_sg43_what_1-hashvalue.
          ENDIF.
         CASE orig_obs.

          WHEN wa_param_node_sg43_n_1-hashvalue.            "H4_4302_4305  incl in H4_43_430X_TO_430Y
          <l_version> = wa_param_node_sg43_n_1-hashvalue.   "H4_4302_4305
          WHEN wa_param_obs_sg43_addhie_i-hashvalue.        "H5_43_GOV
          <l_version> = wa_param_node_sg43_n_1-hashvalue.   "H4_4302_4305

          WHEN wa_param_obs_sg43_what_1-hashvalue.          "H4_43_4301
          IF <l_n_account> <> 'SKF_4306'.
          <l_version> = wa_param_node_sg43_n_1-hashvalue.   "H4_4302_4305
          ELSE.
          <l_version> = wa_param_node_sg43_n_2-hashvalue.   "H4_43_4306
          ENDIF.

          WHEN wa_param_node_sg43_d_1-hashvalue.            "H4_43_430X_TO_430Y
          <l_version> = wa_param_node_sg43_n_2-hashvalue.   "H4_43_4306

          WHEN wa_param_obs_sg43_adjust_i-hashvalue.         "43.S090
          <l_version> = wa_param_obs_sg43_adjust_i-hashvalue."43.S090

          WHEN wa_param_node_sg43_n_2-hashvalue.             "H4_43_4306
* FYI : only SKF_43XX should be tagged as VERSION = H4_43_4306 - done in next loop
          IF <l_n_account> = 'SKF_43XX'.
          <l_version> = wa_param_node_sg43_n_2-hashvalue.    "H4_43_4306
          ENDIF.
* v_key_denom_1 defined in line 472
          WHEN OTHERS.
          ENDCASE.

* transform SKF_43.. into OBS ex. SKF_4302 = 43_4302
         CONCATENATE <l_n_account>+4(2) <l_n_account>+4(4) INTO <l_obs> SEPARATED BY '_'.
         <l_partner> = orig_obs.
         COLLECT <ls_data> INTO <lt_hours_skf>.
* sub_total obs example SKF_4302, obs = 43_43
         CONCATENATE <l_n_account>+4(2) <l_n_account>+4(2) INTO <l_obs> SEPARATED BY '_'.
*keep sub_total 43 OBS member id
         v_sub_tot_obs_43 = <l_obs>.
         CONCATENATE <l_n_account>(6) 'XX' INTO <l_n_account>.
         v_sub_tot_nacc_43 = <l_n_account>.
         IF orig_obs = wa_param_node_sg43_n_2-hashvalue. " zeroify H4_43_4306, will be replaced by next loop collect
         <l_signeddata> = 0.
         ENDIF.
         COLLECT <ls_data> INTO <lt_hours_skf>.

         WHEN OTHERS.   "<l_n_account>(6) not SKF... or N2
         ENDCASE.

WHEN 'AMOUNT'.
    CHECK <l_n_account> = 'N2'.

* if we wanted to limit dataset to any obs 41
*    FIND REGEX '41' IN <l_obs>.    IF sy-subrc = 0. ENDIF.
* we collect the parent OBS and store it in VERSION
     READ TABLE  <fs_obsdata> INTO <fs_obsstruct> WITH KEY
   ('/CPMB/IJDP3S6') = <l_obs>.
        IF sy-subrc = 0 AND NOT <l_parenth4> IS ASSIGNED.
         ASSIGN COMPONENT: 'PARENTH4'      OF STRUCTURE <fs_obsstruct> TO <l_parenth4>.
         <l_version> = <l_parenth4>.
         UNASSIGN: <l_parenth4>.
        ENDIF.
* we collect input OBS and store it in BU_LINE
     READ TABLE  <fs_obsdata> INTO <fs_obsstruct> WITH KEY
     ('/CPMB/IJPCTQT') = <l_obs>
     ('/CPMB/CALC')    = 'N'.
      IF sy-subrc = 0.
      IF NOT <l_obs_id> IS ASSIGNED. ASSIGN COMPONENT: '/CPMB/IJDP3S6' OF STRUCTURE <fs_obsstruct> TO <l_obs_id>. ENDIF.
       <l_bu_line> = <l_obs_id>.
       UNASSIGN: <l_obs_id>.
      ELSE.
* per convention .I suffix if OBS = 41_4102 then 41_4102.I except for WHAT
        CASE <l_obs>.
        WHEN '41_4101'.   "41_4101
        <l_bu_line> = '41_4101.I'.
         WHEN '42_4201'.   "42_4201
        <l_bu_line> = '42_4201.I'.

        WHEN wa_param_obs_sg43_what_1-hashvalue OR c_43_4301.  "H4_43_4301 or 43_4301
         <l_bu_line> = '43_4301.I'.
        WHEN wa_param_obs_sg43_adjust_i-hashvalue.  "43.S090
         <l_bu_line> = '43_4301.I'.
        WHEN wa_param_obs_sg43_adjust_i2-hashvalue. "43.S060
         <l_bu_line> = '43_4301.I'.
        WHEN wa_param_obs_sg43_addhie_i-hashvalue. "H5_43_GOV (sum of 43.S090 and 43.S060
         <l_bu_line> = '43_4301.I'.

        WHEN OTHERS.
         CONCATENATE <l_obs> '.I' INTO <l_bu_line>.
        ENDCASE.
      ENDIF.


      APPEND <ls_data> TO <lt_amount_sga_by_fa>.
* do the next at validation OBS input member (stored in BU_LINE) step
*      <l_functional_area> = ls_fa_basealloc_2.
*      COLLECT <ls_data> INTO <lt_amount_sga>.

WHEN OTHERS.
ENDCASE.
ENDLOOP.

* validate content of AMOUNT internal tables
LOOP AT <lt_amount_sga_by_fa> INTO <ls_data>.

ASSIGN COMPONENT:
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_bu_line>,
'FUNCTIONAL_AREA'   OF STRUCTURE <ls_data> TO <l_functional_area>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>.
IF <l_bu_line> <> '41.4101' AND <l_bu_line> <> '42.4201'.
     READ TABLE  <fs_obsdata> INTO <fs_obsstruct> WITH KEY
     ('/CPMB/IJDP3S6') = <l_bu_line>
     ('/CPMB/CALC')    = 'N'.
      IF sy-subrc = 0.
* BAS(H4_43_4306) = BAS (43_4306)
      IF <l_obs> = wa_param_node_sg43_n_2-hashvalue. <l_obs> = <l_obs>+3(7).ENDIF. "H4_43_4306 > 43_4306
      APPEND <ls_data> TO <lt_amount_sga_by_fab>.
      <l_functional_area> = ls_fa_basealloc_2.
      COLLECT <ls_data> INTO <lt_amount_sga>.
      ENDIF.
ELSE.
      APPEND <ls_data> TO <lt_amount_sga_by_fab>.
      <l_functional_area> = ls_fa_basealloc_2.
      COLLECT <ls_data> INTO <lt_amount_sga>.
ENDIF.
ENDLOOP.
* back_up AMOUNT tables <lt_amount_sga> and <lt_amount_sga_by_fab>
BREAK bb5827.
* sync internal table pointers

REFRESH:<lt_amount_sga_by_fa>.
<lt_amount_sga_by_fa> = <lt_amount_sga_by_fab>.
*REFRESH:<lt_amount_sga_by_fab>.
* keep only required percentages
CLEAR: wa_strg.
CONCATENATE 'VERSION(2) EQ ''' c_prefix_h4  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'AND OBS(2) NE ''' c_prefix_43  '''' INTO wa_strg.
DELETE <lt_hours_skf> WHERE (wa_strg).
* sync hours internal table without any pointers (shorter than un-assign pointers)
REFRESH: <lt_perc_skf>.
<lt_perc_skf> = <lt_hours_skf>.
REFRESH: <lt_hours_skf>.
<lt_hours_skf> = <lt_perc_skf>.
REFRESH: <lt_perc_skf>.
* 41,42: relative percentage + 43: compute cumulative percentage ratio
LOOP AT <lt_hours_skf> ASSIGNING <ls_data> WHERE ('BU_LINE(2) <>  c_prefix_43').
ASSIGN COMPONENT:
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_parenth4>,
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_parenth5>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.

  CASE <l_obs>(2).
  WHEN '41'.
    CHECK <l_obs> <> v_sub_tot_obs_41. "41_41
    READ TABLE <lt_hours_skf> INTO <ls_rec> WITH KEY
    ('OBS')         = v_sub_tot_obs_41
    ('VERSION')     = <l_parenth4>. "41_41XX

    IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
    ASSIGN COMPONENT: 'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
    <l_signeddata> = <l_signeddata> / <l_signedtotal>.
    <l_figures> = c_selector_perc.
    ENDIF.
    UNASSIGN: <l_signedtotal>.
  WHEN '42'.
    CHECK <l_obs> <> v_sub_tot_obs_42.
    READ TABLE <lt_hours_skf> INTO <ls_rec> WITH KEY
    ('OBS')         = v_sub_tot_obs_42
    ('VERSION')     = <l_parenth4>. "42_42XX

    IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
    ASSIGN COMPONENT: 'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
    <l_signeddata> = <l_signeddata> / <l_signedtotal>.
    <l_figures> = c_selector_perc.
    ENDIF.
    UNASSIGN: <l_signedtotal>.

  WHEN OTHERS.
  ENDCASE.

ENDLOOP.
* keep only required percentages
CLEAR: wa_strg.
CONCATENATE 'OBS EQ ''' v_sub_tot_obs_41  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'OR OBS EQ ''' v_sub_tot_obs_42  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'AND FIGURES EQ ''' c_selector_hours  '''' INTO wa_strg.
DELETE <lt_hours_skf> WHERE (wa_strg).
* sync
REFRESH: <lt_perc_skf>.
<lt_perc_skf> = <lt_hours_skf>.
REFRESH: <lt_hours_skf>.
<lt_hours_skf> = <lt_perc_skf>.
REFRESH: <lt_perc_skf>.
* keep only 41 and 42 in <lt_perc_skf>
IF NOT <ls_data> IS ASSIGNED.
ASSIGN ls_rec->* TO <ls_data>.
ENDIF.
LOOP AT <lt_hours_skf> INTO <ls_data> WHERE ('FIGURES = c_selector_perc').
 APPEND <ls_data> TO <lt_perc_skf>.
ENDLOOP.

* collapse H4_4302_4305
CLEAR: wa_strg.
CONCATENATE 'VERSION EQ ''' wa_param_node_sg43_n_1-hashvalue  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'AND N_ACCOUNT NE ''' 'SKF_4306'  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'AND N_ACCOUNT NE ''' 'SKF_43XX'  '''' INTO wa_strg.

LOOP AT <lt_hours_skf> INTO <ls_data> WHERE (wa_strg). "H4_4302_4305
*LOOP AT <lt_hours_skf> ASSIGNING <ls_data> WHERE ('VERSION =  wa_param_node_sg43_n_1-hashvalue'). "H4_4302_4305
ASSIGN COMPONENT:
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_parenth4>,
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_parenth5>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.

         CLEAR: orig_obs, orig_n_account.
          orig_obs =       <l_obs>.
          orig_n_account = <l_n_account>.

    <l_bu_line> = wa_param_obs_sg43_what_1-hashvalue.
    <l_partner> = <l_parenth4>.
    COLLECT <ls_data> INTO <lt_perc_skf>.

    CONCATENATE <l_n_account>(6) 'XX' INTO <l_n_account>.
    <l_obs> = wa_param_node_sg43_n_1-hashvalue.
    COLLECT <ls_data> INTO <lt_perc_skf>.

ENDLOOP.

* collapse H4_43_4306 and save 43.S090
CLEAR: wa_strg.
CONCATENATE 'VERSION EQ ''' wa_param_node_sg43_n_2-hashvalue  '''' INTO wa_strg. "H4_43_4306
CONCATENATE wa_strg ` ` 'AND N_ACCOUNT EQ ''' 'SKF_4306'  '''' INTO wa_strg.
CONCATENATE wa_strg ` ` 'AND OBS EQ ''' '43_4306'  '''' INTO wa_strg.

LOOP AT <lt_hours_skf> INTO <ls_data> WHERE (wa_strg).
*LOOP AT <lt_hours_skf> INTO <ls_data> WHERE ('VERSION =  wa_param_node_sg43_n_2-hashvalue'). "H4_43_4306
ASSIGN COMPONENT:
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'FUNCTIONAL_AREA'   OF STRUCTURE <ls_data> TO <l_functional_area>,
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_parenth4>,
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_parenth5>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
*CHECK <l_n_account> = 'SKF_4306' AND orig_obs = '43_4306'.
         CLEAR: orig_obs, orig_n_account.
          orig_obs       = <l_obs>.
          orig_n_account = <l_n_account>.
          orig_partner   = <l_partner>.

    <l_bu_line> = wa_param_obs_sg43_what_1-hashvalue.
    <l_partner> = <l_parenth4>.
    <l_functional_area> = ls_fa_basealloc_2. "SGA999
    <l_obs> = wa_param_node_sg43_n_2-hashvalue.
    <l_n_account> = v_sub_tot_nacc_43.       "SKF_43XX
    COLLECT <ls_data> INTO <lt_perc_skf>.
 IF orig_partner <> wa_param_obs_sg43_adjust_i-hashvalue. "43.S090
    <l_n_account> = orig_n_account.
    <l_bu_line> = wa_param_obs_sg43_what_1-hashvalue.
    <l_partner> = v_key_denom_1.
    <l_functional_area> = ls_fa_basealloc_2. "SGA999
    COLLECT <ls_data> INTO <lt_perc_skf>.
 ELSE.   "=43.S090
 <l_n_account> = orig_n_account. "43_4306
 <l_obs> = wa_param_obs_sg43_adjust_i-hashvalue.
 APPEND <ls_data> TO <lt_perc_skf>.

 ENDIF.
ENDLOOP.

IF NOT <ls_data> IS ASSIGNED.
ASSIGN ls_rec->* TO <ls_data>.
ENDIF.
* add H4_4302_4305 TO H4_43_4306 on SKF_43XX / H4_43_4306 and add 43.S090 to get H4_4302_4305_+_43.S090
CLEAR: wa_strg.
CONCATENATE 'OBS EQ ''' wa_param_node_sg43_n_1-hashvalue  '''' INTO wa_strg. "H4_4302_4305

*LOOP AT <lt_perc_skf> INTO <ls_data> WHERE ('OBS =  wa_param_node_sg43_n_1-hashvalue'). "H4_4302_4305
LOOP AT <lt_perc_skf> INTO <ls_data> WHERE (wa_strg).
    ASSIGN COMPONENT:
    'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
    'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
    'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
    'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
    CHECK <l_partner> <> v_numer_1 AND <l_partner> <> v_numer_2. "avoid infinite append
    CLEAR: orig_amount.
    orig_amount = <l_signeddata>.

    READ TABLE <lt_perc_skf> INTO <ls_rec> WITH KEY
    ('OBS')         =  wa_param_node_sg43_n_2-hashvalue "H4_43_4306
    ('N_ACCOUNT')   =  v_sub_tot_nacc_43.               "SKF_43XX
        IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
        ASSIGN COMPONENT: 'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
        <l_obs>        = wa_param_node_sg43_n_2-hashvalue.
        <l_signeddata> = <l_signeddata> + <l_signedtotal>.
        <l_partner>    = v_numer_1.          "H4_4302_4305_&_H4_43_4306
        APPEND <ls_data> TO <lt_perc_skf>.
        UNASSIGN: <l_signedtotal>.
        ELSE.
        ENDIF.
    READ TABLE <lt_perc_skf> INTO <ls_rec> WITH KEY
    ('OBS')         =  wa_param_obs_sg43_adjust_i-hashvalue. "43.S090
        IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
        ASSIGN COMPONENT:
        'N_ACCOUNT'  OF STRUCTURE <ls_rec> TO <l_new_n_acc>,
        'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
        <l_n_account>  = <l_new_n_acc>.
        <l_obs>        = v_numer_2. "H4_4302_4305_+_43.S090
        <l_signeddata> = orig_amount     + <l_signedtotal>.
        <l_partner>    = wa_param_obs_sg43_adjust_i-hashvalue.
        APPEND <ls_data> TO <lt_perc_skf>.
        UNASSIGN: <l_signedtotal>.
        ELSE.
        ENDIF.
ENDLOOP.

* sync hours internal table without any pointers (shorter than un-assign pointers)
REFRESH: <lt_hours_skf>.
<lt_hours_skf> = <lt_perc_skf>.
REFRESH: <lt_perc_skf>.
* 43: compute relative percentage for SKF_43..vs H4_4302_4305 and 43_4306 vs H4_43_4306
LOOP AT <lt_hours_skf> ASSIGNING <ls_data> WHERE ('N_ACCOUNT <>  v_sub_tot_nacc_43').
ASSIGN COMPONENT:
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_parenth4>,
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_parenth5>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
CASE <l_obs>(2).
WHEN c_prefix_43.
    READ TABLE <lt_hours_skf> INTO <ls_rec> WITH KEY
    ('OBS')       =  wa_param_node_sg43_n_1-hashvalue. "H4_4302_4305
        IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
        ASSIGN COMPONENT: 'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
        <l_signeddata> = <l_signeddata> / <l_signedtotal>.
        <l_figures> = c_selector_perc.
        UNASSIGN: <l_signedtotal>.
        ELSE.
        ENDIF.
WHEN c_prefix_h4.

    IF <l_partner> = v_key_denom_1. "H4_43_430X_TO_430Y_&_H4_43_4301
    READ TABLE <lt_hours_skf> INTO <ls_rec> WITH KEY
    ('PARTNER')   = v_numer_1                         "H4_4302_4305_&_H4_4306
    ('N_ACCOUNT') = v_sub_tot_nacc_43                 "SKF_43XX
    ('OBS')       = wa_param_node_sg43_n_2-hashvalue. "H4_43_4306
        IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
        ASSIGN COMPONENT: 'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
        <l_obs> = wa_param_node_sg43_n_2-hashvalue+3(7). "added emergency
        <l_signeddata> = <l_signeddata> / <l_signedtotal>.
        <l_figures> = c_selector_perc.
        UNASSIGN: <l_signedtotal>.
        ELSE.
        ENDIF.

    ELSEIF <l_partner> = wa_param_obs_sg43_adjust_i-hashvalue . "43.S090
    READ TABLE <lt_hours_skf> INTO <ls_rec> WITH KEY
    ('PARTNER')   =  v_numer_1                         "H4_4302_4305_&_H4_4306
    ('N_ACCOUNT') =  v_sub_tot_nacc_43
    ('OBS')       =  wa_param_node_sg43_n_2-hashvalue. "H4_43_4306
        IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
        ASSIGN COMPONENT: 'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
        <l_signeddata> =   <l_signeddata> / <l_signedtotal>.
        <l_figures> = c_selector_perc.
        UNASSIGN: <l_signedtotal>.
        ELSE.
        ENDIF.


    ENDIF.

ENDCASE.
ENDLOOP.
* keep only percentages
CLEAR: wa_strg.
CONCATENATE 'FIGURES NE ''' c_selector_perc  '''' INTO wa_strg.
DELETE <lt_hours_skf> WHERE (wa_strg).

* second round except for SKF_4306 / H4_43_4306
LOOP AT <lt_hours_skf> ASSIGNING <ls_data> WHERE ('PARTNER = wa_param_node_sg43_n_1-hashvalue'). "H4_4302_4305
ASSIGN COMPONENT:
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_parenth4>,
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_parenth5>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
    READ TABLE <lt_hours_skf> INTO <ls_rec> WITH KEY
    ('FIGURES')   = c_selector_perc                         "PERCENTAGE
    ('OBS')       = v_numer_2.                              "H4_4302_4305_+_43.S090
        IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
        ASSIGN COMPONENT: 'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signedtotal>.
        <l_signeddata> = <l_signeddata> * <l_signedtotal>.
        <l_figures> = c_selector_perc.
        UNASSIGN: <l_signedtotal>.
        ELSE.
       ENDIF.
ENDLOOP.

* keep only required percentages
CLEAR: wa_strg.
CONCATENATE 'OBS EQ ''' v_numer_2  '''' INTO wa_strg.
DELETE <lt_hours_skf> WHERE (wa_strg).
CLEAR: wa_strg.
CONCATENATE 'OBS EQ ''' wa_param_obs_sg43_adjust_i-hashvalue  '''' INTO wa_strg.
DELETE <lt_hours_skf> WHERE (wa_strg).

* sync hours internal table without any pointers (shorter than un-assign pointers)
REFRESH: <lt_perc_skf>.
<lt_perc_skf> = <lt_hours_skf>.

LOOP AT <lt_perc_skf> ASSIGNING <ls_rec>.
ASSIGN COMPONENT:
'OBS'     OF STRUCTURE <ls_rec> TO <l_obs>,
'VERSION' OF STRUCTURE <ls_rec> TO <l_version>.

        UNASSIGN: <l_bu_line>.
READ TABLE <lt_amount_sga> INTO <ls_data> WITH KEY
    ('OBS')       = <l_obs>.
        IF sy-subrc = 0 AND NOT <l_bu_line> IS ASSIGNED.
        ASSIGN COMPONENT: 'BU_LINE' OF STRUCTURE <ls_data> TO <l_bu_line>.
           <l_version> = <l_bu_line>.
        UNASSIGN: <l_bu_line>.
        ELSE.
       ENDIF.
ENDLOOP.

* back_up point PERCENTAGE table <lt_perc_skf>
REFRESH: <lt_hours_skf>.
<lt_hours_skf> = <lt_perc_skf>.
BREAK bb5827.
* create 41, 42 write-off records and percentage per functional area
CLEAR: wa_strg.
CONCATENATE 'OBS EQ ''' wa_param_obs_gb41_what-hashvalue  '''' INTO wa_strg. "41_4101
CONCATENATE wa_strg ` ` 'OR OBS EQ ''' wa_param_obs_ae42_what-hashvalue  '''' INTO wa_strg. "42_4201

LOOP AT <lt_amount_sga_by_fa> ASSIGNING <ls_data> WHERE (wa_strg).
ASSIGN COMPONENT:
'MEASURES'          OF STRUCTURE <ls_data> TO <l_measures>,
'FUNCTIONAL_AREA'   OF STRUCTURE <ls_data> TO <l_functional_area>,
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.

CHECK <l_figures> <>  c_selector_woff AND <l_figures> <>  c_selector_perc.

* save the amount to write-off for 41/2_01
 <l_figures>    = c_selector_woff.
 <l_measures>   = c_periodic.
 <l_signeddata> = <l_signeddata> * -1.
ENDLOOP.

* 41, 42 allocation have only one scope
CLEAR: wa_strg.
CONCATENATE 'OBS EQ ''' wa_param_obs_gb41_what-hashvalue  '''' INTO wa_strg. "41_4101
CONCATENATE wa_strg ` ` 'OR OBS EQ ''' wa_param_obs_ae42_what-hashvalue  '''' INTO wa_strg. "42_4201
*CONCATENATE wa_strg ` ` 'OR OBS EQ ''' wa_param_obs_sg43_what_1-hashvalue  '''' INTO wa_strg. "H4_43_4301
*CONCATENATE wa_strg ` ` 'OR OBS EQ ''' wa_param_obs_sg43_addhie_i-hashvalue  '''' INTO wa_strg. "H5_43_GOV
CONCATENATE wa_strg ` ` 'AND FIGURES EQ ''' c_selector_woff  '''' INTO wa_strg. "H5_43_GOV

LOOP AT <lt_amount_sga_by_fa> ASSIGNING <ls_data> WHERE (wa_strg).

ASSIGN COMPONENT:
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_bu_line>,
'SMART'             OF STRUCTURE <ls_data> TO <l_smart>,
'DATASRC'           OF STRUCTURE <ls_data> TO <l_datasrc>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'PRODUCT'           OF STRUCTURE <ls_data> TO <l_product>,
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'FUNCTIONAL_AREA'   OF STRUCTURE <ls_data> TO <l_functional_area>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'MEASURES'          OF STRUCTURE <ls_data> TO <l_measures>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_version>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
*CHECK <l_figures> = c_selector_woff.
     CLEAR: orig_amount, orig_obs, <ls_rec>.
* back_up writeoff line
     <ls_rec> = <ls_data>.
     orig_obs    = <l_bu_line>.
     orig_amount = <l_signeddata>.
* BU_LINE was used as placeholder for Support related to profitable Bus Area OBS
    LOOP AT <lt_perc_skf> ASSIGNING <ls_hours_perc> WHERE ('BU_LINE = <l_obs>').

         ASSIGN COMPONENT:

     'FIGURES'    OF STRUCTURE <ls_hours_perc> TO <l_percentage>,
     'VERSION'        OF STRUCTURE <ls_hours_perc> TO <l_newobs>,
     'SIGNEDDATA' OF STRUCTURE <ls_hours_perc> TO <l_perc_skf>.
     CHECK <l_percentage> = c_selector_perc.
     CLEAR: skf_fa_perc, gst_fahier.
            skf_fa_perc     =  ( orig_amount * <l_perc_skf> ) * -1.
        <l_signeddata>  = skf_fa_perc.
      READ TABLE gtt_fahier INTO gst_fahier WITH KEY ('MEMBER') = <l_functional_area>.
      IF sy-subrc = 0.
         IF  gst_fahier-parent     = l_fa_node_def
         OR  gst_fahier-parent_lv1 = l_fa_node_def
         OR  gst_fahier-parent_lv2 = l_fa_node_def
         OR  gst_fahier-parent_lv3 = l_fa_node_def
         OR  gst_fahier-parent_lv4 = l_fa_node_def
         OR  gst_fahier-parent_lv5 = l_fa_node_def
         OR  gst_fahier-parent_lv6 = l_fa_node_def
         OR  gst_fahier-parent_lv7 = l_fa_node_def
         OR  gst_fahier-parent_lv8 = l_fa_node_def
         OR  gst_fahier-parent_lv9 = l_fa_node_def.
         <l_smart> = l_s_smarttarget. "'62694'.
        ELSE.
        <l_smart> = l_o_smarttarget. "'62695'.
        ENDIF.

*Look-up in OBS BPC master data table to post on .I OBS base member attached to OBS node to allocate
* obs input member was stored in <l_bu_line>.

        <l_obs>         = <l_newobs>.
        <l_bu_line>     = l_bu_linetarget.
        <l_datasrc>     = l_datasrctarget.
        <l_version>     = l_versiontarget.
        <l_n_account>   = l_n_accounttarget.
        <l_partner>     = l_partnertarget.
        <l_product>     = l_producttarget.
        <l_figures>     = l_figurestarget.
        <l_measures>    = c_periodic .

        APPEND <ls_data> TO <lt_amount_sga_by_fa>.
      ENDIF.

* restore original write-off line and adjust base target members for posting
     <ls_data> = <ls_rec>.
        <l_obs>         = orig_obs.
        <l_bu_line>     = l_bu_linetarget.
        <l_datasrc>     = l_datasrctarget.
        <l_version>     = l_versiontarget.
        <l_n_account>   = l_n_accounttarget. "43 SCOPE II is : l_n_accounttarget2
        <l_partner>     = l_partnertarget.
        <l_product>     = l_producttarget.
        <l_figures>     = l_figurestarget.
        <l_measures>    = c_periodic .
        READ TABLE gtt_fahier INTO gst_fahier WITH KEY ('MEMBER') = <l_functional_area>.
        IF sy-subrc = 0.
        IF  gst_fahier-parent     = l_fa_node_def
         OR  gst_fahier-parent_lv1 = l_fa_node_def
         OR  gst_fahier-parent_lv2 = l_fa_node_def
         OR  gst_fahier-parent_lv3 = l_fa_node_def
         OR  gst_fahier-parent_lv4 = l_fa_node_def
         OR  gst_fahier-parent_lv5 = l_fa_node_def
         OR  gst_fahier-parent_lv6 = l_fa_node_def
         OR  gst_fahier-parent_lv7 = l_fa_node_def
         OR  gst_fahier-parent_lv8 = l_fa_node_def
         OR  gst_fahier-parent_lv9 = l_fa_node_def.
         <l_smart> = l_s_smarttarget. "'62694'.
        ELSE.
        <l_smart> = l_o_smarttarget. "'62695'.
        ENDIF.
        ELSE.

        ENDIF.
  ENDLOOP.
ENDLOOP.

REFRESH: <lt_perc_skf>.
<lt_perc_skf> = <lt_hours_skf>.

* 43 allocation scope 1 - 43_4301 = H5_43_GOV (43.S060 + 43.S090) + H4_43_4301 (43_4301 exl. 43.S090 and 43.S060)
* 43_4301 added via *XDIM_ADDMEMBERSET OBS = 43_4301
CLEAR: wa_strg.
CONCATENATE wa_strg ` ` 'OBS EQ ''' c_43_4301  '''' INTO wa_strg.                                "43_4301
CONCATENATE wa_strg ` ` 'OR OBS EQ ''' wa_param_obs_sg43_adjust_i-hashvalue  '''' INTO wa_strg.  "43.S090
CONCATENATE wa_strg ` ` 'OR OBS EQ ''' wa_param_obs_sg43_adjust_i2-hashvalue  '''' INTO wa_strg. "43.S060
CONCATENATE wa_strg ` ` 'OR OBS EQ ''' wa_param_obs_sg43_what_1-hashvalue  '''' INTO wa_strg.    "H4_43_4301
* back up AMOUNT for 41,42 select on DATASRC = HQ_DEST
BREAK BB5827.
REFRESH: <lt_amount_sga_by_fab>, <lt_amount_sga>.
LOOP AT <lt_amount_sga_by_fa> ASSIGNING <ls_data> WHERE (wa_strg).
ASSIGN COMPONENT:
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_bu_line>,
'SMART'             OF STRUCTURE <ls_data> TO <l_smart>,
'DATASRC'           OF STRUCTURE <ls_data> TO <l_datasrc>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'PRODUCT'           OF STRUCTURE <ls_data> TO <l_product>,
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'FUNCTIONAL_AREA'   OF STRUCTURE <ls_data> TO <l_functional_area>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'MEASURES'          OF STRUCTURE <ls_data> TO <l_measures>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_version>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.

     CLEAR: orig_obs.
          orig_obs    = <l_obs>.

     IF NOT <ls_rec> IS ASSIGNED.
     ASSIGN ls_rec2->* TO <ls_rec>.
     ELSE.
     CLEAR: <ls_rec>.
     ENDIF.
     <ls_rec> = <ls_data>.
IF orig_obs = '43_4301'. "H4_43_4301.

     CLEAR: orig_amount.
     orig_amount = <l_signeddata>.
* BU_LINE was used as placeholder for Support related to profitable Bus Area OBS
*   LOOP AT <lt_perc_skf> ASSIGNING <ls_hours_perc> WHERE ('BU_LINE = <l_obs>').
   LOOP AT <lt_perc_skf> ASSIGNING <ls_hours_perc> WHERE ('BU_LINE = wa_param_obs_sg43_what_1-hashvalue'). "H4_43_4301
      IF sy-subrc = 0 AND NOT <l_newobs> IS ASSIGNED AND NOT <l_perc_skf> IS ASSIGNED.
      ASSIGN COMPONENT:
     'VERSION'        OF STRUCTURE <ls_hours_perc> TO <l_newobs>,
     'SIGNEDDATA' OF STRUCTURE <ls_hours_perc> TO <l_perc_skf>.

     CLEAR: skf_fa_perc, gst_fahier.
            skf_fa_perc     =  ( orig_amount * <l_perc_skf> ).
        <l_signeddata>  = skf_fa_perc.
      READ TABLE gtt_fahier INTO gst_fahier WITH KEY ('MEMBER') = <l_functional_area>.
      IF sy-subrc = 0.
         IF  gst_fahier-parent     = l_fa_node_def
         OR  gst_fahier-parent_lv1 = l_fa_node_def
         OR  gst_fahier-parent_lv2 = l_fa_node_def
         OR  gst_fahier-parent_lv3 = l_fa_node_def
         OR  gst_fahier-parent_lv4 = l_fa_node_def
         OR  gst_fahier-parent_lv5 = l_fa_node_def
         OR  gst_fahier-parent_lv6 = l_fa_node_def
         OR  gst_fahier-parent_lv7 = l_fa_node_def
         OR  gst_fahier-parent_lv8 = l_fa_node_def
         OR  gst_fahier-parent_lv9 = l_fa_node_def.
         <l_smart> = l_s_smarttarget. "'62694'.
        ELSE.
        <l_smart> = l_o_smarttarget. "'62695'.
        ENDIF.
      ENDIF.

        <l_obs>         = <l_newobs>.
        <l_bu_line>     = l_bu_linetarget.
        <l_datasrc>     = l_datasrctarget.
        <l_version>     = l_versiontarget.

        <l_partner>     = l_partnertarget.
        <l_product>     = l_producttarget.
        <l_figures>     = l_figurestarget.
        <l_measures>    = c_periodic.

        IF <l_obs> <> '43_4306.I'.
        <l_n_account>   = l_n_accounttarget.
        ELSE.
        <l_n_account>   = l_n_accounttarget2.
        ENDIF.

        APPEND <ls_data> TO <lt_amount_sga_by_fab>.

     UNASSIGN: <l_newobs>, <l_perc_skf>.
     ELSE.


     ENDIF.
    ENDLOOP.
*  write off 43.S060 & 43.S090
ELSE. "not orig_obs = '43_4301'.
* save the amount to write-off for 43
 <l_figures>    = c_selector_woff.
 <l_measures>   = c_periodic.
 <l_signeddata> = <l_signeddata> * -1.

            READ TABLE gtt_fahier INTO gst_fahier WITH KEY ('MEMBER') = <l_functional_area>.
            IF sy-subrc = 0.
             IF  gst_fahier-parent     = l_fa_node_def
                OR  gst_fahier-parent_lv1 = l_fa_node_def
                OR  gst_fahier-parent_lv2 = l_fa_node_def
                OR  gst_fahier-parent_lv3 = l_fa_node_def
                OR  gst_fahier-parent_lv4 = l_fa_node_def
                OR  gst_fahier-parent_lv5 = l_fa_node_def
                OR  gst_fahier-parent_lv6 = l_fa_node_def
                OR  gst_fahier-parent_lv7 = l_fa_node_def
                OR  gst_fahier-parent_lv8 = l_fa_node_def
                OR  gst_fahier-parent_lv9 = l_fa_node_def.
             <l_smart>        = l_s_smarttarget. "'62694'.
             ELSE.
             <l_smart>        = l_o_smarttarget. "'62695'.
             ENDIF.
            ENDIF.
             <l_measures>    = c_periodic.
             <l_bu_line>     = l_bu_linetarget.
             <l_datasrc>     = l_datasrctarget.
             <l_version>     = l_versiontarget.
             <l_n_account>   = l_n_accounttarget2.
             <l_partner>     = l_partnertarget.
             <l_product>     = l_producttarget.
             <l_figures>     = l_figurestarget.
            IF orig_obs = wa_param_obs_sg43_what_1-hashvalue.   "H4_43_4301
             <l_n_account>   = l_n_accounttarget.
             CONCATENATE wa_param_obs_sg43_what_1-hashvalue+3(7) '.I' INTO <l_obs>.
            ELSE.
             <l_n_account>   = l_n_accounttarget2.
            ENDIF.
        APPEND <ls_data> TO <lt_amount_sga_by_fab>.

ENDIF.
ENDLOOP.

REFRESH: <lt_amount_sga>.

LOOP AT <lt_amount_sga_by_fab> INTO <ls_data>.
ASSIGN COMPONENT:
'BU_LINE'           OF STRUCTURE <ls_data> TO <l_bu_line>,
'SMART'             OF STRUCTURE <ls_data> TO <l_smart>,
'DATASRC'           OF STRUCTURE <ls_data> TO <l_datasrc>,
'N_ACCOUNT'         OF STRUCTURE <ls_data> TO <l_n_account>,
'PARTNER'           OF STRUCTURE <ls_data> TO <l_partner>,
'PRODUCT'           OF STRUCTURE <ls_data> TO <l_product>,
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'FUNCTIONAL_AREA'   OF STRUCTURE <ls_data> TO <l_functional_area>,
'OBS'               OF STRUCTURE <ls_data> TO <l_obs>,
'MEASURES'          OF STRUCTURE <ls_data> TO <l_measures>,
'VERSION'           OF STRUCTURE <ls_data> TO <l_version>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.

IF <l_obs> = '43_4302.I' or <l_obs> = wa_param_obs_sg43_adjust_i-hashvalue or <l_obs> = wa_param_obs_sg43_adjust_i2-hashvalue.
        <l_smart>           = c_99.
        <l_obs>             = c_zz.
        <l_figures>        = c_selector_woff.
        <l_functional_area> = ls_fa_basealloc_2.
        <l_n_account>       = c_99.
        COLLECT <ls_data> INTO <lt_amount_sga>.
ELSE.
        <l_smart>           = c_99.
        <l_obs>             = c_zz.
        <l_figures>        = c_selector_amt.
        <l_functional_area> = ls_fa_basealloc_2.
        <l_n_account>       = c_99.
        COLLECT <ls_data> INTO <lt_amount_sga>.
ENDIF.
ENDLOOP.
* calculate remainder of allocation and store as DIFFERENCE in <lt_amount_sga>
CLEAR: wa_strg.
CONCATENATE wa_strg ` ` 'OBS EQ ''' c_zz  '''' INTO wa_strg.                    "ZZZZZZ
CONCATENATE wa_strg ` ` 'AND FIGURES EQ ''' c_selector_amt  '''' INTO wa_strg.  "AMOUNT

LOOP AT <lt_amount_sga> INTO <ls_data> WHERE (wa_strg).
ASSIGN COMPONENT:
'FIGURES'           OF STRUCTURE <ls_data> TO <l_figures>,
'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
CHECK  <l_figures> <> c_selector_diff.
 READ TABLE <lt_amount_sga> INTO <ls_rec>
 WITH KEY
 ('OBS')     = c_zz
 ('FIGURES') = c_selector_woff.
 IF sy-subrc = 0 AND NOT <l_signedtotal> IS ASSIGNED.
ASSIGN COMPONENT:
'SIGNEDDATA'        OF STRUCTURE <ls_rec> TO <l_signedtotal>.
       <l_figures>    = c_selector_diff.
       <l_signeddata> = <l_signeddata> + <l_signedtotal>.
APPEND <ls_data> TO <lt_amount_sga>.
UNASSIGN: <l_signedtotal>.
EXIT.
ENDIF.
ENDLOOP.
* store the difference
IF <l_signeddata> IS ASSIGNED. UNASSIGN:  <l_signeddata>. ENDIF.
READ TABLE <lt_amount_sga> INTO <ls_data> WITH KEY ('FIGURES') = c_selector_diff.
IF sy-subrc = 0 AND NOT <l_signeddata> IS ASSIGNED.
ASSIGN COMPONENT: 'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
orig_amount = <l_signeddata>.
UNASSIGN: <l_signeddata>.
ENDIF.

CLEAR: wa_strg.
* keep only 41 and 42 in <lt_amount_sga_by_fa>
CONCATENATE 'DATASRC NE ''' l_datasrctarget  '''' INTO wa_strg. "HQ_DEST
DELETE <lt_amount_sga_by_fa> WHERE (wa_strg).
CLEAR: wa_strg.
CONCATENATE 'OBS(2) NE ''' c_prefix_41  '''' INTO wa_strg. "41
CONCATENATE wa_strg ` ` 'AND OBS(2) NE ''' c_prefix_42  '''' INTO wa_strg. "42
DELETE <lt_amount_sga_by_fa> WHERE (wa_strg).

* remove the difference from the highest allocation amount
SORT <lt_amount_sga_by_fab>  BY ('SIGNEDDATA') DESCENDING.

READ TABLE <lt_amount_sga_by_fab> ASSIGNING <ls_data> INDEX 1.
IF sy-subrc = 0 AND NOT <l_signeddata> IS ASSIGNED.
ASSIGN COMPONENT: 'SIGNEDDATA'        OF STRUCTURE <ls_data> TO <l_signeddata>.
<l_signeddata> = <l_signeddata> - orig_amount.
UNASSIGN: <l_signeddata>.
ENDIF.

APPEND LINES OF <lt_amount_sga_by_fab> TO <lt_amount_sga_by_fa>.
REFRESH: <lt_perc_skf>.
<lt_perc_skf> = <lt_hours_skf>.
BREAK BB5827.
* REMOVE ANY ZERO SIGNEDDATA
CLEAR: wa_strg.
CONCATENATE 'SIGNEDDATA EQ ''' '0'  '''' INTO wa_strg.
DELETE <lt_amount_sga_by_fa> WHERE (wa_strg).

* sync internal table pointers
REFRESH:<lt_amount_sga_by_fab>.
<lt_amount_sga_by_fab> = <lt_amount_sga_by_fa>.
REFRESH:<lt_amount_sga_by_fab>.


REFRESH: ct_data.
ct_data[] = <lt_amount_sga_by_fa>[].

IF ct_data IS INITIAL.
  CONCATENATE 'No additionnal allocation postings to be processed for period: ' current_month  INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
ELSE.
DESCRIBE TABLE ct_data LINES l_lines.
l_lines_on = l_lines.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = l_lines_on
            IMPORTING
              output = l_lines_on.

CONCATENATE 'Attempt to write' l_lines_on 'lines to CT_DATA for model' i_appl_id '.' INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
ENDIF.
REFRESH: <lt_perc_skf>, <lt_amount_sga_by_fab>, <lt_amount_sga_by_fa>, <lt_amount_sga>, <lt_hours_skf>, <lt_databup>,<lt_dataytd>.

ENDIF. "l_subrc <> 0
ENDMETHOD.


METHOD if_uj_custom_logic~init.
* The INIT method can be used for PRE processing.
* The CLEANUP method can be used for POST processing.
* CLEANUP cleans what INIT populated.

* Example can be a script with XDIM_MAXMEMBERS.
* In that case EXECUTE will be called multiple times.
* If it reads Master Data it will be more efficient reading MD in INIT.
*  Then CLEANUP will remove data from those internal tables.

*DATA: t_debug_flag_init VALUE 'X'.
*
*    IF sy-uname = 'BB5827'.
*      DO. IF t_debug_flag_init = ' '. EXIT. ENDIF.
*      ENDDO.
*      BREAK BB5827.
*    ENDIF.

ENDMETHOD.


METHOD process_parameters.

DATA:  is_param TYPE ujk_s_script_logic_hashentry,
       res_param TYPE ujk_s_script_logic_hashentry,
       req_params TYPE TABLE OF ujk_s_script_logic_hashentry,
       ld_log TYPE string.

DATA:  lr_missing_exc TYPE REF TO cx_uj_custom_logic.
* Check for eventually missing parameters in logic script building a reference internal table
  res_param-hashkey = 'WRITE'.                              APPEND res_param TO req_params.
  res_param-hashkey = 'QUERY'.                              APPEND res_param TO req_params.

  res_param-hashkey = 'BASEALLOC'.                          APPEND res_param TO req_params.
  res_param-hashkey = 'BASEALLOC_2'.                        APPEND res_param TO req_params.
  res_param-hashkey = 'FA_NODE_DEFINES_SMART'.              APPEND res_param TO req_params.
  res_param-hashkey = 'O_SMART'.                            APPEND res_param TO req_params.
  res_param-hashkey = 'S_SMART'.                            APPEND res_param TO req_params.
  res_param-hashkey = 'BU_LINE_T'.                          APPEND res_param TO req_params.
  res_param-hashkey = 'DATASRC_T'.                          APPEND res_param TO req_params.
  res_param-hashkey = 'N_ACCOUNT_T'.                        APPEND res_param TO req_params.
  res_param-hashkey = 'N_ACCOUNT_T2'.                       APPEND res_param TO req_params.
  res_param-hashkey = 'PARTNER_T'.                          APPEND res_param TO req_params.
  res_param-hashkey = 'PRODUCT_T'.                          APPEND res_param TO req_params.
  res_param-hashkey = 'VERSION_T'.                          APPEND res_param TO req_params.
  res_param-hashkey = 'FIGURES_T'.                          APPEND res_param TO req_params.
  res_param-hashkey = 'FA_SELECT'.                          APPEND res_param TO req_params.

  res_param-hashkey = 'CHECK_I'.                            APPEND res_param TO req_params.

  res_param-hashkey = 'TIMESELECTION'.                      APPEND res_param TO req_params.
*GB41 - AE42
  res_param-hashkey = 'OBS_GB41_WHAT'.                      APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_GB41_WHERE'.                     APPEND res_param TO req_params.
  res_param-hashkey = 'NODE_GB41_D'.                        APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_GB41_INPUT'.                     APPEND res_param TO req_params.

  res_param-hashkey = 'OBS_AE42_WHAT'.                      APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_AE42_WHERE'.                     APPEND res_param TO req_params.
  res_param-hashkey = 'NODE_AE42_D'.                        APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_AE42_INPUT'.                     APPEND res_param TO req_params.

*SG43
  res_param-hashkey = 'OBS_SG43_WHAT_1'.                    APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_SG43_WHAT_2'.                    APPEND res_param TO req_params.
  res_param-hashkey = 'NODE_SG43_N_1'.                      APPEND res_param TO req_params.
  res_param-hashkey = 'NODE_SG43_D_1'.                      APPEND res_param TO req_params.
  res_param-hashkey = 'NODE_SG43_N_2'.                      APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_SG43_WHERE_1'.                   APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_SG43_INPUT_1'.                   APPEND res_param TO req_params.

  res_param-hashkey = 'OBS_SG43_ADDHIE_I'.                  APPEND res_param TO req_params. "check if valid according DM Prompt
  res_param-hashkey = 'OBS_SG43_ADJUST_I'.                  APPEND res_param TO req_params. "check if valid according DM Prompt
  res_param-hashkey = 'OBS_SG43_ADJUST_I2'.                 APPEND res_param TO req_params. "check if valid according DM Prompt

  res_param-hashkey = 'TOP_FA_NODE'.                        APPEND res_param TO req_params.

   CLEAR:   res_param.
   LOOP AT req_params INTO res_param.
   READ TABLE it_param WITH TABLE KEY hashkey = res_param-hashkey INTO is_param.
   IF sy-subrc = 0.
   ELSE.
**    Raise exception
   CONCATENATE 'Missing parameter ' res_param-hashkey 'in Script Logic !!!' INTO ld_log SEPARATED BY space.
   TRY.
   cl_ujk_logger=>log( i_object = ld_log ).
   CATCH cx_uj_custom_logic INTO lr_missing_exc.
    e_subrc = 4.
   ENDTRY.
   EXIT.
   ENDIF.
   ENDLOOP.
REFRESH: req_params.
*// =======================================================================================================================================================
*// =======================================================================================================================================================
*// NAME: CUST_BADI_RED_ALLOC.lgf
*//
*// CALL: Parameters can be hard coded in Script Logic instead of dynamic call via Data Manager Package
*//
*// DIMPROP: Properties used: OBS.ALLOC_RULE
*//
*// DESC: This script relies on Allocation Rules (derived from  OBS_MBRS_VARIABLES ) + other parameters except TIME (still via DM) are hardcoded
*// DYNAMIC SCRIPT VIA DM : CUST_BADI_ALLOC_SGA.lgf
*//// REVISIONS: BB5827 - Luc Vanrobays
*// July 7th 2019 - Added $FA_ND_DEFINES_SMART$
*// October 25th - Added %__SELECTION% variables
*// March 26th 2021 - added N_ACCOUNT_T2 to post SCope II on 95011
*//
*// ========================================================================================================================================================
*// retrieve OBS members to be allocated - property ALLOC_RULE is used for OBS base for which the parent member is specified
*// main rule to maintain the allocation framework
*// the property ALLOC_RULE on OBS dimension:
*// make sure that all postable base members (since BPC doesn't handle postable nodes) are created
*// and have the higher level node defined in ALLOC_RULE property
*// eg. parentmember "59_ENG" SGA's allocation should be posted on "59_S.I"
*// we capture the OBS nodes for SGA's allocation in next include via *SELECT into %OBS_2_ALLOCATE% WHERE ALLOC_RULE is not null
*
**INCLUDE OBS_MBRS_VARIABLES.LGF
*
*// hardcoded but will be checked inside BADI if equal to passed DM Parameter OBS_SG43_ADDHIE_I
**SELECT(%OBS_SG43_ADJUST_I%,"ID",OBS,ID=43.S090)
**SELECT(%OBS_SG43_ADJUST_I2%,"ID",OBS,ID=43.S060)
*
**XDIM_MEMBERSET OBS =  %OBS_GB41_WHAT%
**XDIM_ADDMEMBERSET OBS = %OBS_GB41_WHERE%
**XDIM_ADDMEMBERSET OBS = %NODE_GB41_D%
**XDIM_ADDMEMBERSET OBS = %OBS_AE42_WHAT%
**XDIM_ADDMEMBERSET OBS = %OBS_AE42_WHERE%
**XDIM_ADDMEMBERSET OBS = %NODE_AE42_D%
**XDIM_ADDMEMBERSET OBS = %OBS_SG43_WHAT_1%
**XDIM_ADDMEMBERSET OBS = %OBS_SG43_WHAT_2%
**XDIM_ADDMEMBERSET OBS = %NODE_SG43_N_1%
**XDIM_ADDMEMBERSET OBS = %NODE_SG43_D_1%
**XDIM_ADDMEMBERSET OBS = %NODE_SG43_N_2%
**XDIM_ADDMEMBERSET OBS = %OBS_SG43_WHERE_1%
*// adjustment OBS 43.S090
**XDIM_ADDMEMBERSET OBS = %OBS_SG43_ADJUST_I%
*// required for scope II
**XDIM_ADDMEMBERSET OBS = 43_4301
*//required to write-off amount for  OBS 43.S060 (and 43.S090 already above )
**XDIM_ADDMEMBERSET OBS = %OBS_SG43_ADJUST_I2%
*
**XDIM_MEMBERSET FUNCTIONAL_AREA = BAS(%FUNCTIONAL_AREA_SET%)
*
*//we need the query results sliced by Functional Area with RED allocations
*//
**XDIM_MEMBERSET FUNCTIONAL_AREA AS %FA_SELECT%=BAS(%FUNCTIONAL_AREA_SET%)
*
**XDIM_MEMBERSET TIME = %TIME_SET%
**XDIM_MEMBERSET CURRENCY = %CURRENCY_SET%
**XDIM_MEMBERSET VERSION = %VERSION_SET%
**XDIM_MEMBERSET N_ACCOUNT = %N_ACCOUNT_SET%
**XDIM_MEMBERSET BU_LINE = %BU_LINE_SET%
**XDIM_MEMBERSET FIGURES = %FIGURES_SET%
**XDIM_MEMBERSET PRODUCT = %PRODUCT_SET%
**XDIM_MEMBERSET DATASRC = %DATASRC_SET%
**XDIM_MEMBERSET SMART = %SMART_SET%
*
**START_BADI SGA_HOURS_BASE_ALLOC_1
*//*START_BADI SGA_HOURS_BASE_ALLOC_1
*
*WRITE = ON
*DEBUG = ON
*BASEALLOC=$BASEALLOC$
*BASEALLOC_2=$BASEALLOC_2$
*FA_NODE_DEFINES_SMART=$FA_NODE_DEFINES_SMART$
*O_SMART=$O_SMART$
*S_SMART=$S_SMART$
*BU_LINE_T=$BU_LINE_T$
*DATASRC_T=$DATASRC_T$
*N_ACCOUNT_T=$N_ACCOUNT_T$
*N_ACCOUNT_T2=$N_ACCOUNT_T2$
*PARTNER_T=$PARTNER_T$
*PRODUCT_T=$PRODUCT_T$
*VERSION_T=$VERSION_T$
*FIGURES_T=$FIGURES_T$
*FA_SELECT=%FA_SELECT%
*
*
*
*// Node was added to OBS Scope via SELECT %OBS_SG43_WHAT_2% and will be checked if equal to DM $OBS_SG43_ADDHIE_I$
*OBS_SG43_ADDHIE_I=$OBS_SG43_ADDHIE_I$
*// Hard Coded via *SELECT(%OBS_SG43_ADJUST_I%,"ID",OBS,ID=43.S090) and will be checked if equal to DM $OBS_SG43_ADJUST_I$
*OBS_SG43_ADJUST_I=$OBS_SG43_ADJUST_I$
*OBS_SG43_ADJUST_I2= %OBS_SG43_ADJUST_I2%
*
*TIMESELECTION=%TIME_SET%
*
*OBS_GB41_WHAT= %OBS_GB41_WHAT%
*OBS_GB41_WHERE= %OBS_GB41_WHERE%
*OBS_GB41_INPUT= %OBS_GB41_INPUT%
*NODE_GB41_D= %NODE_GB41_D%
*
*OBS_AE42_WHAT= %OBS_AE42_WHAT%
*OBS_AE42_WHERE= %OBS_AE42_WHERE%
*OBS_AE42_INPUT= %OBS_AE42_INPUT%
*NODE_AE42_D= %NODE_AE42_D%
*
*OBS_SG43_WHAT_1  = %OBS_SG43_WHAT_1%
*OBS_SG43_WHAT_2  = %OBS_SG43_WHAT_2%
*NODE_SG43_N_1    = %NODE_SG43_N_1%
*NODE_SG43_D_1    = %NODE_SG43_D_1%
*NODE_SG43_N_2    = %NODE_SG43_N_2%
*OBS_SG43_WHERE_1 = %OBS_SG43_WHERE_1%
*// not to query
*OBS_SG43_INPUT_1 = %OBS_SG43_INPUT_1%
*NODE_SG43_D_2    = %NODE_SG43_N_1%
*
*TOP_FA_NODE=F0000U
*//set CHECK_I=Y in last dedicated Data Manager Prompt if *.I naming convention is mandatory for allocation base member posting
*CHECK_I=$CHECK_I$
*END_BADI
*// =======================================================================================================================================================
*// NAME: OBS_MBRS_VARIABLES.lgf
*//
*// CALL: Parameters passed to logic via Data Manager Package
*//
*// DIMPROP: Properties used:
*//
*// DESC: This script contains only SELECT statements to fill variable values that are used in the other scripts,
*// namely Revenue Based Alloc, Hours based Allocation
*//// REVISIONS:
*// Jul 7th 2019 - Added variables %OBS_2_ALLOCATE%
*// Sep 7th 2020 - Excluded RED Companies
*// Jan 8th 2021 - Added GB41 AE42, SG43 WHAT, WHERE and INPUT members
*//
*// ========================================================================================================================================================
*//retrieve OBS members to be allocated - property ALLOC_RULE is used for OBS base for which the parent member is specified
*// we put the nodes subject to iocme based allocation in the ALLOC_RULE properties
*//*SELECT(%OBS_2_ALLOCATE%,"ALLOC_RULE",OBS,"[ALLOC_RULE]<>''")
**SELECT(%OBS_2_ALLOCATE%,"ALLOC_RULE",OBS,"[ALLOC_RULE]<>'' AND [COMP_CODE] <> 'GB41' AND [COMP_CODE] <> 'AE42' AND [COMP_CODE] <> 'SG43'")
**SELECT(%OBS_FOR_INPUT%,"ID",OBS,"[ALLOC_RULE]<>'' AND [COMP_CODE] <> 'GB41' AND [COMP_CODE] <> 'AE42' AND [COMP_CODE] <> 'SG43'")
*// RED Allocations
**SELECT(%OBS_GB41_WHAT%,"ID",OBS,"[ALLOC_RULE]= 'AMOUNT' AND [COMP_CODE]='GB41'")
**SELECT(%OBS_AE42_WHAT%,"ID",OBS,"[ALLOC_RULE]= 'AMOUNT' AND [COMP_CODE]='AE42'")
**SELECT(%OBS_GB41_WHERE%,"ID", "OBS", "[ALLOC_RULE]<>'AMOUNT' AND [BUS_AREA]='4102' AND [BUS_AREA]='4103' AND [BUS_AREA]='4104' AND [BUS_AREA]='4105' AND [CALC] = 'Y'")
*//*XDIM_MEMBERSET OBS AS %OBS_SELECT%=BAS(H4_41_410X_TO_410Y)
*//*SELECT(%OBS_GB41_WHERE%,"ALLOC_RULE", "OBS","[ID]=%OBS_SELECT% AND [ALLOC_RULE]<>''")
**SELECT(%OBS_AE42_WHERE%,"ID", "OBS", "[ALLOC_RULE]<>'AMOUNT' AND [BUS_AREA]='4202' AND [BUS_AREA]='4203' AND [BUS_AREA]='4204' AND [BUS_AREA]='4205' AND [CALC] = 'Y'")
*//*XDIM_MEMBERSET OBS AS %OBS_SELECT%=BAS(H4_42_4202_TO_4205)
*//*SELECT(%OBS_AE42_WHERE%,"ALLOC_RULE", "OBS","[ID]=%OBS_SELECT% AND [ALLOC_RULE]<>'' AND [CALC]= 'N'")
**SELECT(%OBS_GB41_INPUT%,"ID",OBS,"[ALLOC_RULE]<>'' AND [COMP_CODE] = 'GB41' AND [CALC]= 'N'")
**SELECT(%NODE_GB41_D%,"ID",OBS,"[ALLOC_RULE]='HOURS' AND [COMP_CODE] = 'GB41' AND [CALC]= 'Y'")
**SELECT(%OBS_AE42_INPUT%,"ID",OBS,"[ALLOC_RULE]<>'' AND [COMP_CODE] = 'AE42' AND [CALC]= 'N'")
**SELECT(%NODE_AE42_D%,"ID",OBS,"[ALLOC_RULE]='HOURS' AND [COMP_CODE] = 'AE42' AND [CALC]= 'Y'")
*// SG43
**SELECT(%OBS_SG43_WHAT_1%,"ID",OBS,"[ALLOC_RULE]= 'AMOUNT' AND [COMP_CODE]='SG43'")
**SELECT(%OBS_SG43_WHAT_2%,"ID",OBS,"[ALLOC_RULE]= 'AMOUNT_2' AND [COMP_CODE]='SG43'")
**SELECT(%NODE_SG43_N_1%,"ID",OBS,"[ALLOC_RULE]= 'HOURS_N1' AND [COMP_CODE]='SG43'")
**SELECT(%NODE_SG43_D_1%,"ID",OBS,"[ALLOC_RULE]= 'HOURS' AND [CALC] = 'Y' AND [COMP_CODE]='SG43'")
**SELECT(%NODE_SG43_N_2%,"ID",OBS,"[ALLOC_RULE]= 'HOURS_N2' AND [COMP_CODE]='SG43'")
**SELECT(%OBS_SG43_WHERE_1%,"ID", "OBS", "[ALLOC_RULE]<>'AMOUNT' AND [BUS_AREA]='4302' AND [BUS_AREA]='4303' AND [BUS_AREA]='4304' AND [BUS_AREA]='4305' AND [CALC] = 'Y'")
**SELECT(%OBS_SG43_INPUT_1%,"ID",OBS,"[ALLOC_RULE]<>'' AND [BUS_AREA]='4302' AND [BUS_AREA]='4303' AND [BUS_AREA]='4304' AND [BUS_AREA]='4305' AND [CALC]= 'N'")
*&---------------------------------------------------------------------*
* DATAMANAGER PACKAGE
*/CPMB/DEFAULT_FORMULAS
*2_SGA_HOURS_BASED_ALLOCATION_RED
*Group Allocations
*RED SGA Expenses Allocation (across Functional Areas) using HOURS as a Driver
*PROMPT(SELECTINPUT,,,,"%TIME_DIM%,%ACCOUNT_DIM%,%CURRENCY_DIM%,%DATASRC_DIM%,BU_LINE,PRODUCT,PARTNER,FIGURES,FUNCTIONAL_AREA,SMART,VERSION")
*'PROMPT(TEXT,%SCRIPT_FILE%,"Choose Script Logic File",,)
*PROMPT(TEXT,%BASEALLOC%,"Choose the FIGURES Driver Member to base the Allocation (eg. HOURS)",,)
*PROMPT(TEXT,%BASEALLOC_2%,"Choose a Functional Area as 2nd Selector for the Driver  to base the Allocation (eg. SGA999)",,)
*PROMPT(TEXT,%FA_NODE_DEFINES_SMART%,"Select the Functional Area Node that will define the SMART member(eg.FS000)",,)
*PROMPT(TEXT,%S_SMART%,"SMART mbr for Funct Area Parent FS000 (eg.62694)",,)
*PROMPT(TEXT,%O_SMART%,"SMART mbr for Non FA Parent FS000 (eg.62695)",,)
*PROMPT(TEXT,%BU_LINE_T%,"BU_LINE Target Member (eg.B9505)",,)
*PROMPT(TEXT,%DATASRC_T%,"DATASRC Target Member (eg.HQ_DEST)",,)
*PROMPT(TEXT,%N_ACCOUNT_T%,"N_ACCOUNT Scope I Target Member (eg.95010)",,)
*PROMPT(TEXT,%N_ACCOUNT_T2%,"N_ACCOUNT Scope II Target Member (eg.95011)",,)
*PROMPT(TEXT,%PARTNER_T%,"PARTNER Target Member (eg.TP_9999)",,)
*PROMPT(TEXT,%PRODUCT_T%,"PRODUCT Target Member (eg.TE99)",,)
*PROMPT(TEXT,%VERSION_T%,"VERSION Target Member (eg.ACTUALS)",,)
*PROMPT(TEXT,%FIGURES_T%,"FIGURES Target Member (eg.AMOUNT)",,)
*PROMPT(TEXT,%CHECK_I%,"Check .I base member(eg.Y or N)",,)
*PROMPT(TEXT,%OBS_SG43_ADJUST_I%,"Adjust SG43 Scope 1 Numerator with OBS (eg.43.S090)",,)
*PROMPT(TEXT,%OBS_SG43_ADDHIE_I%,"Define Scope 2 in SG43 based on H5 OBS node (eg.H5_43_GOV)",,)
*INFO(%EQU%,=)
*INFO(%TAB%,;)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,TAB,%TAB%)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,EQU,%EQU%)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,SUSER,%USER%)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,SAPPSET,%APPSET%)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,SAPP,%APP%)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,SELECTION,%SELECTION%)
*'TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,LOGICFILENAME,%SCRIPT_FILE%)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,REPLACEPARAM,FA_NODE_DEFINES_SMART%EQU%%FA_NODE_DEFINES_SMART%%TAB%BASEALLOC%EQU%%BASEALLOC%%TAB%BASEALLOC_2%EQU%%BASEALLOC_2%%TAB%S_SMART%EQU%%S_SMART%%TAB%O_SMART%EQU%%O_SMART%%TAB%BU_LINE_T%EQU%%BU_LINE_T
*line1956continued%%TAB%DATASRC_T%EQU%%DATASRC_T%%TAB%N_ACCOUNT_T%EQU%%N_ACCOUNT_T%%TAB%N_ACCOUNT_T2%EQU%%N_ACCOUNT_T2%%TAB%PARTNER_T%EQU%%PARTNER_T%%TAB%PRODUCT_T%EQU%%PRODUCT_T%%TAB%VERSION_T%EQU%%VERSION_T
*line1957continued%%TAB%FIGURES_T%EQU%%FIGURES_T%%TAB%OBS_SG43_ADJUST_I%EQU%%OBS_SG43_ADJUST_I%%TAB%OBS_SG43_ADDHIE_I%EQU%%OBS_SG43_ADDHIE_I%%TAB%CHECK_I%EQU%%CHECK_I%)
*TASK(/CPMB/DEFAULT_FORMULAS_LOGIC,LOGICFILENAME,CUST_BADI_RED_ALLOC.LGF)


ENDMETHOD.
ENDCLASS.
