class ZCL_BPC_ALLOC_SGA definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_UJ_CUSTOM_LOGIC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BPC_ALLOC_SGA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BPC_ALLOC_SGA=>IF_UJ_CUSTOM_LOGIC~CLEANUP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_UJ_CUSTOM_LOGIC~CLEANUP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BPC_ALLOC_SGA->IF_UJ_CUSTOM_LOGIC~EXECUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_APPSET_ID                    TYPE        UJ_APPSET_ID
* | [--->] I_APPL_ID                      TYPE        UJ_APPL_ID
* | [--->] IT_PARAM                       TYPE        UJK_T_SCRIPT_LOGIC_HASHTABLE(optional)
* | [--->] IT_CV                          TYPE        UJK_T_CV
* | [<---] ET_MESSAGE                     TYPE        UJ0_T_MESSAGE
* | [<-->] CT_DATA                        TYPE        STANDARD TABLE(optional)
* | [!CX!] CX_UJ_CUSTOM_LOGIC
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD if_uj_custom_logic~execute.
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

TYPES: BEGIN OF g_t_obshier.
  INCLUDE TYPE uja_s_mbr_node.
  INCLUDE TYPE l_t_unfold.
TYPES: END OF g_t_obshier.

TYPES: BEGIN OF g_t_fahier.
  INCLUDE TYPE uja_s_mbr_node.
  INCLUDE TYPE l_t_unfold.
TYPES: END OF g_t_fahier.

DATA: gst_obshier TYPE g_t_obshier,
      gtt_obshier TYPE STANDARD TABLE OF g_t_obshier WITH KEY member.

DATA: gst_fahier TYPE g_t_fahier,
      gtt_fahier TYPE STANDARD TABLE OF g_t_fahier WITH KEY member.

DATA: w_pred_obs TYPE char4,
      w_pred_fa TYPE char4.
DATA: moff TYPE i,
      mlen TYPE i.

TYPES: BEGIN OF ty_fa,
functional_area TYPE uj_dim_member,
END OF ty_fa.

TYPES: tty_fa TYPE STANDARD TABLE OF ty_fa.
*
TYPES: BEGIN OF ty_obs,
obs TYPE uj_dim_member,
END OF ty_obs.

TYPES: tty_obs TYPE STANDARD TABLE OF ty_obs.

TYPES: BEGIN OF ty_nacc,
n_account TYPE uj_dim_member,
END OF ty_nacc.

TYPES: tty_nacc TYPE STANDARD TABLE OF ty_nacc.

TYPES: BEGIN OF ty_dist_obs,
obs TYPE uj_dim_member,
parent_lv1 TYPE uj_dim_member,
ctr TYPE uj_signeddata,
END OF ty_dist_obs.

TYPES: BEGIN OF ty_dist_fa,
functional_area TYPE uj_dim_member,
parent_lv1 TYPE uj_dim_member,
ctr TYPE uj_signeddata,
END OF ty_dist_fa.

TYPES: BEGIN OF ty_dist_obs_fa,
obs TYPE uj_dim_member,
functional_area TYPE uj_dim_member,
parent_lv1 TYPE uj_dim_member,
ctr TYPE uj_signeddata,
END OF ty_dist_obs_fa.

TYPES: tty_dist_obs     TYPE HASHED TABLE OF ty_dist_obs WITH UNIQUE KEY obs parent_lv1,
       tty_dist_fa      TYPE HASHED TABLE OF ty_dist_fa WITH UNIQUE KEY functional_area parent_lv1,
       tty_dist_obs_fa  TYPE HASHED TABLE OF ty_dist_obs_fa WITH UNIQUE KEY obs functional_area parent_lv1.


DATA: lo_struct TYPE REF TO cl_abap_structdescr,
      lo_type TYPE REF TO cl_abap_typedescr,
      ref_obj_struc TYPE REF TO cl_abap_structdescr,
      ref_obj_table TYPE REF TO cl_abap_tabledescr,
      gso_handle TYPE REF TO data,
      gsx_handle TYPE REF TO data,
      gdx_handle TYPE REF TO data,
      gdo_handle TYPE REF TO data.


DATA: lt_comp TYPE cl_abap_structdescr=>component_table, "=TYPE abap_component_tab.
      ls_comp LIKE LINE OF lt_comp.

DATA: ls_rec   TYPE REF TO data,
      ls_test TYPE REF TO data,
      ls_query_ytd TYPE REF TO data,
      ls_bup   TYPE REF TO data,
      ls_ds0   TYPE REF TO data,
      ls_ds2   TYPE REF TO data,
      lt_final TYPE REF TO data,
      lt_ds0   TYPE REF TO data,
      lt_ds2   TYPE REF TO data,
      lt_bup   TYPE REF TO data,
      lt_query_ytd   TYPE REF TO data,
      orig_perc TYPE uj_signeddata,
      base_amount TYPE uj_signeddata,
      orig_amount TYPE uj_signeddata.

FIELD-SYMBOLS:
      <l_alloc_rule>         TYPE any,
      <l_input_for_node>       TYPE any,
      <l_percentage>         TYPE any, "Dimension
      <l_perc_applied>       TYPE any, "Dimension
      <l_signeddata>         TYPE any, "Dimension - holding the value IT is Standard
      <l_origin_data>        TYPE any, "Dimension
      <l_signedtotal>        TYPE any, "Dimension
      <l_result>             TYPE any, "Dimension
      <l_currency>           TYPE any, "Dimension
      <l_flow>               TYPE any, "Dimension
      <l_version>            TYPE any, "Dimension
      <l_product>            TYPE any, "Dimension
      <l_time>               TYPE any, "Dimension
      <l_account>            TYPE any, "Dimension
      <l_n_account>          TYPE any, "Dimension
      <l_category>           TYPE any, "Dimension
      <l_datasrc>            TYPE any, "Dimension
      <l_datasrc2>            TYPE any, "Dimension
      <l_obs>                TYPE any, "Dimension
      <l_obs_mbr>            TYPE any, "Dimension
      <lev1>                 TYPE any,
            <lev2>                 TYPE any,
                  <lev3>                 TYPE any,
                        <lev4>                 TYPE any,
                              <lev5>                 TYPE any,
                                    <lev6>                 TYPE any,
                                          <lev7>                 TYPE any,
                                               <lev8>                 TYPE any,
                                                  <lev9>                 TYPE any,
      <l_entity>             TYPE any, "Dimension
      <l_parent_lv1>         TYPE any, "Dimension
      <l_obs_node>           TYPE any, "Dimension
      <fs_obs_node>           TYPE any, "Dimension
      <l_obs_xnode>          TYPE any, "Dimension
      <l_bu_line>            TYPE any, "Dimension

      <l_figures>            TYPE any, "Dimension

      <l_partner>           TYPE any, "Dimension
      <l_functional_area>    TYPE any, "Dimension
      <l_measures>     TYPE any, "Dimension

      <l_funct_area>         TYPE any, "Dimension
      <l_smart>              TYPE any, "Dimension
      <l_smart2>              TYPE any, "Dimension
      <l_per_qtd_ytd>              TYPE any,
      <ls_rec>               TYPE any,
      <ls_test>               TYPE any,
      <ls_bup>               TYPE any,
      <ls_ds2>               TYPE any,
      <ls_ds0>               TYPE any,
      <fs_struct> TYPE any,
      <fs_xstruct> TYPE any,
      <fs_data_extd>  TYPE table,
      <fs_base_extd> TYPE table,

      <lt_bup>       TYPE STANDARD TABLE,
      <lt_ds0>       TYPE STANDARD TABLE, "total by n_account 8000 and parent_lv1
      <lt_ds2>       TYPE STANDARD TABLE,
      <lt_ds0detail>       TYPE STANDARD TABLE,
      <lt_final>     TYPE STANDARD TABLE,
      <lt_query_ytd>  TYPE STANDARD TABLE.

DATA: lo_appl TYPE REF TO cl_uja_application,
      lt_appl_dim TYPE uja_t_appl_dim,
      ls_appl_dim LIKE LINE OF lt_appl_dim,
      lt_dim_name TYPE ujq_t_dim,
      ls_dim_name LIKE LINE OF lt_dim_name.

DATA: l_log_msg TYPE uj0_s_message,
      l_lines TYPE i,
      l_lines_o TYPE i,
      l_lines_f TYPE i,
      l_lines_fn TYPE numc10,
      l_lines_on TYPE numc10,
      l_lines_t TYPE numc10,

      ld_log TYPE string.

DATA:
*      dim_obs_table TYPE  ddobjname,
*      dim_fa_table TYPE  ddobjname,
      ls_dfies   TYPE dfies,
      lt_dfies   TYPE TABLE OF dfies.


DATA: ls_dist_obs TYPE ty_dist_obs,
      ls_dist_fa  TYPE ty_dist_fa,
      ls_dist_obs_fa TYPE ty_dist_obs_fa.

DATA: lt_dist_obs TYPE tty_dist_obs,
      lt_dist_fa  TYPE tty_dist_fa,
      lt_dist_obs_fa TYPE tty_dist_obs_fa.

DATA:
*      ls_obs_mdata TYPE ty_obs_mdata,
*      lt_obs_mdata TYPE tty_obs_mdata,
      lt_obs TYPE tty_obs.

DATA: wa_param_appl_t    TYPE ujk_s_script_logic_hashentry,
      wa_param_write     TYPE ujk_s_script_logic_hashentry,
      wa_param_debug     TYPE ujk_s_script_logic_hashentry,
      wa_param_abort     TYPE ujk_s_script_logic_hashentry,
      wa_param_smart     TYPE ujk_s_script_logic_hashentry,
      p_smart_fa_node TYPE uj_dim_member,
      wa_param_smart_o   TYPE ujk_s_script_logic_hashentry,
      wa_param_smart_s   TYPE ujk_s_script_logic_hashentry,
      wa_param_version   TYPE ujk_s_script_logic_hashentry,
      wa_param_excloth     TYPE ujk_s_script_logic_hashentry,
      wa_param_product   TYPE ujk_s_script_logic_hashentry,

      wa_param_figures   TYPE ujk_s_script_logic_hashentry,
      wa_param_n_account TYPE ujk_s_script_logic_hashentry,
      wa_param_datasrc   TYPE ujk_s_script_logic_hashentry,
      wa_param_buline    TYPE ujk_s_script_logic_hashentry,
      wa_param_obs_top_node   TYPE ujk_s_script_logic_hashentry,

      p_obs_topnode TYPE uj_dim_member,
      p_bu_line_topnode  TYPE uj_dim_member,
      p_datasrc_topnode   TYPE uj_dim_member,
      p_n_account_topnode   TYPE uj_dim_member,
      p_partner_topnode   TYPE uj_dim_member,
      p_product_topnode   TYPE uj_dim_member,
      p_smart_topnode   TYPE uj_dim_member,

      p_currency TYPE uj_dim_member,
      p_figures TYPE uj_dim_member,
      p_version TYPE uj_dim_member,

      wa_param_fa_top_node   TYPE ujk_s_script_logic_hashentry,
      wa_param_query TYPE ujk_s_script_logic_hashentry,
      p_fa_topnode TYPE uj_dim_member,
      wa_param_obs_2_allocate    TYPE ujk_s_script_logic_hashentry,
      wa_param_obs_for_input    TYPE ujk_s_script_logic_hashentry,
      wa_param_obs_for_basealloc TYPE ujk_s_script_logic_hashentry,
      wa_param_obs_for_fa_base TYPE ujk_s_script_logic_hashentry,
      wa_param_fa_selection TYPE ujk_s_script_logic_hashentry,
      wa_param_check_i TYPE ujk_s_script_logic_hashentry,
      wa_param_partner TYPE ujk_s_script_logic_hashentry,
*     p_partn TYPE uj_dim_member,
*      p_time TYPE uj_dim_member,
      p_cur TYPE uj_dim_member VALUE 'LC',
      wa_strg TYPE string.

DATA: lt_split_obs_2_allocate  TYPE TABLE OF uj_dim_member,
      lt_split_obs_for_input   TYPE tty_obs,
      ls_split_obs_for_input   TYPE ty_obs,
      base_obs_found TYPE uj_dim_member,
      ls_split_obs_2_allocate  TYPE uj_dim_member,
      ls_split_timeselection TYPE uj_dim_member,
      lt_split_timeselection   TYPE TABLE OF uj_dim_member,
      lt_split_n_account_selection   TYPE tty_nacc,
      current_month   TYPE uj_dim_member,
            previous_month   TYPE uj_dim_member,
            cmth_im TYPE n LENGTH 2,
            cmth_iy TYPE n LENGTH 4,
      lt_fa TYPE tty_fa,
      ls_fa TYPE ty_fa,
      off  TYPE i.
DATA:
      lt_hier_name TYPE uja_t_hier_name,
      ls_hier_name TYPE uj_hier_name,
      lt_sel TYPE uj0_t_sel,
      ls_sel TYPE uj0_s_sel,
*      lr_data TYPE REF TO data,
      ls_obs TYPE REF TO data,
      lt_fa_mbr_name TYPE uja_t_dim_member,
      ls_fa_mbr_name TYPE uj_dim_member,
      lt_fa_mbr_node TYPE uja_t_mbr_node,
      ls_fa_mbr_node TYPE uja_s_mbr_node,

      lt_obs_mbr_name TYPE uja_t_dim_member,
      ls_obs_mbr_name TYPE uj_dim_member,
      lt_obs_mbr_node TYPE uja_t_mbr_node,
      ls_obs_mbr_node TYPE uja_s_mbr_node,

      lt_bu_line_mbr_name TYPE uja_t_dim_member,
      ls_bu_line_mbr_name TYPE uj_dim_member,
      lt_bu_line_mbr_node TYPE uja_t_mbr_node,
      ls_bu_line_mbr_node TYPE uja_s_mbr_node,

      lt_datasrc_mbr_name TYPE uja_t_dim_member,
      ls_datasrc_mbr_name TYPE uj_dim_member,
      lt_datasrc_mbr_node TYPE uja_t_mbr_node,
      ls_datasrc_mbr_node TYPE uja_s_mbr_node,

      lt_n_account_mbr_name TYPE uja_t_dim_member,
      ls_n_account_mbr_name TYPE uj_dim_member,
      lt_n_account_mbr_node TYPE uja_t_mbr_node,
      ls_n_account_mbr_node TYPE uja_s_mbr_node,

      lt_partner_mbr_name TYPE uja_t_dim_member,
      ls_partner_mbr_name TYPE uj_dim_member,
      lt_partner_mbr_node TYPE uja_t_mbr_node,
      ls_partner_mbr_node TYPE uja_s_mbr_node,

      lt_product_mbr_name TYPE uja_t_dim_member,
      ls_product_mbr_name TYPE uj_dim_member,
      lt_product_mbr_node TYPE uja_t_mbr_node,
      ls_product_mbr_node TYPE uja_s_mbr_node,

      lt_smart_mbr_name TYPE uja_t_dim_member,
      ls_smart_mbr_name TYPE uj_dim_member,
      lt_smart_mbr_node TYPE uja_t_mbr_node,
      ls_smart_mbr_node TYPE uja_s_mbr_node.

DATA:
lx_cv TYPE ujk_s_cv,
lt_member TYPE uja_t_dim_member,
lx_member TYPE uj_dim_member.


DATA: l_ctr TYPE i,
      l_ctrdo TYPE i.

DATA: lo_uja_dim TYPE REF TO cl_uja_dim.

FIELD-SYMBOLS: <lt_obs> TYPE STANDARD TABLE,
               <ls_obs> TYPE any,
               <ls_obs_mbr_name> TYPE uj_dim_member,
               <ls_fa_mbr_name> TYPE uj_dim_member,
               <ls_obs_mbr_node> TYPE uja_s_mbr_node,
               <ls_fa_mbr_node> TYPE uja_s_mbr_node.

TYPES: BEGIN OF split_obs_type,
  obs TYPE uj_dim_member, "/CPMB/IJDP3S6
END OF split_obs_type.

TYPES: BEGIN OF split_fa_type,
  fa TYPE uj_dim_member, "/CPMB/IJDO575
END OF split_fa_type.

TYPES: BEGIN OF ty_obs_hstruc.
  INCLUDE TYPE rsndi_s_htab.
  INCLUDE TYPE split_obs_type.
TYPES: END OF ty_obs_hstruc.

TYPES: BEGIN OF ty_fa_hstruc.
  INCLUDE TYPE rsndi_s_htab.
  INCLUDE TYPE split_fa_type.
TYPES: END OF ty_fa_hstruc.

DATA:
      ls_messages  TYPE rsndi_s_message,
      lt_messages  TYPE rsndi_t_message,
      lv_maxlevel TYPE rstlevel,
      do_loop TYPE rstlevel,
      ls_subrc TYPE sy-subrc.
DATA:
      do_s_obs TYPE rsndi_s_htabstr,
      do_t_obs TYPE HASHED TABLE OF rsndi_s_htabstr WITH UNIQUE KEY hieid objvers nodeid iobjnm nodename.

DATA: l_t_obs TYPE TABLE OF uj_dim_member,
      l_s_obs TYPE uj_dim_member,
      lo_obsstruct TYPE REF TO cl_abap_structdescr,
      lo_fastruct TYPE REF TO cl_abap_structdescr,
      gdo_obsdata TYPE REF TO data,
      gdo_fadata TYPE REF TO data,
      ref_obj_obsstruc TYPE REF TO cl_abap_structdescr,
      ref_obj_obstable TYPE REF TO cl_abap_tabledescr,
      ref_obj_fastruc TYPE REF TO cl_abap_structdescr,
      ref_obj_fatable TYPE REF TO cl_abap_tabledescr,
      gso_obshandle TYPE REF TO data,
      gdo_obshandle TYPE REF TO data,
      gso_fahandle TYPE REF TO data,
      gdo_fahandle TYPE REF TO data.

DATA: lt_obscomp TYPE cl_abap_structdescr=>component_table, "=TYPE abap_component_tab.
      ls_obscomp LIKE LINE OF lt_comp.

DATA: lt_facomp TYPE cl_abap_structdescr=>component_table, "=TYPE abap_component_tab.
      ls_facomp LIKE LINE OF lt_comp.

DATA: is_param TYPE ujk_s_script_logic_hashentry,
      res_param TYPE ujk_s_script_logic_hashentry,
      req_params TYPE TABLE OF ujk_s_script_logic_hashentry.

FIELD-SYMBOLS:
               <gs_obsstruc>    TYPE any,
               <fs_obsstruct>     TYPE any,
               <fs_obsdata>  TYPE table,
               <fs_obsdata_input>  TYPE table,
               <gs_fastruc>    TYPE any,
               <fs_fastruct>     TYPE any,
               <fs_fadata>  TYPE table.

CONSTANTS: c_sep TYPE char1 VALUE ',',
           c_pattern TYPE char2 VALUE '.I'.
*line type of z_UJO_S_MEMBER
TYPES: BEGIN OF z_ujo_s_member.
      INCLUDE TYPE ujo_s_member.
TYPES: END OF z_ujo_s_member.
*
** table type of z_UJO_T_MEMBERS
TYPES: z_ujo_t_members TYPE STANDARD TABLE OF z_ujo_s_member WITH KEY dimension member.
*
** table type of z_UJO_T_QUERY_DIM
TYPES: z_ujo_t_query_dim TYPE STANDARD TABLE OF z_ujo_t_members.


DATA:
l_appset_id TYPE uj_appset_id VALUE 'TRACTEBEL_GLO',
l_appl_id TYPE uj_appl_id VALUE 'SGA',

lt_dim_list TYPE uja_t_dim_list,
lt_axis TYPE ujo_t_query_dim,
ls_axis_time TYPE z_ujo_t_members,
lt_axis_time TYPE z_ujo_t_query_dim,
ls_axis TYPE ujo_t_members,
ls_member TYPE ujo_s_member,
l_dimname TYPE uj_dim_name,
l_member TYPE uj_dim_member,
ls_slicer TYPE ujo_t_members,
lt_slicer TYPE ujo_t_members,
lo_appl_mgr TYPE REF TO if_uja_application_manager,
lo_sqe TYPE REF TO if_ujo_query,
lr_rec_ytd TYPE REF TO data,
lr_rec_ytd_bup TYPE REF TO data,
lr_rec_mth_bup TYPE REF TO data,
lx_static TYPE REF TO cx_uj_static_check,
lr_data TYPE REF TO data,
lr_data_ytd TYPE REF TO data,
lr_data_mth_bup TYPE REF TO data.

DATA: lt_cv TYPE ujk_t_cv,
      ls_cv TYPE ujk_s_cv.

DATA:
lr_rec TYPE REF TO data,
l_time_cond TYPE string,
lr_key TYPE REF TO data,
lr_hashtable TYPE REF TO data.

DATA: loop_tabix TYPE sy-tabix,
      time_tabix TYPE sy-tabix,
      measures_tabix TYPE sy-tabix,
      lt_selection_dim TYPE uj0_t_sel,
      lx_selection_dim TYPE uj0_s_sel,
      lx_selection TYPE uj0_s_range,
      lt_selection TYPE uj0_t_range.

FIELD-SYMBOLS: <lt_inp_ytd_result> TYPE STANDARD TABLE,
               <lt_inp_mth_result_bup> TYPE STANDARD TABLE,
               <ls_inp_ytd_result>        TYPE any,
               <ls_inp_ytd_result_bup>        TYPE any,
               <ls_inp_mth_result_bup> TYPE any,
               <ls_query_ytd>      TYPE any,
               <ls_cv> TYPE ujk_s_cv,
               <ls_dim_list> TYPE uj_dim_name,
               <ls_axis> TYPE ujo_t_members,
               <ls_axis_time> TYPE z_ujo_t_members.
FIELD-SYMBOLS:
               <lht_data> TYPE HASHED TABLE ,
               <ls_key> TYPE any,
               <ls_member> TYPE ujo_s_member.
"Range Dimension
DEFINE mc_sel_dimension.
      &1-dimension = &2.
      &1-attribute = &3.
      &1-sign = 'I'.
      &1-option = 'EQ'.
      &1-low = &4.
      APPEND &1 TO &5.
    END-OF-DEFINITION.
"Range Model
    DEFINE mc_sel_model.
      CLEAR &1.
      &1-sign = 'I'.
      &1-option = 'EQ'.
      &1-dimension = &2.
      &1-low = &3.
      APPEND &1 TO &4.
    END-OF-DEFINITION.

"Range of Member
    DEFINE mc_sel_range.
      CLEAR &1.
      &1-sign = 'I'.
      &1-option = 'EQ '.
      &1-low = &2.
      APPEND &1 TO &3.
    END-OF-DEFINITION.

DEFINE mc_selection_dim.
CLEAR &1.
&1-sign = 'I'.
&1-option = 'EQ'.
&1-low = &2.
&1-dimension = &3.
APPEND &1 TO &4.
END-OF-DEFINITION.

* START
CLEAR : wa_param_appl_t, wa_param_write, wa_param_debug, is_param, res_param.

READ TABLE it_param WITH TABLE KEY hashkey = 'WRITE' INTO wa_param_write.
READ TABLE it_param WITH TABLE KEY hashkey = 'DEBUG' INTO wa_param_debug.

*endless loop for debugging in sm66_old
DATA: t_debug_flag_start VALUE 'X'.
IF wa_param_debug-hashvalue = 'ON' AND sy-tcode <> 'UJKT'.
DO.
IF t_debug_flag_start = ' '.
EXIT.
ENDIF.
ENDDO.
BREAK-POINT.
ENDIF.
* < endless loop for debugging
*&---------------------------------------------------------------------*
*& GET INPUT MEMBERS FROM DATA MANAGER PROMPTS
*&---------------------------------------------------------------------*
CLEAR: lt_selection_dim.
LOOP AT it_cv INTO lx_cv
WHERE user_specified = 'X'.
lt_member = lx_cv-member.
LOOP AT lt_member INTO lx_member.
mc_selection_dim lx_selection_dim
lx_member lx_cv-dimension lt_selection_dim.
ENDLOOP.
ENDLOOP.

* Check for eventually missing parameters in logic script
  res_param-hashkey = 'WRITE'. APPEND res_param TO req_params.
  res_param-hashkey = 'DEBUG'. APPEND res_param TO req_params.
  res_param-hashkey = 'FA_NODE_DEFINES_SMART'. APPEND res_param TO req_params.
  res_param-hashkey = 'O_SMART'. APPEND res_param TO req_params.
  res_param-hashkey = 'S_SMART'. APPEND res_param TO req_params.
  res_param-hashkey = 'VERSION_T'. APPEND res_param TO req_params.
  res_param-hashkey = 'TOP_OBS_NODE'. APPEND res_param TO req_params.
  res_param-hashkey = 'TOP_FA_NODE'. APPEND res_param TO req_params.
  res_param-hashkey = 'PRODUCT_T'. APPEND res_param TO req_params.
  res_param-hashkey = 'PARTNER_T'. APPEND res_param TO req_params.
  res_param-hashkey = 'FIGURES_T'. APPEND res_param TO req_params.
  res_param-hashkey = 'N_ACCOUNT_T'. APPEND res_param TO req_params.
  res_param-hashkey = 'DATASRC_T'. APPEND res_param TO req_params.
  res_param-hashkey = 'BU_LINE_T'. APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_2_ALLOCATE'. APPEND res_param TO req_params.
  res_param-hashkey = 'TOP_FA_NODE'. APPEND res_param TO req_params.
  res_param-hashkey = 'OBS_FOR_INPUT'. APPEND res_param TO req_params.
  res_param-hashkey = 'BASEALLOC'. APPEND res_param TO req_params.
  res_param-hashkey = 'FA_BASE'. APPEND res_param TO req_params.
  res_param-hashkey = 'FA_SELECT'. APPEND res_param TO req_params.
  res_param-hashkey = 'CHECK_I'. APPEND res_param TO req_params.

CLEAR:   res_param.
LOOP AT req_params INTO res_param.
READ TABLE it_param WITH TABLE KEY hashkey = res_param-hashkey INTO is_param.
IF sy-subrc = 0.
ELSE.
** Raise exception
CONCATENATE 'Missing parameter ' res_param-hashkey 'in Script Logic !!!' INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.
ENDLOOP.

REFRESH: req_params.
* various parameters ex. base functional_area for allocation is passed as a parameter in CUST_BADI
READ TABLE it_param WITH TABLE KEY hashkey = 'FA_NODE_DEFINES_SMART' INTO wa_param_smart. "eg.FS000
READ TABLE it_param WITH TABLE KEY hashkey = 'O_SMART' INTO wa_param_smart_o.
READ TABLE it_param WITH TABLE KEY hashkey = 'S_SMART' INTO wa_param_smart_s.
READ TABLE it_param WITH TABLE KEY hashkey = 'VERSION_T' INTO wa_param_version.
READ TABLE it_param WITH TABLE KEY hashkey = 'TOP_OBS_NODE' INTO wa_param_obs_top_node.
READ TABLE it_param WITH TABLE KEY hashkey = 'TOP_FA_NODE' INTO wa_param_fa_top_node.
READ TABLE it_param WITH TABLE KEY hashkey = 'QUERY' INTO wa_param_query.


MOVE: wa_param_obs_top_node-hashvalue TO p_obs_topnode,
      wa_param_fa_top_node-hashvalue TO p_fa_topnode,
      wa_param_smart-hashvalue TO p_smart_fa_node.

READ TABLE it_param WITH TABLE KEY hashkey = 'PRODUCT_T' INTO wa_param_product.
READ TABLE it_param WITH TABLE KEY hashkey = 'PARTNER_T' INTO wa_param_partner.
READ TABLE it_param WITH TABLE KEY hashkey = 'FIGURES_T' INTO wa_param_figures.
READ TABLE it_param WITH TABLE KEY hashkey = 'N_ACCOUNT_T' INTO wa_param_n_account.
READ TABLE it_param WITH TABLE KEY hashkey = 'DATASRC_T' INTO wa_param_datasrc.
READ TABLE it_param WITH TABLE KEY hashkey = 'BU_LINE_T' INTO wa_param_buline.
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_2_ALLOCATE' INTO wa_param_obs_2_allocate.
IF sy-subrc <> 0.
  CONCATENATE 'Missing Parameter - OBS_2_ALLOCATE' '- Check your Logic Script'
INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

READ TABLE it_param WITH TABLE KEY hashkey = 'BASEALLOC' INTO wa_param_obs_for_basealloc.
READ TABLE it_param WITH TABLE KEY hashkey = 'FA_BASE' INTO wa_param_obs_for_fa_base.
READ TABLE it_param WITH TABLE KEY hashkey = 'FA_SELECT' INTO wa_param_fa_selection.
READ TABLE it_param WITH TABLE KEY hashkey = 'CHECK_I' INTO wa_param_check_i.

REFRESH: lt_split_obs_2_allocate, lt_split_obs_for_input.
SPLIT wa_param_obs_2_allocate-hashvalue AT c_sep INTO TABLE lt_split_obs_2_allocate.
SPLIT wa_param_fa_selection-hashvalue AT c_sep INTO TABLE lt_fa.
*&---------------------------------------------------------------------*
*& Prepare selection to run SQE query lo_sqe->run_axis_query_symm
*&---------------------------------------------------------------------*
*  > selecting using DM context except for
* TIME ? and OBS retrieveing .I base members
READ TABLE it_param WITH TABLE KEY hashkey = 'OBS_FOR_INPUT' INTO wa_param_obs_for_input.
SPLIT wa_param_obs_for_input-hashvalue AT c_sep INTO TABLE lt_split_obs_for_input.

* get the position of MEASURES in ct_data
CREATE DATA ls_test LIKE LINE OF ct_data.
ASSIGN ls_test->* TO <ls_test>.
lo_struct ?= cl_abap_typedescr=>describe_by_data( <ls_test> ).
lt_comp = lo_struct->get_components( ).

READ TABLE lt_comp WITH TABLE KEY name = 'MEASURES' TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
loop_tabix = sy-tabix.
REFRESH: lt_comp.
ENDIF.
* retrieve DM prompts context values in variables with compatible types
  LOOP AT it_cv ASSIGNING <ls_cv>.
*CURRENCY,FIGURES and VERSION : should only have one base member in DM selection
* BU_LINE, DATASRC, PARTNER, PRODUCT, SMART have one parent node while N_ACCOUNT has node and base member
  CASE <ls_cv>-dimension.
    WHEN 'CURRENCY'.
      READ TABLE <ls_cv>-member INTO p_currency INDEX 1.
    WHEN 'FIGURES'.
      READ TABLE <ls_cv>-member INTO p_figures INDEX 1.
    WHEN 'VERSION'.
      READ TABLE <ls_cv>-member INTO p_version INDEX 1.
    WHEN 'BU_LINE'.
      READ TABLE it_cv ASSIGNING <ls_cv> WITH TABLE KEY dim_upper_case = 'BU_LINE'.
      READ TABLE <ls_cv>-member INTO p_bu_line_topnode INDEX 1.
    WHEN 'DATASRC'.
      READ TABLE it_cv ASSIGNING <ls_cv> WITH TABLE KEY dim_upper_case = 'DATASRC'.
      READ TABLE <ls_cv>-member INTO p_datasrc_topnode INDEX 1.
    WHEN  'N_ACCOUNT'.
      READ TABLE it_cv ASSIGNING <ls_cv> WITH TABLE KEY dim_upper_case = 'N_ACCOUNT'.
       LOOP AT <ls_cv>-member INTO p_n_account_topnode
         WHERE table_line <> wa_param_obs_for_basealloc-hashvalue. " excluding 8000
       EXIT.
      ENDLOOP.
    WHEN 'PARTNER'.
      READ TABLE it_cv ASSIGNING <ls_cv> WITH TABLE KEY dim_upper_case = 'PARTNER'.
      READ TABLE <ls_cv>-member INTO p_partner_topnode INDEX 1.
    WHEN 'PRODUCT'.
      READ TABLE it_cv ASSIGNING <ls_cv> WITH TABLE KEY dim_upper_case = 'PRODUCT'.
      READ TABLE <ls_cv>-member INTO p_product_topnode INDEX 1.
    WHEN 'SMART'.
      READ TABLE it_cv ASSIGNING <ls_cv> WITH TABLE KEY dim_upper_case = 'SMART'.
      READ TABLE <ls_cv>-member INTO p_smart_topnode INDEX 1.
    WHEN OTHERS.
  ENDCASE.
  ENDLOOP.

* get the table name for FUNCTIONAL_AREA BPC master_data
SELECT appset_id, dimension, tech_name, data_table
  INTO TABLE @DATA(fa_info)
  FROM uja_dimension
  WHERE appset_id = @i_appset_id AND dimension = 'FUNCTIONAL_AREA'.

  DATA(wa_fa_info) = fa_info[ 1 ].

* get the table name for OBS BPC master_data
SELECT appset_id, dimension, tech_name, data_table
  INTO TABLE @DATA(obs_info)
  FROM uja_dimension
  WHERE appset_id = @i_appset_id AND dimension = 'OBS'.

DATA(wa_obs_info) = obs_info[ 1 ].
* create dynamically internal table to store mdata mixed with hierarchy
CREATE DATA gdo_obsdata TYPE (wa_obs_info-data_table).
ASSIGN gdo_obsdata->* TO <gs_obsstruc>.
CHECK ( <gs_obsstruc> IS ASSIGNED ).

CREATE DATA gdo_fadata TYPE (wa_fa_info-data_table).
ASSIGN gdo_fadata->* TO <gs_fastruc>.
CHECK ( <gs_fastruc> IS ASSIGNED ).

lo_fastruct ?= cl_abap_typedescr=>describe_by_data( <gs_fastruc> ).
lt_facomp = lo_fastruct->get_components( ).

lo_obsstruct ?= cl_abap_typedescr=>describe_by_data( <gs_obsstruc> ).
lt_obscomp = lo_obsstruct->get_components( ).

CLEAR: ls_obscomp,l_lines, l_lines_o, l_lines_f, l_lines_on, l_lines_fn.
REFRESH: lt_dfies.
DESCRIBE TABLE lt_obscomp LINES l_lines_o.
DESCRIBE TABLE lt_facomp LINES l_lines_f.
ADD: 1 TO l_lines_o, 1 TO l_lines_f.
l_lines_fn = l_lines_f.
l_lines_on = l_lines_o.
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
 ls_obscomp-name = 'PARENT_LV9'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV9'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV8'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV8'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV7'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV7'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV6'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV6'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV5'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
  ls_facomp-name = 'PARENT_LV5'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
  ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV4'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV4'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV3'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV3'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV2'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV2'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.

 ls_obscomp-name = 'PARENT_LV1'.
 ls_obscomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_obscomp INTO lt_obscomp INDEX l_lines_o.
 ADD 1 TO l_lines_o.
 ls_facomp-name = 'PARENT_LV1'.
 ls_facomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_facomp INTO lt_facomp INDEX l_lines_f.
 ADD 1 TO l_lines_f.
ENDIF.

ref_obj_obsstruc = cl_abap_structdescr=>get( p_components = lt_obscomp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_obj_obstable ?= cl_abap_tabledescr=>create( ref_obj_obsstruc ).

ref_obj_fastruc = cl_abap_structdescr=>get( p_components = lt_facomp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_obj_fatable ?= cl_abap_tabledescr=>create( ref_obj_fastruc ).

CREATE DATA gso_obshandle TYPE HANDLE ref_obj_obsstruc.
ASSIGN gso_obshandle->* TO <fs_obsstruct>.

CREATE DATA gdo_obshandle TYPE HANDLE ref_obj_obstable.
ASSIGN gdo_obshandle->* TO <fs_obsdata>.

CREATE DATA gdo_obshandle TYPE HANDLE ref_obj_obstable.
ASSIGN gdo_obshandle->* TO <fs_obsdata_input>.

CREATE DATA gso_fahandle TYPE HANDLE ref_obj_fastruc.
ASSIGN gso_fahandle->* TO <fs_fastruct>.

CREATE DATA gdo_fahandle TYPE HANDLE ref_obj_fatable.
ASSIGN gdo_fahandle->* TO <fs_fadata>.

REFRESH: lt_sel, lt_hier_name.
CLEAR: ls_sel, ls_hier_name.
*BREAK BB5827.
* get the hierarchies for OBS and FUNCTIONAL_AREA, and other dimension

IF p_obs_topnode IS NOT INITIAL AND
  p_smart_fa_node IS NOT INITIAL AND
  p_bu_line_topnode IS NOT INITIAL AND
  p_datasrc_topnode IS NOT INITIAL AND
  p_n_account_topnode IS NOT INITIAL AND
  p_partner_topnode IS NOT INITIAL AND
  p_product_topnode IS NOT INITIAL AND
  p_smart_topnode IS NOT INITIAL.

TRY.
* ALL, DEP ( members one level below ), BAS
  CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'BU_LINE'
    i_parent_mbr = p_bu_line_topnode              "TOT_BL
    i_type       = 'BAS'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_bu_line_mbr_name
    et_mbr_node  = lt_bu_line_mbr_node
    .

CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'DATASRC'
    i_parent_mbr = p_datasrc_topnode "PL_PRIM
    i_type       = 'BAS'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_datasrc_mbr_name
    et_mbr_node  = lt_datasrc_mbr_node
        .

CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'N_ACCOUNT'
    i_parent_mbr = p_n_account_topnode "PL_PRIM
    i_type       = 'BAS'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_n_account_mbr_name
    et_mbr_node  = lt_n_account_mbr_node
    .

CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'PARTNER'
    i_parent_mbr = p_partner_topnode "PL_PRIM
    i_type       = 'BAS'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_partner_mbr_name
    et_mbr_node  = lt_partner_mbr_node
    .

CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'PRODUCT'
    i_parent_mbr = p_product_topnode "PL_PRIM
    i_type       = 'BAS'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_product_mbr_name
    et_mbr_node  = lt_product_mbr_node
    .
CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'SMART'
    i_parent_mbr = p_smart_topnode "PL_PRIM
    i_type       = 'BAS'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_smart_mbr_name
    et_mbr_node  = lt_smart_mbr_node
    .
CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'OBS'
    i_parent_mbr = p_obs_topnode
    i_type       = 'ALL'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_obs_mbr_name
    et_mbr_node  = lt_obs_mbr_node
    .
 CATCH cx_uj_static_check .
ENDTRY.
*Functional_area hierarchy thru fa top node as a parameter in Logic Script
TRY.
CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = i_appset_id
    i_dim        = 'FUNCTIONAL_AREA'
    i_parent_mbr = p_fa_topnode
    i_type       = 'ALL'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_fa_mbr_name
    et_mbr_node  = lt_fa_mbr_node
    .
 CATCH cx_uj_static_check .
ENDTRY.

CLEAR: gst_obshier, gst_fahier, l_ctr, l_ctrdo.
* flattening OBS hierarchy
LOOP AT lt_obs_mbr_node INTO ls_obs_mbr_node.
CLEAR: gst_obshier, l_ctrdo.
MOVE-CORRESPONDING ls_obs_mbr_node TO gst_obshier.
ADD 1 TO l_ctr.
gst_obshier-seqnr = l_ctr.
IF ls_obs_mbr_node-tlevel > 2.
lv_maxlevel = ls_obs_mbr_node-tlevel.
SUBTRACT 2 FROM lv_maxlevel.
do_loop = lv_maxlevel.
CHECK do_loop GT 0.
DO do_loop TIMES.
  ADD 1 TO l_ctrdo.
READ TABLE lt_obs_mbr_node ASSIGNING <ls_obs_mbr_node>
WITH KEY member = ls_obs_mbr_node-parent.
IF sy-subrc = 0.
CASE l_ctrdo.
WHEN '1'.
gst_obshier-parent_lv1 = <ls_obs_mbr_node>-parent.
WHEN '2'.
gst_obshier-parent_lv2 = <ls_obs_mbr_node>-parent.
WHEN '3'.
gst_obshier-parent_lv3 = <ls_obs_mbr_node>-parent.
WHEN '4'.
gst_obshier-parent_lv4 = <ls_obs_mbr_node>-parent.
WHEN '5'.
gst_obshier-parent_lv5 = <ls_obs_mbr_node>-parent.
WHEN '6'.
gst_obshier-parent_lv6 = <ls_obs_mbr_node>-parent.
WHEN '7'.
gst_obshier-parent_lv7 = <ls_obs_mbr_node>-parent.
WHEN '8'.
gst_obshier-parent_lv8 = <ls_obs_mbr_node>-parent.
WHEN '9'.
gst_obshier-parent_lv9 = <ls_obs_mbr_node>-parent.
ENDCASE.
ENDIF.
ls_obs_mbr_node-parent = <ls_obs_mbr_node>-parent.
ENDDO.
ENDIF.
APPEND gst_obshier TO gtt_obshier.
ENDLOOP.
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

ELSE.
IF gtt_obshier IS INITIAL.
CONCATENATE 'Specified ' p_obs_topnode ' hierarchy node seems to be empty for OBS' INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

IF gtt_fahier IS INITIAL.
CONCATENATE 'Specified ' p_fa_topnode ' hierarchy node seems to be empty for FUNCTIONAL_AREA' INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.
ENDIF. "Check if Top Node is provided in Script Logic Variable

* get the H name for OBS and FA hierarchy
* predicate LIKE '%H4%' LIKE '%H7%' should not be hardcoded

CLEAR: ls_hier_name, w_pred_obs, w_pred_fa, moff, mlen.
READ TABLE gtt_fahier INTO gst_fahier INDEX 1.
FIND REGEX 'H.' IN gst_fahier-hier_name MATCH OFFSET moff MATCH LENGTH mlen.
CONCATENATE '%' gst_fahier-hier_name+moff(mlen) '%' INTO w_pred_fa.

READ TABLE gtt_obshier INTO gst_obshier INDEX 1.
FIND REGEX 'H.' IN gst_obshier-hier_name MATCH OFFSET moff MATCH LENGTH mlen.
CONCATENATE '%' gst_obshier-hier_name+moff(mlen) '%' INTO w_pred_obs.

SELECT * FROM (wa_obs_info-data_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_obsdata>
WHERE /cpmb/hir LIKE w_pred_obs. "'%H4%'

SELECT * FROM (wa_fa_info-data_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_fadata>
WHERE /cpmb/hir LIKE w_pred_fa. "'%H7%'

* Mixing Master data and Flattened Hierarchy
*FUNCTIONAL_AREA
LOOP AT <fs_fadata> ASSIGNING <fs_fastruct>.
ASSIGN COMPONENT:
'/CPMB/IJDO575' OF STRUCTURE <fs_fastruct> TO <l_functional_area>,
'PARENT_LV1' OF STRUCTURE <fs_fastruct> TO <lev1>,
'PARENT_LV2' OF STRUCTURE <fs_fastruct> TO <lev2>,
'PARENT_LV3' OF STRUCTURE <fs_fastruct> TO <lev3>,
'PARENT_LV4' OF STRUCTURE <fs_fastruct> TO <lev4>,
'PARENT_LV5' OF STRUCTURE <fs_fastruct> TO <lev5>,
'PARENT_LV6' OF STRUCTURE <fs_fastruct> TO <lev6>,
'PARENT_LV7' OF STRUCTURE <fs_fastruct> TO <lev7>,
'PARENT_LV8' OF STRUCTURE <fs_fastruct> TO <lev8>,
'PARENT_LV9' OF STRUCTURE <fs_fastruct> TO <lev9>.
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

LOOP AT <fs_obsdata> ASSIGNING <fs_obsstruct>.
ASSIGN COMPONENT:
'/CPMB/IJDP3S6' OF STRUCTURE <fs_obsstruct> TO <l_obs>,
'PARENT_LV1' OF STRUCTURE <fs_obsstruct> TO <lev1>,
'PARENT_LV2' OF STRUCTURE <fs_obsstruct> TO <lev2>,
'PARENT_LV3' OF STRUCTURE <fs_obsstruct> TO <lev3>,
'PARENT_LV4' OF STRUCTURE <fs_obsstruct> TO <lev4>,
'PARENT_LV5' OF STRUCTURE <fs_obsstruct> TO <lev5>,
'PARENT_LV6' OF STRUCTURE <fs_obsstruct> TO <lev6>,
'PARENT_LV7' OF STRUCTURE <fs_obsstruct> TO <lev7>,
'PARENT_LV8' OF STRUCTURE <fs_obsstruct> TO <lev8>,
'PARENT_LV9' OF STRUCTURE <fs_obsstruct> TO <lev9>.
READ TABLE gtt_obshier INTO gst_obshier WITH TABLE KEY ('MEMBER') = <l_obs>.
IF sy-subrc = 0.
  <lev1> = gst_obshier-parent.
  <lev2> = gst_obshier-parent_lv1.
  <lev3> = gst_obshier-parent_lv2.
  <lev4> = gst_obshier-parent_lv3.
  <lev5> = gst_obshier-parent_lv4.
  <lev6> = gst_obshier-parent_lv5.
  <lev7> = gst_obshier-parent_lv6.
  <lev8> = gst_obshier-parent_lv7.
  <lev9> = gst_obshier-parent_lv8.
ENDIF.
ENDLOOP.

lt_cv[] = it_cv[]. "because it_cv is private and not sortable

SORT lt_cv BY dimension dim_upper_case ASCENDING.
    LOOP AT lt_cv ASSIGNING <ls_cv>.
     l_dimname = <ls_cv>-dimension.
      IF  l_dimname = 'TIME'.
       LOOP AT <ls_cv>-member INTO l_member.
        ls_split_timeselection = l_member.
        INSERT ls_split_timeselection INTO TABLE lt_split_timeselection.
       ENDLOOP.
      ENDIF.
    ENDLOOP.
* from context values : store current month selection as the max and derive previous_month
CLEAR: current_month, previous_month,cmth_iy,cmth_im.

SORT lt_split_timeselection DESCENDING.
READ TABLE lt_split_timeselection INDEX 1 INTO current_month.

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

DESCRIBE TABLE lt_split_obs_2_allocate LINES l_lines_on.
DESCRIBE TABLE lt_split_obs_for_input  LINES l_lines_t.

IF l_lines_on <> l_lines_t.
CONCATENATE 'Number of base members used to allocate is different from number of their OBS parent nodes' l_lines_on 'vs' l_lines_t
INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.
 lt_obs[] = lt_split_obs_for_input[].
* check consistency of alloc_rule for base member - should always end with '.I' c_pattern and their supposed parent node
IF wa_param_check_i-hashvalue = 'Y'.
LOOP AT lt_split_obs_for_input INTO ls_split_obs_for_input.
  base_obs_found = ls_split_obs_for_input-obs.
  FIND c_pattern IN ls_split_obs_for_input MATCH OFFSET off.
  SHIFT ls_split_obs_for_input BY off PLACES.
  FIND FIRST OCCURRENCE OF c_pattern IN ls_split_obs_for_input.
IF sy-subrc = 0.
ELSE.
** Raise exception
CONCATENATE 'Wrong data in base member used to allocate .I expected, but found: ' base_obs_found ' Assess to set DM Parameter CHECK_I = N'
      INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.
ENDLOOP.
ENDIF.
"Creating the incoming data Line structure ( requires QUERY = ON )
CREATE DATA ls_rec LIKE LINE OF ct_data.
ASSIGN ls_rec->* TO <ls_rec>.

IF wa_param_query-hashvalue <> 'ON'.
  CONCATENATE 'Script Logic Query parameter is set to :' wa_param_query-hashvalue 'Please adjust to QUERY = ON in CUST_BADI_SGA_ALLOC.lgf Logic Script ' INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.
* year-to-date

IF lt_fa IS NOT INITIAL AND lt_split_obs_for_input IS NOT INITIAL.
* remove SGA999 used to post analytical revenues
DELETE lt_fa WHERE functional_area = wa_param_obs_for_fa_base-hashvalue.
* check if CT_DATA ( based on scope or data region defined by DM) contains any analytical income records for base allocation
READ TABLE ct_data INTO <ls_rec>
WITH KEY
('FUNCTIONAL_AREA') = wa_param_obs_for_fa_base-hashvalue
('TIME') = current_month.
IF sy-subrc <> 0.
** Raise exception
CONCATENATE 'Inexisting records for functional_area base' wa_param_obs_for_fa_base-hashvalue 'in incoming dataset, period: ' current_month ' ,please assess to run DM package 1_TRFR_ANA_INC_FROM_PRD_2_SGA for period ' current_month INTO ld_log SEPARATED
BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.
READ TABLE ct_data INTO <ls_rec>
WITH KEY ('N_ACCOUNT') = wa_param_obs_for_basealloc-hashvalue
('TIME') = current_month.
IF sy-subrc <> 0.
** Raise exception
CONCATENATE 'Inexisting records for basealloc' wa_param_obs_for_basealloc-hashvalue 'in incoming dataset, period: ' current_month ' ,please assess to run DM package 1_TRFR_ANA_INC_FROM_PRD_2_SGA for period ' current_month INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ENDIF.

"Creating additional incoming data Line structure

CREATE DATA ls_bup LIKE LINE OF ct_data.
ASSIGN ls_bup->* TO <ls_bup>.

CREATE DATA lt_bup LIKE ct_data.
ASSIGN lt_bup->* TO <lt_bup>.

* Group by ct_data on current month: MEASURES is not available as a prompt in DM
ASSIGN COMPONENT:
'TIME' OF STRUCTURE <ls_rec> TO <l_time>,
'MEASURES' OF STRUCTURE <ls_rec> TO <l_per_qtd_ytd>.
BREAK-POINT. "export ct_data as ct_data_by_mth
LOOP AT ct_data INTO <ls_rec>.
  <l_time> = current_month.
  <l_per_qtd_ytd> = 'YTD'.
  COLLECT <ls_rec> INTO <lt_bup>.
ENDLOOP.

REFRESH: ct_data.
BREAK-POINT. "export <lt_bup> as ct_data_ytd
ct_data[] = <lt_bup>[].
REFRESH:<lt_bup>.
REFRESH:<lt_bup>.
* Start - Check Any Running DM package running + Validate DM package selections and parameters
* check if Base members used to post allocation are well described in parenth4 hierarchy
*int table <fs_obsdata> <fs_fadata>
DATA:   gdo_obs_mdata TYPE REF TO data,
        lo_obs_mdata_struct TYPE REF TO cl_abap_structdescr,
        lt_obs_mdata_comp TYPE cl_abap_structdescr=>component_table,
        ref_obj_obs_mdata_struc TYPE REF TO cl_abap_structdescr,
        ref_obj_obs_mdata_table TYPE REF TO cl_abap_tabledescr,
        gso_obs_mdata_handle  TYPE REF TO data,
        gdo_obs_mdata_handle  TYPE REF TO data.

FIELD-SYMBOLS: <gs_obs_mdata_struc>   TYPE any,
               <fs_obs_madata_struct>    TYPE any,
               <fs_obs_mdata_data> TYPE INDEX TABLE.

CREATE DATA gdo_obs_mdata TYPE (wa_obs_info-data_table).
ASSIGN gdo_obs_mdata->* TO <gs_obs_mdata_struc>.
CHECK ( <gs_obs_mdata_struc> IS ASSIGNED ).

lo_obs_mdata_struct ?= cl_abap_typedescr=>describe_by_data( <gs_obs_mdata_struc> ).
lt_obs_mdata_comp = lo_obs_mdata_struct->get_components( ).
DELETE lt_obs_mdata_comp WHERE name = 'MANDT' OR name = 'LANGU' OR name = 'OBJVERS'.

* create dynamic structure and table for descriptions
ref_obj_obs_mdata_struc = cl_abap_structdescr=>get( p_components = lt_obs_mdata_comp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_obj_obs_mdata_table ?= cl_abap_tabledescr=>create( ref_obj_obs_mdata_struc ).

CREATE DATA gso_obs_mdata_handle TYPE HANDLE ref_obj_obs_mdata_struc.
ASSIGN gso_obs_mdata_handle->* TO <fs_obs_madata_struct>.

CREATE DATA gdo_obs_mdata_handle TYPE HANDLE ref_obj_obs_mdata_table.
ASSIGN gdo_obs_mdata_handle->* TO <fs_obs_mdata_data>.

  SELECT * FROM (wa_obs_info-data_table)
  INTO CORRESPONDING FIELDS OF TABLE <fs_obs_mdata_data>
  FOR ALL ENTRIES IN lt_obs
  WHERE /cpmb/ijdp3s6 = lt_obs-obs AND /cpmb/hir NOT LIKE w_pred_obs. "'%H4%' .

IF NOT <fs_obs_mdata_data> IS INITIAL.
READ TABLE <fs_obs_mdata_data> INTO <fs_obs_madata_struct> INDEX 1.
CONCATENATE 'Some Base Members used to post allocation are not in H4 hierarchy' <fs_obs_madata_struct> INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
  EXIT.
ENDIF.
* check if Parent Nodes of Base members used to post allocation are well described in parenth4 hierarchy
REFRESH: lt_obs, <fs_obs_mdata_data>.
lt_obs[] = lt_split_obs_2_allocate[].

SELECT * FROM (wa_obs_info-data_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_obs_mdata_data>
FOR ALL ENTRIES IN lt_obs
WHERE /cpmb/ijdp3s6 = lt_obs-obs AND /cpmb/hir NOT LIKE w_pred_obs. "'%H4%'.

IF NOT <fs_obs_mdata_data> IS INITIAL.
READ TABLE <fs_obs_mdata_data> INTO <fs_obs_madata_struct>  INDEX 1.
CONCATENATE 'Some Parent Nodes of Members used to post allocation are not in H4 hierarchy' <fs_obs_madata_struct> INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
  EXIT.
ENDIF.

* check if other allocation run is running
SELECT *  FROM zbpc_dmpackages
  INTO TABLE @DATA(other_job_running)
  WHERE appset_id = @i_appset_id AND
  app_id = @i_appl_id AND
  sm37_status = 'R' AND
  status = 0 AND
  ( package_id = '2_SGA_ANA_INC_BASED_ALLOCATION').
DESCRIBE TABLE other_job_running LINES l_lines.
IF l_lines > 1.
DATA(wa_other_job_running) = other_job_running[ 1 ].
DATA: wa_procid TYPE string.
wa_procid = wa_other_job_running-wpprocid.
CONCATENATE 'Other Similar' wa_other_job_running-package_desc 'jobs ALREADY running with process_id: ' wa_procid  INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
  EXIT.
ELSE.
* end * Check Any Running DM package running + Validate DM package selections and parameters

"Get list of fields in structure to adjust initial structure of dataset
lo_struct ?= cl_abap_typedescr=>describe_by_data( <ls_rec> ).
lt_comp = lo_struct->get_components( ).

*"Create the int table lt_ds0 for dataset 0 ie. group by parent_lv1 to compute percentage
CREATE DATA lt_ds0 LIKE ct_data.
ASSIGN lt_ds0->* TO <lt_ds0>.

CREATE DATA ls_ds0 LIKE LINE OF ct_data.
ASSIGN ls_ds0->* TO <ls_ds0>.
* Create the int table lt_ds2 for dataset 2 ie. group by functional_area
CREATE DATA lt_ds2 LIKE ct_data.
ASSIGN lt_ds2->* TO <lt_ds2>.

CREATE DATA ls_ds2 LIKE LINE OF ct_data.
ASSIGN ls_ds2->* TO <ls_ds2>.

* hierarchy parenth4 is used for obs
* dim_obs_table = '/1CPMB/IJDP3S6' "this table doesn't have field PARENT_LV1

" Adding one hierarchy level up kinda property to dimension OBS
CALL FUNCTION 'DDIF_FIELDINFO_GET'
  EXPORTING
    tabname              = 'UJA_DIM_APPL'
    fieldname            = 'DEF_INPUT_MBR'
    langu                = sy-langu
 TABLES
   dfies_tab            = lt_dfies
*   FIXED_VALUES         =
 EXCEPTIONS
   not_found            = 1
   internal_error       = 2
   OTHERS               = 3.

   CLEAR ls_comp.
READ TABLE lt_dfies INDEX 1 INTO ls_dfies.
IF sy-subrc <> 0.
* Implement suitable error handling here
ELSE.
 ls_comp-name = 'PARENT_LV1'.
 ls_comp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_comp INTO lt_comp INDEX 1.
ENDIF.
* lt_comp holds now the additional property PARENT_LV1

ref_obj_struc = cl_abap_structdescr=>get( p_components = lt_comp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_obj_table ?= cl_abap_tabledescr=>create( ref_obj_struc ).
* Create 2 working internal tables and assign to Field Symbol
CREATE DATA gso_handle TYPE HANDLE ref_obj_struc.
ASSIGN gso_handle->* TO <fs_struct>.
CREATE DATA gdo_handle TYPE HANDLE ref_obj_table.
ASSIGN gdo_handle->* TO <fs_data_extd>.

CREATE DATA gsx_handle TYPE HANDLE ref_obj_struc.
ASSIGN gsx_handle->* TO <fs_xstruct>.
CREATE DATA gdx_handle TYPE HANDLE ref_obj_table.
ASSIGN gdx_handle->* TO <fs_base_extd>.

REFRESH: lt_dist_obs, lt_dist_fa, lt_dist_obs_fa.
CLEAR: ls_dist_obs, ls_dist_fa, ls_dist_obs_fa,
<ls_rec>, <fs_struct>,<fs_xstruct>.

ASSIGN COMPONENT:
'OBS' OF STRUCTURE <ls_rec> TO <l_entity>,
'TIME' OF STRUCTURE <ls_rec> TO <l_time>,
'FUNCTIONAL_AREA' OF STRUCTURE <ls_rec> TO <l_functional_area>,
'PARENT_LV1' OF STRUCTURE <fs_struct> TO <l_obs_node>,
'PARENT_LV1' OF STRUCTURE <fs_xstruct> TO <l_obs_xnode>.

LOOP AT ct_data INTO <ls_rec>.
*  <l_time> = current_month.
  READ TABLE  <fs_obsdata> INTO <fs_obsstruct> WITH KEY ('/CPMB/IJDP3S6') = <l_entity>.
  IF sy-subrc = 0.
  ASSIGN COMPONENT 'PARENT_LV1' OF STRUCTURE <fs_obsstruct> TO <fs_obs_node>.
    MOVE-CORRESPONDING <ls_rec> TO <fs_struct>.
        MOVE-CORRESPONDING <ls_rec> TO <fs_xstruct>.
   <l_obs_xnode> = <fs_obs_node>.
      <l_obs_node> = <fs_obs_node>.
   COLLECT <fs_struct> INTO <fs_data_extd>. "was append <fs_data_extd> = ct_data with OBS.PARENT_LV1 added
   COLLECT <fs_xstruct> INTO <fs_base_extd>. "idem
* collecting distinct values for N2 ie FA <> SGA999

  MOVE: <l_entity> TO ls_dist_obs-obs, <fs_obs_node> TO ls_dist_obs-parent_lv1, 1 TO ls_dist_obs-ctr.
   MOVE: <l_functional_area> TO ls_dist_fa-functional_area, <fs_obs_node> TO ls_dist_fa-parent_lv1, 1 TO ls_dist_fa-ctr.
     MOVE: <l_entity> TO ls_dist_obs_fa-obs, <fs_obs_node> TO ls_dist_obs_fa-parent_lv1, 1 TO ls_dist_obs_fa-ctr.
      MOVE: <l_functional_area> TO ls_dist_obs_fa-functional_area, <fs_obs_node> TO ls_dist_obs_fa-parent_lv1, 1 TO ls_dist_obs_fa-ctr.
         COLLECT: ls_dist_obs INTO lt_dist_obs,
                  ls_dist_fa  INTO lt_dist_fa,
                  ls_dist_obs_fa INTO lt_dist_obs_fa.
    IF  <l_functional_area> <> wa_param_obs_for_fa_base-hashvalue. "'SGA999'.
    <l_entity> = <fs_obs_node>."wa_mdata_obs-parent_lv1.
    COLLECT <ls_rec> INTO <lt_ds2>. "OBS changed to parent comp_code/parent node and holds total for N2 by FUNCTIONAL_AREA
    ELSE.
    <l_entity> = <fs_obs_node>."wa_mdata_obs-parent_lv1.
    COLLECT <ls_rec> INTO <lt_ds0>. "OBS changed to parent comp_code and holds total for 8000 by FA SGA999
    ENDIF.
    ENDIF.
ENDLOOP.
BREAK-POINT. "export <lt_ds2>
* get the properties of OBS parent node to allocate
LOOP AT lt_dist_obs INTO ls_dist_obs.
ASSIGN COMPONENT: 'OBS' OF STRUCTURE <fs_obsstruct> TO <l_obs_mbr>.
READ TABLE <fs_obsdata> INTO <fs_obsstruct>
WITH KEY ('/CPMB/IJPCTQT') = ls_dist_obs-obs.
IF sy-subrc = 0.
  APPEND <fs_obsstruct> TO <fs_obsdata_input>.
ENDIF.
ENDLOOP.

IF <fs_obsdata> IS INITIAL OR <fs_fadata> IS INITIAL.
ld_log = 'No data to be processed'.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.

ELSE.
CLEAR wa_strg.
CONCATENATE 'FUNCTIONAL_AREA NE ''' wa_param_obs_for_fa_base-hashvalue '''' INTO wa_strg.
* keeping only revenue deleting non SGA999
DELETE <fs_base_extd> WHERE (wa_strg).

ASSIGN COMPONENT:
 'SIGNEDDATA' OF STRUCTURE <ls_ds0> TO <l_signedtotal>,
 'SIGNEDDATA' OF STRUCTURE <fs_struct> TO <l_signeddata>,
 'PARENT_LV1' OF STRUCTURE <fs_struct> TO <l_parent_lv1>.

REFRESH: <fs_data_extd>.
CLEAR:<fs_struct>.
* Now calculating the percentage of total revenue
LOOP AT <fs_base_extd> INTO <fs_struct>.
orig_amount = <l_signeddata>.

READ TABLE <lt_ds0> INTO <ls_ds0> WITH KEY ('OBS') = <l_parent_lv1>."
IF sy-subrc = 0.
<l_signeddata> = orig_amount / <l_signedtotal>.
 APPEND <fs_struct> TO <fs_data_extd>.
 ELSE.

ENDIF.
ENDLOOP.
ASSIGN COMPONENT:
 'OBS' OF STRUCTURE <ls_ds2> TO <l_entity>,
 'FUNCTIONAL_AREA' OF STRUCTURE <ls_ds2> TO <l_functional_area>,
 'SIGNEDDATA' OF STRUCTURE <ls_ds2> TO <l_signeddata>, "Standard
 'PARENT_LV1' OF STRUCTURE <fs_struct> TO <l_parent_lv1>,
 'SIGNEDDATA' OF STRUCTURE <fs_struct> TO <l_percentage>,
 'SIGNEDDATA' OF STRUCTURE <fs_xstruct> TO <l_perc_applied>,
 'FUNCTIONAL_AREA' OF STRUCTURE <fs_xstruct> TO <l_funct_area>,
 'N_ACCOUNT' OF STRUCTURE <fs_xstruct> TO <l_n_account>.

* we apply the percentage calculated in previous step <fs_base_extd> vs <lt_ds0>
REFRESH: <fs_base_extd>.
CLEAR: <ls_ds2>,  <fs_xstruct>.
LOOP AT <lt_ds2> INTO <ls_ds2>.
      orig_amount = <l_signeddata>.
  LOOP AT <fs_data_extd> INTO <fs_struct> WHERE ('PARENT_LV1 = <l_entity>').
      orig_perc = <l_percentage>.
    MOVE-CORRESPONDING <fs_struct> TO <fs_xstruct>.
    <l_perc_applied> = orig_amount * orig_perc.
    <l_funct_area> = <l_functional_area>.
    <l_n_account> = 'N2'.
    APPEND <fs_xstruct> TO <fs_base_extd>.
  ENDLOOP.
ENDLOOP.

ASSIGN COMPONENT:

'FUNCTIONAL_AREA' OF STRUCTURE <fs_struct> TO <l_functional_area>,
'OBS' OF STRUCTURE <fs_struct> TO <l_entity>,
'SIGNEDDATA' OF STRUCTURE <fs_struct> TO <l_perc_applied>,
'SIGNEDDATA' OF STRUCTURE <ls_rec> TO <l_signeddata>,
'SIGNEDDATA' OF STRUCTURE <fs_xstruct> TO <l_result>.
* Deducting the amount to allocate from what is already posted on Functional_Areas
REFRESH: <fs_data_extd>.
CLEAR: <fs_struct>, <fs_xstruct>.
LOOP AT <fs_base_extd> INTO <fs_struct>.
  orig_amount = <l_perc_applied>.
READ TABLE ct_data INTO <ls_rec>
WITH KEY ('FUNCTIONAL_AREA') = <l_functional_area> ('OBS') = <l_entity>." BINARY SEARCH.
IF sy-subrc = 0.
base_amount = <l_signeddata>.
ELSE.
base_amount = 0.
ENDIF.
MOVE-CORRESPONDING <fs_struct> TO <fs_xstruct>.
<l_result> = ( base_amount - orig_amount ).
APPEND <fs_xstruct> TO <fs_data_extd>.
ENDLOOP.
CLEAR: orig_amount.
ASSIGN COMPONENT:
 'VERSION' OF STRUCTURE <fs_xstruct> TO <l_version>, "ACTUALS
 'PRODUCT' OF STRUCTURE <fs_xstruct> TO <l_product>, "TE99
 'PARTNER' OF STRUCTURE <fs_xstruct> TO <l_partner>, "SGA
 'OBS' OF STRUCTURE <fs_xstruct> TO <l_obs>, "derived from id and alloc_rule
 'N_ACCOUNT' OF STRUCTURE <fs_xstruct> TO <l_n_account>, "95000
 'DATASRC' OF STRUCTURE <fs_xstruct> TO <l_datasrc>. "HQ_DEST
ASSIGN COMPONENT:
 'BU_LINE' OF STRUCTURE <fs_xstruct> TO <l_bu_line>, "B9500
 'SMART' OF STRUCTURE <fs_xstruct> TO <l_smart>, "62694 or 62695
 'FUNCTIONAL_AREA' OF STRUCTURE <fs_xstruct> TO <l_functional_area>.

* Defines the target members for the posting
* based on DM package params passed to BADi / derive from some property...
CLEAR: <fs_xstruct>.
REFRESH: <fs_base_extd>.

LOOP AT <fs_data_extd> INTO <fs_xstruct>.
* we check if any parent member of a given functional_area is FS000 or DM manager parameter FA_NODE_DEFINES_SMART
** to clean READ TABLE lt_fa_h7 INTO ls_fa_h7 WITH KEY ('ID') = <l_functional_area>.
READ TABLE gtt_fahier INTO gst_fahier WITH KEY ('MEMBER') = <l_functional_area>.
IF sy-subrc = 0.
IF   gst_fahier-parent = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv1 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv2 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv3 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv4 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv5 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv6 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv7 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv8 = wa_param_smart-hashvalue
  OR gst_fahier-parent_lv9 = wa_param_smart-hashvalue.
  <l_smart> = wa_param_smart_s-hashvalue. "'62694'.
ELSE.
  <l_smart> = wa_param_smart_o-hashvalue. "'62695'.
ENDIF.
ENDIF.
*Look-up in OBS BPC master data table to post on .I OBS base member attached to OBS node to allocate
* selecting obs input member via property 'ALLOC_RULE') = <l_obs>.
ASSIGN COMPONENT '/CPMB/IJDP3S6' OF STRUCTURE <fs_obsstruct> TO <l_obs_mbr>.
READ TABLE <fs_obsdata_input> INTO <fs_obsstruct> WITH KEY ('/CPMB/IJPCTQT') = <l_obs>.

IF sy-subrc = 0.
  <l_obs> = <l_obs_mbr>.
ENDIF.
<l_n_account>  = wa_param_n_account-hashvalue.
<l_datasrc>    = wa_param_datasrc-hashvalue.
<l_bu_line>    = wa_param_buline-hashvalue.
<l_partner>    = wa_param_partner-hashvalue.
<l_product>    = wa_param_product-hashvalue.
<l_version>    = wa_param_version-hashvalue.
APPEND <fs_xstruct> TO <fs_base_extd>.
ENDLOOP.
CLEAR: l_lines, l_lines_on.

*ct_data = CORRESPONDING #( <fs_base_extd> EXCEPT PARENT_LV1 ).
DESCRIBE TABLE <fs_base_extd> LINES  l_lines.
l_lines_on = l_lines.
REFRESH: ct_data.
CLEAR:<fs_xstruct>, <ls_rec>.

IF <fs_base_extd> IS INITIAL.
ld_log = 'No final data to be processed'.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
EXIT.
ELSE.

CONCATENATE 'Wrote' l_lines_on 'lines to CT_DATA for model' i_appl_id '.' INTO ld_log SEPARATED BY space.
* reset MEASURE to PERIODIC
ASSIGN COMPONENT:
 'MEASURES' OF STRUCTURE <fs_xstruct> TO <l_per_qtd_ytd>,
 'SIGNEDDATA' OF STRUCTURE <fs_xstruct> TO  <l_signeddata>. "added
LOOP AT <fs_base_extd> INTO <fs_xstruct>.
<l_per_qtd_ytd> = 'PERIODIC'.
<l_signeddata> = <l_signeddata> * -1. "added
 IF <l_signeddata> <> 0.
 MOVE-CORRESPONDING <fs_xstruct> TO <ls_rec>.
 APPEND <ls_rec> TO ct_data.
 ENDIF.
ENDLOOP.
BREAK-POINT. "export ct_data as posted_0x
IF ct_data IS INITIAL.
  CONCATENATE 'No additionnal allocation postings to be processed for period: ' current_month  INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
ELSE.
DESCRIBE TABLE ct_data LINES l_lines.
CONCATENATE 'Wrote' l_lines_on 'lines to CT_DATA for model' i_appl_id '.' INTO ld_log SEPARATED BY space.
ENDIF.
REFRESH: <fs_base_extd>, lt_dist_obs, lt_dist_obs_fa, lt_dist_fa, <fs_obsdata_input>, <fs_obsdata>, <fs_fadata>.
ENDIF.


ENDIF. " check lt_obs_h4 or lt_fa_h7 is empty

ENDIF. "check other BPC jobs running
ENDIF.
* set WRITE = ON in logic script to write SGA model
IF wa_param_write-hashvalue = 'OFF'.

IF wa_param_debug-hashvalue = 'ON'.
CONCATENATE 'When Write is OFF WITH debugging: ' wa_param_debug-hashvalue ' No records are inserted in Model' INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
 EXIT.
ELSE.
  CONCATENATE 'Even WITH debugging: ' wa_param_debug-hashvalue ' No records are inserted in Model When Write is OFF' INTO ld_log SEPARATED BY space.
cl_ujk_logger=>log( i_object = ld_log ).
RAISE EXCEPTION TYPE cx_uj_custom_logic.
  EXIT.
ENDIF.

REFRESH:ct_data. "useless exit before...
ENDIF.

ENDMETHOD.



* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BPC_ALLOC_SGA=>IF_UJ_CUSTOM_LOGIC~INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_APPSET_ID                    TYPE        UJ_APPSET_ID
* | [--->] I_APPL_ID                      TYPE        UJ_APPL_ID
* | [--->] IT_PARAM                       TYPE        UJK_T_SCRIPT_LOGIC_HASHTABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_UJ_CUSTOM_LOGIC~INIT.
  endmethod.
ENDCLASS.
