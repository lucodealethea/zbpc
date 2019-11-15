*&---------------------------------------------------------------------*
*& Report ZBPC_BROWSE_HIER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbpc_browse_hier.
TABLES: uja_dimension.


PARAMETERS: s_dim LIKE uja_dimension-dimension OBLIGATORY." DEFAULT 'OBS'.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE TEXT-opt.
PARAMETERS:
plugin TYPE rshienm MODIF ID uid,
p_hnode TYPE  uj_dim_member OBLIGATORY,
p_appset TYPE uj_appset_id DEFAULT 'TRACTEBEL_GLO'.
SELECTION-SCREEN END OF BLOCK opt.

TYPES: BEGIN OF t_nodes,
         object TYPE rsshnodenamestr,
         text   TYPE ko100-text,
       END OF t_nodes.

TYPES: BEGIN OF t_plugin,
         object TYPE rshienm,
         text   TYPE ko100-text,
       END OF t_plugin.

DATA: p_dim TYPE uj_dim_name.

DATA: pluginline          TYPE t_plugin,
      pluginlist          TYPE TABLE OF t_plugin,
      nodesline TYPE t_nodes,
      nodeslist TYPE TABLE OF t_nodes.

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

TYPES: BEGIN OF g_t_dimhier.
  INCLUDE TYPE uja_s_mbr_node.
  INCLUDE TYPE l_t_unfold.
TYPES: END OF g_t_dimhier.


DATA: gst_dimhier TYPE g_t_dimhier,
      gtt_dimhier TYPE STANDARD TABLE OF g_t_dimhier WITH KEY member.

DATA: ls_uja_dim_attr_name TYPE uj_attr_name.

DATA:
      ls_dfies   TYPE dfies,
      lt_dfies   TYPE TABLE OF dfies.

DATA:
      gdo_dimdata  TYPE REF TO data,
      gdo_descdata  TYPE REF TO data,
      lo_dimstruct TYPE REF TO cl_abap_structdescr,
      lo_descstruct TYPE REF TO cl_abap_structdescr,
      l_lines TYPE i,
      n_lines TYPE i,
      m3           TYPE string,
      wa_strg TYPE string.

DATA: lt_dimcomp        TYPE cl_abap_structdescr=>component_table, "TYPE abap_component_tab,
      ls_dimcomp        LIKE LINE OF lt_dimcomp,
      lt_desccomp        TYPE cl_abap_structdescr=>component_table, "TYPE abap_component_tab,
      ls_desccomp        LIKE LINE OF lt_desccomp,
      ref_obj_dimstruc  TYPE REF TO cl_abap_structdescr,
      ref_obj_dimtable TYPE REF TO cl_abap_tabledescr,
      ref_obj_descstruc  TYPE REF TO cl_abap_structdescr,

      ref_obj_desctable TYPE REF TO cl_abap_tabledescr,
      gso_dimhandle TYPE REF TO data,
gso_deschandle  TYPE REF TO data,
      gso_wadimhandle TYPE REF TO data,
      gdo_dimhandle TYPE REF TO data,
gdo_deschandle TYPE REF TO data,
      ref_objx_dimstruc  TYPE REF TO cl_abap_structdescr,
      ref_objx_dimtable TYPE REF TO cl_abap_tabledescr,
      gsox_dimhandle TYPE REF TO data,
      gdox_dimhandle TYPE REF TO data.
DATA:
      lt_dim_mbr_name TYPE uja_t_dim_member,
      ls_dim_mbr_name TYPE uj_dim_member,
      lt_dim_mbr_node TYPE uja_t_mbr_node,
      ls_dim_mbr_node TYPE uja_s_mbr_node.

DATA:
      lt_dim_mbr_nameh TYPE uja_t_dim_member,
      ls_dim_mbr_nameh TYPE uj_dim_member,
      lt_dim_mbr_nodeh TYPE uja_t_mbr_node,
      ls_dim_mbr_nodeh TYPE uja_s_mbr_node,
      wa_p TYPE uj_dim_member.

DATA: top_missing TYPE uj_dim_member.

DATA: l_ctr TYPE i,
      l_ctrdo TYPE i,
      lv_maxlevel TYPE rstlevel,
      do_loop TYPE rstlevel.
DATA:
      w_pred_dim TYPE char4,
      ls_hier_name TYPE uj_hier_name,
      moff TYPE i,
      mlen TYPE i,
      wa_dim_tech_name TYPE uj_tech_name,
      hir TYPE c LENGTH 2.


DATA:       ls_test TYPE REF TO data,
            l_title            TYPE sy-title.

  DATA:
    l_hieid         TYPE rshieid,
    l_t_rshiedir    TYPE rshi_t_hiedir,
    l_s_rshiedirtxt TYPE rshi_s_hiedirtxt,
    l_t_rshiedirtxt TYPE rshi_t_hiedirtxt,
    lt_rootnodes TYPE rssh_t_htab,
    ls_rootnodes TYPE rssh_s_htab.

  DATA :
    t_field TYPE STANDARD TABLE OF dynpread, "WITH HEADER LINE, <<comment
    s_field TYPE dynpread,
    scr_name TYPE d020s-dnum.

FIELD-SYMBOLS:   <ls_test> TYPE any,
                 <gd_fld>      TYPE any,
                 <gd_fld2>      TYPE any.

FIELD-SYMBOLS: <gs_dimstruc>    TYPE any,
               <gs_descstruc>   TYPE any,
               <fs_dimstruct>     TYPE any,
               <fs_descstruct>    TYPE any,
               <fsx_dimstruct>     TYPE any,
               <l_dim>                TYPE any, "Dimension
               <l_text>                TYPE any, "Dimension
               <ls_dim_mbr_node> TYPE uja_s_mbr_node,
               <fs_dimdata>  TYPE table,
               <fs_descdata> TYPE INDEX TABLE,
               <fsx_dimdata>  TYPE table,
      <l_fieldname> TYPE any,
      <l_objvers> TYPE any,
      <lev1>                 TYPE any,
            <lev2>                 TYPE any,
                  <lev3>                 TYPE any,
                        <lev4>                 TYPE any,
                              <lev5>                 TYPE any,
                                    <lev6>                 TYPE any,
                                          <lev7>                 TYPE any,
                                               <lev8>                 TYPE any,
                                                   <lev9>                 TYPE any,
      <l_desc> TYPE any,
      <ls_dimcomp>        LIKE LINE OF lt_dimcomp.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR plugin.
s_field-fieldname = 'S_DIM'.
APPEND s_field TO t_field.
CLEAR s_field.
scr_name = sy-dynnr .
CALL FUNCTION 'DYNP_VALUES_READ'
EXPORTING
dyname = sy-repid
dynumb = scr_name
TABLES
dynpfields = t_field[].
READ TABLE t_field INTO s_field WITH KEY fieldname = 'S_DIM'.
p_dim = s_field-fieldvalue.
  REFRESH pluginlist.
  PERFORM f_get_hier_text TABLES pluginlist USING p_dim.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJECT'
      window_title    = 'Available Hierarchies'(inp)
      dynpprog        = sy-repid
      dynpnr          = '2000'
      dynprofield     = 'PLUGIN'
      value_org       = 'S'
    TABLES
      value_tab       = pluginlist
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

REFRESH t_field.
s_field-fieldname = 'PLUGIN'.
APPEND s_field TO t_field.
CLEAR s_field.
scr_name = sy-dynnr .
CALL FUNCTION 'DYNP_VALUES_READ'
EXPORTING
dyname = sy-repid
dynumb = scr_name
TABLES
dynpfields = t_field[].
READ TABLE t_field INTO s_field WITH KEY fieldname = 'PLUGIN'.
IF sy-subrc = 0.
READ TABLE l_t_rshiedirtxt INTO l_s_rshiedirtxt WITH KEY hienm = s_field-fieldvalue.
IF sy-subrc = 0.
  READ TABLE lt_rootnodes INTO ls_rootnodes WITH KEY hieid = l_s_rshiedirtxt-hieid.
    MOVE:
    ls_rootnodes-nodename TO nodesline-object,
    ls_rootnodes-nodename TO wa_p.
   APPEND nodesline TO nodeslist.
ENDIF.
ENDIF.
REFRESH: lt_dim_mbr_name, lt_dim_mbr_node.

TRY.
CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = p_appset
    i_dim        = p_dim
    i_parent_mbr = wa_p
    i_type       = 'ALL'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_dim_mbr_name
    et_mbr_node  = lt_dim_mbr_node
    .
 CATCH cx_uj_static_check .
ENDTRY.
REFRESH:nodeslist.

LOOP AT lt_dim_mbr_node INTO ls_dim_mbr_node WHERE hier_name = s_field-fieldvalue.
MOVE ls_dim_mbr_node-parent TO nodesline-object.
COLLECT nodesline INTO nodeslist.
ENDLOOP.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_hnode.

IF nodeslist IS INITIAL OR s_dim IS INITIAL.
  s_field-fieldname = 'S_DIM'.
APPEND s_field TO t_field.
CLEAR s_field.
scr_name = sy-dynnr .
CALL FUNCTION 'DYNP_VALUES_READ'
EXPORTING
dyname = sy-repid
dynumb = scr_name
TABLES
dynpfields = t_field[].
READ TABLE t_field INTO s_field WITH KEY fieldname = 'S_DIM'.
p_dim = s_field-fieldvalue.

  REFRESH pluginlist.
  PERFORM f_get_hier_text TABLES pluginlist USING p_dim.
  REFRESH t_field.
s_field-fieldname = 'PLUGIN'.
APPEND s_field TO t_field.
CLEAR s_field.
scr_name = sy-dynnr .
CALL FUNCTION 'DYNP_VALUES_READ'
EXPORTING
dyname = sy-repid
dynumb = scr_name
TABLES
dynpfields = t_field[].

READ TABLE t_field INTO s_field WITH KEY fieldname = 'PLUGIN'.

READ TABLE l_t_rshiedirtxt INTO l_s_rshiedirtxt WITH KEY hienm = s_field-fieldvalue.

  READ TABLE lt_rootnodes INTO ls_rootnodes WITH KEY hieid = l_s_rshiedirtxt-hieid.
    MOVE:
    ls_rootnodes-nodename TO nodesline-object,
    ls_rootnodes-nodename TO wa_p.

REFRESH: lt_dim_mbr_name, lt_dim_mbr_node.

TRY.
CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = p_appset
    i_dim        = p_dim
    i_parent_mbr = wa_p
    i_type       = 'ALL'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_dim_mbr_name
    et_mbr_node  = lt_dim_mbr_node
    .
 CATCH cx_uj_static_check .
ENDTRY.
REFRESH:nodeslist.

LOOP AT lt_dim_mbr_node INTO ls_dim_mbr_node WHERE hier_name = s_field-fieldvalue.
MOVE ls_dim_mbr_node-parent TO nodesline-object.
COLLECT nodesline INTO nodeslist.
ENDLOOP.

PERFORM feed_nodes_and_desc USING nodeslist. "changing nodeslist.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJECT'
      window_title    = 'Available Nodes'(inh)
      dynpprog        = sy-repid
      dynpnr          = '2000'
      dynprofield     = 'P_HNODE'
      value_org       = 'S'
    TABLES
      value_tab       = nodeslist
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ELSE.
  REFRESH: nodeslist.
ENDIF.


START-OF-SELECTION.

IF plugin IS INITIAL OR p_hnode IS INITIAL OR p_dim IS INITIAL OR s_dim IS INITIAL.
ENDIF.

p_dim = s_dim.
*BREAK-POINT.
* get the table name for BPC dimension master data
SELECT appset_id, dimension, tech_name, data_table, desc_table
  INTO TABLE @DATA(dimx_info)
  FROM uja_dimension
  WHERE appset_id = @p_appset AND dimension = @p_dim.

TRY.
DATA(wax_dim_info) = dimx_info[ 1 ].
    CATCH cx_sy_itab_line_not_found INTO DATA(itab_line_not_found).
ENDTRY.

* create dynamically internal table to store desc-txtlg
CREATE DATA gdo_descdata TYPE (wax_dim_info-desc_table).
ASSIGN gdo_descdata->* TO <gs_descstruc>.
CHECK ( <gs_descstruc> IS ASSIGNED ).

lo_descstruct ?= cl_abap_typedescr=>describe_by_data( <gs_descstruc> ).
lt_desccomp = lo_descstruct->get_components( ).
DELETE lt_desccomp WHERE name = 'MANDT' OR name = 'LANGU' OR name = 'OBJVERS'.

* create dynamically internal table to store mdata mixed with hierarchy
CREATE DATA gdo_dimdata TYPE (wax_dim_info-data_table).
ASSIGN gdo_dimdata->* TO <gs_dimstruc>.
CHECK ( <gs_dimstruc> IS ASSIGNED ).

lo_dimstruct ?= cl_abap_typedescr=>describe_by_data( <gs_dimstruc> ).
lt_dimcomp = lo_dimstruct->get_components( ).
DELETE lt_dimcomp WHERE name = 'MANDT'.
CLEAR: ls_dimcomp,l_lines.
REFRESH: lt_dfies.
DESCRIBE TABLE lt_dimcomp LINES l_lines.

ADD: 1 TO l_lines.

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
 ls_dimcomp-name = 'PARENT_LV1'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV2'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV3'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV4'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV5'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV6'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV7'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV8'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

 ls_dimcomp-name = 'PARENT_LV9'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
 ADD 1 TO l_lines.

ENDIF.

REFRESH: lt_dfies.
DESCRIBE TABLE lt_dimcomp LINES l_lines.
ADD: 1 TO l_lines.
* now adding TXTLG
CALL FUNCTION 'DDIF_FIELDINFO_GET'
  EXPORTING
    tabname              = 'RSTHIERNODE'
    fieldname            = 'TXTLG'
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
 ls_dimcomp-name = 'TXTLG'.
 ls_dimcomp-type = cl_abap_elemdescr=>get_string( ).
 INSERT ls_dimcomp INTO lt_dimcomp INDEX l_lines.
ENDIF.

* create dynamic structure and table for mdata mixed
ref_obj_dimstruc = cl_abap_structdescr=>get( p_components = lt_dimcomp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_obj_dimtable ?= cl_abap_tabledescr=>create( ref_obj_dimstruc ).

CREATE DATA gso_dimhandle TYPE HANDLE ref_obj_dimstruc.
ASSIGN gso_dimhandle->* TO <fs_dimstruct>.

CREATE DATA gdo_dimhandle TYPE HANDLE ref_obj_dimtable.
ASSIGN gdo_dimhandle->* TO <fs_dimdata>.

* create dynamic structure and table for descriptions
ref_obj_descstruc = cl_abap_structdescr=>get( p_components = lt_desccomp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_obj_desctable ?= cl_abap_tabledescr=>create( ref_obj_descstruc ).

CREATE DATA gso_deschandle TYPE HANDLE ref_obj_descstruc.
ASSIGN gso_deschandle->* TO <fs_descstruct>.

CREATE DATA gdo_deschandle TYPE HANDLE ref_obj_desctable.
ASSIGN gdo_deschandle->* TO <fs_descdata>.

* get hierarchy tree
REFRESH: lt_dim_mbr_name, lt_dim_mbr_node.
TRY.
CALL METHOD cl_ujk_model=>get_children
  EXPORTING
    i_appset_id  = p_appset
    i_dim        = p_dim
    i_parent_mbr = p_hnode
    i_type       = 'ALL'
    if_self      = abap_true
  IMPORTING
    et_bas_list  = lt_dim_mbr_name
    et_mbr_node  = lt_dim_mbr_node
    .
 CATCH cx_uj_static_check .
ENDTRY.

* save eventual missing ( despite if_self = true ) top node when intermediate browsing
DESCRIBE TABLE lt_dim_mbr_node LINES n_lines.
READ TABLE lt_dim_mbr_node INTO ls_dim_mbr_node INDEX 1.
IF sy-subrc = 0.
top_missing = ls_dim_mbr_node-parent.
ENDIF.
READ TABLE lt_dim_mbr_node INTO ls_dim_mbr_node WITH KEY member = top_missing.
IF sy-subrc <> 0 AND top_missing IS NOT INITIAL.
  ls_dim_mbr_node-member = top_missing.
  ls_dim_mbr_node-parent = ''.
  ls_dim_mbr_node-tlevel = ls_dim_mbr_node-tlevel - 1.
  ls_dim_mbr_node-children_cnt = n_lines.
  INSERT ls_dim_mbr_node INTO lt_dim_mbr_node INDEX 1.
ENDIF.
CLEAR: gst_dimhier, l_ctr, l_ctrdo.
* get the descriptions
SELECT * FROM (wax_dim_info-desc_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_descdata>
WHERE langu = sy-langu AND objvers = 'A'.

* flattening DIM hierarchy
IF  lt_dim_mbr_node IS NOT INITIAL.
LOOP AT lt_dim_mbr_node INTO ls_dim_mbr_node.
CLEAR: gst_dimhier, l_ctrdo.
MOVE-CORRESPONDING ls_dim_mbr_node TO gst_dimhier.
ADD 1 TO l_ctr.
gst_dimhier-seqnr = l_ctr.
IF ls_dim_mbr_node-tlevel > 2.
lv_maxlevel = ls_dim_mbr_node-tlevel.
SUBTRACT 2 FROM lv_maxlevel.
do_loop = lv_maxlevel.
CHECK do_loop GT 0.
DO do_loop TIMES.
  ADD 1 TO l_ctrdo.
READ TABLE lt_dim_mbr_node ASSIGNING <ls_dim_mbr_node>
WITH KEY member = ls_dim_mbr_node-parent.
IF sy-subrc = 0.
CASE l_ctrdo.
WHEN '1'.
gst_dimhier-parent_lv1 = <ls_dim_mbr_node>-parent.
WHEN '2'.
gst_dimhier-parent_lv2 = <ls_dim_mbr_node>-parent.
WHEN '3'.
gst_dimhier-parent_lv3 = <ls_dim_mbr_node>-parent.
WHEN '4'.
gst_dimhier-parent_lv4 = <ls_dim_mbr_node>-parent.
WHEN '5'.
gst_dimhier-parent_lv5 = <ls_dim_mbr_node>-parent.
WHEN '6'.
gst_dimhier-parent_lv6 = <ls_dim_mbr_node>-parent.
WHEN '7'.
gst_dimhier-parent_lv7 = <ls_dim_mbr_node>-parent.
WHEN '8'.
gst_dimhier-parent_lv8 = <ls_dim_mbr_node>-parent.
WHEN '9'.
gst_dimhier-parent_lv9 = <ls_dim_mbr_node>-parent.
ENDCASE.
ENDIF.
IF NOT ls_dim_mbr_node-parent IS INITIAL.
ls_dim_mbr_node-parent = <ls_dim_mbr_node>-parent.
ENDIF.
ENDDO.
ELSE.

ENDIF.

APPEND gst_dimhier TO gtt_dimhier.
ENDLOOP.

* get the H name for dim hierarchy
* predicate LIKE '%H4%' LIKE '%H7%' should not be hardcoded

CLEAR: ls_hier_name, w_pred_dim, moff, mlen, hir.
READ TABLE gtt_dimhier INTO gst_dimhier INDEX 1.
FIND REGEX 'H.' IN gst_dimhier-hier_name MATCH OFFSET moff MATCH LENGTH mlen.
hir = gst_dimhier-hier_name+moff(mlen).
CONCATENATE '%' hir '%' INTO w_pred_dim.

* retrieving properties for dim mdata
SELECT * FROM (wax_dim_info-data_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_dimdata>
WHERE /cpmb/hir LIKE w_pred_dim. "'%H4%'

* Mixing Master data and Flattened Hierarchy

SELECT SINGLE h~dim_tech_name INTO wa_dim_tech_name
  FROM uja_dim_hie_map2 AS h
  INNER JOIN uja_dimension AS di ON di~tech_name = h~dim_tech_name
  WHERE di~appset_id = p_appset AND di~dimension = p_dim.

  LOOP AT <fs_dimdata> ASSIGNING <fs_dimstruct>.
ASSIGN COMPONENT:
wa_dim_tech_name OF STRUCTURE <fs_dimstruct> TO <l_dim>,
'PARENT_LV1' OF STRUCTURE <fs_dimstruct> TO <lev1>,
'PARENT_LV2' OF STRUCTURE <fs_dimstruct> TO <lev2>,
'PARENT_LV3' OF STRUCTURE <fs_dimstruct> TO <lev3>,
'PARENT_LV4' OF STRUCTURE <fs_dimstruct> TO <lev4>,
'PARENT_LV5' OF STRUCTURE <fs_dimstruct> TO <lev5>,
'PARENT_LV6' OF STRUCTURE <fs_dimstruct> TO <lev6>,
'PARENT_LV7' OF STRUCTURE <fs_dimstruct> TO <lev7>,
'PARENT_LV8' OF STRUCTURE <fs_dimstruct> TO <lev8>,
'PARENT_LV9' OF STRUCTURE <fs_dimstruct> TO <lev9>,
'TXTLG'      OF STRUCTURE <fs_dimstruct> TO <l_desc>.
READ TABLE gtt_dimhier INTO gst_dimhier WITH TABLE KEY ('MEMBER') = <l_dim>.
IF sy-subrc = 0.
  <lev1> = gst_dimhier-parent.
  <lev2> = gst_dimhier-parent_lv1.
  <lev3> = gst_dimhier-parent_lv2.
  <lev4> = gst_dimhier-parent_lv3.
  <lev5> = gst_dimhier-parent_lv4.
  <lev6> = gst_dimhier-parent_lv5.
  <lev7> = gst_dimhier-parent_lv6.
  <lev8> = gst_dimhier-parent_lv7.
  <lev9> = gst_dimhier-parent_lv8.
ENDIF.

READ TABLE <fs_descdata> INTO <fs_descstruct> WITH KEY (wa_dim_tech_name) = <l_dim>.
IF sy-subrc = 0.
ASSIGN COMPONENT:
'TXTLG' OF STRUCTURE <fs_descstruct> TO <l_text>.
 <l_desc> = <l_text>.
ENDIF.
*table_line->parent
ENDLOOP.

* create another components internal table with human readable fieldnames
CREATE DATA ls_test LIKE LINE OF lt_dimcomp.
ASSIGN ls_test->* TO <ls_test>.


LOOP AT lt_dimcomp ASSIGNING <ls_test>.
  ASSIGN COMPONENT: 'NAME' OF STRUCTURE <ls_test> TO <l_fieldname>.
  IF <l_fieldname> = wax_dim_info-tech_name.
    <l_fieldname> = wax_dim_info-dimension.
  ELSE.
  SELECT SINGLE attribute_name INTO ls_uja_dim_attr_name
  FROM uja_dim_attr WHERE appset_id = p_appset AND dimension = p_dim AND tech_name = <l_fieldname>.
    IF sy-subrc = 0.
      <l_fieldname> = ls_uja_dim_attr_name.
    ENDIF.
  ENDIF.
ENDLOOP.

ref_objx_dimstruc = cl_abap_structdescr=>get( p_components = lt_dimcomp p_strict = ' ' ). "CALL METHOD cl_abap_structdescr=>create
ref_objx_dimtable ?= cl_abap_tabledescr=>create( ref_objx_dimstruc ).

CREATE DATA gsox_dimhandle TYPE HANDLE ref_objx_dimstruc.
ASSIGN gsox_dimhandle->* TO <fsx_dimstruct>.

CREATE DATA gdox_dimhandle TYPE HANDLE ref_objx_dimtable.
ASSIGN gdox_dimhandle->* TO <fsx_dimdata>.

DESCRIBE TABLE lt_dimcomp LINES l_lines.

LOOP AT <fs_dimdata> ASSIGNING <fs_dimstruct>.
DO l_lines TIMES.
ASSIGN COMPONENT sy-index OF STRUCTURE <fs_dimstruct> TO <gd_fld>.
ASSIGN COMPONENT sy-index OF STRUCTURE <fsx_dimstruct> TO <gd_fld2>.
TRY.
  <gd_fld2> = <gd_fld>.
CATCH:
cx_sy_conversion_no_number,
cx_sy_conversion_overflow,
cx_sy_move_cast_error.        " Error message ?
ENDTRY.
ENDDO.
APPEND <fsx_dimstruct> TO <fsx_dimdata>.
ENDLOOP.
* using OBJVERS TO flag id's withing hierarchy node

LOOP AT <fsx_dimdata> ASSIGNING <fsx_dimstruct>.
  ASSIGN COMPONENT:
'OBJVERS' OF STRUCTURE <fsx_dimstruct> TO <l_objvers>,
'PARENT_LV1' OF STRUCTURE <fsx_dimstruct> TO <lev1>,
'PARENT_LV2' OF STRUCTURE <fsx_dimstruct> TO <lev2>,
'PARENT_LV3' OF STRUCTURE <fsx_dimstruct> TO <lev3>,
'PARENT_LV4' OF STRUCTURE <fsx_dimstruct> TO <lev4>,
'PARENT_LV5' OF STRUCTURE <fsx_dimstruct> TO <lev5>,
'PARENT_LV6' OF STRUCTURE <fsx_dimstruct> TO <lev6>,
'PARENT_LV7' OF STRUCTURE <fsx_dimstruct> TO <lev7>,
'PARENT_LV8' OF STRUCTURE <fsx_dimstruct> TO <lev8>,
'PARENT_LV9' OF STRUCTURE <fsx_dimstruct> TO <lev9>.
  IF
    <lev1> = p_hnode OR
    <lev1> = p_hnode OR
    <lev2> = p_hnode OR
    <lev3> = p_hnode OR
    <lev4> = p_hnode OR
    <lev5> = p_hnode OR
    <lev6> = p_hnode OR
    <lev7> = p_hnode OR
    <lev8> = p_hnode OR
    <lev9> = p_hnode.
    <l_objvers> = 'X'.
  ENDIF.
ENDLOOP.
REFRESH <fs_dimdata>.

CLEAR wa_strg.
CONCATENATE 'OBJVERS NE ''' 'X' '''' INTO wa_strg.
*BREAK-POINT.
DELETE <fsx_dimdata> WHERE (wa_strg).

CONCATENATE 'All Base Members and Sub-Nodes of: ' p_hnode ' for ' p_dim INTO l_title SEPARATED BY space.
PERFORM callback_alv
USING l_title abap_true <fsx_dimdata>.
ENDIF.
FORM callback_alv USING
  VALUE(i_title) TYPE sy-title
  VALUE(i_sort)  TYPE abap_bool
  it_data        TYPE ANY TABLE.

  IF it_data IS INITIAL.
  CONCATENATE 'No data found with : ' p_dim '/' plugin '/' p_hnode INTO m3.
    MESSAGE i799(rsm1) WITH m3.

  ELSE.

    IF i_sort = abap_true.
      SORT it_data.
    ENDIF.

    CALL FUNCTION 'RSDU_CALL_ALV_TABLE'
      EXPORTING
        i_title   = i_title
        i_ta_data = it_data.

  ENDIF.

ENDFORM.

FORM f_get_hier_node TABLES nodeslist USING p_hnode.
DATA: l_iobjnm TYPE  rsiobjnm.
* get the table name for BPC dimension master data
SELECT appset_id, dimension, tech_name, data_table, desc_table
  INTO TABLE @DATA(dim_info)
  FROM uja_dimension
  WHERE appset_id = @p_appset AND dimension = @p_dim.
TRY.
DATA(wa_dim_info) = dim_info[ 1 ].
    CATCH cx_sy_itab_line_not_found INTO DATA(itab_line_not_found).
ENDTRY.
MOVE wa_dim_info-tech_name TO l_iobjnm.
          CALL FUNCTION 'RSSH_GET_HIERARCHIES_OF_IOBJNM'
           EXPORTING
             i_objvers                            = 'A'
             i_iobjnm                             = l_iobjnm "'/CPMB/IJDCE8Q'
             i_langu                              = sy-langu
             i_asso_chabasnm                      = 'X'
             i_update_remote_hier                 = '1'
*             I_POSTFILTER                         = ''
           IMPORTING
*             e_t_rshiedir                         = l_t_rshiedir
*             e_t_rshiedirtxt                      = l_t_rshiedirtxt
             e_t_rootnodes                        = lt_rootnodes
           EXCEPTIONS
             iobj_not_found                       = 1
             hierarchy_not_allowed_for_iobj       = 2
             hierarchy_not_found                  = 3
             internal_error                       = 4
             OTHERS                               = 5
                    .
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ELSE.

            LOOP AT lt_rootnodes INTO ls_rootnodes.
              MOVE:
              ls_rootnodes-nodename TO nodesline-object.
              APPEND nodesline TO nodeslist.
            ENDLOOP.
          ENDIF.
ENDFORM.

 FORM f_get_hier_text TABLES pluginlist USING p_dim.

data: l_iobjnm type  rsiobjnm.
* get the table name for BPC dimension master data
SELECT appset_id, dimension, tech_name, data_table, desc_table
  INTO TABLE @DATA(dim_info)
  FROM uja_dimension
  WHERE appset_id = @p_appset AND dimension = @p_dim.
TRY.
DATA(wa_dim_info) = dim_info[ 1 ].
    CATCH cx_sy_itab_line_not_found INTO DATA(itab_line_not_found).
ENDTRY.
MOVE wa_dim_info-tech_name TO l_iobjnm.
          CALL FUNCTION 'RSSH_GET_HIERARCHIES_OF_IOBJNM'
           EXPORTING
             i_objvers                            = 'A'
             i_iobjnm                             = l_iobjnm "'/CPMB/IJDCE8Q'
             i_langu                              = sy-langu
             i_asso_chabasnm                      = 'X'
             i_update_remote_hier                 = '1'
*             I_POSTFILTER                         = ''
           IMPORTING
*             e_t_rshiedir                         = l_t_rshiedir
             e_t_rshiedirtxt                      = l_t_rshiedirtxt
             e_t_rootnodes                        = lt_rootnodes
           EXCEPTIONS
             iobj_not_found                       = 1
             hierarchy_not_allowed_for_iobj       = 2
             hierarchy_not_found                  = 3
             internal_error                       = 4
             OTHERS                               = 5
                    .
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ELSE.

            LOOP AT l_t_rshiedirtxt INTO l_s_rshiedirtxt.
              MOVE:
              l_s_rshiedirtxt-hienm TO pluginline-object,
              l_s_rshiedirtxt-txtsh TO pluginline-text.
              APPEND pluginline TO pluginlist.
            ENDLOOP.
          ENDIF.

ENDFORM.

FORM feed_nodes_and_desc USING VALUE(f_nodeslist) LIKE nodeslist.

TYPES: BEGIN OF t_timenodes,
         object TYPE rsshnodenamestr,
         text   TYPE ko100-text,
       END OF t_timenodes.

TYPES: BEGIN OF t_technical,
         dimension  TYPE uj_dim_name,
         dim_tech_name TYPE uj_tech_name,
         hier_table TYPE tabname,
       END OF t_technical.

  DATA: ls_technical TYPE t_technical,
        lt_technical TYPE STANDARD TABLE OF t_technical,
        ls_timenodes type t_timenodes,
        lt_timenodes type TABLE OF t_timenodes.

  FIELD-SYMBOLS: <f_nodesline> LIKE LINE OF f_nodeslist.

* get dim technical name along with hierarchy table needed for time
SELECT SINGLE di~dimension h~dim_tech_name h~hier_table INTO CORRESPONDING FIELDS OF ls_technical
  FROM uja_dim_hie_map2 AS h
  INNER JOIN uja_dimension AS di ON di~tech_name = h~dim_tech_name
  WHERE di~appset_id = p_appset AND di~dimension = p_dim.

IF p_dim = 'TIME'.
  REFRESH:f_nodeslist.
SELECT DISTINCT PARENT INTO ls_timenodes
  FROM (ls_technical-hier_table)
  WHERE PARENT <> ''.
  APPEND ls_timenodes TO lt_timenodes.
ENDSELECT.

f_nodeslist[] = lt_timenodes[].
REFRESH:lt_timenodes.
DELETE f_nodeslist WHERE object is INITIAL.
SORT f_nodeslist BY object ASCENDING.
ELSE.

* get the table name for BPC dimension master data
SELECT appset_id, dimension, tech_name, data_table, desc_table
  INTO TABLE @DATA(dimx_info)
  FROM uja_dimension
  WHERE appset_id = @p_appset AND dimension = @p_dim.
  TRY.
DATA(wax_dim_info) = dimx_info[ 1 ].
    CATCH cx_sy_itab_line_not_found INTO DATA(itab_line_not_found).
ENDTRY.

* create dynamically internal table to store desc-txtlg
CREATE DATA gdo_descdata TYPE (wax_dim_info-desc_table).
ASSIGN gdo_descdata->* TO <gs_descstruc>.
CHECK ( <gs_descstruc> IS ASSIGNED ).

lo_descstruct ?= cl_abap_typedescr=>describe_by_data( <gs_descstruc> ).
lt_desccomp = lo_descstruct->get_components( ).
DELETE lt_desccomp WHERE name = 'MANDT' OR name = 'LANGU' OR name = 'OBJVERS'.

* create dynamic structure and table for descriptions
ref_obj_descstruc = cl_abap_structdescr=>get( p_components = lt_desccomp p_strict = ' ' ).
ref_obj_desctable ?= cl_abap_tabledescr=>create( ref_obj_descstruc ).

CREATE DATA gso_deschandle TYPE HANDLE ref_obj_descstruc.
ASSIGN gso_deschandle->* TO <fs_descstruct>.

CREATE DATA gdo_deschandle TYPE HANDLE ref_obj_desctable.
ASSIGN gdo_deschandle->* TO <fs_descdata>.

* get the descriptions
SELECT * FROM (wax_dim_info-desc_table)
INTO CORRESPONDING FIELDS OF TABLE <fs_descdata>
WHERE langu = sy-langu AND objvers = 'A'.
* update the texts
  LOOP AT f_nodeslist ASSIGNING <f_nodesline>.
    ASSIGN COMPONENT: 'TXTLG' OF STRUCTURE <fs_descstruct> TO <l_text>.
    READ TABLE <fs_descdata> INTO <fs_descstruct> WITH KEY (ls_technical-dim_tech_name) = <f_nodesline>-object.
     IF sy-subrc = 0.
      <f_nodesline>-text = <l_text>.
     ENDIF.
  ENDLOOP.
  REFRESH: nodeslist.
ENDIF.

  nodeslist[] = f_nodeslist[].
ENDFORM.
