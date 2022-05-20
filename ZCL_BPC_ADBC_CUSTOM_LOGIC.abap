class ZCL_BPC_ADBC_CUSTOM_LOGIC definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_UJ_CUSTOM_LOGIC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BPC_ADBC_CUSTOM_LOGIC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BPC_ADBC_CUSTOM_LOGIC=>IF_UJ_CUSTOM_LOGIC~CLEANUP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_UJ_CUSTOM_LOGIC~CLEANUP.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BPC_ADBC_CUSTOM_LOGIC->IF_UJ_CUSTOM_LOGIC~EXECUTE
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
     DATA:
      wa_param_debug     TYPE ujk_s_script_logic_hashentry.
     DATA :
      lo_sql_stmt TYPE REF TO cl_sql_statement,
      lo_conn     TYPE REF TO cl_sql_connection,
      lo_result   TYPE REF TO cl_sql_result_set,
      lv_sql      TYPE string,
      lx_sql_exc  TYPE REF TO cx_sql_exception,
      lv_text     TYPE string,
      lv_conn     TYPE dbcon-con_name,
      es_message  LIKE LINE OF et_message,
          ls_application      TYPE uja_s_application,
           lo_appl_mgr         TYPE REF TO if_uja_application_manager,
           ls_dimensions       TYPE uja_s_dimension,
           lt_dim_list         TYPE uja_t_dim_list,
           wa_dim_list         LIKE LINE OF lt_dim_list,
           lr_data TYPE REF TO DATA.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.
*endless loop for debugging in sm66_old
* START
CLEAR : wa_param_debug.
READ TABLE it_param WITH TABLE KEY hashkey = 'DEBUG' INTO wa_param_debug.

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
*Check if SQL is passed through script
* SQL columns must have 'PERIODIC' AS MEASURES or 'YTD' AS MEASURES, (between FUNCTIONAL_AREA and N_ACCOUNT )
    READ TABLE it_param INTO DATA(wa_param)
    WITH KEY hashkey = 'SQL'.
    CHECK sy-subrc = 0.

    TRY.
        lv_conn = 'DEFAULT'.
        lv_sql = wa_param-hashvalue.
* not required if HANA dbms, connection to HANA implicit        
*        lo_conn = cl_sql_connection=>get_connection( lv_conn ).
*        lo_sql_stmt = lo_conn->create_statement( ).
*        lo_result = lo_sql_stmt->execute_query( lv_sql ).
        lo_result = NEW cl_sql_statement( )->execute_query( lv_sql ).
        GET REFERENCE OF CT_DATA INTO lr_data.
        lo_result->set_param_table( lr_data ).
        lo_result->next_package( ).
      CATCH cx_sql_exception INTO lx_sql_exc.
        IF lx_sql_exc IS NOT INITIAL.
          lv_text = lx_sql_exc->get_text( ).
*        MESSAGE lv_text TYPE 'E'.
          es_message-recno = 1.
          es_message-msgno = '147'.
          es_message-msgid = 'UJ0_EXCEPTION'.
          es_message-msgty = 'E'.
          es_message-msgv1 = lv_text.
          APPEND es_message TO et_message.
          RETURN.
        ENDIF.


    ENDTRY.
* template for SQL string (maximum 65535 continuous SQL string without CRLF character )
* CTE is used in this template
*START_BADI CALL_SQL
QUERY=OFF
WRITE=ON
DEBUG=OFF
SQL=WITH CTE_SGA AS (SELECT BU_LINE,CURRENCY,DATASRC,FIGURES,FUNCTIONAL_AREA,'PERIODIC' AS MEASURES, N_ACCOUNT,OBS,PARTNER,PRODUCT,SMART,TIME,VERSION,SIGNED_DATA AS SIGNEDDATA FROM <CDS_DDIC_VIEW_ON TOP_OF_BPC_MODEL> WHERE OBS = 'ENTITY0123' AND TIME_YEAR = '2022' AND BASE_PERIOD = '12')SELECT * FROM CTE_SGA

*END_BADI 
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BPC_ADBC_CUSTOM_LOGIC=>IF_UJ_CUSTOM_LOGIC~INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_APPSET_ID                    TYPE        UJ_APPSET_ID
* | [--->] I_APPL_ID                      TYPE        UJ_APPL_ID
* | [--->] IT_PARAM                       TYPE        UJK_T_SCRIPT_LOGIC_HASHTABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
ENDCLASS.
