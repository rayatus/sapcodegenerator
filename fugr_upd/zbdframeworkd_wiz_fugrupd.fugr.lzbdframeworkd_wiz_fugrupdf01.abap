*----------------------------------------------------------------------*
***INCLUDE LZBDFRAMEWORKD_WIZ_FUGRUPDF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHK_NAMESPACE
*&---------------------------------------------------------------------*
FORM chk_namespace
  USING pd_namespace LIKE gs_codegen_data-namespace.

  DATA lo_engine TYPE REF TO zcl_zdbframework_engine_base.

  CREATE OBJECT lo_engine.
  lo_engine->chk_is_namespace_allowed( EXPORTING id_namespace = pd_namespace
                                       EXCEPTIONS OTHERS      = 999 ).
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID      sy-msgid
            TYPE    sy-msgty
            NUMBER  sy-msgno
            WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHK_TABNAME
*&---------------------------------------------------------------------*
FORM chk_tabname
  USING pd_tabname LIKE gs_codegen_data-dbname.

  DATA lo_engine TYPE REF TO zcl_zdbframework_engine_base.

  CREATE OBJECT lo_engine.

  lo_engine->chk_exist_dbname( EXPORTING  id_dbname = pd_tabname
                               EXCEPTIONS OTHERS    = 999 ).
  IF sy-subrc IS INITIAL.
    lo_engine->chk_is_dbname_permitted( EXPORTING  id_dbname = pd_tabname
                                        EXCEPTIONS OTHERS    = 999 ).
  ENDIF.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID      sy-msgid
            TYPE    sy-msgty
            NUMBER  sy-msgno
            WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FUNCTION_INIT_VALUE
*&---------------------------------------------------------------------*
FORM set_function_init_value.

  zcl_zdbframework_engine_updfg=>propose_names(
    EXPORTING
      id_dbname         = gs_codegen_data-dbname
      id_namespace      = gs_codegen_data-namespace
    IMPORTING
      ed_function_group = gs_codegen_data-function_group
      ed_function_s     = gs_codegen_data-function_s
      ed_function_t     = gs_codegen_data-function_t
      ed_ttyp           = gs_codegen_data-table_type
  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHK_FUNCTION_GROUP_NOT_EXIST
*&---------------------------------------------------------------------*
FORM chk_function_group_not_exist
  USING pd_function_group LIKE gs_codegen_data-function_group.


  DATA ld_dummy TYPE string.

  CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
    EXPORTING
      function_pool = pd_function_group
    EXCEPTIONS
      OTHERS        = 999.

  IF sy-subrc IS INITIAL.
    CONCATENATE 'Function group'(005) pd_function_group 'already exists'(007)
      INTO ld_dummy
      SEPARATED BY space ##NO_TEXT.
    MESSAGE ld_dummy TYPE 'E' ##NO_TEXT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHK_FUNCTION_NOT_EXIST
*&---------------------------------------------------------------------*
FORM chk_function_not_exist
  USING pd_function LIKE gs_codegen_data-function_s.

  DATA ld_dummy TYPE string.

  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname = pd_function
    EXCEPTIONS
      OTHERS   = 999.
  IF sy-subrc IS INITIAL.
    CONCATENATE 'Function'(008) pd_function 'already exists'(007)
      INTO ld_dummy
      SEPARATED BY space ##NO_TEXT.
    MESSAGE ld_dummy TYPE 'E' ##NO_TEXT.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHK_TTYP
*&---------------------------------------------------------------------*
FORM chk_ttyp
  USING pd_table_type LIKE gs_codegen_data-table_type.

  DATA lo_engine TYPE REF TO zcl_zdbframework_engine_base.

  CREATE OBJECT lo_engine.

  lo_engine->chk_exists_typedata(
    EXPORTING
      id_typedata         = pd_table_type
    EXCEPTIONS
      OTHERS              = 999
  ).
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID      sy-msgid
            TYPE    sy-msgty
            NUMBER  sy-msgno
            WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
