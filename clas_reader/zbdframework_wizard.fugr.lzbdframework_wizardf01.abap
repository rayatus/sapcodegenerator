*/---------------------------------------------------------------------\
*|   This file is part of SAPCodeGenerator.                            |
*|                                                                     |
*|   ZDBFRAMEWORK is free software; you can redistribute it            |
*|   and/or modify it under the terms of the GNU General Public License|
*|   as published  by the Free Software Foundation                     |
*|                                                                     |
*|   ZDBFRAMEWORK is distributed in the hope that it will be useful,   |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*\---------------------------------------------------------------------/
*/---------------------------------------------------------------------\                                                                     |
*|   SPECIAL THANKS                                                    |
*|                                                                     |
*|   We would like to thanks SAPLink project because we learned how to |
*|   create an ABAP Classe studying their code                         |
*\---------------------------------------------------------------------/
*/---------------------------------------------------------------------\
*| For a full list of contributors visit:                              |
*|                                                                     |
*| project homepage: https://github.com/rayatus/sapcodegenerator        |
*\---------------------------------------------------------------------/

*----------------------------------------------------------------------*
***INCLUDE LZBDFRAMEWORK_WIZARDF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHK_0200_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_0200_data .

* Check if TabName already exists in active version
  PERFORM chk_tabname USING gs_codegen_data-tabname.
* Check if GetList table type already exists in active version
  PERFORM chk_typedata USING gs_codegen_data-getlist_struct_name .
* Propose Class Name
  PERFORM set_clsname_init_value.

ENDFORM.                    " CHK_0200_DATA
*&---------------------------------------------------------------------*
*&      Form  CHK_TYPEDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TYPEDATA  text
*----------------------------------------------------------------------*
FORM chk_tabname  USING    p_name.
  DATA: lo_engine_base TYPE REF TO zcl_zdbframework_engine_base.

  CREATE OBJECT lo_engine_base.
  "DB Table exists?
  lo_engine_base->chk_exist_dbname( EXPORTING  id_dbname = p_name
                                    EXCEPTIONS OTHERS    = 999 ).

  "Is allowed to create a READ class for this table?
  IF sy-subrc IS INITIAL.
    lo_engine_base->chk_is_dbname_permitted( EXPORTING  id_dbname = p_name
                                             EXCEPTIONS OTHERS    = 999 ).
  ENDIF.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID      sy-msgid
            TYPE    sy-msgty
            NUMBER  sy-msgno
            WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " CHK_TYPEDATA
*&---------------------------------------------------------------------*
*&      Form  CHK_TYPEDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_CODEGEN_DATA_GETLIST_STRUCT  text
*----------------------------------------------------------------------*
FORM chk_typedata  USING    p_name.

  DATA: lo_engine_base TYPE REF TO zcl_zdbframework_engine_base.

  CREATE OBJECT lo_engine_base.
  lo_engine_base->chk_exists_typedata( EXPORTING  id_typedata = p_name
                                       EXCEPTIONS OTHERS      = 999 ).
  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID      sy-msgid
            TYPE    sy-msgty
            NUMBER  sy-msgno
            WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CHK_TYPEDATA
*&---------------------------------------------------------------------*
*&      Form  SET_CLSNAME_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_clsname_init_value .

  CONCATENATE gs_codegen_data-prefix
              gs_codegen_data-tabname
              gs_codegen_data-suffix
              INTO gs_codegen_data-full_clsname.
  CONDENSE gs_codegen_data-full_clsname.

ENDFORM.                    " SET_CLSNAME_INIT_VALUE
