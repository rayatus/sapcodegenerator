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
*| project homepage: https://github.com/rayatus/sapcodegenerator       |
*\---------------------------------------------------------------------/

"! <p class="shorttext synchronized" lang="en">Base generator</p>
CLASS zcl_zdbframework_engine_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES mtyp_t_namespaces TYPE STANDARD TABLE OF trnspace-namespace WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF mc_msgtyp,
                 error TYPE string VALUE 'E' ##NO_TEXT,
               END OF mc_msgtyp.

    CONSTANTS mc_procuder_namespace TYPE string VALUE 'P' ##NO_TEXT.
    CONSTANTS mc_custom_namespace_z TYPE string VALUE 'Z' ##NO_TEXT.
    CONSTANTS mc_custom_namespace_y TYPE string VALUE 'Y' ##NO_TEXT.
    CONSTANTS mc_custom_namespace_t TYPE string VALUE 'T' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">Verifies if DB Table is permitted</p>
    "! <br/>
    "! Only tablenames in <em>customer namespace</em> are allowed.
    "!
    "! @parameter id_dbname | <p class="shorttext synchronized" lang="en">DB tabname</p>
    "! @exception dbname_not_permited | <p class="shorttext synchronized" lang="en">Table name is not permited</p>
    METHODS chk_is_dbname_permitted
      IMPORTING  id_dbname TYPE tabname16
      EXCEPTIONS dbname_not_permited.

    METHODS chk_exist_dbname
      IMPORTING  id_dbname TYPE tabname16
      EXCEPTIONS dbname_not_exists.

    METHODS chk_exists_typedata
      IMPORTING  id_typedata TYPE dd40l-typename
      EXCEPTIONS tpyedata_not_exists.

    METHODS chk_is_namespace_allowed
      IMPORTING  id_namespace TYPE namespace
      EXCEPTIONS namespace_not_allowed.

    METHODS get_allowed_namespaces
      EXPORTING et_namespaces TYPE mtyp_t_namespaces.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS mc_active_version TYPE string VALUE 'A' ##NO_TEXT.

ENDCLASS.



CLASS zcl_zdbframework_engine_base IMPLEMENTATION.

  METHOD get_allowed_namespaces.
    FIELD-SYMBOLS <ls_allowed_namespace> LIKE LINE OF et_namespaces.

    CLEAR et_namespaces.

    "Search for producer namespaces.
    SELECT namespace
      FROM trnspace
      INTO TABLE et_namespaces
      WHERE role = mc_procuder_namespace.

    INSERT INITIAL LINE INTO TABLE et_namespaces ASSIGNING <ls_allowed_namespace>.
    <ls_allowed_namespace> = mc_custom_namespace_z.
    INSERT INITIAL LINE INTO TABLE et_namespaces ASSIGNING <ls_allowed_namespace>.
    <ls_allowed_namespace> = mc_custom_namespace_y.
    INSERT INITIAL LINE INTO TABLE et_namespaces ASSIGNING <ls_allowed_namespace>.
    <ls_allowed_namespace> = mc_custom_namespace_t.

  ENDMETHOD.


  METHOD chk_is_dbname_permitted.
    DATA lt_allowed_namespaces TYPE mtyp_t_namespaces.
    DATA lf_is_allowed TYPE abap_bool VALUE abap_false.
    DATA long TYPE i.

    FIELD-SYMBOLS <ls_allowed_namespace> LIKE LINE OF lt_allowed_namespaces.

    get_allowed_namespaces( IMPORTING et_namespaces = lt_allowed_namespaces ).

    LOOP AT lt_allowed_namespaces ASSIGNING <ls_allowed_namespace>.
      long = strlen( <ls_allowed_namespace> ).
      IF id_dbname+0(long) = <ls_allowed_namespace>.
        lf_is_allowed = abap_true.
      ENDIF.
    ENDLOOP.

    IF lf_is_allowed = abap_false AND sy-sysid <> 'NPL'  ##NO_TEXT.
      MESSAGE 'Only tables in an allowed namespace are permitted.'(001)
        TYPE mc_msgtyp-error
        RAISING dbname_not_permited.
    ENDIF.
  ENDMETHOD.

  METHOD chk_exist_dbname.

    SELECT COUNT( * ) INTO sy-dbcnt FROM dd02l
      WHERE tabname  = id_dbname
        AND as4local = mc_active_version.
    IF sy-dbcnt IS INITIAL.
      MESSAGE e007(e2) WITH id_dbname RAISING dbname_not_exists.
*     & does not exist. Check name
    ENDIF.
  ENDMETHOD.

  METHOD chk_exists_typedata.

    SELECT COUNT( * ) INTO sy-dbcnt FROM dd40l
      WHERE typename = id_typedata
        AND as4local = mc_active_version.
    IF sy-dbcnt IS INITIAL.
      MESSAGE e007(e2) WITH id_typedata RAISING tpyedata_not_exists.
*     & does not exist. Check name
    ENDIF.
  ENDMETHOD.

  METHOD chk_is_namespace_allowed.
    DATA lt_namespaces TYPE mtyp_t_namespaces.

    get_allowed_namespaces( IMPORTING et_namespaces = lt_namespaces ).
    READ TABLE lt_namespaces WITH KEY table_line = id_namespace TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Namespace is not allowed'(002)
        TYPE mc_msgtyp-error
        RAISING namespace_not_allowed .
    ENDIF.


  ENDMETHOD.

ENDCLASS.
