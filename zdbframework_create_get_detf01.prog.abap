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

*----------------------------------------------------------------------*
***INCLUDE ZDBFRAMEWORK_CREATE_GET_DETF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_get_detail  USING    id_clsname           TYPE seoclsname
                                 id_tabname           TYPE tabname16
                                 it_fieldlist         TYPE ddfields
                                 id_typlist           TYPE typename
                        CHANGING ct_methods           TYPE seoo_methods_r
                                 ct_method_sources    TYPE seo_method_source_table
                                 ct_parameters        TYPE seos_parameters_r
                                 ct_exceptions        TYPE seos_exceptions_r.

  FIELD-SYMBOLS: <ls_fieldlist>      LIKE LINE OF it_fieldlist,
                 <ls_method>         LIKE LINE OF ct_methods,
                 <ls_method_sources> LIKE LINE OF ct_method_sources,
                 <ls_parameter>      LIKE LINE OF ct_parameters,
                 <ld_source>         LIKE LINE OF <ls_method_sources>-source,
                 <ls_exception>      LIKE LINE OF ct_exceptions.

  DATA: ld_order   LIKE <ls_parameter>-editorder,
        ld_string2 TYPE string,
        ld_string  TYPE string.

  DEFINE mac_add_source.
    insert initial line into table <ls_method_sources>-source assigning <ld_source>.
    <ld_source> = &1.
  END-OF-DEFINITION.

  INSERT INITIAL LINE INTO TABLE ct_method_sources ASSIGNING <ls_method_sources>.
  INSERT INITIAL LINE INTO TABLE ct_methods        ASSIGNING <ls_method>.
  <ls_method>-clsname   = id_clsname.
  <ls_method>-state     = seoc_state_implemented.
  <ls_method>-mtdnewexc = abap_true.
  <ls_method>-mtddecltyp = '1'.
  <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'GET_DETAILS'.
  <ls_method>-descript  = 'Find details by keys'.
  <ls_method>-exposure  = seoc_exposure_public.

  mac_add_source space.
  CONCATENATE '  DATA lt_list      TYPE' id_typlist '.' INTO ld_string SEPARATED BY space.
  mac_add_source ld_string.
  mac_add_source '  DATA ls_list      LIKE LINE OF lt_list.'.
  mac_add_source '  DATA lo_exception TYPE REF TO cx_c2s_data_not_found.'.


  mac_add_source space.
  mac_add_source '  READ TABLE mt_buffer INTO rs_result'.
  mac_add_source '    WITH KEY'.
  LOOP AT it_fieldlist ASSIGNING <ls_fieldlist> .
    CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO ld_string.
    CONCATENATE  <ls_fieldlist>-fieldname '=' ld_string INTO ld_string SEPARATED BY space.

    AT FIRST.
      CONCATENATE <ld_source> ld_string INTO <ld_source> SEPARATED BY space.
      CONTINUE.
    ENDAT.

    CLEAR ld_string2.
    DO 6 TIMES.
      CONCATENATE ld_string2 cl_abap_char_utilities=>horizontal_tab INTO ld_string2.
    ENDDO.
    CONCATENATE ld_string2 ld_string INTO ld_string SEPARATED BY space.
    mac_add_source ld_string.

  ENDLOOP.
  CONCATENATE <ld_source> '.' INTO <ld_source>.

  mac_add_source space.
  mac_add_source '  IF NOT sy-subrc IS INITIAL.'.

  mac_add_source space.
  mac_add_source '    TRY.'.
  mac_add_source '        get_list( EXPORTING'.

* Sólo trabajamos con los campos Clave
  LOOP AT it_fieldlist ASSIGNING <ls_fieldlist> .
    ADD 1 TO ld_order.

    INSERT INITIAL LINE INTO TABLE ct_parameters ASSIGNING <ls_parameter>.

    <ls_parameter>-clsname    =  <ls_method>-clsname.
    <ls_parameter>-cmpname    =  <ls_method>-cmpname.
    CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO <ls_parameter>-sconame.
    <ls_parameter>-descript   = <ls_fieldlist>-scrtext_m.
    <ls_parameter>-cmptype    = seoo_cmptype_attribute.
    <ls_parameter>-mtdtype    = seoo_mtdtype_method.
    <ls_parameter>-editorder  = ld_order.
    <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
    <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
    <ls_parameter>-typtype    = seoo_typtype_type.
    CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO <ls_parameter>-type SEPARATED BY '-'.

    CONCATENATE <ls_parameter>-sconame '=' <ls_parameter>-sconame INTO ld_string SEPARATED BY space.

    IF ld_order = 1.
      CONCATENATE <ld_source> space ld_string INTO <ld_source> SEPARATED BY space.
    ELSE.
      CLEAR ld_string2.
      DO 14 TIMES.
        CONCATENATE ld_string2 cl_abap_char_utilities=>horizontal_tab INTO ld_string2.
      ENDDO.
      CONCATENATE ld_string2 ld_string INTO ld_string SEPARATED BY space.
      mac_add_source ld_string.
    ENDIF.

  ENDLOOP.
  mac_add_source '                  IMPORTING  et_list = lt_list ).'.

  mac_add_source space.
  mac_add_source '      READ TABLE lt_list INDEX 1 INTO ls_list.'.
  mac_add_source '      MOVE-CORRESPONDING ls_list TO rs_result.'.

  mac_add_source space.
  mac_add_source '      CATCH cx_c2s_data_not_found INTO lo_exception.'.
  mac_add_source '        RAISE EXCEPTION lo_exception.'.
  mac_add_source '    ENDTRY.'.

  mac_add_source space.
  mac_add_source '  ENDIF.'.


* Creamos el parámetro de retorno
  ADD 1 TO ld_order.
  INSERT INITIAL LINE INTO TABLE ct_parameters ASSIGNING <ls_parameter>.
  <ls_parameter>-clsname   =  <ls_method>-clsname.
  <ls_parameter>-cmpname   =  <ls_method>-cmpname.
  <ls_parameter>-sconame   = 'RS_RESULT'.
  <ls_parameter>-descript  = 'Details'.
  <ls_parameter>-cmptype   = seoo_cmptype_attribute.
  <ls_parameter>-mtdtype   = seoo_mtdtype_method.
  <ls_parameter>-editorder = ld_order.
  <ls_parameter>-typtype   = seoo_typtype_type.
  <ls_parameter>-pardecltyp = seos_pardecltyp_returning.
  <ls_parameter>-type      = <ls_fieldlist>-tabname.
  <ls_parameter>-parpasstyp = seos_parpasstyp_byvalue.

*  Añadimos la excepción de "no hay datos"
  INSERT INITIAL LINE INTO TABLE ct_exceptions ASSIGNING <ls_exception>.
  <ls_exception>-clsname  = <ls_method>-clsname.
  <ls_exception>-cmpname  = <ls_method>-cmpname.
  <ls_exception>-sconame  = 'CX_C2S_DATA_NOT_FOUND'.
*  <ls_exception>-version  = seoc_version_active.
  <ls_exception>-descript = 'Not data found'.

ENDFORM.                    " CREATE_GET_DETAIL

*&---------------------------------------------------------------------*
*&      Form  create_init_buffer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ID_CLSNAME         text
*      -->CT_METHODS         text
*      -->CT_METHOD_SOURCES  text
*      -->CT_PARAMETERS      text
*      -->CT_EXCEPTIONS      text
*----------------------------------------------------------------------*
FORM create_init_buffer  USING    id_clsname           TYPE seoclsname
                         CHANGING ct_methods           TYPE seoo_methods_r
                                 ct_method_sources    TYPE seo_method_source_table
                                 ct_parameters        TYPE seos_parameters_r
                                 ct_exceptions        TYPE seos_exceptions_r.

  FIELD-SYMBOLS: <ls_method>         LIKE LINE OF ct_methods,
                 <ls_method_sources> LIKE LINE OF ct_method_sources,
                 <ls_parameter>      LIKE LINE OF ct_parameters,
                 <ld_source>         LIKE LINE OF <ls_method_sources>-source,
                 <ls_exception>      LIKE LINE OF ct_exceptions.

  DATA: ld_order  LIKE <ls_parameter>-editorder,
        ld_string TYPE string.

  DEFINE mac_add_source.
    insert initial line into table <ls_method_sources>-source assigning <ld_source>.
    <ld_source> = &1.
  END-OF-DEFINITION.


  INSERT INITIAL LINE INTO TABLE ct_method_sources ASSIGNING <ls_method_sources>.
  INSERT INITIAL LINE INTO TABLE ct_methods        ASSIGNING <ls_method>.
  <ls_method>-clsname   = id_clsname.
  <ls_method>-state     = seoc_state_implemented.
  <ls_method>-mtdnewexc = abap_true.
  <ls_method>-mtddecltyp = '1'.
  <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'INIT_BUFFER'.
  <ls_method>-descript  = 'Initializes Buffer Data'.
  <ls_method>-exposure  = seoc_exposure_public.

  mac_add_source '  CLEAR mt_buffer[].'.

ENDFORM.        "create_init_buffer
