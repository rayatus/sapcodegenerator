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
***INCLUDE ZDBFRAMEWORK_CREATE_FIND_MUF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIND_MULTIPLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_find_multiple  USING id_clsname           TYPE seoclsname
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
        ld_string  TYPE string,
        ld_string2 TYPE string.

  DEFINE mac_add_source.
    insert initial line into table <ls_method_sources>-source assigning <ld_source>.
    <ld_source> = &1.
  END-OF-DEFINITION.

  INSERT INITIAL LINE INTO TABLE ct_method_sources ASSIGNING <ls_method_sources>.
  INSERT INITIAL LINE INTO TABLE ct_methods        ASSIGNING <ls_method>.
  <ls_method>-clsname    = id_clsname.
  <ls_method>-state      = seoc_state_implemented.
  <ls_method>-mtdnewexc  = abap_true.
  <ls_method>-mtddecltyp = '1'.
  <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'GET_LIST'.
  <ls_method>-descript   = 'Find Multiple details by keys'.
  <ls_method>-exposure   = seoc_exposure_public.

  mac_add_source space.
  mac_add_source '  CLEAR ms_ranges.'.

  LOOP AT it_fieldlist ASSIGNING <ls_fieldlist>.
    CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO ld_string.

    mac_add_source space.
    CONCATENATE '  IF' ld_string 'IS SUPPLIED.' INTO ld_string2 SEPARATED BY space.
    mac_add_source ld_string2.
    CONCATENATE '    _add_range( EXPORTING id_low   =' ld_string INTO ld_string2 SEPARATED BY space.
    mac_add_source ld_string2.
    CONCATENATE '                CHANGING  ct_range = ms_ranges-' <ls_fieldlist>-fieldname ' ).' INTO ld_string2.
    mac_add_source ld_string2.
    mac_add_source '  ENDIF.'.
  ENDLOOP.


  mac_add_source space.
  mac_add_source 'SELECT * INTO TABLE et_list'.
  CONCATENATE    '  FROM' id_tabname INTO ld_string SEPARATED BY space.
  mac_add_source ld_string.


* Sólo trabajamos con los campos Clave
  LOOP AT it_fieldlist ASSIGNING <ls_fieldlist>.
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
    <ls_parameter>-paroptionl = abap_true.
    CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO <ls_parameter>-type SEPARATED BY '-'.

*   Continuamos montando el WHERE
    CONCATENATE  'ms_ranges-' <ls_fieldlist>-fieldname INTO ld_string.
    IF ld_order = 1.
      CONCATENATE '  WHERE' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space.
    ELSE.
      CONCATENATE '    AND' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space.
    ENDIF.
    mac_add_source ld_string.

  ENDLOOP.
* Finalizamos el Select
  CONCATENATE <ld_source> '.' INTO <ld_source>.

  mac_add_source space.
  mac_add_source '  INSERT LINES OF et_list INTO TABLE mt_buffer.'.
  mac_add_source '  SORT mt_buffer.'.
  mac_add_source '  DELETE ADJACENT DUPLICATES FROM mt_buffer.'.

* Si no ha encontrado nada lanzamos excepción
  mac_add_source space.
  mac_add_source '  IF et_list IS INITIAL.'.
  mac_add_source '    RAISE EXCEPTION TYPE cx_c2s_data_not_found.'.
  mac_add_source '  ENDIF.'.


* Creamos el parámetro de retorno
  ADD 1 TO ld_order.
  INSERT INITIAL LINE INTO TABLE ct_parameters ASSIGNING <ls_parameter>.
  <ls_parameter>-clsname    =  <ls_method>-clsname.
  <ls_parameter>-cmpname    =  <ls_method>-cmpname.
  <ls_parameter>-sconame    = 'ET_LIST'.
  <ls_parameter>-descript   = 'List Details'.
  <ls_parameter>-cmptype    = seoo_cmptype_attribute.
  <ls_parameter>-mtdtype    = seoo_mtdtype_method.
  <ls_parameter>-editorder  = ld_order.
  <ls_parameter>-typtype    = seoo_typtype_type.
  <ls_parameter>-pardecltyp = seos_pardecltyp_exporting.
  <ls_parameter>-type       = id_typlist.
  <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.


*  Añadimos la excepción de "no hay datos"
  INSERT INITIAL LINE INTO TABLE ct_exceptions ASSIGNING <ls_exception>.
  <ls_exception>-clsname  = <ls_method>-clsname.
  <ls_exception>-cmpname  = <ls_method>-cmpname.
  <ls_exception>-sconame  = 'CX_C2S_DATA_NOT_FOUND'.
*  <ls_exception>-version  = seoc_version_active.
  <ls_exception>-descript = 'Not Found'.

ENDFORM.                    " CREATE_FIND_MULTIPLE
