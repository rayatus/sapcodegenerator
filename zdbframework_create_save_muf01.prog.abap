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
***INCLUDE ZDBFRAMEWORK_CREATE_SAVE_MUF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_SAVE_MULTIPLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_save_multiple USING   id_clsname          TYPE seoclsname
                                 id_tabname           TYPE tabname16
                                 it_fieldlist         TYPE ddfields
                                 id_typlist           TYPE typename
                        CHANGING ct_methods           TYPE seoo_methods_r
                                 ct_method_sources    TYPE seo_method_source_table
                                 ct_parameters        TYPE seos_parameters_r
                                 ct_exceptions        TYPE seos_exceptions_r.

  FIELD-SYMBOLS:  <ls_fieldlist>      LIKE LINE OF it_fieldlist,
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

* Creamos el método
  INSERT INITIAL LINE INTO TABLE ct_method_sources ASSIGNING <ls_method_sources>.
  INSERT INITIAL LINE INTO TABLE ct_methods        ASSIGNING <ls_method>.
  <ls_method>-clsname    = id_clsname.
  <ls_method>-state      = seoc_state_implemented.
  <ls_method>-mtdnewexc  = abap_true.
  <ls_method>-mtddecltyp = '1'.
  <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'SAVE_LIST'.
  <ls_method>-descript   = 'Save Multiple'.
  <ls_method>-exposure   = seoc_exposure_public.

* Creamos el parámetro tabla de entrada
  ADD 1 TO ld_order.
  INSERT INITIAL LINE INTO TABLE ct_parameters ASSIGNING <ls_parameter>.
  <ls_parameter>-clsname    =  <ls_method>-clsname.
  <ls_parameter>-cmpname    =  <ls_method>-cmpname.
  <ls_parameter>-sconame    = 'IT_LIST'.
  <ls_parameter>-descript   = 'List Details'.
  <ls_parameter>-cmptype    = seoo_cmptype_attribute.
  <ls_parameter>-mtdtype    = seoo_mtdtype_method.
  <ls_parameter>-editorder  = ld_order.
  <ls_parameter>-typtype    = seoo_typtype_type.
  <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
  <ls_parameter>-type       = id_typlist.
  <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.

*  Añadimos la excepción de "error al guardar
  INSERT INITIAL LINE INTO TABLE ct_exceptions ASSIGNING <ls_exception>.
  <ls_exception>-clsname  = <ls_method>-clsname.
  <ls_exception>-cmpname  = <ls_method>-cmpname.
  <ls_exception>-sconame  = 'CX_C2S_DB'.
*  <ls_exception>-version  = seoc_version_active.
  <ls_exception>-descript = 'Error while saving'.

** Limpiamos registros duplicados
*  mac_add_source space.
*  mac_add_source '  SORT it_list.'.
*  mac_add_source '  DELETE ADJACENT DUPLICATES FROM it_list.'.

* Procedemos a guardar en BBDD (creando o modificando el registro por claves)
  mac_add_source space.
  mac_add_source '  CHECK it_list[] IS NOT INITIAL.'.
  CONCATENATE    '  MODIFY' id_tabname ' FROM TABLE it_list.' INTO ld_string SEPARATED BY space.
  mac_add_source ld_string.

* Si ha dado error....
  mac_add_source space.
  mac_add_source '  IF NOT sy-subrc IS INITIAL.'.
  mac_add_source '    RAISE EXCEPTION TYPE cx_c2s_db.'.
  mac_add_source '  ENDIF.'.


ENDFORM.                    " CREATE_SAVE_MULTIPLE
