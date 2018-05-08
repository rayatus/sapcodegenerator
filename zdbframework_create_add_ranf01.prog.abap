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
***INCLUDE ZDBFRAMEWORK_CREATE_ADD_RANF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_ADD_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_add_range      USING id_clsname           TYPE seoclsname
                        CHANGING ct_methods           TYPE seoo_methods_r
                                 ct_method_sources    TYPE seo_method_source_table
                                 ct_parameters        TYPE seos_parameters_r.

  FIELD-SYMBOLS:
                 <ls_method>         LIKE LINE OF ct_methods,
                 <ls_method_sources> LIKE LINE OF ct_method_sources,
                 <ls_parameter>      LIKE LINE OF ct_parameters,
                 <ld_source>         LIKE LINE OF <ls_method_sources>-source.

  DATA: ld_order   LIKE <ls_parameter>-editorder.

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
  <ls_method_sources>-cpdname = <ls_method>-cmpname   = '_ADD_RANGE'.
  <ls_method>-descript   = 'Add to Range'.
  <ls_method>-exposure   = seoc_exposure_private.


  ADD 1 TO ld_order.
  INSERT INITIAL LINE INTO TABLE ct_parameters ASSIGNING <ls_parameter>.
  <ls_parameter>-clsname    = <ls_method>-clsname.
  <ls_parameter>-cmpname    = <ls_method>-cmpname.
  <ls_parameter>-sconame    = 'ID_LOW'.
  <ls_parameter>-cmptype    = seoo_cmptype_attribute.
  <ls_parameter>-mtdtype    = seoo_mtdtype_method.
  <ls_parameter>-editorder  = ld_order.
  <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
  <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
  <ls_parameter>-typtype    = seoo_typtype_type.
  <ls_parameter>-type       = 'ANY'.

  ADD 1 TO ld_order.
  INSERT INITIAL LINE INTO TABLE ct_parameters ASSIGNING <ls_parameter>.
  <ls_parameter>-clsname    = <ls_method>-clsname.
  <ls_parameter>-cmpname    = <ls_method>-cmpname.
  <ls_parameter>-sconame    = 'CT_RANGE'.
  <ls_parameter>-cmptype    = seoo_cmptype_attribute.
  <ls_parameter>-mtdtype    = seoo_mtdtype_method.
  <ls_parameter>-editorder  = ld_order.
  <ls_parameter>-pardecltyp = seos_pardecltyp_changing.
  <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
  <ls_parameter>-typtype    = seoo_typtype_type.
  <ls_parameter>-type       = 'ANY TABLE'.


  mac_add_source space.
  mac_add_source '  FIELD-SYMBOLS: <ls_row>    TYPE any,'.
  mac_add_source '                 <ld_sign>   TYPE any,'.
  mac_add_source '                 <ld_option> TYPE any,'.
  mac_add_source '                 <ld_low>    TYPE any.'.
  mac_add_source space.
  mac_add_source '  INSERT INITIAL LINE INTO TABLE ct_range ASSIGNING <ls_row>.'.
  mac_add_source space.
  mac_add_source text-001.
  mac_add_source '    <ld_low> = id_low.'.
  mac_add_source space.
  mac_add_source text-002.
  mac_add_source text-004.
  mac_add_source space.
  mac_add_source text-003.
  mac_add_source text-005.


ENDFORM.                    "CREATE_ADD_RANGE
