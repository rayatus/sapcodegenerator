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
*| project homepage: http://code.google.com/p/sapcodegenerator/        |
*\---------------------------------------------------------------------/

*----------------------------------------------------------------------*
***INCLUDE ZDBFRAMEWORK_CREATE_TYPESF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_TYPES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_types  USING    id_clsname    TYPE seoclsname
                            it_fieldlist  TYPE ddfields
                            id_typlist    TYPE typename
                   CHANGING ct_attributes TYPE seoo_attributes_r
                            ct_types      TYPE seoo_types_r.

  DATA: ld_string TYPE string.
  DATA: ld_string2 TYPE string.

  FIELD-SYMBOLS: <ls_attribute> LIKE LINE OF ct_attributes,
                 <ls_fieldlist> LIKE LINE OF it_fieldlist,
                 <ls_type>      LIKE LINE OF ct_types.

  DEFINE mac_add_type.
    concatenate <ls_type>-typesrc &1 CL_ABAP_CHAR_UTILITIES=>newline into <ls_type>-typesrc separated by space.
  END-OF-DEFINITION.

  INSERT INITIAL LINE INTO TABLE ct_attributes ASSIGNING <ls_attribute>.
  <ls_attribute>-clsname    = id_clsname.
  <ls_attribute>-cmpname    = 'MS_RANGES'.
  <ls_attribute>-descript   = 'Selection Ranges'.
  <ls_attribute>-exposure   = seoc_exposure_private.
  <ls_attribute>-state      = seoc_state_implemented.
  <ls_attribute>-editorder  = 1.
  <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
  <ls_attribute>-type       = 'MTYP_RANGES'.
  <ls_attribute>-TYPTYPE    = 1.

  INSERT INITIAL LINE INTO TABLE ct_attributes ASSIGNING <ls_attribute>.
  <ls_attribute>-clsname    = id_clsname.
  <ls_attribute>-cmpname    = 'MT_BUFFER'.
  <ls_attribute>-descript   = 'Data Buffer'.
  <ls_attribute>-exposure   = seoc_exposure_private.
  <ls_attribute>-state      = seoc_state_implemented.
  <ls_attribute>-editorder  = 2.
  <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
  <ls_attribute>-type       = id_typlist.
  <ls_attribute>-TYPTYPE    = 1.


  LOOP AT it_fieldlist ASSIGNING <ls_fieldlist>.
    AT FIRST.
      INSERT INITIAL LINE INTO TABLE ct_types ASSIGNING <ls_type>.
      <ls_type>-clsname   = id_clsname.
      <ls_type>-cmpname   = 'MTYP_RANGES'.
      <ls_type>-exposure  = seoc_exposure_private.
      <ls_type>-state     = seoc_state_implemented.
      <ls_type>-editorder = 1.
      <ls_type>-TYPTYPE = 4.

      mac_add_type  'BEGIN OF mtyp_ranges,'.
    ENDAT.

    CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO ld_string2 SEPARATED BY '-'.
    CONCATENATE <ls_fieldlist>-fieldname 'TYPE RANGE OF' ld_string2 ',' INTO ld_string SEPARATED BY space.
    mac_add_type ld_string.

    AT LAST.
      mac_add_type  '       END   OF mtyp_ranges'.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " CREATE_TYPES
