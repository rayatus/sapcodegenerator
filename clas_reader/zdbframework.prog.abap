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

*&---------------------------------------------------------------------*
*& Report  ZDBFRAMEWORK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdbframework.


PARAMETERS: p_clname TYPE vseoclass-clsname OBLIGATORY,
            p_dbname TYPE tabname16         OBLIGATORY.

PARAMETERS p_ttyp TYPE typename OBLIGATORY.
PARAMETERS p_devcls TYPE devclass OBLIGATORY.

START-OF-SELECTION.

  DATA: ld_string  TYPE string,
        ld_clsname TYPE seoclsname.

  zcl_zdbframework_engine_read=>generate_class(
    EXPORTING
      id_devclass         = p_devcls
      id_dbname           = p_dbname
      id_classname        = p_clname
      id_ttyp             = p_ttyp
      IMPORTING
        ed_classname      = ld_clsname
    EXCEPTIONS
      OTHERS              = 999
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CONCATENATE 'Class '(007)  ld_clsname  ' created successfullly'(008) INTO ld_string SEPARATED BY space.
    MESSAGE ld_string TYPE 'I'.
  ENDIF.
