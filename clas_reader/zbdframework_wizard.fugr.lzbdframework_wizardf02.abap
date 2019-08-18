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
***INCLUDE LZBDFRAMEWORK_WIZARDF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHK_0300_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_0300_data .
  DATA: ls_seoclass TYPE seoclass.

  sy-msgv1 = gs_codegen_data-full_clsname.
* ClassName must contain CF or CL characters
  IF NOT ( gs_codegen_data-full_clsname CS 'CL' OR gs_codegen_data-full_clsname CS 'CF' ).
    MESSAGE e899(bd) WITH 'ClassName must contain CL or CF.'(005).
  ENDIF.

* ABAP Class already exists?
  SELECT SINGLE * INTO ls_seoclass
    FROM seoclass WHERE clsname = gs_codegen_data-full_clsname.
  IF sy-subrc IS INITIAL.
    MESSAGE e899(bd) WITH 'ABAP Class'(006) gs_codegen_data-full_clsname 'already exists. Enter a new one'(007).
  ENDIF.

ENDFORM.                    " CHK_0300_DATA
