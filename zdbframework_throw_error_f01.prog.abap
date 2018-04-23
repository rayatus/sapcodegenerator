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

*&---------------------------------------------------------------------*
*&  Include           ZDBFRAMEWORK_THROW_ERROR_F01
*&---------------------------------------------------------------------*
FORM throw_error.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "throw_error
