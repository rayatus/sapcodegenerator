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

FUNCTION WZ_ZBDFRAMEWORK_WIZARD_START.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXCEPTIONS
*"      CANCELLED_BY_USER
*"      ERROR
*"----------------------------------------------------------------------
*----------------- do not remove this include --------------------------
  INCLUDE <sbpt_wizard>.
*----------------- do not remove this include --------------------------

* begin of local data definition
  DATA:
    l_wizard_steps TYPE STANDARD TABLE OF swf_wizdef,
    l_subrc        TYPE sy-subrc.
* end of local data definition

* define wizard steps
  PERFORM wz_define_wizard_steps
             TABLES
                l_wizard_steps.

* initialize wizard data
  PERFORM usr_initialize_wizard_data.

* process wizard steps
  PERFORM wz_wizard_process
             TABLES
                l_wizard_steps
             CHANGING
                l_subrc.
  CASE l_subrc.
    WHEN 1.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING cancelled_by_user.
    WHEN 2.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING error.
  ENDCASE.
ENDFUNCTION.
