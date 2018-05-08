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
*|   We would like to thank SAPLink project because we learned how to  |
*|   create an ABAP Classe studying their code                         |
*\---------------------------------------------------------------------/
*/---------------------------------------------------------------------\
*| For a full list of contributors visit:                              |
*|                                                                     |
*| project homepage: http://code.google.com/p/sapcodegenerator/        |
*\---------------------------------------------------------------------/

*----------------------------------------------------------------------*
***INCLUDE LSWZ_WIZARD_TEMPLATEUSR .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  WZ_INITIALIZE_WIZARD_DATA
*&---------------------------------------------------------------------*
FORM usr_initialize_wizard_data.
*** initialize global wizard data
  gs_codegen_data-prefix = 'ZRE_CL_'(008).
  gs_codegen_data-suffix = '_READ'(009).
ENDFORM.                    " USR_INITIALIZE_WIZARD_DATA
*&---------------------------------------------------------------------*
*&      FORM  USR_PROCESS_WIZARD_DATA
*&---------------------------------------------------------------------*
FORM usr_process_wizard_data.
*** process global data collected by wizard

  SUBMIT zdbframework
    WITH p_clname  = gs_codegen_data-full_clsname
    WITH p_dbname  = gs_codegen_data-tabname
    WITH p_ttyp    = gs_codegen_data-getlist_struct_name
    WITH p_devcls  = gs_codegen_data-devclass
    AND RETURN.

ENDFORM.                    " USR_PROCESS_WIZARD_DATA
*&--------------------------------------------------------------*
*&      Form  USR_SET_SUBSCREEN_DATA_0200
*&--------------------------------------------------------------*
FORM usr_set_subscreen_data_0200.

ENDFORM.                    " USR_SET_SUBSCREEN_DATA_0200
*&--------------------------------------------------------------*
*&      Form  USR_GET_SUBSCREEN_DATA_0200
*&--------------------------------------------------------------*
FORM usr_get_subscreen_data_0200.

ENDFORM.                    " USR_GET_SUBSCREEN_DATA_0200
*&--------------------------------------------------------------*
*&      Module  USR_CHECK_CONSISTENCY_0200
*&--------------------------------------------------------------*
MODULE usr_check_consistency_0200 INPUT.

  IF g_save_ok_code = wizard_command_continue.
    PERFORM usr_check_consistency_0200
            CHANGING g_error.
    IF NOT g_error IS INITIAL.
      swf_refresh.
    ENDIF.
  ENDIF.

ENDMODULE.                  " USR_CHECK_CONSISTENCY_0200 INPUT
*&--------------------------------------------------------------*
*&      Form  USR_CHECK_CONSISTENCY_0200
*&--------------------------------------------------------------*
FORM usr_check_consistency_0200
         CHANGING
            p_error         TYPE sy-input.

* here we check if all entries on this screen are consistent
* (field checks have to be done on the screen, not here)

* initialize error indicator
  CLEAR: p_error.

ENDFORM.                    " USR_CHECK_CONSISTENCY_0200
*&--------------------------------------------------------------*
*&      Form  USR_SET_SUBSCREEN_DATA_0300
*&--------------------------------------------------------------*
FORM usr_set_subscreen_data_0300.

ENDFORM.                    " USR_SET_SUBSCREEN_DATA_0300
*&--------------------------------------------------------------*
*&      Form  USR_GET_SUBSCREEN_DATA_0300
*&--------------------------------------------------------------*
FORM usr_get_subscreen_data_0300.


ENDFORM.                    " USR_GET_SUBSCREEN_DATA_0300
*&--------------------------------------------------------------*
*&      Module  USR_CHECK_CONSISTENCY_0300
*&--------------------------------------------------------------*
MODULE usr_check_consistency_0300 INPUT.

  IF g_save_ok_code = wizard_command_continue.
    PERFORM usr_check_consistency_0300
            CHANGING g_error.
    IF NOT g_error IS INITIAL.
      swf_refresh.
    ENDIF.
  ENDIF.

ENDMODULE.                  " USR_CHECK_CONSISTENCY_0300 INPUT
*&--------------------------------------------------------------*
*&      Form  USR_CHECK_CONSISTENCY_0300
*&--------------------------------------------------------------*
FORM usr_check_consistency_0300
         CHANGING
            p_error         TYPE sy-input.

* here we check if all entries on this screen are consistent
* (field checks have to be done on the screen, not here)

* initialize error indicator
  CLEAR: p_error.

ENDFORM.                    " USR_CHECK_CONSISTENCY_0300
