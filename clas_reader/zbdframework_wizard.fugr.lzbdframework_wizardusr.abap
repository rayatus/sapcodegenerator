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
*| project homepage: https://github.com/rayatus/sapcodegenerator       |
*\---------------------------------------------------------------------/

*----------------------------------------------------------------------*
***INCLUDE LSWZ_WIZARD_TEMPLATEUSR .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  WZ_INITIALIZE_WIZARD_DATA
*&---------------------------------------------------------------------*
FORM usr_initialize_wizard_data.
*** initialize global wizard data
  gs_codegen_data-prefix = zcl_zdbframework_engine_read=>get_class_prefix( ).
  gs_codegen_data-suffix = zcl_zdbframework_engine_read=>get_class_suffix( ).
ENDFORM.                    " USR_INITIALIZE_WIZARD_DATA
*&---------------------------------------------------------------------*
*&      FORM  USR_PROCESS_WIZARD_DATA
*&---------------------------------------------------------------------*
FORM usr_process_wizard_data.
*** process global data collected by wizard

  DATA: ld_string    TYPE string,
        ld_classname TYPE seoclsname.

  zcl_zdbframework_engine_read=>generate_class(
    EXPORTING
      id_devclass         = gs_codegen_data-devclass
      id_dbname           = gs_codegen_data-tabname
      id_classname        = gs_codegen_data-full_clsname
      id_ttyp             = gs_codegen_data-getlist_struct_name
     IMPORTING
       ed_classname        = ld_classname
    EXCEPTIONS
      OTHERS              = 999
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CONCATENATE 'Class '  ld_classname  ' created successfullly' INTO ld_string SEPARATED BY space.
    MESSAGE ld_string TYPE 'I'.
  ENDIF.

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
