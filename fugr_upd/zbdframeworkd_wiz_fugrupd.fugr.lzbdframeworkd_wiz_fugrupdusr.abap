*----------------------------------------------------------------------*
***INCLUDE LSWZ_WIZARD_TEMPLATEUSR .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  WZ_INITIALIZE_WIZARD_DATA
*&---------------------------------------------------------------------*
FORM usr_initialize_wizard_data.
  CLEAR gs_codegen_data.
ENDFORM.                    " USR_INITIALIZE_WIZARD_DATA
*&---------------------------------------------------------------------*
*&      FORM  USR_PROCESS_WIZARD_DATA
*&---------------------------------------------------------------------*
FORM usr_process_wizard_data.
  DATA lo_exception TYPE REF TO zcx_zdbframework_engine.
  DATA ld_new_function_group TYPE rs38l_area.
  DATA ld_string TYPE string.

  TRY.
      zcl_zdbframework_engine_updfg=>generate_upd_function_group(
        EXPORTING
          id_devclass             = gs_codegen_data-devclass
          id_dbname               = gs_codegen_data-dbname
          id_function_group       = gs_codegen_data-function_group
          id_function_s           = gs_codegen_data-function_s
          id_function_t           = gs_codegen_data-function_t
          id_ttyp                 = gs_codegen_data-table_type
      IMPORTING
        ed_function_group       = ld_new_function_group
      ).

      CONCATENATE 'Function group'(005)
                  ld_new_function_group
                  ' created successfullly'(006)
        INTO ld_string SEPARATED BY space.
      MESSAGE ld_string TYPE 'I' ##NO_TEXT.

    CATCH zcx_zdbframework_engine INTO lo_exception.
      lo_exception->raise_as_message( ).
      MESSAGE ID      sy-msgid
              TYPE    sy-msgty
              NUMBER  sy-msgno
              WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

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
