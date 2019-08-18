*&---------------------------------------------------------------------*
*& Report  ZDBFRAMEWORK_WIZARD
*&
*&---------------------------------------------------------------------*
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
REPORT zdbframeworl_tst_updfg.

START-OF-SELECTION.

  TRY.
      CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
        EXPORTING
          function_pool = 'ZFG_ZTESTUPD_UPD'
        EXCEPTIONS
          OTHERS        = 999.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
          EXPORTING
            area            = 'ZFG_ZTESTUPD_UPD'
            suppress_popups = abap_true
          EXCEPTIONS
            OTHERS          = 999.
        IF sy-subrc <> 0.
          WRITE: 'Unable to delete already existing resulting FUGR'.
        ENDIF.
      ENDIF.

      zcl_zdbframework_engine_updfg=>generate_upd_function_group(
        EXPORTING
          id_devclass             = '$TMP'
          id_dbname               = 'ZTESTUPD'
          id_function_group       = 'ZFG_ZTESTUPD_UPD'
          id_function_s           = 'Z_FM_ZTESTUPD_UPD_S'
          id_function_t           = 'Z_FM_ZTESTUPD_UPD_T'
          id_ttyp                 = 'ZTT_ZTESTUPD_KZ'
      ).


      CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
        EXPORTING
          function_pool = 'ZFG_ZTESTUPD_UPD'
        EXCEPTIONS
          OTHERS        = 999.
      IF sy-subrc <> 0.
        WRITE: 'Resulting FunctionGroup does not exist.' .
      ENDIF.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname = 'Z_FM_ZTESTUPD_UPD_S'
        EXCEPTIONS
          OTHERS   = 999.
      IF sy-subrc <> 0.
        WRITE:  'Function ZFM_ZTESTUPD_UPD_S does not exists' .
      ENDIF.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname = 'Z_FM_ZTESTUPD_UPD_T'
        EXCEPTIONS
          OTHERS   = 999.
      IF sy-subrc <> 0.
        WRITE: 'Function ZFM_ZTESTUPD_UPD_T does not exists'.
      ELSE.
        WRITE: 'correct'.
      ENDIF.

    CATCH zcx_zdbframework_engine INTO DATA(lo_exception).    " .
      MESSAGE ID      lo_exception->if_t100_message~t100key-msgid
              TYPE    'E'
              NUMBER  lo_exception->if_t100_message~t100key-msgno
              WITH    lo_exception->if_t100_message~t100key-attr1
                      lo_exception->if_t100_message~t100key-attr2
                      lo_exception->if_t100_message~t100key-attr3
                      lo_exception->if_t100_message~t100key-attr4
              INTO DATA(ld_dummy).

      cl_demo_output=>display(
        EXPORTING
          data = ld_dummy
      ).
  ENDTRY.
