CLASS ltcl_zdbframework_engine_updfg DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_zdbframework_engine_updfg IMPLEMENTATION.

  METHOD first_test.
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
            cl_abap_unit_assert=>fail( 'Unable to delete already existing resulting FUGR' ).
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
          cl_abap_unit_assert=>fail( 'Resulting FunctionGroup does not exist.' ).
        ENDIF.

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname = 'Z_FM_ZTESTUPD_UPD_S'
          EXCEPTIONS
            OTHERS   = 999.
        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail( 'Function Z_FM_ZTESTUPD_UPD_S does not exists' ).
        ENDIF.

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname = 'Z_FM_ZTESTUPD_UPD_T'
          EXCEPTIONS
            OTHERS   = 999.
        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail( 'Function Z_FM_ZTESTUPD_UPD_T does not exists' ).
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

        cl_abap_unit_assert=>fail( msg = ld_dummy ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
