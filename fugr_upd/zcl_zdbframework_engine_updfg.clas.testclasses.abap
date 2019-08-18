CLASS ltcl_zdbframework_engine_updfg DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_zdbframework_engine_updfg IMPLEMENTATION.

  METHOD first_test.

    DATA ld_new_fugr            TYPE rs38l_area   VALUE 'Z_FG_ZTESTUPD_UPD'.
    DATA ld_dbname              TYPE tabname16    VALUE 'ZTESTUPD'.
    DATA ld_new_devclass        TYPE devclass     VALUE '$TMP'.
    DATA ld_new_function_single TYPE rs38l_fnam   VALUE 'Z_FM_ZTESTUPD_UPD_S'.
    DATA ld_new_function_table  TYPE rs38l_fnam   VALUE 'Z_FM_ZTESTUPD_UPD_T'.
    DATA ld_db_table_type_upd   TYPE typename     VALUE 'ZTT_ZTESTUPD_KZ'.

    TRY.
        CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
          EXPORTING
            function_pool = ld_new_fugr
          EXCEPTIONS
            OTHERS        = 999.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
            EXPORTING
              area            = ld_new_fugr
              suppress_popups = abap_true
            EXCEPTIONS
              OTHERS          = 999.
          IF sy-subrc <> 0.
            cl_abap_unit_assert=>fail( |Unable to delete already existing function group { ld_new_fugr }| ).
          ENDIF.
        ENDIF.

        zcl_zdbframework_engine_updfg=>generate_upd_function_group(
          EXPORTING
            id_devclass             = ld_new_devclass
            id_dbname               = ld_dbname
            id_function_group       = ld_new_fugr
            id_function_s           = ld_new_function_single
            id_function_t           = ld_new_function_table
            id_ttyp                 = ld_db_table_type_upd
        ).


        CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
          EXPORTING
            function_pool = ld_new_fugr
          EXCEPTIONS
            OTHERS        = 999.
        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail( |Resulting FunctionGroup { ld_new_fugr } does not exist.| ).
        ENDIF.

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname = ld_new_function_single
          EXCEPTIONS
            OTHERS   = 999.
        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail( |Function { ld_new_function_single } does not exists| ).
        ENDIF.

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname = ld_new_function_table
          EXCEPTIONS
            OTHERS   = 999.
        IF sy-subrc <> 0.
          cl_abap_unit_assert=>fail( |Function { ld_new_function_table } does not exists| ).
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
