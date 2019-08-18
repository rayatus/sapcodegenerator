FUNCTION Z_FM_SFLIGHT_UPD_T.
*"--------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_DATA) TYPE  ZTT_SFLIGHT_KZ
*"  EXCEPTIONS
*"      DB_UPDATE_ERROR
*"      INCORRECT_CHANGE_INDICATOR
*"--------------------------------------------------------------------
  DATA: ls_buffer     LIKE LINE OF gt_buffer_i.
  FIELD-SYMBOLS: <ls_data> LIKE LINE OF it_data.

  LOOP AT it_data ASSIGNING <ls_data>.
    MOVE-CORRESPONDING <ls_data> TO ls_buffer.
    CASE <ls_data>-kz.
      WHEN gc_chngind-insert. INSERT ls_buffer INTO TABLE gt_buffer_i.
      WHEN gc_chngind-update. INSERT ls_buffer INTO TABLE gt_buffer_u.
      WHEN gc_chngind-delete. INSERT ls_buffer INTO TABLE gt_buffer_d.
      WHEN OTHERS.
        MESSAGE a431(e0) RAISING incorrect_change_indicator.
    ENDCASE.
  ENDLOOP.

  IF sy-oncom = gc_commit-synchronous.
    PERFORM buffer_update.
  ELSE.
    PERFORM buffer_update ON COMMIT.
    PERFORM buffer_clear  ON ROLLBACK.
  ENDIF.


ENDFUNCTION.
