FUNCTION ZFM_SFLIGHT_UPDATE_T .
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_SFLIGHT) TYPE  ZTT_SFLIGHT_KZ
*"  EXCEPTIONS
*"      DB_UPDATE_ERROR
*"      INCORRECT_CHANGE_INDICATOR
*"----------------------------------------------------------------------
  DATA: ls_sflight_buffer     LIKE LINE OF gt_buffer_i.
  FIELD-SYMBOLS: <ls_sflight> LIKE LINE OF it_sflight.

  LOOP AT it_sflight ASSIGNING <ls_sflight>.
    MOVE-CORRESPONDING <ls_sflight> TO ls_sflight_buffer.
    CASE <ls_sflight>-kz.
      WHEN gc_chngind-insert. INSERT ls_sflight_buffer INTO TABLE gt_buffer_i.
      WHEN gc_chngind-update. INSERT ls_sflight_buffer INTO TABLE gt_buffer_u.
      WHEN gc_chngind-delete. INSERT ls_sflight_buffer INTO TABLE gt_buffer_d.
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
