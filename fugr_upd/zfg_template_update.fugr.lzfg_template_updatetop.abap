FUNCTION-POOL zfg_template_update.          "MESSAGE-ID ..

*====================================================================
CONSTANTS:
*====================================================================
  BEGIN OF gc_commit,
    synchronous TYPE sy-oncom VALUE 'S',
  END OF gc_commit,

  BEGIN OF gc_chngind,
    insert TYPE cdchngind VALUE 'I',
    update TYPE cdchngind VALUE 'U',
    delete TYPE cdchngind VALUE 'D',
  END   OF gc_chngind.

*====================================================================
DATA:
*====================================================================
  gt_buffer_i TYPE STANDARD TABLE OF sflight,
  gt_buffer_u TYPE STANDARD TABLE OF sflight,
  gt_buffer_d TYPE STANDARD TABLE OF sflight.
