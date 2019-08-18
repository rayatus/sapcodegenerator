FUNCTION WZ_ZBDFRAMEWORKD_WIZ_FUGRUPD_S.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXCEPTIONS
*"      CANCELLED_BY_USER
*"      ERROR
*"--------------------------------------------------------------------
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
