CLASS zcx_zdbframework_engine DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .

    CLASS-METHODS raise_from_symsg
      RAISING zcx_zdbframework_engine.

    METHODS raise_as_message
      IMPORTING  id_msgty TYPE sy-msgty DEFAULT 'E'
      EXCEPTIONS error.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_zdbframework_engine IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_from_symsg.
    DATA ls_textid TYPE scx_t100key.

    ls_textid-msgid = sy-msgid.
    ls_textid-msgno = sy-msgno.
    ls_textid-attr1 = sy-msgv1.
    ls_textid-attr2 = sy-msgv2.
    ls_textid-attr3 = sy-msgv3.
    ls_textid-attr4 = sy-msgv4.

    RAISE EXCEPTION TYPE zcx_zdbframework_engine
      EXPORTING
        textid = ls_textid.
  ENDMETHOD.

  METHOD raise_as_message.

    MESSAGE ID      me->if_t100_message~t100key-msgid
            TYPE    id_msgty
            NUMBER  me->if_t100_message~t100key-msgno
            WITH    me->if_t100_message~t100key-attr1
                    me->if_t100_message~t100key-attr2
                    me->if_t100_message~t100key-attr3
                    me->if_t100_message~t100key-attr4
            RAISING error.

  ENDMETHOD.

ENDCLASS.
