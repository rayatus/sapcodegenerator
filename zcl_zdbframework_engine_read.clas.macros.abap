*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*======================================================================
 DEFINE mac_error.
*======================================================================
   IF sy-subrc IS NOT INITIAL.
     IF sy-msgid  IS NOT INITIAL
     AND sy-msgty IS NOT INITIAL
     AND sy-msgno IS NOT INITIAL.
       MESSAGE ID      sy-msgid
               TYPE    sy-msgty
               NUMBER  sy-msgno
               WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING error.
     ELSE.
       MESSAGE 'Unexpected error' TYPE 'E' RAISING error ##NO_TEXT.
     ENDIF.
   ENDIF.
 END-OF-DEFINITION.

*======================================================================
 DEFINE mac_add_type.
*======================================================================
   CONCATENATE <ls_type>-typesrc &1 cl_abap_char_utilities=>newline
    INTO <ls_type>-typesrc SEPARATED BY space.
 END-OF-DEFINITION.

*======================================================================
 DEFINE mac_add_source.
*======================================================================
   INSERT INITIAL LINE INTO TABLE <ls_method_sources>-source ASSIGNING <ld_source>.
   <ld_source> = &1.
 END-OF-DEFINITION.
