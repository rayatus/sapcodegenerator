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
*| project homepage: http://code.google.com/p/sapcodegenerator/        |
*\---------------------------------------------------------------------/

*&---------------------------------------------------------------------*
*& Report  ZDBFRAMEWORK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdbframework.

TYPE-POOLS: seoc, seoo, seos.


PARAMETERS: p_clname  TYPE vseoclass-clsname OBLIGATORY,
            p_dbname  TYPE tabname16         OBLIGATORY.

PARAMETERS p_ttyp TYPE typename OBLIGATORY.
PARAMETERS p_devcls TYPE devclass OBLIGATORY.

DATA: lo_structdescr TYPE REF TO  cl_abap_structdescr,
      lt_fieldlist   TYPE ddfields.

DATA: lt_method_sources TYPE seo_method_source_table,
      ls_class          TYPE vseoclass,
      ld_string         TYPE string,
      ld_obj_name       TYPE e071-obj_name,
      lt_attrs          TYPE seoo_attributes_r,
      lt_meths          TYPE seoo_methods_r,
      lt_types          TYPE seoo_types_r,
      lt_parms          TYPE seos_parameters_r,
      ls_e_corrnr       TYPE trkorr,
      ls_i_korrnr       TYPE trkorr,
      lt_exceps         TYPE seos_exceptions_r.

INCLUDE zdbframework_create_find_muf01.
INCLUDE zdbframework_create_add_ranf01.
INCLUDE zdbframework_create_typesf01.
INCLUDE zdbframework_create_get_detf01.
INCLUDE zdbframework_create_save_def01.
INCLUDE zdbframework_create_save_muf01.
INCLUDE zdbframework_create_delete_f01.
INCLUDE zdbframework_create_delete_f02.
INCLUDE zdbframework_throw_error_f01.

DEFINE mac_error.

END-OF-DEFINITION.

START-OF-SELECTION.

  lo_structdescr ?= cl_abap_typedescr=>describe_by_name( p_dbname ).
  lt_fieldlist[] = lo_structdescr->get_ddic_field_list( ).
  DELETE lt_fieldlist WHERE keyflag = abap_false OR fieldname = 'MANDT' OR datatype = 'CLNT'.

  ls_class-clsname = p_clname.
  CONCATENATE 'DB Framework for:'(006) p_dbname INTO ls_class-descript SEPARATED BY space.
  ls_class-state   = seoc_state_implemented.

  PERFORM create_types USING    ls_class-clsname
                                lt_fieldlist[]
                                p_ttyp
                       CHANGING lt_attrs[]
                                lt_types[].

  PERFORM create_add_range      USING ls_class-clsname
                             CHANGING lt_meths
                                      lt_method_sources[]
                                      lt_parms[].

  PERFORM create_find_multiple  USING ls_class-clsname
                                      p_dbname
                                      lt_fieldlist[]
                                      p_ttyp
                             CHANGING lt_meths
                                      lt_method_sources[]
                                      lt_parms[]
                                      lt_exceps[].

  PERFORM create_get_detail  USING    ls_class-clsname
                                      p_dbname
                                      lt_fieldlist[]
                                      p_ttyp
                             CHANGING lt_meths
                                      lt_method_sources[]
                                      lt_parms[]
                                      lt_exceps[].

  PERFORM create_save_detail USING    ls_class-clsname
                                      p_dbname
                                      lt_fieldlist[]
                                      p_ttyp
                             CHANGING lt_meths
                                      lt_method_sources[]
                                      lt_parms[]
                                      lt_exceps[].

  PERFORM create_save_multiple USING    ls_class-clsname
                                        p_dbname
                                        lt_fieldlist[]
                                        p_ttyp
                               CHANGING lt_meths
                                        lt_method_sources[]
                                        lt_parms[]
                                        lt_exceps[].

  PERFORM create_delete_detail USING  ls_class-clsname
                                      p_dbname
                                      lt_fieldlist[]
                                      p_ttyp
                             CHANGING lt_meths
                                      lt_method_sources[]
                                      lt_parms[]
                                      lt_exceps[].

  PERFORM create_delete_multiple USING  ls_class-clsname
                                        p_dbname
                                        lt_fieldlist[]
                                        p_ttyp
                               CHANGING lt_meths
                                        lt_method_sources[]
                                        lt_parms[]
                                        lt_exceps[].


  PERFORM create_init_buffer USING    ls_class-clsname
                             CHANGING lt_meths
                                      lt_method_sources[]
                                      lt_parms[]
                                      lt_exceps[].

  CALL FUNCTION 'SEO_BUFFER_INIT'.

  CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
    EXPORTING
      corrnr         = ls_e_corrnr
      devclass       = p_devcls
      version        = seoc_version_active
      genflag        = abap_true
      overwrite      = abap_true
      method_sources = lt_method_sources
    IMPORTING
      korrnr         = ls_i_korrnr
    CHANGING
      class          = ls_class
      attributes     = lt_attrs
      methods        = lt_meths
      types          = lt_types
      parameters     = lt_parms
      exceps         = lt_exceps
    EXCEPTIONS
      OTHERS         = 999.
  mac_error.

* insert inactive sections into worklist
  ld_obj_name = ls_class-clsname.
  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = 'CPUB'
      obj_name          = ld_obj_name
    EXCEPTIONS
      wrong_object_name = 1.
  mac_error.

  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = 'CPRO'
      obj_name          = ld_obj_name
    EXCEPTIONS
      wrong_object_name = 1.
  mac_error.

  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object            = 'CPRI'
      obj_name          = ld_obj_name
    EXCEPTIONS
      wrong_object_name = 1.
  mac_error.


  CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
             author              = sy-uname
             global_lock         = 'X'
             object              = ld_obj_name
             object_class        = 'CLASS'
             devclass            = p_devcls
*            KORRNUM             = CORRNUMBER_LOCAL
             master_language     = sy-langu
*            PROGRAM             = PROGRAM_LOCAL
             mode                = 'INSERT'
*       IMPORTING
*            AUTHOR              = UNAME
*            KORRNUM             = CORRNUMBER_LOCAL
*            DEVCLASS            = DEVCLASS_LOCAL
        EXCEPTIONS
             cancelled           = 1
             permission_failure  = 2
             unknown_objectclass = 3.
  mac_error.

  SET PARAMETER ID 'CLASS' FIELD ls_class-clsname.

  CONCATENATE 'Class '(007)  ls_class-clsname  ' created successfullly'(008) INTO ld_string SEPARATED BY space.
  MESSAGE ld_string TYPE 'I'.
