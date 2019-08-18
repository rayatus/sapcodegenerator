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

"! <p class="shorttext synchronized" lang="en">Creates a READ ABAP class</p>
CLASS zcl_zdbframework_engine_read DEFINITION
  PUBLIC
  CREATE PROTECTED
  INHERITING FROM zcl_zdbframework_engine_base.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Generates an ABAP READ</p>
    "! Generates an ABAP class which encapsulates some logic in order to retrieve content from the specified DB Table.
    "! <br/>
    "! Only tablenames in <em>customer namespace</em> are allowed.
    "!
    "! @parameter id_devclass | <p class="shorttext synchronized" lang="en">DevClass where generated ABAP Class will be placed</p>
    "! @parameter id_dbname | <p class="shorttext synchronized" lang="en">Table which will be used for creating the READ Class</p>
    "! @parameter id_classname | <p class="shorttext synchronized" lang="en">Class name which will be created</p>
    "! @parameter id_ttyp | <p class="shorttext synchronized" lang="en">Table type name for returning several entries</p>
    "! @parameter id_trkorr | <p class="shorttext synchronized" lang="en">Transport order where the class will be placed</p>
    "! @exception dbname_not_permited | <p class="shorttext synchronized" lang="en">Table name is not permited</p>
    "! @exception error | <p class="shorttext synchronized" lang="en">Unexpected error</p>
    CLASS-METHODS generate_class
      IMPORTING  id_devclass         TYPE devclass
                 id_dbname           TYPE tabname16
                 id_classname        TYPE vseoclass-clsname
                 id_ttyp             TYPE typename
                 id_trkorr           TYPE trkorr OPTIONAL
      EXPORTING  VALUE(ed_classname) TYPE vseoclass-clsname
                 VALUE(ed_korrnum)   TYPE trkorr
      EXCEPTIONS dbname_not_permited
                 error.

    "! <p class="shorttext synchronized" lang="en">Gets ABAP Class prefix</p>
    "! Retrieves the prefix for ABAP class name
    "!
    "! @parameter rd_prefix | <p class="shorttext synchronized" lang="en">Prefix</p>
    CLASS-METHODS get_class_prefix
      RETURNING VALUE(rd_prefix) TYPE string.
    "! <p class="shorttext synchronized" lang="en">Gets ABAP Class suffix</p>
    "!
    "! @parameter rd_suffix | <p class="shorttext synchronized" lang="en">Suffix</p>
    CLASS-METHODS get_class_suffix
      RETURNING VALUE(rd_suffix) TYPE string.

  PROTECTED SECTION.

    TYPE-POOLS:
      seoc,
      seoo,
      seos.

    DATA:
      "! DevClass where generated ABAP Class will be placed
      md_devclass    TYPE devclass,
      "! Table which will be used for creating the READ Class
      md_dbname      TYPE tabname16,
      "! Fieldlist of DBNAME table
      mt_fieldlist   TYPE ddfields,
      "! Table type name which will be used for returning several entries by GET_LIST method
      md_ttyp        TYPE typename,
      "! Transport order where the generated ABAP class will be/has been placed
      md_korrnum     TYPE trkorr,
      "! ABAP Class information
      ms_class       TYPE vseoclass,
      "! Generated class attributes
      mt_attributes  TYPE seoo_attributes_r,
      "! Generated class types
      mt_types       TYPE seoo_types_r,
      "! Generated methods
      mt_methods     TYPE seoo_methods_r,
      "! Generated parameters per method
      mt_parameters  TYPE seos_parameters_r,
      "! Generated source code per method
      mt_source_code TYPE seo_method_source_table,
      "! Generated method exceptions per method
      mt_exceptions  TYPE seos_exceptions_r.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter id_devclass | <p class="shorttext synchronized" lang="en">DevClass where generated ABAP Class will be placed</p>
    "! @parameter id_dbname | <p class="shorttext synchronized" lang="en">Table which will be used for creating the READ Class</p>
    "! @parameter id_classname | <p class="shorttext synchronized" lang="en">Class name which will be created</p>
    "! @parameter id_ttyp | <p class="shorttext synchronized" lang="en">Table type name for returning several entries</p>
    "! @parameter id_trkorr | <p class="shorttext synchronized" lang="en">Transport order where the class will be placed</p>
    METHODS constructor
      IMPORTING id_devclass  TYPE devclass
                id_dbname    TYPE tabname16
                id_classname TYPE vseoclass-clsname
                id_ttyp      TYPE typename
                id_trkorr    TYPE trkorr OPTIONAL.
    "! <p class="shorttext synchronized" lang="en">Retrieves DB fields</p>
    "! Retrieves DB table current fields excluding client field (MANDT/CLNT)
    METHODS _get_fieldlist.
    "! <p class="shorttext synchronized" lang="en">Generates Class header</p>
    "!
    METHODS _generate_class_header.
    "! <p class="shorttext synchronized" lang="en">Generates ABAP Class types</p>
    "!
    METHODS _generate_types.
    "! <p class="shorttext synchronized" lang="en">Generates SELECT_ALL</p>
    "!
    METHODS _generate_method_select_all.
    "! <p class="shorttext synchronized" lang="en">Generates ADD_RANGE</p>
    "!
    METHODS _generate_method_add_range.
    "! <p class="shorttext synchronized" lang="en">Generates GET_LIST</p>
    "!
    METHODS _generate_method_get_list.
    "! <p class="shorttext synchronized" lang="en">Generates GET_DETAIL</p>
    "!
    METHODS _generate_method_get_detail.
    "! <p class="shorttext synchronized" lang="en">Generates INIT_BUFFER</p>
    "!
    METHODS _generate_method_init_buffer.
    "! <p class="shorttext synchronized" lang="en">ABAP Class generator</p>
    "! According to the generated source code and method signatures this method creates in system the corresponding READ class and
    "! places it's creation in a transport order.
    "!
    "! @parameter ed_classname | <p class="shorttext synchronized" lang="en">Name of the generated ABAP class</p>
    "! @parameter ed_korrnum | <p class="shorttext synchronized" lang="en">Transport order where the generated class has been placed.</p>
    "! @exception error | <p class="shorttext synchronized" lang="en">Unexpected error</p>
    METHODS _generate_class
      EXPORTING
        VALUE(ed_classname) TYPE vseoclass-clsname
        VALUE(ed_korrnum)   TYPE trkorr
      EXCEPTIONS
        error.
    "! <p class="shorttext synchronized" lang="en">Determines if table is a customizing table or not</p>
    "!
    "! @parameter rf_is_custo | <p class="shorttext synchronized" lang="en">Returns <em>abap_true</em> if is a customizing table.</p>
    METHODS _is_custo
      RETURNING
        VALUE(rf_is_custo) TYPE abap_bool.



  PRIVATE SECTION.
    CONSTANTS: BEGIN OF mc_fieldname,
                 mandant TYPE string VALUE 'MANDT' ##NO_TEXT,
                 client  TYPE string VALUE 'CLNT' ##NO_TEXT,
               END OF mc_fieldname.

    CONSTANTS: BEGIN OF mc_class_section,
                 public    TYPE c LENGTH 4 VALUE 'CPUB' ##NO_TEXT,
                 protected TYPE c LENGTH 4 VALUE 'CPRO'  ##NO_TEXT,
                 private   TYPE c LENGTH 4 VALUE 'CPRI' ##NO_TEXT,
                 all_class TYPE c LENGTH 5 VALUE 'CLASS' ##NO_TEXT,
               END OF mc_class_section.

    CONSTANTS mc_exception_not_found TYPE string VALUE 'CX_NO_DATA_FOUND' ##NO_TEXT.
    CONSTANTS mc_class_prefix TYPE string VALUE 'ZCL_' ##NO_TEXT.
    CONSTANTS mc_class_suffix TYPE string VALUE '_READ' ##NO_TEXT.
    CONSTANTS mc_insert TYPE c LENGTH 6 VALUE 'INSERT' ##NO_TEXT.
    CONSTANTS mc_customizing_table TYPE string VALUE 'C' ##NO_TEXT.
    CONSTANTS mc_active_version TYPE string VALUE 'A' ##NO_TEXT.





ENDCLASS.



CLASS zcl_zdbframework_engine_read IMPLEMENTATION.

  METHOD generate_class.

    DATA: lo_engine TYPE REF TO zcl_zdbframework_engine_read.

    CREATE OBJECT lo_engine
      EXPORTING
        id_devclass  = id_devclass
        id_dbname    = id_dbname
        id_classname = id_classname
        id_ttyp      = id_ttyp.

    lo_engine->chk_is_dbname_permitted( EXPORTING  id_dbname = id_dbname
                                        EXCEPTIONS OTHERS    = 999 ).
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID      sy-msgid
              TYPE    sy-msgty
              NUMBER  sy-msgno
              WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING dbname_not_permited .
    ENDIF.

    lo_engine->_get_fieldlist( ).

    lo_engine->_generate_class_header( ).
    lo_engine->_generate_types( ).
    lo_engine->_generate_method_add_range( ).

    IF lo_engine->_is_custo( ) = abap_true.
      lo_engine->_generate_method_select_all( ).
    ENDIF.
    lo_engine->_generate_method_get_list( ).
    lo_engine->_generate_method_init_buffer( ).

    lo_engine->_generate_method_get_detail( ).


    lo_engine->_generate_class( IMPORTING ed_classname = ed_classname
                                          ed_korrnum   = ed_korrnum
                                EXCEPTIONS OTHERS      = 999 ).
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID      sy-msgid
              TYPE    sy-msgty
              NUMBER  sy-msgno
              WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING error.
    ENDIF.

  ENDMETHOD.



  METHOD get_class_prefix.
    rd_prefix = mc_class_prefix.
  ENDMETHOD.
  METHOD get_class_suffix.
    rd_suffix = mc_class_suffix.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

    me->md_dbname         = id_dbname.
    me->ms_class-clsname  = id_classname.
    me->md_devclass       = id_devclass.
    me->md_ttyp           = id_ttyp.
  ENDMETHOD.

  METHOD _get_fieldlist.
    DATA lo_structdescr TYPE REF TO  cl_abap_structdescr.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( md_dbname ).
    mt_fieldlist[] = lo_structdescr->get_ddic_field_list( ).
    DELETE mt_fieldlist WHERE keyflag = abap_false OR fieldname = mc_fieldname-mandant OR datatype = mc_fieldname-client.

  ENDMETHOD.

  METHOD _generate_class_header.

    CONCATENATE TEXT-006 md_dbname INTO ms_class-descript SEPARATED BY space.
    ms_class-state   = seoc_state_implemented.

  ENDMETHOD.

  METHOD _generate_types.

    DATA: ld_string TYPE string.
    DATA: ld_string2 TYPE string.

    FIELD-SYMBOLS: <ls_attribute> LIKE LINE OF mt_attributes,
                   <ls_fieldlist> LIKE LINE OF mt_fieldlist,
                   <ls_type>      LIKE LINE OF mt_types.


    INSERT INITIAL LINE INTO TABLE mt_attributes ASSIGNING <ls_attribute>.
    <ls_attribute>-clsname    = ms_class-clsname.
    <ls_attribute>-cmpname    = 'MS_RANGES' ##NO_TEXT.
    <ls_attribute>-descript   = 'Selection Ranges'(012).
    <ls_attribute>-exposure   = seoc_exposure_private.
    <ls_attribute>-state      = seoc_state_implemented.
    <ls_attribute>-editorder  = 1.
    <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
    <ls_attribute>-type       = 'MTYP_RANGES' ##NO_TEXT.
    <ls_attribute>-typtype    = 1.

    INSERT INITIAL LINE INTO TABLE mt_attributes ASSIGNING <ls_attribute>.
    <ls_attribute>-clsname    = ms_class-clsname.
    <ls_attribute>-cmpname    = 'MT_BUFFER' ##NO_TEXT.
    <ls_attribute>-descript   = 'Data Buffer'(011).
    <ls_attribute>-exposure   = seoc_exposure_private.
    <ls_attribute>-state      = seoc_state_implemented.
    <ls_attribute>-editorder  = 2.
    <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
    <ls_attribute>-type       = md_ttyp.
    <ls_attribute>-typtype    = 1.

    IF _is_custo( ) = abap_true.
      INSERT INITIAL LINE INTO TABLE mt_attributes ASSIGNING <ls_attribute>.
      <ls_attribute>-clsname    = ms_class-clsname.
      <ls_attribute>-cmpname    = 'MF_ALL_DATA_RETRIEVED' ##NO_TEXT.
      <ls_attribute>-descript   = 'All data has been retrieved'(010).
      <ls_attribute>-exposure   = seoc_exposure_private.
      <ls_attribute>-state      = seoc_state_implemented.
      <ls_attribute>-editorder  = 3.
      <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
      <ls_attribute>-type       = 'ABAP_BOOL' ##NO_TEXT.
      <ls_attribute>-typtype    = 1.
    ENDIF.

    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist>.
      AT FIRST.
        INSERT INITIAL LINE INTO TABLE mt_types ASSIGNING <ls_type>.
        <ls_type>-clsname   = ms_class-clsname.
        <ls_type>-cmpname   = 'MTYP_RANGES' ##NO_TEXT.
        <ls_type>-exposure  = seoc_exposure_private.
        <ls_type>-state     = seoc_state_implemented.
        <ls_type>-editorder = 1.
        <ls_type>-typtype = 4.

        mac_add_type  'BEGIN OF mtyp_ranges,' ##NO_TEXT.
      ENDAT.

      CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO ld_string2 SEPARATED BY '-'.
      CONCATENATE <ls_fieldlist>-fieldname 'TYPE RANGE OF' ld_string2 ',' INTO ld_string SEPARATED BY space ##NO_TEXT..
      mac_add_type ld_string.

      AT LAST.
        mac_add_type  '       END   OF mtyp_ranges' ##NO_TEXT..
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD _generate_method_select_all.

    DATA:
      ld_string           TYPE string.

    FIELD-SYMBOLS:
      <ls_method>         LIKE LINE OF mt_methods,
      <ls_method_sources> LIKE LINE OF mt_source_code,
      <ld_source>         LIKE LINE OF <ls_method_sources>-source.



    INSERT INITIAL LINE INTO TABLE mt_source_code ASSIGNING <ls_method_sources>.
    INSERT INITIAL LINE INTO TABLE mt_methods     ASSIGNING <ls_method>.
    <ls_method>-clsname    = ms_class-clsname.
    <ls_method>-state      = seoc_state_implemented.
    <ls_method>-mtdnewexc  = abap_true.
    <ls_method>-mtddecltyp = '1'.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = '_SELECT_ALL' ##NO_TEXT.
    <ls_method>-descript   = 'Selects all data from DB and stores in BUFFER'(009).
    <ls_method>-exposure   = seoc_exposure_private.

    mac_add_source space.
    CONCATENATE '  SELECT * FROM' md_dbname 'INTO TABLE mt_buffer ORDER BY PRIMARY KEY.' INTO ld_string SEPARATED BY space ##NO_TEXT.
    mac_add_source ld_string.
    mac_add_source space.
    mac_add_source '  mf_all_data_retrieved = abap_true.' ##NO_TEXT.
    mac_add_source space.

  ENDMETHOD.

  METHOD _generate_method_add_range.

    FIELD-SYMBOLS:
      <ls_method>         LIKE LINE OF mt_methods,
      <ls_method_sources> LIKE LINE OF mt_source_code,
      <ls_parameter>      LIKE LINE OF mt_parameters,
      <ld_source>         LIKE LINE OF <ls_method_sources>-source.

    DATA: ld_order   LIKE <ls_parameter>-editorder.

    INSERT INITIAL LINE INTO TABLE mt_source_code ASSIGNING <ls_method_sources>.
    INSERT INITIAL LINE INTO TABLE mt_methods        ASSIGNING <ls_method>.
    <ls_method>-clsname    = ms_class-clsname.
    <ls_method>-state      = seoc_state_implemented.
    <ls_method>-mtdnewexc  = abap_true.
    <ls_method>-mtddecltyp = '1'  ##NO_TEXT.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = '_ADD_RANGE' ##NO_TEXT.
    <ls_method>-descript   = 'Add to Range'(013).
    <ls_method>-exposure   = seoc_exposure_private.


    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname    = <ls_method>-clsname.
    <ls_parameter>-cmpname    = <ls_method>-cmpname.
    <ls_parameter>-sconame    = 'ID_LOW' ##NO_TEXT.
    <ls_parameter>-cmptype    = seoo_cmptype_attribute.
    <ls_parameter>-mtdtype    = seoo_mtdtype_method.
    <ls_parameter>-editorder  = ld_order.
    <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
    <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
    <ls_parameter>-typtype    = seoo_typtype_type.
    <ls_parameter>-type       = 'ANY' ##NO_TEXT.

    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname    = <ls_method>-clsname.
    <ls_parameter>-cmpname    = <ls_method>-cmpname.
    <ls_parameter>-sconame    = 'CT_RANGE'  ##NO_TEXT.
    <ls_parameter>-cmptype    = seoo_cmptype_attribute.
    <ls_parameter>-mtdtype    = seoo_mtdtype_method.
    <ls_parameter>-editorder  = ld_order.
    <ls_parameter>-pardecltyp = seos_pardecltyp_changing.
    <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
    <ls_parameter>-typtype    = seoo_typtype_type.
    <ls_parameter>-type       = 'ANY TABLE'  ##NO_TEXT.


    mac_add_source space.
    mac_add_source '  FIELD-SYMBOLS: <ls_row>    TYPE any,' ##NO_TEXT.
    mac_add_source '                 <ld_sign>   TYPE any,' ##NO_TEXT.
    mac_add_source '                 <ld_option> TYPE any,' ##NO_TEXT.
    mac_add_source '                 <ld_low>    TYPE any.' ##NO_TEXT.
    mac_add_source space.
    mac_add_source '  INSERT INITIAL LINE INTO TABLE ct_range ASSIGNING <ls_row>.' ##NO_TEXT.
    mac_add_source space.
    mac_add_source TEXT-001.
    mac_add_source '    <ld_low> = id_low.' ##NO_TEXT.
    mac_add_source space.
    mac_add_source TEXT-002.
    mac_add_source TEXT-004.
    mac_add_source space.
    mac_add_source TEXT-003.
    mac_add_source TEXT-005.
  ENDMETHOD.

  METHOD _generate_method_get_list.

    FIELD-SYMBOLS: <ls_fieldlist>      LIKE LINE OF mt_fieldlist,
                   <ls_method>         LIKE LINE OF mt_methods,
                   <ls_method_sources> LIKE LINE OF mt_source_code,
                   <ls_parameter>      LIKE LINE OF mt_parameters,
                   <ld_source>         LIKE LINE OF <ls_method_sources>-source,
                   <ls_exception>      LIKE LINE OF mt_exceptions.

    DATA: ld_order   LIKE <ls_parameter>-editorder,
          ld_string  TYPE string,
          ld_string2 TYPE string.

    INSERT INITIAL LINE INTO TABLE mt_source_code ASSIGNING <ls_method_sources>.
    INSERT INITIAL LINE INTO TABLE mt_methods        ASSIGNING <ls_method>.
    <ls_method>-clsname    = ms_class-clsname.
    <ls_method>-state      = seoc_state_implemented.
    <ls_method>-mtdnewexc  = abap_true.
    <ls_method>-mtddecltyp = '1'  ##NO_TEXT.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'GET_LIST' ##NO_TEXT.
    <ls_method>-descript   = 'Find Multiple details by keys'(014) ##NO_TEXT.
    <ls_method>-exposure   = seoc_exposure_public.


    mac_add_source space.
    mac_add_source '  DATA: ls_buffer LIKE LINE OF mt_buffer.' ##NO_TEXT.
    mac_add_source space.
    mac_add_source '  FIELD-SYMBOLS: <ls_list> LIKE LINE OF et_list.' ##NO_TEXT.
    mac_add_source space.

    IF _is_custo( ) = abap_true.
      mac_add_source space.
      mac_add_source '  IF mf_all_data_retrieved = abap_false.' ##NO_TEXT.
      mac_add_source '    _SELECT_ALL( ).' ##NO_TEXT.
      mac_add_source '  ENDIF.' ##NO_TEXT.
    ENDIF.

    mac_add_source space.
    mac_add_source '  CLEAR ms_ranges.' ##NO_TEXT.

    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist>.
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO ld_string ##NO_TEXT.

      mac_add_source space.
      CONCATENATE '  IF' ld_string 'IS SUPPLIED.' INTO ld_string2 SEPARATED BY space ##NO_TEXT.
      mac_add_source ld_string2.
      CONCATENATE '    _add_range( EXPORTING id_low   =' ld_string INTO ld_string2 SEPARATED BY space ##NO_TEXT.
      mac_add_source ld_string2.
      CONCATENATE '                CHANGING  ct_range = ms_ranges-' <ls_fieldlist>-fieldname ' ).' INTO ld_string2 ##NO_TEXT.
      mac_add_source ld_string2.
      mac_add_source '  ENDIF.' ##NO_TEXT.
    ENDLOOP.

    "First of all try to read buffered data from previous SELECt
    mac_add_source space.
    mac_add_source space.
    mac_add_source '  LOOP AT mt_buffer INTO ls_buffer' ##NO_TEXT.

    " prepare Where for Loop with only key fields
    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist>.
      ADD 1 TO ld_order.

      INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.

      <ls_parameter>-clsname    =  <ls_method>-clsname.
      <ls_parameter>-cmpname    =  <ls_method>-cmpname.
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO <ls_parameter>-sconame ##NO_TEXT.
      <ls_parameter>-descript   = <ls_fieldlist>-scrtext_m.
      <ls_parameter>-cmptype    = seoo_cmptype_attribute.
      <ls_parameter>-mtdtype    = seoo_mtdtype_method.
      <ls_parameter>-editorder  = ld_order.
      <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
      <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
      <ls_parameter>-typtype    = seoo_typtype_type.
      <ls_parameter>-paroptionl = abap_true.
      CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO <ls_parameter>-type SEPARATED BY '-' ##NO_TEXT.

*   Build WHERE clause
      CONCATENATE  'ms_ranges-' <ls_fieldlist>-fieldname INTO ld_string ##NO_TEXT.
      IF ld_order = 1.
        CONCATENATE '  WHERE' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space ##NO_TEXT.
      ELSE.
        CONCATENATE '    AND' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space ##NO_TEXT.
      ENDIF.
      mac_add_source ld_string.

    ENDLOOP.

    CONCATENATE <ld_source> '.' INTO <ld_source> ##NO_TEXT.

    mac_add_source space.
    mac_add_source '  INSERT INITIAL LINE INTO TABLE et_list ASSIGNING <ls_list>.' ##NO_TEXT.
    mac_add_source '  MOVE-CORRESPONDING ls_buffer to <ls_list>.' ##NO_TEXT.
    mac_add_source space.
    mac_add_source '  ENDLOOP.' ##NO_TEXT.


    IF _is_custo( ) = abap_false.
      "Now, if it's a customized table and nothing is stored in BUFFER lets try with a direc Select
      mac_add_source space.
      mac_add_source 'IF sy-subrc IS NOT INITIAL.' ##NO_TEXT.
      mac_add_source space.
      CONCATENATE    '  SELECT * FROM ' md_dbname INTO ld_string SEPARATED BY space RESPECTING BLANKS ##NO_TEXT.
      mac_add_source ld_string.
      mac_add_source '  APPENDING CORRESPONDING FIELDS OF TABLE et_list' ##NO_TEXT.

* Only key fields
      CLEAR ld_order.
      LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist>.
        ADD 1 TO ld_order.

*   Build WHERE clause
        CONCATENATE  'ms_ranges-' <ls_fieldlist>-fieldname INTO ld_string ##NO_TEXT.
        IF ld_order = 1.
          CONCATENATE '  WHERE' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space ##NO_TEXT.
        ELSE.
          CONCATENATE '    AND' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space ##NO_TEXT.
        ENDIF.
        mac_add_source ld_string.

      ENDLOOP.

      CONCATENATE <ld_source> '.' INTO <ld_source> ##NO_TEXT.
      mac_add_source '    INSERT LINES OF et_list INTO TABLE mt_buffer.' ##NO_TEXT.
      mac_add_source '    SORT mt_buffer.' ##NO_TEXT.
      mac_add_source '    DELETE ADJACENT DUPLICATES FROM mt_buffer.' ##NO_TEXT.


      mac_add_source 'ENDIF.' ##NO_TEXT.

    ENDIF.


    mac_add_source space.
    mac_add_source '  IF et_list IS INITIAL.' ##NO_TEXT.
    CONCATENATE '    RAISE EXCEPTION TYPE ' mc_exception_not_found '.' INTO ld_string RESPECTING BLANKS ##NO_TEXT.
    mac_add_source ld_string.

    mac_add_source '  ENDIF.' ##NO_TEXT.


* Return parameters
    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname    =  <ls_method>-clsname.
    <ls_parameter>-cmpname    =  <ls_method>-cmpname.
    <ls_parameter>-sconame    = 'ET_LIST' ##NO_TEXT.
    <ls_parameter>-descript   = 'List Details'(015).
    <ls_parameter>-cmptype    = seoo_cmptype_attribute.
    <ls_parameter>-mtdtype    = seoo_mtdtype_method.
    <ls_parameter>-editorder  = ld_order.
    <ls_parameter>-typtype    = seoo_typtype_type.
    <ls_parameter>-pardecltyp = seos_pardecltyp_exporting.
    <ls_parameter>-type       = md_ttyp.
    <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.


*  Add exception
    INSERT INITIAL LINE INTO TABLE mt_exceptions ASSIGNING <ls_exception>.
    <ls_exception>-clsname  = <ls_method>-clsname.
    <ls_exception>-cmpname  = <ls_method>-cmpname.
    <ls_exception>-sconame  = mc_exception_not_found ##NO_TEXT.
*  <ls_exception>-version  = seoc_version_active.
    <ls_exception>-descript = 'Not Found'(016).
  ENDMETHOD.

  METHOD _generate_method_get_detail.
    FIELD-SYMBOLS: <ls_fieldlist>      LIKE LINE OF mt_fieldlist,
                   <ls_method>         LIKE LINE OF mt_methods,
                   <ls_method_sources> LIKE LINE OF mt_source_code,
                   <ls_parameter>      LIKE LINE OF mt_parameters,
                   <ld_source>         LIKE LINE OF <ls_method_sources>-source,
                   <ls_exception>      LIKE LINE OF mt_exceptions.

    DATA: ld_order   LIKE <ls_parameter>-editorder,
          ld_string2 TYPE string,
          ld_string  TYPE string.

    INSERT INITIAL LINE INTO TABLE mt_source_code ASSIGNING <ls_method_sources>.
    INSERT INITIAL LINE INTO TABLE mt_methods     ASSIGNING <ls_method>.
    <ls_method>-clsname   = ms_class-clsname.
    <ls_method>-state     = seoc_state_implemented.
    <ls_method>-mtdnewexc = abap_true.
    <ls_method>-mtddecltyp = '1' ##NO_TEXT.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'GET_DETAILS' ##NO_TEXT.
    <ls_method>-descript  = 'Find details by keys'(017).
    <ls_method>-exposure  = seoc_exposure_public.

    mac_add_source space.
    CONCATENATE '  DATA lt_list      TYPE' md_ttyp '.' INTO ld_string SEPARATED BY space ##NO_TEXT.
    mac_add_source ld_string.
    mac_add_source '  DATA ls_list      LIKE LINE OF lt_list.' ##NO_TEXT.
    CONCATENATE '  DATA lo_exception TYPE REF TO ' mc_exception_not_found '.' INTO ld_string SEPARATED BY space ##NO_TEXT.
    mac_add_source ld_string.


    mac_add_source space.
    mac_add_source '  READ TABLE mt_buffer INTO rs_result' ##NO_TEXT.
    mac_add_source '    WITH KEY' ##NO_TEXT.
    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist> .
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO ld_string ##NO_TEXT.
      CONCATENATE  <ls_fieldlist>-fieldname '=' ld_string INTO ld_string SEPARATED BY space ##NO_TEXT.

      AT FIRST.
        CONCATENATE <ld_source> ld_string INTO <ld_source> SEPARATED BY space.
        CONTINUE.
      ENDAT.

      CLEAR ld_string2.
      DO 6 TIMES.
        CONCATENATE ld_string2 cl_abap_char_utilities=>horizontal_tab INTO ld_string2.
      ENDDO.
      CONCATENATE ld_string2 ld_string INTO ld_string SEPARATED BY space.
      mac_add_source ld_string.

    ENDLOOP.
    CONCATENATE <ld_source> '.' INTO <ld_source> ##NO_TEXT.

    mac_add_source space.
    mac_add_source '  IF NOT sy-subrc IS INITIAL.' ##NO_TEXT.

    mac_add_source space.
    mac_add_source '    TRY.' ##NO_TEXT.
    mac_add_source '        get_list( EXPORTING' ##NO_TEXT.

* Only key fields
    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist> .
      ADD 1 TO ld_order.

      INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.

      <ls_parameter>-clsname    =  <ls_method>-clsname.
      <ls_parameter>-cmpname    =  <ls_method>-cmpname.
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO <ls_parameter>-sconame ##NO_TEXT.
      <ls_parameter>-descript   = <ls_fieldlist>-scrtext_m.
      <ls_parameter>-cmptype    = seoo_cmptype_attribute.
      <ls_parameter>-mtdtype    = seoo_mtdtype_method.
      <ls_parameter>-editorder  = ld_order.
      <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
      <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
      <ls_parameter>-typtype    = seoo_typtype_type.
      CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO <ls_parameter>-type SEPARATED BY '-' ##NO_TEXT.

      CONCATENATE <ls_parameter>-sconame '=' <ls_parameter>-sconame INTO ld_string SEPARATED BY space ##NO_TEXT.

      IF ld_order = 1.
        CONCATENATE <ld_source> space ld_string INTO <ld_source> SEPARATED BY space.
      ELSE.
        CLEAR ld_string2.
        DO 14 TIMES.
          CONCATENATE ld_string2 cl_abap_char_utilities=>horizontal_tab INTO ld_string2.
        ENDDO.
        CONCATENATE ld_string2 ld_string INTO ld_string SEPARATED BY space.
        mac_add_source ld_string.
      ENDIF.

    ENDLOOP.
    mac_add_source '                  IMPORTING  et_list = lt_list ).' ##NO_TEXT.

    mac_add_source space.
    mac_add_source '      READ TABLE lt_list INDEX 1 INTO ls_list.' ##NO_TEXT.
    mac_add_source '      MOVE-CORRESPONDING ls_list TO rs_result.' ##NO_TEXT.

    mac_add_source space.
    CONCATENATE '      CATCH ' mc_exception_not_found ' INTO lo_exception .' INTO ld_string RESPECTING BLANKS ##NO_TEXT.
    mac_add_source ld_string.

    mac_add_source '        RAISE EXCEPTION lo_exception.' ##NO_TEXT.
    mac_add_source '    ENDTRY.' ##NO_TEXT.

    mac_add_source space.
    mac_add_source '  ENDIF.' ##NO_TEXT.


* Add return parameter
    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname   =  <ls_method>-clsname.
    <ls_parameter>-cmpname   =  <ls_method>-cmpname.
    <ls_parameter>-sconame   = 'RS_RESULT' ##NO_TEXT.
    <ls_parameter>-descript  = 'Details'(018).
    <ls_parameter>-cmptype   = seoo_cmptype_attribute.
    <ls_parameter>-mtdtype   = seoo_mtdtype_method.
    <ls_parameter>-editorder = ld_order.
    <ls_parameter>-typtype   = seoo_typtype_type.
    <ls_parameter>-pardecltyp = seos_pardecltyp_returning.
    <ls_parameter>-type      = <ls_fieldlist>-tabname.
    <ls_parameter>-parpasstyp = seos_parpasstyp_byvalue.

*  add Excepction
    INSERT INITIAL LINE INTO TABLE mt_exceptions ASSIGNING <ls_exception>.
    <ls_exception>-clsname  = <ls_method>-clsname.
    <ls_exception>-cmpname  = <ls_method>-cmpname.
    <ls_exception>-sconame  = mc_exception_not_found.
*  <ls_exception>-version  = seoc_version_active.
    <ls_exception>-descript = 'Not data found'(019).
  ENDMETHOD.

  METHOD _generate_method_init_buffer.
    FIELD-SYMBOLS: <ls_method>         LIKE LINE OF mt_methods,
                   <ls_method_sources> LIKE LINE OF mt_source_code,
                   <ls_parameter>      LIKE LINE OF mt_parameters,
                   <ld_source>         LIKE LINE OF <ls_method_sources>-source,
                   <ls_exception>      LIKE LINE OF mt_exceptions.

    DATA: ld_order  LIKE <ls_parameter>-editorder,
          ld_string TYPE string.


    INSERT INITIAL LINE INTO TABLE mt_source_code ASSIGNING <ls_method_sources>.
    INSERT INITIAL LINE INTO TABLE mt_methods     ASSIGNING <ls_method>.
    <ls_method>-clsname   = ms_class-clsname.
    <ls_method>-state     = seoc_state_implemented.
    <ls_method>-mtdnewexc = abap_true.
    <ls_method>-mtddecltyp = '1' ##NO_TEXT.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'INIT_BUFFER' ##NO_TEXT.
    <ls_method>-descript  = 'Initializes Buffer Data'(021).
    <ls_method>-exposure  = seoc_exposure_public.

    mac_add_source '  CLEAR mt_buffer[].' ##NO_TEXT.
    IF _is_custo( ) = abap_true.
      mac_add_source '  mf_all_data_retrieved = abap_false.' ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD _generate_class.

    DATA: ld_obj_name TYPE e071-obj_name,
          ld_string   TYPE string,
          ls_e_corrnr TYPE trkorr,
          ls_i_korrnr TYPE trkorr.


*   Generate ABAP Class
    CALL FUNCTION 'SEO_BUFFER_INIT'.

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        corrnr         = ls_e_corrnr
        devclass       = md_devclass
        version        = seoc_version_active
        genflag        = abap_true
        overwrite      = abap_true
        method_sources = mt_source_code
      IMPORTING
        korrnr         = ls_i_korrnr
      CHANGING
        class          = ms_class
        attributes     = mt_attributes
        methods        = mt_methods
        types          = mt_types
        parameters     = mt_parameters
        exceps         = mt_exceptions
      EXCEPTIONS
        OTHERS         = 999.
    mac_error.

*   INSERT inactive sections INTO worklist
    ld_obj_name = ms_class-clsname.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object   = mc_class_section-public
        obj_name = ld_obj_name
      EXCEPTIONS
        OTHERS   = 999.
    mac_error.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object   = mc_class_section-protected
        obj_name = ld_obj_name
      EXCEPTIONS
        OTHERS   = 999.
    mac_error.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object   = mc_class_section-private
        obj_name = ld_obj_name
      EXCEPTIONS
        OTHERS   = 999.
    mac_error.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        author          = sy-uname
        global_lock     = abap_true
        object          = ld_obj_name
        object_class    = mc_class_section-all_class
        devclass        = md_devclass
        korrnum         = md_korrnum
        master_language = sy-langu
*       PROGRAM         = PROGRAM_LOCAL
        mode            = mc_insert
      IMPORTING
*       AUTHOR          = UNAME
        korrnum         = ed_korrnum
*       DEVCLASS        = DEVCLASS_LOCAL
      EXCEPTIONS
        OTHERS          = 999.
    mac_error.

    ed_classname = ms_class-clsname.
    SET PARAMETER ID mc_class_section-all_class FIELD ed_classname.


  ENDMETHOD.

  METHOD _is_custo.
    "Is a customizing table?
    SELECT COUNT( * ) FROM dd02l
      WHERE tabname = md_dbname
        AND contflag = mc_customizing_table
        AND actflag  = mc_active_version.
    IF sy-subrc IS INITIAL.
      rf_is_custo = abap_true.
    ELSE.
      rf_is_custo = abap_false.
    ENDIF.
  ENDMETHOD.



ENDCLASS.
