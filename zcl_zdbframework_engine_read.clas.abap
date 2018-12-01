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
  CREATE PROTECTED .

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
    "! <p class="shorttext synchronized" lang="en">Verifies if DB Table is permitted</p>
    "! <br/>
    "! Only tablenames in <em>customer namespace</em> are allowed.
    "!
    "! @parameter id_dbname | <p class="shorttext synchronized" lang="en">DB tabname</p>
    "! @exception dbname_not_permited | <p class="shorttext synchronized" lang="en">Table name is not permited</p>
    CLASS-METHODS chk_is_dbname_permitted
      IMPORTING  id_dbname TYPE tabname16
      EXCEPTIONS dbname_not_permited.
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

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_zdbframework_engine_read IMPLEMENTATION.

  METHOD generate_class.

    DATA: lo_engine TYPE REF TO zcl_zdbframework_engine_read.

    chk_is_dbname_permitted( EXPORTING id_dbname = id_dbname
                             EXCEPTIONS OTHERS = 999 ).
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID      sy-msgid
              TYPE    sy-msgty
              NUMBER  sy-msgno
              WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING dbname_not_permited .
    ENDIF.

    CREATE OBJECT lo_engine
      EXPORTING
        id_devclass  = id_devclass
        id_dbname    = id_dbname
        id_classname = id_classname
        id_ttyp      = id_ttyp.

    lo_engine->_get_fieldlist( ).

    lo_engine->_generate_class_header( ).
    lo_engine->_generate_types( ).
    lo_engine->_generate_method_add_range( ).
    lo_engine->_generate_method_select_all( ).
    lo_engine->_generate_method_get_list( ).
    lo_engine->_generate_method_get_detail( ).
    lo_engine->_generate_method_init_buffer( ).

    lo_engine->_generate_class( IMPORTING ed_classname = ed_classname
                                          ed_korrnum   = ed_korrnum
                                EXCEPTIONS OTHERS       = 999 ).
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID      sy-msgid
              TYPE    sy-msgty
              NUMBER  sy-msgno
              WITH    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING error.
    ENDIF.

  ENDMETHOD.

  METHOD chk_is_dbname_permitted.
*    IF id_dbname(1) <> 'Z' OR id_dbname(1) <> 'Y'.
*      MESSAGE 'Only tables starting by Z or Y are allowed.' TYPE 'E' RAISING dbname_not_permited.
*    ENDIF.
  ENDMETHOD.

  METHOD get_class_prefix.
    rd_prefix = 'ZCL_'.
  ENDMETHOD.
  METHOD get_class_suffix.
    rd_suffix = '_READ'.
  ENDMETHOD.

  METHOD constructor.
    me->md_dbname         = id_dbname.
    me->ms_class-clsname  = id_classname.
    me->md_devclass       = id_devclass.
    me->md_ttyp           = id_ttyp.
  ENDMETHOD.

  METHOD _get_fieldlist.
    DATA lo_structdescr TYPE REF TO  cl_abap_structdescr.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_name( md_dbname ).
    mt_fieldlist[] = lo_structdescr->get_ddic_field_list( ).
    DELETE mt_fieldlist WHERE keyflag = abap_false OR fieldname = 'MANDT' OR datatype = 'CLNT'.

  ENDMETHOD.

  METHOD _generate_class_header.

    CONCATENATE 'DB Framework for:' md_dbname INTO ms_class-descript SEPARATED BY space.
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
    <ls_attribute>-cmpname    = 'MS_RANGES'.
    <ls_attribute>-descript   = 'Selection Ranges'.
    <ls_attribute>-exposure   = seoc_exposure_private.
    <ls_attribute>-state      = seoc_state_implemented.
    <ls_attribute>-editorder  = 1.
    <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
    <ls_attribute>-type       = 'MTYP_RANGES'.
    <ls_attribute>-typtype    = 1.

    INSERT INITIAL LINE INTO TABLE mt_attributes ASSIGNING <ls_attribute>.
    <ls_attribute>-clsname    = ms_class-clsname.
    <ls_attribute>-cmpname    = 'MT_BUFFER'.
    <ls_attribute>-descript   = 'Data Buffer'.
    <ls_attribute>-exposure   = seoc_exposure_private.
    <ls_attribute>-state      = seoc_state_implemented.
    <ls_attribute>-editorder  = 2.
    <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
    <ls_attribute>-type       = md_ttyp.
    <ls_attribute>-typtype    = 1.

    INSERT INITIAL LINE INTO TABLE mt_attributes ASSIGNING <ls_attribute>.
    <ls_attribute>-clsname    = ms_class-clsname.
    <ls_attribute>-cmpname    = 'MF_ALL_DATA_RETRIEVED'.
    <ls_attribute>-descript   = 'All data has been retrieved'.
    <ls_attribute>-exposure   = seoc_exposure_private.
    <ls_attribute>-state      = seoc_state_implemented.
    <ls_attribute>-editorder  = 3.
    <ls_attribute>-attdecltyp = seoo_attdecltyp_statics.
    <ls_attribute>-type       = 'ABAP_BOOL'.
    <ls_attribute>-typtype    = 1.


    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist>.
      AT FIRST.
        INSERT INITIAL LINE INTO TABLE mt_types ASSIGNING <ls_type>.
        <ls_type>-clsname   = ms_class-clsname.
        <ls_type>-cmpname   = 'MTYP_RANGES'.
        <ls_type>-exposure  = seoc_exposure_private.
        <ls_type>-state     = seoc_state_implemented.
        <ls_type>-editorder = 1.
        <ls_type>-typtype = 4.

        mac_add_type  'BEGIN OF mtyp_ranges,'.
      ENDAT.

      CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO ld_string2 SEPARATED BY '-'.
      CONCATENATE <ls_fieldlist>-fieldname 'TYPE RANGE OF' ld_string2 ',' INTO ld_string SEPARATED BY space.
      mac_add_type ld_string.

      AT LAST.
        mac_add_type  '       END   OF mtyp_ranges'.
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
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = '_SELECT_ALL'.
    <ls_method>-descript   = 'Selects all data from DB and stores in BUFFER'.
    <ls_method>-exposure   = seoc_exposure_private.

    mac_add_source space.
    CONCATENATE '  SELECT * FROM' md_dbname 'INTO TABLE mt_buffer.' INTO ld_string SEPARATED BY space.
    mac_add_source ld_string.
    mac_add_source space.
    mac_add_source '  mf_all_data_retrieved = abap_true.'.
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
    <ls_method>-mtddecltyp = '1'.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = '_ADD_RANGE'.
    <ls_method>-descript   = 'Add to Range'.
    <ls_method>-exposure   = seoc_exposure_private.


    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname    = <ls_method>-clsname.
    <ls_parameter>-cmpname    = <ls_method>-cmpname.
    <ls_parameter>-sconame    = 'ID_LOW'.
    <ls_parameter>-cmptype    = seoo_cmptype_attribute.
    <ls_parameter>-mtdtype    = seoo_mtdtype_method.
    <ls_parameter>-editorder  = ld_order.
    <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
    <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
    <ls_parameter>-typtype    = seoo_typtype_type.
    <ls_parameter>-type       = 'ANY'.

    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname    = <ls_method>-clsname.
    <ls_parameter>-cmpname    = <ls_method>-cmpname.
    <ls_parameter>-sconame    = 'CT_RANGE'.
    <ls_parameter>-cmptype    = seoo_cmptype_attribute.
    <ls_parameter>-mtdtype    = seoo_mtdtype_method.
    <ls_parameter>-editorder  = ld_order.
    <ls_parameter>-pardecltyp = seos_pardecltyp_changing.
    <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
    <ls_parameter>-typtype    = seoo_typtype_type.
    <ls_parameter>-type       = 'ANY TABLE'.


    mac_add_source space.
    mac_add_source '  FIELD-SYMBOLS: <ls_row>    TYPE any,'.
    mac_add_source '                 <ld_sign>   TYPE any,'.
    mac_add_source '                 <ld_option> TYPE any,'.
    mac_add_source '                 <ld_low>    TYPE any.'.
    mac_add_source space.
    mac_add_source '  INSERT INITIAL LINE INTO TABLE ct_range ASSIGNING <ls_row>.'.
    mac_add_source space.
    mac_add_source TEXT-001.
    mac_add_source '    <ld_low> = id_low.'.
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
    <ls_method>-mtddecltyp = '1'.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'GET_LIST'.
    <ls_method>-descript   = 'Find Multiple details by keys'.
    <ls_method>-exposure   = seoc_exposure_public.


    mac_add_source space.
    mac_add_source '  DATA: ls_buffer LIKE LINE OF mt_buffer.'.
    mac_add_source space.
    mac_add_source '  FIELD-SYMBOLS: <ls_list> LIKE LINE OF et_list.'.
    mac_add_source space.
    mac_add_source space.
    mac_add_source '  IF mf_all_data_retrieved = abap_false.'.
    mac_add_source '    _SELECT_ALL( ).'.
    mac_add_source '  ENDIF.'.
    mac_add_source space.
    mac_add_source '  CLEAR ms_ranges.'.

    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist>.
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO ld_string.

      mac_add_source space.
      CONCATENATE '  IF' ld_string 'IS SUPPLIED.' INTO ld_string2 SEPARATED BY space.
      mac_add_source ld_string2.
      CONCATENATE '    _add_range( EXPORTING id_low   =' ld_string INTO ld_string2 SEPARATED BY space.
      mac_add_source ld_string2.
      CONCATENATE '                CHANGING  ct_range = ms_ranges-' <ls_fieldlist>-fieldname ' ).' INTO ld_string2.
      mac_add_source ld_string2.
      mac_add_source '  ENDIF.'.
    ENDLOOP.

    mac_add_source space.
    mac_add_source space.
    mac_add_source '  LOOP AT mt_buffer INTO ls_buffer'.

* Only key fields
    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist>.
      ADD 1 TO ld_order.

      INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.

      <ls_parameter>-clsname    =  <ls_method>-clsname.
      <ls_parameter>-cmpname    =  <ls_method>-cmpname.
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO <ls_parameter>-sconame.
      <ls_parameter>-descript   = <ls_fieldlist>-scrtext_m.
      <ls_parameter>-cmptype    = seoo_cmptype_attribute.
      <ls_parameter>-mtdtype    = seoo_mtdtype_method.
      <ls_parameter>-editorder  = ld_order.
      <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
      <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
      <ls_parameter>-typtype    = seoo_typtype_type.
      <ls_parameter>-paroptionl = abap_true.
      CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO <ls_parameter>-type SEPARATED BY '-'.

*   Build WHERE clause
      CONCATENATE  'ms_ranges-' <ls_fieldlist>-fieldname INTO ld_string.
      IF ld_order = 1.
        CONCATENATE '  WHERE' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space.
      ELSE.
        CONCATENATE '    AND' <ls_fieldlist>-fieldname 'IN' ld_string INTO ld_string SEPARATED BY space.
      ENDIF.
      mac_add_source ld_string.

    ENDLOOP.

    CONCATENATE <ld_source> '.' INTO <ld_source>.

    mac_add_source space.
    mac_add_source '  INSERT INITIAL LINE INTO TABLE et_list ASSIGNING <ls_list>.'.
    mac_add_source '  MOVE-CORRESPONDING ls_buffer to <ls_list>.'.
    mac_add_source space.
    mac_add_source '  ENDLOOP.'.


    mac_add_source space.
    mac_add_source '  IF et_list IS INITIAL.'.
    mac_add_source '    RAISE EXCEPTION TYPE cx_c2s_data_not_found.'.
    mac_add_source '  ENDIF.'.


* Return parameters
    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname    =  <ls_method>-clsname.
    <ls_parameter>-cmpname    =  <ls_method>-cmpname.
    <ls_parameter>-sconame    = 'ET_LIST'.
    <ls_parameter>-descript   = 'List Details'.
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
    <ls_exception>-sconame  = 'CX_C2S_DATA_NOT_FOUND'.
*  <ls_exception>-version  = seoc_version_active.
    <ls_exception>-descript = 'Not Found'.
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
    <ls_method>-mtddecltyp = '1'.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'GET_DETAILS'.
    <ls_method>-descript  = 'Find details by keys'.
    <ls_method>-exposure  = seoc_exposure_public.

    mac_add_source space.
    CONCATENATE '  DATA lt_list      TYPE' md_ttyp '.' INTO ld_string SEPARATED BY space.
    mac_add_source ld_string.
    mac_add_source '  DATA ls_list      LIKE LINE OF lt_list.'.
    mac_add_source '  DATA lo_exception TYPE REF TO cx_c2s_data_not_found.'.


    mac_add_source space.
    mac_add_source '  READ TABLE mt_buffer INTO rs_result'.
    mac_add_source '    WITH KEY'.
    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist> .
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO ld_string.
      CONCATENATE  <ls_fieldlist>-fieldname '=' ld_string INTO ld_string SEPARATED BY space.

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
    CONCATENATE <ld_source> '.' INTO <ld_source>.

    mac_add_source space.
    mac_add_source '  IF NOT sy-subrc IS INITIAL.'.

    mac_add_source space.
    mac_add_source '    TRY.'.
    mac_add_source '        get_list( EXPORTING'.

* Only key fields
    LOOP AT mt_fieldlist ASSIGNING <ls_fieldlist> .
      ADD 1 TO ld_order.

      INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.

      <ls_parameter>-clsname    =  <ls_method>-clsname.
      <ls_parameter>-cmpname    =  <ls_method>-cmpname.
      CONCATENATE 'ID_' <ls_fieldlist>-fieldname INTO <ls_parameter>-sconame.
      <ls_parameter>-descript   = <ls_fieldlist>-scrtext_m.
      <ls_parameter>-cmptype    = seoo_cmptype_attribute.
      <ls_parameter>-mtdtype    = seoo_mtdtype_method.
      <ls_parameter>-editorder  = ld_order.
      <ls_parameter>-pardecltyp = seos_pardecltyp_importing.
      <ls_parameter>-parpasstyp = seos_parpasstyp_byreference.
      <ls_parameter>-typtype    = seoo_typtype_type.
      CONCATENATE <ls_fieldlist>-tabname <ls_fieldlist>-fieldname INTO <ls_parameter>-type SEPARATED BY '-'.

      CONCATENATE <ls_parameter>-sconame '=' <ls_parameter>-sconame INTO ld_string SEPARATED BY space.

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
    mac_add_source '                  IMPORTING  et_list = lt_list ).'.

    mac_add_source space.
    mac_add_source '      READ TABLE lt_list INDEX 1 INTO ls_list.'.
    mac_add_source '      MOVE-CORRESPONDING ls_list TO rs_result.'.

    mac_add_source space.
    mac_add_source '      CATCH cx_c2s_data_not_found INTO lo_exception.'.
    mac_add_source '        RAISE EXCEPTION lo_exception.'.
    mac_add_source '    ENDTRY.'.

    mac_add_source space.
    mac_add_source '  ENDIF.'.


* Add return parameter
    ADD 1 TO ld_order.
    INSERT INITIAL LINE INTO TABLE mt_parameters ASSIGNING <ls_parameter>.
    <ls_parameter>-clsname   =  <ls_method>-clsname.
    <ls_parameter>-cmpname   =  <ls_method>-cmpname.
    <ls_parameter>-sconame   = 'RS_RESULT'.
    <ls_parameter>-descript  = 'Details'.
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
    <ls_exception>-sconame  = 'CX_C2S_DATA_NOT_FOUND'.
*  <ls_exception>-version  = seoc_version_active.
    <ls_exception>-descript = 'Not data found'.
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
    <ls_method>-mtddecltyp = '1'.
    <ls_method_sources>-cpdname = <ls_method>-cmpname   = 'INIT_BUFFER'.
    <ls_method>-descript  = 'Initializes Buffer Data'.
    <ls_method>-exposure  = seoc_exposure_public.

    mac_add_source '  CLEAR mt_buffer[].'.
    mac_add_source '  mf_all_data_retrieved = abap_false.'.
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
        object   = 'CPUB'
        obj_name = ld_obj_name
      EXCEPTIONS
        OTHERS   = 999.
    mac_error.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object   = 'CPRO'
        obj_name = ld_obj_name
      EXCEPTIONS
        OTHERS   = 999.
    mac_error.

    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object   = 'CPRI'
        obj_name = ld_obj_name
      EXCEPTIONS
        OTHERS   = 999.
    mac_error.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        author          = sy-uname
        global_lock     = 'X'
        object          = ld_obj_name
        object_class    = 'CLASS'
        devclass        = md_devclass
        korrnum         = md_korrnum
        master_language = sy-langu
*       PROGRAM         = PROGRAM_LOCAL
        mode            = 'INSERT'
      IMPORTING
*       AUTHOR          = UNAME
        korrnum         = ed_korrnum
*       DEVCLASS        = DEVCLASS_LOCAL
      EXCEPTIONS
        OTHERS          = 999.
    mac_error.

    ed_classname = ms_class-clsname.
    SET PARAMETER ID 'CLASS' FIELD ed_classname.


  ENDMETHOD.
ENDCLASS.
