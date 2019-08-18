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
*/---------------------------------------------------------------------\
*| For a full list of contributors visit:                              |
*|                                                                     |
*| project homepage: https://github.com/rayatus/sapcodegenerator       |
*\---------------------------------------------------------------------/


"! <p class="shorttext synchronized" lang="en">Generates a function group for updating DB
"! table content</p>
CLASS zcl_zdbframework_engine_updfg DEFINITION
  PUBLIC
  CREATE PROTECTED
  INHERITING FROM zcl_zdbframework_engine_base.

  PUBLIC SECTION.

    CLASS-METHODS generate_upd_function_group
      IMPORTING id_devclass              TYPE devclass
                id_dbname                TYPE tabname16
                id_function_group        TYPE rs38l_area
                id_function_s            TYPE rs38l_fnam
                id_function_t            TYPE rs38l_fnam
                id_ttyp                  TYPE typename
                id_trkorr                TYPE trkorr OPTIONAL
      EXPORTING VALUE(ed_function_group) TYPE rs38l_area
                VALUE(ed_korrnum)        TYPE trkorr
      RAISING   zcx_zdbframework_engine.


    CLASS-METHODS propose_names
      IMPORTING VALUE(id_dbname)    TYPE tabname16
                VALUE(id_namespace) TYPE namespace DEFAULT 'Z'
      EXPORTING ed_function_group   TYPE rs38l_area
                ed_function_s       TYPE rs38l_fnam
                ed_function_t       TYPE rs38l_fnam
                ed_ttyp             TYPE typename.


  PROTECTED SECTION.

    METHODS constructor
      IMPORTING id_devclass       TYPE devclass
                id_dbname         TYPE tabname16
                id_function_group TYPE rs38l_area
                id_function_s     TYPE rs38l_fnam
                id_function_t     TYPE rs38l_fnam
                id_ttyp           TYPE typename
                id_trkorr         TYPE trkorr OPTIONAL
      RAISING   zcx_zdbframework_engine.

    METHODS generate
      RAISING zcx_zdbframework_engine.


  PRIVATE SECTION.

    TYPES mtyp_t_tfdir      TYPE STANDARD TABLE OF tfdir WITH DEFAULT KEY.
    TYPES mtyp_t_tftit      TYPE STANDARD TABLE OF tftit WITH DEFAULT KEY.
    TYPES mtyp_t_funct      TYPE STANDARD TABLE OF funct WITH DEFAULT KEY.
    TYPES mtyp_t_enlfdir    TYPE STANDARD TABLE OF enlfdir WITH DEFAULT KEY.
    TYPES mtyp_t_trdir      TYPE STANDARD TABLE OF trdir WITH DEFAULT KEY.
    TYPES mtyp_t_sfupararef TYPE STANDARD TABLE OF sfupararef WITH DEFAULT KEY.
    TYPES mtyp_t_source255  TYPE STANDARD TABLE OF abaptxt255  WITH DEFAULT KEY.
    TYPES: BEGIN OF mtyp_s_funcname_info,
             funcname   TYPE rs38l_fnam,
             tfdir      TYPE mtyp_t_tfdir,
             tftit      TYPE mtyp_t_tftit,
             funct      TYPE mtyp_t_funct,
             enlfdir    TYPE mtyp_t_enlfdir,
             trdir      TYPE mtyp_t_trdir,
             sfupararef TYPE mtyp_t_sfupararef,
             source255  TYPE mtyp_t_source255,
           END   OF mtyp_s_funcname_info.

    TYPES: BEGIN OF mtyp_s_header,
             funcname TYPE rs38l_area,
             uname    TYPE uname,
             areat    TYPE areat,
           END   OF mtyp_s_header.

    TYPES: BEGIN OF mtyp_s_source,
             progname TYPE programm,
             source   TYPE rsfb_source,
           END   OF mtyp_s_source.
    TYPES mtyp_t_source TYPE STANDARD TABLE OF mtyp_s_source WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF mtyp_s_method.
        INCLUDE TYPE tfdir.
    TYPES: stext     TYPE rs38l_ftxt,
           active    TYPE activef,
           generated TYPE generated,
           global    TYPE rs38l_glob,
           loc_priv  TYPE rs38l_temp,
           exten1    TYPE spras,
           exten2    TYPE rs38l_korr,
           exten3    TYPE fnkey,
           exten4    TYPE fnkey,
           exten5    TYPE fnkey,
           END   OF mtyp_s_method.
    TYPES mtyp_t_method TYPE STANDARD TABLE OF mtyp_s_method WITH DEFAULT KEY.

    TYPES: BEGIN OF mtyp_s_parameter.
        INCLUDE TYPE sfupararef.
    TYPES: stext TYPE paramtext,
           END   OF mtyp_s_parameter.
    TYPES mtyp_t_parameters TYPE STANDARD TABLE OF mtyp_s_parameter WITH DEFAULT KEY.

    TYPES mtyp_t_progdir      TYPE STANDARD TABLE OF progdir WITH DEFAULT KEY.
    TYPES mtyp_t_poolcontents TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY.

    TYPES: BEGIN OF mtyp_s_function_group_data,
             header         TYPE mtyp_s_header,
             source         TYPE mtyp_t_source,
             includes       TYPE mtyp_t_progdir,
             poolcontents   TYPE mtyp_t_poolcontents,
             methods        TYPE mtyp_t_method,
             methods_source TYPE mtyp_t_source,
             parameters     TYPE mtyp_t_parameters,
           END   OF mtyp_s_function_group_data.

    CONSTANTS: BEGIN OF mc_parameter_type,
                 importing TYPE c VALUE 'I' ##NO_TEXT,
                 exporting TYPE c VALUE 'E' ##NO_TEXT,
                 changing  TYPE c VALUE 'C' ##NO_TEXT,
                 table     TYPE c VALUE 'T' ##NO_TEXT,
                 exception TYPE c VALUE 'X' ##NO_TEXT,
               END   OF mc_parameter_type.

    DATA ms_templ_function_group TYPE   mtyp_s_function_group_data.
    DATA ms_new_function_group   TYPE   mtyp_s_function_group_data.

    DATA md_dbname         TYPE tabname16.
    DATA md_devclass       TYPE devclass.
    DATA md_tabletype_name TYPE typename.
    DATA md_trkorr         TYPE trkorr.
    DATA md_function_group TYPE rs38l_area.
    DATA md_function_s     TYPE rs38l_fnam.
    DATA md_function_t     TYPE rs38l_fnam.

    CONSTANTS mc_templ_fugr   TYPE rs38l_area  VALUE 'ZFG_TEMPLATE_UPDATE' ##NO_TEXT.
    CONSTANTS mc_templ_function_s TYPE rs38l_fnam VALUE 'Z_FM_SFLIGHT_UPD_S' ##NO_TEXT.
    CONSTANTS mc_templ_function_t TYPE rs38l_fnam VALUE 'Z_FM_SFLIGHT_UPD_T' ##NO_TEXT.
    CONSTANTS mc_templ_name   TYPE tabname16   VALUE 'TEMPLATE' ##NO_TEXT.
    CONSTANTS mc_templ_dbname TYPE tabname16   VALUE 'SFLIGHT' ##NO_TEXT.
    CONSTANTS mc_templ_ttype  TYPE typename    VALUE 'ZTT_SFLIGHT_KZ' ##NO_TEXT.

    METHODS _read_template
      IMPORTING id_funcname TYPE rs38l_area
      RAISING   zcx_zdbframework_engine.

    METHODS _do_generate
      RAISING zcx_zdbframework_engine.

    METHODS _replace_template
      RAISING zcx_zdbframework_engine.
    METHODS _replace_function
      IMPORTING
        id_templ_funcname      TYPE rs38l_fnam
        id_new_funcname        TYPE rs38l_fnam
      CHANGING
        cs_function_group_data TYPE zcl_zdbframework_engine_updfg=>mtyp_s_function_group_data.


ENDCLASS.



CLASS zcl_zdbframework_engine_updfg IMPLEMENTATION.

  METHOD generate_upd_function_group.

    DATA lo_engine TYPE REF TO zcl_zdbframework_engine_updfg.


    CREATE OBJECT lo_engine
      EXPORTING
        id_devclass       = id_devclass
        id_dbname         = id_dbname
        id_function_group = id_function_group
        id_function_s     = id_function_s
        id_function_t     = id_function_t
        id_ttyp           = id_ttyp
        id_trkorr         = id_trkorr.

    lo_engine->generate( ).

  ENDMETHOD.

  METHOD _do_generate.

    DATA:
      lt_parimp    TYPE TABLE OF rsimp,
      lt_parexp    TYPE TABLE OF rsexp,
      lt_partbl    TYPE TABLE OF rstbl,
      lt_parcha    TYPE TABLE OF rscha,
      lt_except    TYPE TABLE OF rsexc,
      lt_source    TYPE TABLE OF rssource,

      ls_parimp    LIKE LINE OF lt_parimp,
      ls_parexp    LIKE LINE OF lt_parexp,
      ls_partbl    LIKE LINE OF lt_partbl,
      ls_parcha    LIKE LINE OF lt_parcha,
      ls_except    LIKE LINE OF lt_except,

      lt_dwinactiv TYPE TABLE OF dwinactiv,
      ls_dwinactiv LIKE LINE OF lt_dwinactiv.

    FIELD-SYMBOLS:
      <ls_local_source>  LIKE LINE OF ms_new_function_group-source,
      <ls_method>        LIKE LINE OF ms_new_function_group-methods,
      <ls_parameter>     LIKE LINE OF ms_new_function_group-parameters,
      <ls_method_source> LIKE LINE OF ms_new_function_group-methods_source.


    CALL FUNCTION 'RS_FUNCTION_POOL_COPY'
      EXPORTING
        function_pool      = ms_templ_function_group-header-funcname
        function_pool_copy = ms_new_function_group-header-funcname
        korrnum            = md_trkorr
        without_functions  = abap_true
        short_text         = ms_new_function_group-header-areat
        responsible        = sy-uname
        devclass           = md_devclass
      EXCEPTIONS
        OTHERS             = 999.
    IF sy-subrc <> 0.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

    ls_dwinactiv-object   = 'FUGR' ##NO_TEXT.
    ls_dwinactiv-obj_name = ms_new_function_group-header-funcname.
    APPEND ls_dwinactiv TO lt_dwinactiv.

    CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
      TABLES
        objects = lt_dwinactiv
      EXCEPTIONS
        OTHERS  = 999.
    IF sy-subrc <> 0.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

    LOOP AT ms_new_function_group-source ASSIGNING <ls_local_source>.
      INSERT REPORT <ls_local_source>-progname FROM <ls_local_source>-source.
    ENDLOOP.

*-----------------------------------------------------------------------
* generate function modules
    LOOP AT ms_new_function_group-methods ASSIGNING <ls_method>.

*   get function parameters
      CLEAR lt_parimp.
      CLEAR lt_parexp.
      CLEAR lt_partbl.
      CLEAR lt_parcha.
      CLEAR lt_except.

      LOOP AT ms_new_function_group-parameters
        ASSIGNING <ls_parameter>
        WHERE funcname = <ls_method>-funcname.

        CASE <ls_parameter>-paramtype.

          WHEN mc_parameter_type-importing.
*         importing parameter
            CLEAR ls_parimp.
            ls_parimp-parameter = <ls_parameter>-parameter.
*         ls_parimp-dbfield   =
            ls_parimp-default   = <ls_parameter>-defaultval.
*         ls_parimp-types     = ...-type.     "BUG: genererates ref to
            ls_parimp-optional  = <ls_parameter>-optional.
            ls_parimp-reference = <ls_parameter>-reference.
*         ls_parimp-changing  =
            ls_parimp-typ       = <ls_parameter>-structure.
            ls_parimp-class     = <ls_parameter>-class.
            ls_parimp-ref_class = <ls_parameter>-ref_class.
            ls_parimp-line_of   = <ls_parameter>-line_of.
            ls_parimp-table_of  = <ls_parameter>-table_of.
            APPEND ls_parimp TO lt_parimp.

          WHEN mc_parameter_type-exporting.
*         exporting parameter
            CLEAR ls_parexp.
            ls_parexp-parameter = <ls_parameter>-parameter.
*         ls_parexp-dbfield   =
*         ls_parexp-types     = ...-type.     "BUG: genererates ref to
            ls_parexp-reference = <ls_parameter>-reference.
            ls_parexp-typ       = <ls_parameter>-structure.
            ls_parexp-class     = <ls_parameter>-class.
            ls_parexp-ref_class = <ls_parameter>-ref_class.
            ls_parexp-line_of   = <ls_parameter>-line_of.
            ls_parexp-table_of  = <ls_parameter>-table_of.
            APPEND ls_parexp TO lt_parexp.

          WHEN mc_parameter_type-changing.
*         changing parameter
            CLEAR ls_parcha.
            ls_parcha-parameter = <ls_parameter>-parameter.
*         ls_parcha-dbfield   =
            ls_parcha-default   = <ls_parameter>-defaultval.
*         ls_parcha-types     = ...-type.     "BUG: genererates ref to
            ls_parcha-optional  = <ls_parameter>-optional.
            ls_parcha-reference = <ls_parameter>-reference.
*         ls_parcha-changing  =
            ls_parcha-typ       = <ls_parameter>-structure.
            ls_parcha-class     = <ls_parameter>-class.
            ls_parcha-ref_class = <ls_parameter>-ref_class.
            ls_parcha-line_of   = <ls_parameter>-line_of.
            ls_parcha-table_of  = <ls_parameter>-table_of.
            APPEND ls_parcha TO lt_parcha.

          WHEN mc_parameter_type-table.
*         tables parameter
            CLEAR ls_partbl.
            ls_partbl-parameter = <ls_parameter>-parameter.
            ls_partbl-dbstruct  = <ls_parameter>-structure.
*         ls_partbl-types     = ...-type.     "BUG: genererates ref to
            ls_partbl-optional  = <ls_parameter>-optional.
            ls_partbl-typ       = <ls_parameter>-structure.
            ls_partbl-class     = <ls_parameter>-class.
            ls_partbl-ref_class = <ls_parameter>-ref_class.
            ls_partbl-line_of   = <ls_parameter>-line_of.
            ls_partbl-table_of  = <ls_parameter>-table_of.
            APPEND ls_partbl TO lt_partbl.

          WHEN mc_parameter_type-exception.
*         exception parameter
            ls_except-exception = <ls_parameter>-parameter.
            APPEND ls_except TO lt_except.

        ENDCASE.

      ENDLOOP.

*   get function source
      CLEAR lt_source.
      READ TABLE ms_new_function_group-methods_source
        WITH KEY progname = <ls_method>-funcname
        ASSIGNING  <ls_method_source>.
      lt_source = <ls_method_source>-source.


*   generate new function module
      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname            = <ls_method>-funcname
          function_pool       = ms_new_function_group-header-funcname
*         INTERFACE_GLOBAL    = ' '
*         REMOTE_CALL         = ' '
          short_text          = <ls_method>-stext
          suppress_corr_check = ' '
          update_task         = <ls_method>-utask
          corrnum             = md_trkorr
*         NAMESPACE           = ' '
*         SUPPRESS_LANGUAGE_CHECK = 'X'
*         AUTHORITY_CHECK     = 'X'
          save_active         = abap_true
        TABLES
          import_parameter    = lt_parimp
          export_parameter    = lt_parexp
          tables_parameter    = lt_partbl
          changing_parameter  = lt_parcha
          exception_list      = lt_except
          source              = lt_source
        EXCEPTIONS
          OTHERS              = 999.
      IF sy-subrc <> 0.
        zcx_zdbframework_engine=>raise_from_symsg( ).
      ENDIF.

    ENDLOOP.

*-----------------------------------------------------------------------
* activate complete function pool
    ls_dwinactiv-object   = 'FUGR' ##NO_TEXT.
    ls_dwinactiv-obj_name = ms_new_function_group-header-funcname.
    APPEND ls_dwinactiv TO lt_dwinactiv.

    CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
      TABLES
        objects = lt_dwinactiv
      EXCEPTIONS
        OTHERS  = 999.
    IF sy-subrc <> 0.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

  ENDMETHOD.

  METHOD _read_template.

    DATA ls_funcname_info TYPE mtyp_s_funcname_info.
    DATA ls_tfdir         LIKE LINE OF ls_funcname_info-tfdir.
    DATA ls_tftit         LIKE LINE OF ls_funcname_info-tftit.
    DATA ls_enlfdir       LIKE LINE OF ls_funcname_info-enlfdir.
    DATA ls_sfupararef    LIKE LINE OF ls_funcname_info-sfupararef.
    DATA ls_funct         LIKE LINE OF ls_funcname_info-funct.

    DATA ls_source        LIKE LINE OF ms_templ_function_group-source.
    DATA ls_include       LIKE LINE OF ms_templ_function_group-includes.
    DATA ls_poolcontent   LIKE LINE OF ms_templ_function_group-poolcontents.
    DATA ls_method        LIKE LINE OF ms_templ_function_group-methods.
    DATA ls_parameter     LIKE LINE OF ms_templ_function_group-parameters.

    ms_templ_function_group-header-funcname = id_funcname.

    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool = ms_templ_function_group-header-funcname
      EXCEPTIONS
        OTHERS        = 999.
    IF sy-subrc IS NOT INITIAL.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

* get owner
    SELECT  SINGLE uname
      INTO  ms_templ_function_group-header-uname
      FROM  tlibg
      WHERE area = ms_templ_function_group-header-funcname.

* get text
    SELECT  SINGLE areat
      INTO  ms_templ_function_group-header-areat
      FROM  tlibt
      WHERE ( spras = sy-langu )
        AND ( area  = ms_templ_function_group-header-funcname  ).

*-----------------------------------------------------------------------
* GET FUNCTION POOL SOURCE

* get TOP-include
    CONCATENATE 'L' ms_templ_function_group-header-funcname 'TOP'
      INTO ls_source-progname ##NO_TEXT.

    READ REPORT ls_source-progname INTO ls_source-source.
    IF sy-subrc IS INITIAL.
      INSERT ls_source INTO TABLE ms_templ_function_group-source.
    ENDIF.

* get Fxx-incudes
    CONCATENATE 'L' ms_templ_function_group-header-funcname 'F%'
      INTO ls_source-progname ##NO_TEXT.

    SELECT     DISTINCT name
      INTO     CORRESPONDING FIELDS OF TABLE ms_templ_function_group-includes
      FROM     progdir
      WHERE    name LIKE ls_source-progname
      ORDER BY name.

    LOOP AT ms_templ_function_group-includes INTO ls_include.
      ls_source-progname = ls_include-name.
      READ REPORT ls_source-progname INTO ls_source-source.
      IF sy-subrc IS INITIAL.
        INSERT ls_source INTO TABLE ms_templ_function_group-source.
      ENDIF.
    ENDLOOP.

*-----------------------------------------------------------------------
* GET FUNCTION MODULE DATA AND SOURCE

* get function names
    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = ms_templ_function_group-header-funcname
      TABLES
        functab                 = ms_templ_function_group-poolcontents
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc IS NOT INITIAL.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

* get function details
    LOOP AT ms_templ_function_group-poolcontents INTO ls_poolcontent.

      ls_funcname_info-funcname = ls_poolcontent-funcname.
      CALL FUNCTION 'FUNC_GET_OBJECT'
        EXPORTING
          funcname   = ls_funcname_info-funcname
        TABLES
          ptfdir     = ls_funcname_info-tfdir
          ptftit     = ls_funcname_info-tftit
          pfunct     = ls_funcname_info-funct
          penlfdir   = ls_funcname_info-enlfdir
          ptrdir     = ls_funcname_info-trdir
          pfupararef = ls_funcname_info-sfupararef
          uincl      = ls_funcname_info-source255
        EXCEPTIONS
          OTHERS     = 999.
      IF sy-subrc IS NOT INITIAL.
        zcx_zdbframework_engine=>raise_from_symsg( ).
      ENDIF.

*      lt_rssource[] = lt_rssource255[].

*   get function header
      READ TABLE ls_funcname_info-tfdir INDEX 1 INTO ls_tfdir.
      MOVE-CORRESPONDING ls_tfdir TO ls_method.             "#EC ENHOK
      CONCATENATE 'SAPL' ms_templ_function_group-header-funcname
                  INTO ls_method-pname ##NO_TEXT.

      READ TABLE ls_funcname_info-tftit INTO ls_tftit WITH KEY spras = sy-langu.
      ls_method-stext = ls_tftit-stext.

      READ TABLE ls_funcname_info-enlfdir INDEX 1 INTO ls_enlfdir.
      MOVE-CORRESPONDING ls_enlfdir TO ls_method.           "#EC ENHOK

      INSERT ls_method INTO TABLE ms_templ_function_group-methods.

*   get function parameter
      SORT ls_funcname_info-sfupararef BY pposition.
      SORT ls_funcname_info-funct      BY spras funcname parameter kind.
      LOOP AT ls_funcname_info-sfupararef INTO ls_sfupararef.

        CLEAR ls_parameter.
        MOVE-CORRESPONDING ls_sfupararef
                        TO ls_parameter.                    "#EC ENHOK

        READ TABLE ls_funcname_info-funct INTO ls_funct
                            WITH KEY spras     = sy-langu
                                     funcname  = ls_sfupararef-funcname
                                     parameter = ls_sfupararef-parameter
                            BINARY SEARCH.
        IF sy-subrc = 0.
          ls_parameter-stext = ls_funct-stext.
        ENDIF.

        INSERT ls_parameter  INTO TABLE ms_templ_function_group-parameters.

      ENDLOOP.

*   get function source
      ls_source-progname = ls_funcname_info-funcname.

*   delete FUNCTION ... ENDFUNCTION statement and signature comments
      DELETE ls_funcname_info-source255 WHERE line CP 'FUNCTION *'  ##NO_TEXT.
      DELETE ls_funcname_info-source255 WHERE line CP 'ENDFUNCTION.'  ##NO_TEXT.
      DELETE ls_funcname_info-source255  WHERE line(2) = '*"'  ##NO_TEXT.

      ls_source-source   = ls_funcname_info-source255.
      INSERT ls_source INTO TABLE ms_templ_function_group-methods_source.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

    chk_is_dbname_permitted( EXPORTING  id_dbname           = id_dbname
                             EXCEPTIONS dbname_not_permited = 999 ).
    IF sy-subrc IS NOT INITIAL.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

    chk_exists_typedata( EXPORTING  id_typedata = id_ttyp
                         EXCEPTIONS OTHERS      = 999 ).
    IF sy-subrc IS NOT INITIAL.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

    chk_exist_dbname( EXPORTING  id_dbname = id_dbname
                      EXCEPTIONS OTHERS    = 999 ).
    IF sy-subrc IS NOT INITIAL.
      zcx_zdbframework_engine=>raise_from_symsg( ).
    ENDIF.

    md_dbname           = id_dbname.
    md_devclass         = id_devclass.
    md_function_group   = id_function_group.
    md_function_s       = id_function_s.
    md_function_t       = id_function_t.
    md_tabletype_name   = id_ttyp.


  ENDMETHOD.

  METHOD generate.
    _read_template( id_funcname = mc_templ_fugr ).
    _replace_template( ).
    _do_generate( ).

  ENDMETHOD.


  METHOD _replace_template.

    DATA: ld_search       TYPE string,
          ld_replace      TYPE string,
          ld_old_progname TYPE string.

    FIELD-SYMBOLS: <ls_source>        LIKE LINE OF ms_new_function_group-source,
                   <ls_parameter>     LIKE LINE OF ms_new_function_group-parameters,
                   <ls_method_source> LIKE LINE OF ms_new_function_group-methods_source,
                   <ld_source_code>   LIKE LINE OF <ls_source>-source,
                   <ls_include>       LIKE LINE OF ms_new_function_group-includes.

    ms_new_function_group                 = ms_templ_function_group.
    ms_new_function_group-header-funcname = md_function_group.

    "-------------------------------------------------------------------------------------
    LOOP AT ms_new_function_group-source ASSIGNING <ls_source>.

      "Replace function group name
      ld_search             = ms_templ_function_group-header-funcname.
      ld_replace            = ms_new_function_group-header-funcname.
      ld_old_progname       = <ls_source>-progname.


      REPLACE ld_search
        WITH ld_replace
        INTO <ls_source>-progname.


      CONDENSE <ls_source>-progname NO-GAPS.

      "Replace include source code
      LOOP AT <ls_source>-source ASSIGNING <ld_source_code>.
        TRANSLATE <ld_source_code> TO UPPER CASE.

        ld_search  = ms_templ_function_group-header-funcname.
        ld_replace = ms_new_function_group-header-funcname.

        REPLACE ld_search
          WITH  ld_replace
          INTO <ld_source_code>.

        "Replace Buffer internal table definition for linking with the new db
        ld_search  = mc_templ_dbname.
        ld_replace = md_dbname.

        REPLACE ld_search
          WITH  ld_replace
          INTO <ld_source_code>.

      ENDLOOP.

      "Replace include names
      LOOP AT ms_new_function_group-includes ASSIGNING <ls_include> WHERE name = ld_old_progname.

        <ls_include>-name = <ls_source>-progname.

      ENDLOOP.

    ENDLOOP.

    _replace_function( EXPORTING id_templ_funcname = mc_templ_function_s
                                 id_new_funcname   = md_function_s
                       CHANGING  cs_function_group_data = ms_new_function_group ).

    _replace_function( EXPORTING id_templ_funcname = mc_templ_function_t
                                 id_new_funcname   = md_function_t
                       CHANGING  cs_function_group_data = ms_new_function_group ).


  ENDMETHOD.

  METHOD propose_names.

    DATA ld_replace      TYPE string.
    DATA ld_search       TYPE string.
    DATA lt_dbname_parts TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA ld_lines        TYPE i.
    ed_function_group = mc_templ_fugr.
    ed_function_s     = mc_templ_function_s.
    ed_function_t     = mc_templ_function_t.
    ed_ttyp           = mc_templ_ttype.

    "Replace namespace
    ld_search  = 'Z' ##NO_TEXT.
    ld_replace = id_namespace.
    REPLACE ld_search WITH ld_replace INTO ed_function_group.
    CONDENSE ed_function_group NO-GAPS.

    REPLACE ld_search WITH ld_replace INTO ed_ttyp.
    CONDENSE ed_ttyp NO-GAPS.

    "If functions start with Z or Y after namespace it is required to have an underscore
    IF id_namespace = 'Z' OR id_namespace = 'Y' ##NO_TEXT.
      CONCATENATE id_namespace '_' INTO id_namespace ##NO_TEXT.
      ld_replace = id_namespace.
    ENDIF.

    REPLACE ld_search WITH ld_replace INTO ed_function_s.
    CONDENSE ed_function_s NO-GAPS.

    REPLACE ld_search WITH ld_replace INTO ed_function_t.
    CONDENSE ed_function_t NO-GAPS.


    "If dbname starts with a customer namespace get rid of it
    IF id_dbname+0(1) = '/' ##NO_TEXT.
      SPLIT id_dbname AT '/' INTO TABLE lt_dbname_parts ##NO_TEXT.
      DESCRIBE TABLE lt_dbname_parts LINES ld_lines.
      READ TABLE lt_dbname_parts INDEX ld_lines INTO id_dbname.
    ENDIF.

    ld_search  = mc_templ_name.
    ld_replace = id_dbname.
    REPLACE ld_search WITH ld_replace INTO ed_function_group.
    CONDENSE ed_function_group NO-GAPS.

    ld_search  = mc_templ_dbname.
    ld_replace = id_dbname.
    REPLACE ld_search WITH ld_replace INTO ed_function_s.
    CONDENSE ed_function_s NO-GAPS.

    REPLACE ld_search WITH ld_replace INTO ed_function_t.
    CONDENSE ed_function_t NO-GAPS.

    REPLACE ld_search WITH ld_replace INTO ed_ttyp.
    CONDENSE ed_ttyp NO-GAPS.


  ENDMETHOD.



  METHOD _replace_function.

    DATA: ld_search  TYPE string,
          ld_replace TYPE string,
          ld_length  TYPE i,
          ld_suffix  TYPE string.

    FIELD-SYMBOLS: <ls_poolcontent>   LIKE LINE OF ms_new_function_group-poolcontents,
                   <ls_method>        LIKE LINE OF ms_new_function_group-methods,
                   <ls_parameter>     LIKE LINE OF ms_new_function_group-parameters,
                   <ls_method_source> LIKE LINE OF ms_new_function_group-methods_source,
                   <ld_source_code>   type line of rsfb_source.

    "Replace function name
    READ TABLE ms_new_function_group-poolcontents
         WITH KEY funcname = id_templ_funcname
         ASSIGNING <ls_poolcontent>.
    <ls_poolcontent>-funcname = id_new_funcname.

    "Replace include name
    ld_length = strlen( <ls_poolcontent>-include ).
    ld_length = ld_length - 3.
    ld_suffix = <ls_poolcontent>-include+ld_length(3).
    CONCATENATE 'L' md_function_group ld_suffix INTO <ls_poolcontent>-include.
    CONDENSE <ls_poolcontent>-include NO-GAPS.



    LOOP AT ms_new_function_group-methods ASSIGNING <ls_method>
                                          WHERE funcname = id_templ_funcname.
      "Replace function names
      <ls_method>-funcname = id_new_funcname.
      "Replace main program name
      CONCATENATE 'SAPL' md_function_group INTO <ls_method>-pname.
      "Replace function description
      ld_search  = mc_templ_dbname.
      ld_replace = md_dbname.
      REPLACE ld_search WITH ld_replace INTO <ls_method>-stext.
      CONDENSE <ls_method>-stext.

    ENDLOOP.

    "Function parameter
    LOOP AT ms_new_function_group-parameters ASSIGNING <ls_parameter>
                                             WHERE funcname = id_templ_funcname.
      "Replace function name
      <ls_parameter>-funcname = id_new_funcname.

      "Replace parameter name
      ld_search  = mc_templ_dbname.
      ld_replace = md_dbname.
      REPLACE ld_search WITH ld_replace INTO <ls_parameter>-parameter.
      CONDENSE <ls_parameter>-parameter NO-GAPS.

      "Replace parameter structure table type
      ld_search  = mc_templ_ttype.
      ld_replace = md_tabletype_name.
      REPLACE ld_search WITH ld_replace INTO <ls_parameter>-structure.
      CONDENSE <ls_parameter>-structure NO-GAPS.

      "Replace parameter structure
      ld_search  = mc_templ_dbname.
      ld_replace = md_dbname.
      REPLACE ld_search WITH ld_replace INTO <ls_parameter>-structure.
      CONDENSE <ls_parameter>-structure NO-GAPS.
    ENDLOOP.

    "Function source
    LOOP AT ms_new_function_group-methods_source ASSIGNING <ls_method_source>
                                                 where progname = id_templ_funcname.

*     "Replace function name
      <ls_method_source>-progname = id_new_funcname.

      LOOP AT <ls_method_source>-source ASSIGNING <ld_source_code>.
        TRANSLATE <ld_source_code> TO UPPER CASE.
        "Replace parameter name
        ld_search  = mc_templ_dbname.
        ld_replace = md_dbname.
        REPLACE ld_search WITH ld_replace INTO <ld_source_code>.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
