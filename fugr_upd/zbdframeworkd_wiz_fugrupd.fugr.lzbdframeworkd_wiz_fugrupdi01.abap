*----------------------------------------------------------------------*
***INCLUDE LZBDFRAMEWORKD_WIZ_FUGRUPDI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHK_0200_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE chk_0200_data INPUT.
* Check if TabName already exists in active version
  PERFORM chk_tabname USING gs_codegen_data-dbname.
* Check if Namespace is ok
  PERFORM chk_namespace USING gs_codegen_data-namespace.
* Propose Function group and function names
  PERFORM set_function_init_value.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHK_0300_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE chk_0300_data INPUT.
  "Check function group does not exist.
  PERFORM chk_function_group_not_exist USING gs_codegen_data-function_group.
  "Check function S does not exist.
  PERFORM chk_function_not_exist USING gs_codegen_data-function_s.
  "Check function T does not exist.
  PERFORM chk_function_not_exist USING gs_codegen_data-function_t.
  "Chech TableType does exist.
  PERFORM chk_ttyp USING gs_codegen_data-table_type.

ENDMODULE.
