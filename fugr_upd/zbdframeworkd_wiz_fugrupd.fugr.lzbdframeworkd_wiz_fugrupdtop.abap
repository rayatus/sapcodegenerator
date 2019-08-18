FUNCTION-POOL zbdframeworkd_wiz_fugrupd. "MESSAGE-ID ..

* wizard system include (do not remove)
INCLUDE <wizard>.

************************************************************************
* Wizard:
*   general data
************************************************************************
DATA:
  g_ok_code      TYPE sy-ucomm,
  g_save_ok_code TYPE sy-ucomm,
  g_error        TYPE sy-input.

************************************************************************
* Userdefined wizard data
************************************************************************
TYPES: BEGIN OF gtyp_s_codegen_data,
         devclass       TYPE devclass,
         namespace      TYPE namespace,
         dbname         TYPE tabname16,
         function_group TYPE rs38l_area,
         function_s     TYPE rs38l_fnam,
         function_t     TYPE rs38l_fnam,
         table_type     TYPE typename,
       END   OF gtyp_s_codegen_data.

DATA: gs_codegen_data TYPE gtyp_s_codegen_data.
