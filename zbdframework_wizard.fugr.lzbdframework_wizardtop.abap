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
*|   We would like to thank SAPLink project because we learned how to  |
*|   create an ABAP Classe studying their code                         |
*\---------------------------------------------------------------------/
*/---------------------------------------------------------------------\
*| For a full list of contributors visit:                              |
*|                                                                     |
*| project homepage: https://github.com/rayatus/sapcodegenerator       |
*\---------------------------------------------------------------------/

FUNCTION-POOL zbdframework_wizard.                    "MESSAGE-ID ..

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
TYPES: BEGIN OF gtyp_codegen_data,
        prefix              TYPE string,
        suffix              TYPE string,
        tabname             TYPE tabname16,
        getlist_struct_name TYPE typename,
        full_clsname        TYPE vseoclass-clsname,
        devclass            TYPE devclass,
       END   OF gtyp_codegen_data.

DATA: gs_codegen_data TYPE gtyp_codegen_data.
