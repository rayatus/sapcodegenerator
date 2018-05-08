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

*----------------------------------------------------------------------*
***INCLUDE LWZ_WIZARDFCB .
*----------------------------------------------------------------------*
*&WZ_PATTERN_WIZARD_CALLBACK&
*&---------------------------------------------------------------
*&      FORM  CB_PROCESS_STEP_START
*&---------------------------------------------------------------
FORM cb_process_step_start                                  "#EC CALLED
        TABLES
           container
        USING
           command.
* begin of local data definition
  DATA:
    l_wizard    TYPE swf_wizard.
* end of local data definition

* first screen (button 'back' will be automatically switched off
* and there will be NO popup 'you will lose data'...)
  l_wizard-screen_typ = wizard_screen_start.

* initialize wizard step
* the wizard has one unique title while it is running
  l_wizard-title      = text-ttl.
  l_wizard-descobject = 'ZBDFRAMEWORK_WIZARD_001'
  . "this is a text in dialog (SE61)
* call wizard screen
  CALL FUNCTION 'SWF_WIZARD_CALL'
    EXPORTING
      wizard_data                 = l_wizard
    EXCEPTIONS
      operation_cancelled_by_user = 01
      back                        = 02.
  swf_evaluate command.


ENDFORM.                    " CB_PROCESS_STEP_START
*&---------------------------------------------------------------
*&      FORM  CB_PROCESS_STEP_0200
*&---------------------------------------------------------------
FORM cb_process_step_0200                                   "#EC CALLED
        TABLES
           container
        USING
           command.
* begin of local data definition
  DATA:
    l_wizard    TYPE swf_wizard.
* end of local data definition

* initialize wizard step
* the wizard has one unique title while it is running
  l_wizard-title      = text-ttl.
  l_wizard-descobject = 'ZBDFRAMEWORK_WIZARD_002'
  . "this is a text in dialog (SE61)
  l_wizard-subscreen1 = '0200'.
  l_wizard-subscpool1 = 'SAPLZBDFRAMEWORK_WIZARD'.

* set subscreen data (transport data out of global wizard data
* to global screen data )
  PERFORM usr_set_subscreen_data_0200.

* call wizard screen
  CALL FUNCTION 'SWF_WIZARD_CALL'
    EXPORTING
      wizard_data                 = l_wizard
    EXCEPTIONS
      operation_cancelled_by_user = 01
      back                        = 02.
  swf_evaluate command.

* set subscreen data from global screen data back to the global
* wizard area
  PERFORM usr_get_subscreen_data_0200.


ENDFORM.                    " CB_PROCESS_STEP_0200
*&---------------------------------------------------------------
*&      FORM  CB_PROCESS_STEP_0300
*&---------------------------------------------------------------
FORM cb_process_step_0300                                   "#EC CALLED
        TABLES
           container
        USING
           command.
* begin of local data definition
  DATA:
    l_wizard    TYPE swf_wizard.
* end of local data definition

* initialize wizard step
* the wizard has one unique title while it is running
  l_wizard-title      = text-ttl.
  l_wizard-descobject = 'ZBDFRAMEWORK_WIZARD_003'
  . "this is a text in dialog (SE61)
  l_wizard-subscreen1 = '0300'.
  l_wizard-subscpool1 = 'SAPLZBDFRAMEWORK_WIZARD'.

* set subscreen data (transport data out of global wizard data
* to global screen data )
  PERFORM usr_set_subscreen_data_0300.

* call wizard screen
  CALL FUNCTION 'SWF_WIZARD_CALL'
    EXPORTING
      wizard_data                 = l_wizard
    EXCEPTIONS
      operation_cancelled_by_user = 01
      back                        = 02.
  swf_evaluate command.

* set subscreen data from global screen data back to the global
* wizard area
  PERFORM usr_get_subscreen_data_0300.


ENDFORM.                    " CB_PROCESS_STEP_0300
*&---------------------------------------------------------------
*&      FORM  CB_PROCESS_STEP_FINISH
*&---------------------------------------------------------------
FORM cb_process_step_finish                                 "#EC CALLED
        TABLES
           container
        USING
           command.
* begin of local data definition
  DATA:
    l_wizard    TYPE swf_wizard.
* end of local data definition

* initialize wizard step
* the wizard has one unique title while it is running
  l_wizard-title      = text-ttl.
  l_wizard-descobject = 'ZBDFRAMEWORK_WIZARD_007'
  . "this is a text in dialog (SE61)
* last screen (button 'continue' will be called 'done')
  l_wizard-screen_typ = wizard_screen_end.

* call wizard screen
  CALL FUNCTION 'SWF_WIZARD_CALL'
    EXPORTING
      wizard_data                 = l_wizard
    EXCEPTIONS
      operation_cancelled_by_user = 01
      back                        = 02.
  swf_evaluate command.

* process data collected by wizard
  PERFORM usr_process_wizard_data.

ENDFORM.                    " CB_PROCESS_STEP_FINISH
