//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORMNS("..\..\Source\Common\CnCompAboutFrm.pas", Cncompaboutfrm, CnCompAboutForm);
USEFORMNS("..\..\Source\MultiLang\CnTransEditor.pas", Cntranseditor, FrmTransEditor);
USEFORMNS("..\..\Source\MultiLang\CnTransFilter.pas", Cntransfilter, FrmTransFilter);
USEFORMNS("..\..\Source\DbReport\CnADOUpdateSQLFrm.pas", Cnadoupdatesqlform, CnADOUpdateSQLForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------