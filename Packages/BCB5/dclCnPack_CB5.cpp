//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("dclCnPack_CB5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("Vclx50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("dclado50.bpi");
USEPACKAGE("vclado50.bpi");
USEPACKAGE("vclmid50.bpi");
USEPACKAGE("CnPack_CB5.bpi");
USEUNIT("..\..\Source\Common\CnPropEditors.pas");
USEUNIT("..\..\Source\Common\CnOTAUtils.pas");
USEFORMNS("..\..\Source\Common\CnCompAboutFrm.pas", Cncompaboutfrm, CnCompAboutForm);
USEUNIT("..\..\Source\Common\CnPackRegister.pas");
USEUNIT("..\..\Source\DbReport\CnDBRegister.pas");
USEUNIT("..\..\Source\DbReport\CnADOUpdateSQLEditor.pas");
USEFORMNS("..\..\Source\DbReport\CnADOUpdateSQLFrm.pas", Cnadoupdatesqlform, CnADOUpdateSQLForm);
USEUNIT("..\..\Source\Graphics\CnGraphPropEditors.pas");
USEUNIT("..\..\Source\Graphics\CnGraphRegister.pas");
USEUNIT("..\..\Source\Graphics\CnAAFontEditor.pas");
USEUNIT("..\..\Source\MultiLang\CnLangEditors.pas");
USEUNIT("..\..\Source\MultiLang\CnLangReg.pas");
USEFORMNS("..\..\Source\MultiLang\CnTransEditor.pas", Cntranseditor, FrmTransEditor);
USEFORMNS("..\..\Source\MultiLang\CnTransFilter.pas", Cntransfilter, FrmTransFilter);
USEUNIT("..\..\Source\NetComm\CnNetPropEditor.pas");
USEUNIT("..\..\Source\NetComm\CnNetRegister.pas");
USEUNIT("..\..\Source\NonVisual\CnCompRegister.pas");
USEUNIT("..\..\Source\NonVisual\CnASPropEditors.pas");
USEUNIT("..\..\Source\NonVisual\CnDockPropertyReg.pas");
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