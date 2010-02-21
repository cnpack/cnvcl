@ECHO CnPack Component Package
@ECHO Original English Resource Files will be Copied to Here. 
@PAUSE
copy ..\..\Graphics\CnGraphConsts.pas  CnGraphConsts.pas   /Y
copy ..\..\MultiLang\CnLangConsts.pas  CnLangConsts.pas    /Y
copy ..\..\NetComm\CnNetConsts.pas     CnNetConsts.pas     /Y
copy ..\..\Graphics\CnAAFontDialog.dfm CnAAFontDialog.dfm  /Y
copy ..\..\Common\CnCompAboutFrm.dfm   CnCompAboutFrm.dfm  /Y
copy ..\..\ObjRep\CnFoxmailMsgFrm.dfm  CnFoxmailMsgFrm.dfm /Y
copy ..\..\ObjRep\CnProgressFrm.dfm    CnProgressFrm.dfm   /Y
copy ..\..\NetComm\CnRS232Dialog.dfm   CnRS232Dialog.dfm   /Y
copy ..\..\NonVisual\CnCompConsts.pas  CnCompConsts.pas    /Y
copy ..\..\Common\CnConsts.pas         CnConsts.pas        /Y
copy ..\..\NonVisual\CnDockGlobal.pas  CnDockGlobal.pas    /Y
copy ..\..\DbReport\CnDBConsts.pas     CnDBConsts.pas      /Y
@ECHO CnPack Component Package: Original Resource Files Copied.
@PAUSE