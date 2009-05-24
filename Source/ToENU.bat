@ECHO CnPack Component Package
@ECHO English Resource Files will be Copied to overwrite Chinese Files 
@PAUSE
copy .\Lang\1033\CnGraphConsts.pas   .\Graphics\CnGraphConsts.pas  /Y
copy .\Lang\1033\CnLangConsts.pas    .\MultiLang\CnLangConsts.pas  /Y
copy .\Lang\1033\CnNetConsts.pas     .\NetComm\CnNetConsts.pas     /Y
copy .\Lang\1033\CnAAFontDialog.dfm  .\Graphics\CnAAFontDialog.dfm /Y
copy .\Lang\1033\CnCompAboutFrm.dfm  .\Common\CnCompAboutFrm.dfm   /Y
copy .\Lang\1033\CnFoxmailMsgFrm.dfm .\ObjRep\CnFoxmailMsgFrm.dfm  /Y
copy .\Lang\1033\CnProgressFrm.dfm   .\ObjRep\CnProgressFrm.dfm    /Y
copy .\Lang\1033\CnRS232Dialog.dfm   .\NetComm\CnRS232Dialog.dfm   /Y
copy .\Lang\1033\CnCompConsts.pas    .\NonVisual\CnCompConsts.pas  /Y
copy .\Lang\1033\CnConsts.pas        .\Common\CnConsts.pas         /Y
copy .\Lang\1033\CnDockGlobal.pas    .\NonVisual\CnDockGlobal.pas  /Y
copy .\Lang\1033\CnDBConsts.pas      .\DbReport\CnDBConsts.pas     /Y
@ECHO CnPack Component Package: English Resource Files Copied.
@PAUSE