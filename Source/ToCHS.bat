@ECHO CnPack Component Package
@ECHO Chinese-Simplified Resource Files will be Copied to overwrite Current Files 
@PAUSE
copy .\Lang\2052\CnGraphConsts.pas   .\Graphics\CnGraphConsts.pas  /Y
copy .\Lang\2052\CnLangConsts.pas    .\MultiLang\CnLangConsts.pas  /Y
copy .\Lang\2052\CnNetConsts.pas     .\NetComm\CnNetConsts.pas     /Y
copy .\Lang\2052\CnAAFontDialog.dfm  .\Graphics\CnAAFontDialog.dfm /Y
copy .\Lang\2052\CnCompAboutFrm.dfm  .\Common\CnCompAboutFrm.dfm   /Y
copy .\Lang\2052\CnFoxmailMsgFrm.dfm .\ObjRep\CnFoxmailMsgFrm.dfm  /Y
copy .\Lang\2052\CnProgressFrm.dfm   .\ObjRep\CnProgressFrm.dfm    /Y
copy .\Lang\2052\CnRS232Dialog.dfm   .\NetComm\CnRS232Dialog.dfm   /Y
copy .\Lang\2052\CnCompConsts.pas    .\NonVisual\CnCompConsts.pas  /Y
copy .\Lang\2052\CnConsts.pas        .\Common\CnConsts.pas         /Y
copy .\Lang\2052\CnDockGlobal.pas    .\NonVisual\CnDockGlobal.pas  /Y
copy .\Lang\2052\CnDBConsts.pas      .\DbReport\CnDBConsts.pas     /Y
@ECHO CnPack Component Package: Chinese-Simplified Resource Files Copied.
@PAUSE