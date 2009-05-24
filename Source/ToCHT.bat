@ECHO CnPack Component Package
@ECHO CHT Resource Files will be Copied to overwrite CHS Files 
@PAUSE
copy .\Lang\1028\CnGraphConsts.pas   .\Graphics\CnGraphConsts.pas  /Y
copy .\Lang\1028\CnLangConsts.pas    .\MultiLang\CnLangConsts.pas  /Y
copy .\Lang\1028\CnNetConsts.pas     .\NetComm\CnNetConsts.pas     /Y
copy .\Lang\1028\CnAAFontDialog.dfm  .\Graphics\CnAAFontDialog.dfm /Y
copy .\Lang\1028\CnCompAboutFrm.dfm  .\Common\CnCompAboutFrm.dfm   /Y
copy .\Lang\1028\CnFoxmailMsgFrm.dfm .\ObjRep\CnFoxmailMsgFrm.dfm  /Y
copy .\Lang\1028\CnProgressFrm.dfm   .\ObjRep\CnProgressFrm.dfm    /Y
copy .\Lang\1028\CnRS232Dialog.dfm   .\NetComm\CnRS232Dialog.dfm   /Y
copy .\Lang\1028\CnCompConsts.pas    .\NonVisual\CnCompConsts.pas  /Y
copy .\Lang\1028\CnConsts.pas        .\Common\CnConsts.pas         /Y
copy .\Lang\1028\CnDockGlobal.pas    .\NonVisual\CnDockGlobal.pas  /Y
copy .\Lang\1028\CnDBConsts.pas      .\DbReport\CnDBConsts.pas     /Y
@ECHO CnPack Component Package: CHT Resource Files Copied.
@PAUSE