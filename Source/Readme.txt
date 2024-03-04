cnpack\Source 目录内容说明：

\Common －公共库目录。注：组件包主图标文件为 CnPack.dcr，但 BCB6 下不认识，因此复制了一份改名为 CnPack.res

  CnAntiCheater.pas
    使用内存变换防明文搜索的工具类

  CnBinaryDiffPatch.pas
    二进制差分与合并算法实现单元

  CnBloomFilter.pas
    散花过滤实现单元，用多散列函数配合 Bit 位记录字符串

  CnCalClass.pas
    历法计算类库

  CnCalendar.pas
    历法计算的函数库

  CnCallBack.pas
    回调函数转换库

  CnClasses.pas
    基础类定义单元

  CnCommon.pas
    公共运行基础库单元

  CnCompAboutFrm.pas
    开发包公共组件的关于窗口单元

  CnCompUtils.pas
    组件工具单元

  CnConsts.pas
    公共资源字符串定义

  CnContainers.pas
    自定义容器实现单元

  CnDancingLinks.pas
    十字循环双向链表的稀疏矩阵以及跳舞链表的实现单元

  CnDebug.pas
    CnDebug 调试信息输出接口单元

  CnDynObjBuilder.pas
    根据字符串动态创建对象

  CnEventBus.pas
    模拟 EventBus 实现的事件通知单元

  CnEventHook.pas
    事件挂接的实现单元

  CnFitCurve.pas
    曲线拟合实现单元

  CnFloat.pas
    浮点数转换为二、八、十六进制的实现单元

  CnFmxUtils.pas
    FMX 辅助库单元

  CnGB18030.pas
    支持 GB18030 中文大字符集处理以及与 Unicode 互相转换的工具单元

  CnGraph.pas
    有向图与无向图实现单元

  CnGraphUtils.pas
    公共图像函数库

  CnHardWareInfo.pas
    硬件信息获取单元

  CnHashMap.pas
    CnHashMap 实现单元

  CnHashTable.pas
    高性能 HashTable 实现单元

  CnIni.pas
    扩展的 INI 访问单元

  CnIniCfg.pas
    利用 RTTI 在 INI 中保存配置参数的基类

  CnIniStrUtils.pas
    扩展的 INI 访问的字符串处理

  CnLockFree.pas
    无锁数据结构实现单元

  CnLinkedList.pas
    双向链表实现单元

  CnMath.pas
    数学计算库

  CnMatrix.pas
    整数以及有理数矩阵运算的实现单元

  CnMemProf.pas
    一个简单的内存防护管理器实现单元

  CnMethodHook.pas
    方法挂接类

  CnMulticastEvent.pas
    多播实现类

  CnObjAuto.pas
    部分自动化对象实现单元

  CnOTAUtils.pas
    设计期组件包工具类

  CnPack.pas
    用于在 C++Builder 设计期包中引入 CnPack.dcr 文件的空单元

  CnPackRegister.pas
    组件包注册单元

  CnPDF.pas
    一个简易的 PDF 文件解析及根据图像生成 PDF 文件的实现单元

  CnPE.pas
    PE 文件解析实现单元

  CnPropEditors.pas
    开发包属性组件编辑器

  CnPropSheetFrm.pas
    CnDebug用于显示对象属性的窗体单元

  CnQuantum.pas
    部分量子基础实现单元

  CnRopes.pas
    Ropes 数据结构实现单元

  CnRTL.pas
    运行期异常捕捉实现单元

  CnShellUtils.pas
    Shell 工具单元

  CnSingleton.pas
    单实例对象实现单元

  CnSingletonComp.pas
    单实例组件实现单元

  CnSkipList.pas
    跳跃链表实现单元

  CnSQLite.pas
    SQLite 数据库的 Delphi 封装

  CnStrDiff.pas
    字符串详细比较

  CnStream.pas
    扩展的 Stream 类实现

  CnStrings.pas
    Ansi 版 StringList 实现单元

  CnThreadTaskMgr.pas
    多线程任务管理单元

  CnTree.pas
    单根无序树的类实现单元

  CnTreeClasses.pas
    二叉树、字典搜索树等的类实现单元

  CnVarList.pas
    变体列表实现单元

  CnVCLBase.pas
    基本类定义单元

  CnVclFmxMixed.pas
    VCL与FMX相关的分离单元，避免引用过多导致体积过大的问题

  CnWideStrings.pas
    Wide 版 StringList 实现单元

  CnWinSvc.pas
    Windows 服务封装单元

  CnXMLPersistent.pas
    XML 流化类的工具类单元

  CnZip.pas
    Zip 压缩文件实现单元

              68 文件

\Crypto - 加解密算法目录

  Cn25519.pas
    Curve25519/Ed25519/448 系列椭圆曲线实现单元

  CnAEAD.pas
    AEAD 关联数据认证加密算法单元

  CnAES.pas
    AES 算法单元

  CnBase64.pas
    Base64 编码解码算法单元

  CnBerUtils.pas
    BER/DER 编码格式解析单元

  CnBigNumber.pas
    大数计算单元

  CnCertificateAuthority.pas
    证书签名验证的实现单元

  CnChaCha20.pas
    ChaCha20 算法实现单元

  CnComplex.pas
    浮点复数实现单元

  CnCRC32.pas
    CRC32循环冗余校验单元

  CnDES.pas
    DES 加密解密算法实现单元

  CnDFT.pas
    离散傅立叶变换实现单元

  CnECC.pas
    椭圆曲线加密算法实现单元

  CnFEC.pas
    前向纠错编码解码算法的部分实现单元

  CnFNV.pas
    FNV 杂凑算法的实现单元

  CnInt128.pas
    128 位整数及其运算实现单元

  CnKDF.pas
    密码生成算法单元

  CnLattice.pas
    格密码计算单元

  CnMD5.pas
    MD5 算法单元

  CnNative.pas
    Win32/64 的部分通用声明单元

  CnOTP.pas
    动态口令实现单元

  CnOTS.pas
    一次性杂凑签名实现单元

  CnPaillier.pas
    加法同态 Paillier 算法实现单元

  CnPDFCrypt.pas
    PDF 文件加解密实现单元

  CnPemUtils.pas
    PEM 格式处理工具单元

  CnPoly1305.pas
    Poly1305 算法实现单元

  CnPolynomial.pas
    多项式运算单元

  CnPrimeNumber.pas
    素数运算单元

  CnRandom.pas
    随机数生成单元

  CnRC4.pas
    RC4 算法实现单元

  CnRSA.pas
    RSA 算法实现单元

  CnSecretSharing.pas
    秘密共享场景的 Shamir 门限方案实现单元

  CnSHA1.pas
    SHA1 算法实现单元

  CnSHA2.pas
    SHA2 算法实现单元

  CnSHA3.pas
    SHA3 算法实现单元

  CnSM2.pas
    中国国家密码管理局发布的 SM2 椭圆曲线算法实现单元

  CnSM3.pas
    中国国家密码管理局发布的 SM3 密码杂凑算法实现单元

  CnSM4.pas
    中国国家密码管理局发布的 SM4 分组密码算法实现单元

  CnSM9.pas
    中国国家密码管理局发布的 SM9 标识密码算法实现单元

  CnTEA.pas
    TEA/XTEA/XXTEA 算法实现单元

  CnVector.pas
    向量计算实现单元

  CnZUC.pas
    祖冲之算法实现单元

                共 43 文件

\DbReport －数据库组件与报表库目录

  CnADOBinding.pas
    查询分析器组件仿 VC++ 的数据绑定单元

  CnADOUpdateSQL.pas
    ADOUpdateSQL 实现单元，在 ADO 下实现 UpdateSQL 组件的功能

  CnADOUpdateSQLEditor.pas
    ADOUpdateSQL 的组件编辑器的实现单元

  CnADOUpdateSQLFrm.pas
    ADOUpdateSQL 的组件编辑器的窗体实现单元

  CnDataGrid.pas
    查询分析器组件中装载数据的网格控件

  CnDBConsts.pas
    CnPack 数据库报表系列组件常量定义

  CnDBRegister.pas
    CnPack 数据库报表组件与编辑器注册单元

  CnExcelUnit.pas
    与 Excel 交互的封装单元

  CnPagedGrid.pas
    支持分页的数据网格组件实现单元

  CnRunSqlFrame.pas
    查询分析器组件界面 Frame 实现单元

  CnRunSqlUnit.pas
    查询分析器组件查询运行线程单元

  CnSQLAnalyzer.pas
    查询分析器组件实现单元

  CnXlsWriter.pas
    一简单的 XLS 文件生成类实现单元

                13 文件

\Graphics －图像界面控件库目录

  CnAACtrl.pas
    平滑特效字体控件单元

  CnAAFont.pas
    平滑特效字体单元

  CnAAFontDialog.pas
    平滑特效字体对话框控件单元

  CnAAFontEditor.pas
    平滑特效字体属性、组件编辑器单元

  CnAOTreeView.pas
    自动参数设置 TreeView 组件实现单元

  CnAutoOption.pas
    自动参数设置类定义单元

  CnButtonEdit.pas
    带按钮的编辑框控件实现单元

  CnButtons.pas
    CnButton 和 CnBitBtn 的自画按钮实现单元

  CnCheckTreeView.pas
    带复选框的 TreeView 组件实现单元

  CnColorGrid.pas
    颜色网格实现单元

  CnEdit.pas
    CnEdit 控件实现单元

  CnErrorProvider.pas
    CnErrorProvider 错误信息提示组件实现单元

  CnGauge.pas
    CnGauge 支持渐变色与图像的进度条实现单元

  CnGraphConsts.pas
    图像界面库的资源字符串定义单元

  CnGraphics.pas
    界面控件包原快速图像处理单元

  CnGraphPropEditors.pas
    界面控件包属性编辑器单元

  CnGraphRegister.pas
    界面控件包注册单元

  CnHexEditor.pas
    CnHexEditor 十六进制查看编辑单元

  CnHint.pas
    CnHint 提示控件实现单元

  CnIconUtils.pas
    Ico 图标处理辅助单元

  CnImage.pas
    调试用界面控件 TCnImage、TCnPaintBox 单元

  CnLED.pas
    LED 效果的显示组件实现单元，支持汉字。

  CnListBox.pas
    支持自画效果的 CnListBox 控件实现单元

  CnMandelbrotImage.pas
    曼德布罗集图实现单元

  CnMemo.pas
    CnMemo 控件实现单元

  CnMonthCalendar.pas
    中文月历的界面组件实现单元

  CnOpenGLPaintBox.pas
    利用 OpenGL 硬件加速的画布控件实现单元

  CnPanel.pas
    带透明效果的Panel实现单元

  CnSearchCombo.pas
    带下拉搜索匹配的 ComboBox 控件单元

  CnShellCtrls.pas
    移植的 ShellCtrls 控件单元

  CnSkinMagic.pas
    运行期皮肤框架组件实现单元

  CnSpin.pas
    CnSpin 控件单元

  CnSplitter.pas
    CnSplitter 分隔条实现单元

  CnTabSet.pas
    CnTabSet 组件实现单元

  CnTextControl.pas
    基础文本编辑控件实现单元

  CnValidateImage.pas
    简单的验证码生成图片单元

  CnWaterEffect.pas
    水波效果处理单元

  CnWaterImage.pas
    水波效果图像 WaterImage 控件

  CnWizardImage.pas
    向导界面图像控件

               39 文件

\Lang\2052
\Lang\1028
\Lang\1033

    － 本两目录存储多语字符串的其他语种版本，
       2052 为简体中文，1028 为繁体中文，1033 为英语。

  CnCompConsts.pas
  CnConsts.pas
  CnGraphConsts.pas
  CnLangConsts.pas
  CnNetConsts.pas
  CnDockGlobal.pas
  CnDBConst.pas
  CnRS232Dialog.dfm
  CnProgressFrm.dfm
  CnFoxmailMsgFrm.dfm
  CnCompAboutFrm.dfm
  CnAAFontDialog.dfm

                12 文件，说明同各目录下的同名文件。

\MultiLang －多语组件

  CnHashIniFile.pas
    实现 HashIni 文件

  CnHashLangStorage.pas
    Hash 文本多语存储组件实现单元

  CnIniLangFileStorage.pas
    Ini 多语存储组件单元

  CnLangCollection.pas
    语言条目描述及其列表类单元

  CnLangConsts.pas
    多语包常量定义单元

  CnLangEditors.pas
    多语包部分属性编辑器

  CnLangMgr.pas
    多语管理器基础类

  CnLangReg.pas
    多语包组件注册单元

  CnLangStorage.pas
    多语存储组件基类

  CnLangTranslator.pas
    多语包翻译器组件单元

  CnLangUtils.pas
    多语包工具类，实现了和 DEP 兼容的 CnLanguages 列表类

  CnTransEditor.pas
    多语包 IDE 翻译编辑器

  CnTransFilter.pas
    多语包 IDE 翻译编辑器的过滤设置窗体单元

               13 文件

\NetComm －网络通讯

  CnCameraEye.pas
    摄像头控制组件实现单元

  CnDialUp.pas
    拨号连接组件实现单元

  CnDNS.pas
    利用 UDP 实现 DNS 发送与解析的组件实现单元

  CnIISCtrl.pas
    IIS 控制组件实现单元

  CnInetUtils.pas
    WinInet 封装单元

  CnIocpSimpleMemPool.pas
    IOCP 完成端口配套的内存池实现单元

  CnIocpSocketAdapter.pas
    IOCP 完成端口实现单元

  CnIP.pas
    IP 获取与计算组件实现单元

  CnModem.pas
    CnModem 标准调制解调器组件单元

  CnNetConsts.pas
    网络部分的资源字符串定义单元

  CnNetwork.pas
    网络部分的各种协议包头格式定义单元

  CnNetPropEditor.pas
    网络通讯类属性编辑器单元

  CnNetRegister.pas
    网络通讯组件包注册单元

  CnPing.pas
    Ping 功能实现组件单元

  CnRedisClient.pas
    Redis 客户端封装单元

  CnRS232.pas
    CnRS232 串口通讯组件单元

  CnRS232Dialog.pas
    CnRS232Dialog 串口设置对话框组件及窗体单元

  CnSocket.pas
    封装跨平台的 Socket 操作函数的单元

  CnTCPClient.pas
    TCP 客户端控件实现单元

  CnTCPForwarder.pas
    TCP 端口转发控件实现单元

  CnThreadingTCPServer.pas
    多线程 TCP Server 控件实现单元

  CnTwain.pas
    扫描仪控制组件实现单元，仅支持部分类型的扫描仪

  CnUDP.pas
    UDP 通讯组件单元

                24 文件

\NonVisual －不可视组件

  CnActionListHook.pas
    ActionList 挂接服务单元

  CnActiveScript.pas
    ActiveScript 脚本引擎封装组件单元

  CnADOConPool.pas
    ADOConnection 对象池单元

  CnASCommon.pas
    ActiveScript Host 服务单元

  CnASHostServices.pas
    ActiveScript Host 服务单元

  CnASIDispatchProxy.pas
    ActiveScript Host 对象 IDispatch 代理接口单元

  CnASInvoker.pas
    ActiveScript 中根据接口动态调用方法

  CnASPropEditors.pas
    ActiveScript 脚本引擎属性编辑器单元

  CnCompConsts.pas
    不可视组件的字符串定义单元

  CnCompRegister.pas
    不可视工具组件包注册单元

  CnConsole.pas
    控制台组件 CnConsole 的实现单元

  CnControlHook.pas
    控件消息处理过程挂接组件单元

  CnDragResizer.pas
    一设计期模式的拖动组件实现单元

  CnGlobalKeyHook.pas
    用RegisterHotKey实现的全局键盘监控组件实现单元

  CnFilePacker.pas
    文件目录打包组件单元

  CnFileSystemWatcher.pas
    文件目录监视组件单元

  CnFormScaler.pas
    在不同的屏幕 DPI 下，自动调整窗体的字体和大小的组件单元

  CnKeyBlocker.pas
    利用键盘钩子在系统范围内屏蔽某些组合键的组件实现单元

  CnMDIBackGround.pas
    MDI 主窗体背景控件

  CnMenuHook.pas
    菜单挂接服务单元

  CnObjectPool.pas
    可扩展的高性能对象池 CnObjectPool 实现单元

  CnOuterControls.pas
    外部窗口控制组件实现单元

  CnRestoreSystemMenu.pas
    用来恢复编辑器控件右键菜单的组件

  CnThreadPool.pas
    线程池 CnThreadPool 实现单元

  CnTimer.pas
    高精度定时器 CnTimer 实现单元

  CnTrayIcon.pas
    系统托盘控件 CnTrayIcon 实现单元

  CnVolumeCtrl.pas
    音量控制组件 CnVolumeCtrl 实现单元

  CnWinampCtrl.pas
    Winamp 控制器组件 CnWinampCtrl 的实现单元

  CnDelphiDockStyle.pas
  CnDockFormControl.pas
  CnDockGlobal.pas
  CnDockHashTable.pas
  CnDockInfo.pas
  CnDockPropertyReg.pas
  CnDockSupportClass.pas
  CnDockSupportControl.pas
  CnDockSupportProc.pas
  CnDockTree.pas
  CnVCDockStyle.pas
  CnVIDDockStyle.pas
  CnVSNETDockStyle.pas
    Dock 系列组件（原 DockPresident）实现单元

               41 文件

\ObjRep －其他界面实现

  CnFoxmailMsgFrm.pas
    仿 Foxmail 的动态提示窗体单元

  CnProgressFrm.pas
    通用进度条窗体单元

                2 文件
