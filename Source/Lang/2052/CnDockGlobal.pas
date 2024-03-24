{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

{*******************************************************}
{                                                       }
{       定义一些全局的变量                              }
{       CnDockGlobal 单元                               }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnDockGlobal;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：停靠组件的一些全局变量单元 
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.07.13 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Messages, CnDockFormControl, CnDockInfo, CnDockSupportControl;

const
  {控件名称}
  gs_CnProductName = 'CnPack Dock Component';
  gs_CnDcokServerName = '停靠服务控件';
  gs_CnDcokClientName = '停靠客户控件';
  gs_CnDockStyleName = '停靠风格控件';
  {版本}
  gs_CnDockManagerVersion = '1.0.0.0'; {不是固定的}
  gs_CnDockStyleVersion   = '1.0.0.0'; {不是固定的}
  {时间}
  gs_CnDockManagerCopyRightBegin = '2002';
  gs_CnDockManagerCopyRightEnd = '2003';
  gs_CnDockStyleCopyRightBegin = '2002';
  gs_CnDockStyleCopyRightEnd = '2003';
  {作者}
  gs_CnAuthorName = '周益波';
  gs_CnComparyName = '还没成立';
  gs_CnHomePage = 'http://www.pigtwo.com' + #10#13 +
  'http://www.pigtwo.com/CtrlData/WebSite/luxiaoban.htm';
  gs_CnEmail = 'zhouyibo2000@sina.com' + #10#13 +
  'luxiaoban@sina.com';
  {关于}
  gs_CnAbout = '关于';
  gs_CnDockManagerAbout = '这是一个 %s, 版本是 %s,' + #10#13 +
                          '版权: %s-%s, 作者: %s,公司: %s,' + #10#13 +
                          '个人主页: %s,' + #10#13 +
                          'Email: %s';
  gs_CnDockStyleAbout =   '这是一个 %s, 版本是 %s,' + #10#13 +
                          '版权: %s-%s, 作者: %s,公司: %s,' + #10#13 +
                          '个人主页: %s,' + #10#13 +
                          'Email: %s';
  {字符分割符号}
  gs_CnStringSplitter = ' ';
  gs_CnDockInfoSplitter = '@';

  {提示信息}
  gs_CnDockTreeCloseBtnHint = '关闭';
  gs_CnVCDockTreeExpandBtnHint = '扩展';
  gs_CnVSNETDockTreeAutoHideBtnHint = '自动隐藏';
  gs_CnDockTreeVSplitterHint = '垂直分割条';
  gs_CnDockTreeHSplitterHint = '水平分割条';

  { Hash表的提示信息 }
  gs_CnTableIndexError = '桶索引超出范围';
  gs_CnNodeExistedError = '节点已经存在了';
  gs_CnComProcError = '比较的函数指针为空';

  { CnDockTree的错误信息 }
  gs_ControlCannotIsNil = '参数Control不能为nil';
  gs_CannotGetValueWithNoOrient = '不能获得没有停靠方向的Control的数据';
  gs_CannotSetValueWithNoOrient = '不能设置没有停靠方向的Control的数据';

  { CnDockFormControl的错误信息 }
  gs_CannotChangeDockStyleProperty = '不能在运行期改变DockStyle属性';
  gs_CannotLayAnother = '在窗体上已经放了一个%s,不能再放另一个%s了';

  { CnDelphiDockStyle的信息 }
  gs_LikeDelphiStyle = '类似 Delphi 的%s';

  { CnVCDockStyle的信息 }
  gs_LikeVCStyle = '类似 Visual C++ 的%s';

  { CnVIDDockStyle的信息 }
  gs_CannotSetTabPosition = '不能设置TabPosition为tpLeft或者tpRight';
  gs_LikeVIDStyle = '类似 Visual InterDev 的%s';
  gs_TabPositionMustBetpBottom = 'TabPosition一定要设置成tpBottom';

  { CnVSNETDockStyle的信息 }
  gs_LikeVSNETStyle = '类似 Visual Studio.net 的%s';

  { CnEclipseDockStyle的信息 }
  gs_LikeEclipseStyle = '类似 Java eclipse 的%s';

  { CnDcokInfo的信息 }
  gs_CannotFindWindow = '!@#找不到这个窗体#@!';

  { DockTree的版本,当进行停靠信息的装载和存储的时候要用到它 }
  gs_BaseDockTreeVersion = $00040000;
  { DockTree的VC版本 }
  gs_VCDockTreeVersion = $00040010;

  { 当停靠客户和服务器的距离小于DefExpandoRect时将发生停靠操作 }
  DefExpandoRect = 10;

  { WM_NCxxx的开始和WM_NCxxx的结束 }
  WM_NCMOUSEFIRST = WM_NCMOUSEMOVE;
  WM_NCMOUSELAST  = WM_NCMBUTTONDBLCLK;

var
  { CnGlobalDockManager被唯一创建，用来管理停靠窗体 }
  CnGlobalDockPresident: TCnDockPresident = nil;

  { 是否正在从文件或注册表中装载停靠信息 }
  IsLoading: Boolean = False;

  { 当鼠标左键单击窗体上的标题栏时，它上面的TCnDockClient赋值给GlobalDockClient }
  GlobalDockClient: TCnDockClient = nil;
  
implementation

end.
