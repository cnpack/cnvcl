{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
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
* 单元名称：停靠组件的一些全局变量单元的英译版
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.07.17 V1.0
*                翻译单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Messages, CnDockFormControl, CnDockInfo, CnDockSupportControl;

const
  {Component Names}
  gs_CnProductName = 'CnPack Dock Components';
  gs_CnDcokServerName = 'Dock Server Component';
  gs_CnDcokClientName = 'Dock Client Component';
  gs_CnDockStyleName = 'Dock Style Component';
  {Version}
  gs_CnDockManagerVersion = '1.0.0.0'; {NOT Fixed}
  gs_CnDockStyleVersion   = '1.0.0.0'; {NOT Fixed}
  {Time}
  gs_CnDockManagerCopyRightBegin = '2002';
  gs_CnDockManagerCopyRightEnd = '2003';
  gs_CnDockStyleCopyRightBegin = '2002';
  gs_CnDockStyleCopyRightEnd = '2003';
  {Author}
  gs_CnAuthorName = 'Zhou Yibo';
  gs_CnComparyName = 'None';
  gs_CnHomePage = 'http://www.pigtwo.com' + #10#13 +
  'http://www.pigtwo.com/CtrlData/WebSite/luxiaoban.htm';
  gs_CnEmail = 'zhouyibo2000@sina.com' + #10#13 +
  'luxiaoban@sina.com';
  {About}
  gs_CnAbout = 'About';
  gs_CnDockManagerAbout = 'It is %s, version %s,' + #10#13 +
                          'Copywrite: %s-%s, Author: %s, Company: %s,' + #10#13 +
                          'Website: %s,' + #10#13 +
                          'Email: %s';
  gs_CnDockStyleAbout =   'It is %s, version %s,' + #10#13 +
                          'Copywright: %s-%s, Author: %s, Company: %s,' + #10#13 +
                          'Website: %s,' + #10#13 +
                          'Email: %s';
  {Splitter}
  gs_CnStringSplitter = ' ';
  gs_CnDockInfoSplitter = '@';

  {Hint}
  gs_CnDockTreeCloseBtnHint = 'Close';
  gs_CnVCDockTreeExpandBtnHint = 'Expand';
  gs_CnVSNETDockTreeAutoHideBtnHint = 'Auto Hide';
  gs_CnDockTreeVSplitterHint = 'Vertical Splitter';
  gs_CnDockTreeHSplitterHint = 'Horizontal Splitter';

  { Hash Table }
  gs_CnTableIndexError = 'Table Index Error';
  gs_CnNodeExistedError = 'Node Existed';
  gs_CnComProcError = 'Compare Proc Pointer is nil';

  { CnDockTree }
  gs_ControlCannotIsNil = 'Control can NOT be nil';
  gs_CannotGetValueWithNoOrient = 'Can NOT Get Control Value without Dock Orient';
  gs_CannotSetValueWithNoOrient = 'Can NOT Set Control Value without Dock Orient';

  { CnDockFormControl }
  gs_CannotChangeDockStyleProperty = 'Can NOT Change DockStyle in Runtime';
  gs_CannotLayAnother = 'A %s already Exists, Can NOT put Another %s';

  { CnDelphiDockStyle }
  gs_LikeDelphiStyle = 'Delphi %s';

  { CnVCDockStyle }
  gs_LikeVCStyle = 'Visual C++ %s';

  { CnVIDDockStyle }
  gs_CannotSetTabPosition = 'Can NOT Set TabPosition to tpLeft/tpRight';
  gs_LikeVIDStyle = 'Visual InterDev %s';
  gs_TabPositionMustBetpBottom = 'TabPosition must be tpBottom';

  { CnVSNETDockStyle }
  gs_LikeVSNETStyle = 'Visual Studio.net %s';

  { CnEclipseDockStyle}
  gs_LikeEclipseStyle = 'Java Eclipse %s';

  { CnDcokInfo }
  gs_CannotFindWindow = '!@# Can NOT Find Window #@!';

  { DockTree Version }
  gs_BaseDockTreeVersion = $00040000;
  { DockTree VC Version }
  gs_VCDockTreeVersion = $00040010;

  { Dock Instance: DefExpandoRect }
  DefExpandoRect = 10;

  { WM_NCxxx Start and End }
  WM_NCMOUSEFIRST = WM_NCMOUSEMOVE;
  WM_NCMOUSELAST  = WM_NCMBUTTONDBLCLK;

var
  { CnGlobalDockManager }
  CnGlobalDockPresident: TCnDockPresident = nil;

  { Is Loading from File or Registry }
  IsLoading: Boolean = False;

  {GlobalDockClient will be set to a TCnDockClient on a form when Caption clicked. }
  GlobalDockClient: TCnDockClient = nil;
  
implementation

end.
