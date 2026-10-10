{******************************************************************************}
{                                                                              }
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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
* 修改记录：2007.07.17 V1.0
*                翻译单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Messages, CnDockFormControl, CnDockInfo, CnDockSupportControl;

const
  {北兜嘿}
  gs_CnProductName = 'CnPack Dock Component';
  gs_CnDcokServerName = '氨綼狝叭北兜';
  gs_CnDcokClientName = '氨綼め北兜';
  gs_CnDockStyleName = '氨綼北兜';
  {セ}
  gs_CnDockManagerVersion = '1.0.0.0'; {ぃ琌㏕﹚}
  gs_CnDockStyleVersion   = '1.0.0.0'; {ぃ琌㏕﹚}
  {丁}
  gs_CnDockManagerCopyRightBegin = '2002';
  gs_CnDockManagerCopyRightEnd = '2003';
  gs_CnDockStyleCopyRightBegin = '2002';
  gs_CnDockStyleCopyRightEnd = '2003';
  {}
  gs_CnAuthorName = '㏄痲猧';
  gs_CnComparyName = '临⊿Θミ';
  gs_CnHomePage = 'http://www.pigtwo.com' + #10#13 +
  'http://www.pigtwo.com/CtrlData/WebSite/luxiaoban.htm';
  gs_CnEmail = 'zhouyibo2000@sina.com' + #10#13 +
  'luxiaoban@sina.com';
  {闽}
  gs_CnAbout = '闽';
  gs_CnDockManagerAbout = '硂琌 %s, セ琌 %s,' + #10#13 +
                          '舦: %s-%s, : %s,そ: %s,' + #10#13 +
                          ': %s,' + #10#13 +
                          'Email: %s';
  gs_CnDockStyleAbout =   '硂琌 %s, セ琌 %s,' + #10#13 +
                          '舦: %s-%s, : %s,そ: %s,' + #10#13 +
                          ': %s,' + #10#13 +
                          'Email: %s';
  {じだ澄才腹}
  gs_CnStringSplitter = ' ';
  gs_CnDockInfoSplitter = '@';

  {矗ボ戈癟}
  gs_CnDockTreeCloseBtnHint = '闽超';
  gs_CnVCDockTreeExpandBtnHint = '耎甶';
  gs_CnVSNETDockTreeAutoHideBtnHint = '笆留旅';
  gs_CnDockTreeVSplitterHint = 'だ澄兵';
  gs_CnDockTreeHSplitterHint = '非だ澄兵';

  { Hash矗ボ戈癟 }
  gs_CnTableIndexError = '表ま禬絛瞅';
  gs_CnNodeExistedError = '竊翴竒';
  gs_CnComProcError = 'ゑ耕ㄧ计夹';

  { CnDockTree岿粇戈癟 }
  gs_ControlCannotIsNil = '把计Controlぃnil';
  gs_CannotGetValueWithNoOrient = 'ぃ莉眔⊿Τ氨綼よControl戈';
  gs_CannotSetValueWithNoOrient = 'ぃ砞竚⊿Τ氨綼よControl戈';

  { CnDockFormControl岿粇戈癟 }
  gs_CannotChangeDockStyleProperty = 'ぃ笲︽戳э跑DockStyle妮┦';
  gs_CannotLayAnother = '虫竒%s,ぃ%s';

  { CnDelphiDockStyle獺 }
  gs_LikeDelphiStyle = '摸 Delphi %s';

  { CnVCDockStyle獺 }
  gs_LikeVCStyle = '摸 Visual C++ %s';

  { CnVIDDockStyle獺 }
  gs_CannotSetTabPosition = 'ぃ砞竚TabPositiontpLeft┪tpRight';
  gs_LikeVIDStyle = '摸 Visual InterDev %s';
  gs_TabPositionMustBetpBottom = 'TabPosition﹚璶砞竚ΘtpBottom';

  { CnVSNETDockStyle獺 }
  gs_LikeVSNETStyle = '摸 Visual Studio.net %s';

  { CnEclipseDockStyle獺 }
  gs_LikeEclipseStyle = '摸 Java eclipse %s';

  { CnDcokInfo獺 }
  gs_CannotFindWindow = '!@#тぃ硂虫#@!';

  { DockTreeセ,讽秈︽氨綼戈癟杆更㎝纗璶ノウ }
  gs_BaseDockTreeVersion = $00040000;
  { DockTreeVCセ }
  gs_VCDockTreeVersion = $00040010;

  { 讽氨綼め㎝狝竟禯瞒DefExpandoRect盢祇ネ氨綼巨 }
  DefExpandoRect = 10;

  { WM_NCxxx秨﹍㎝WM_NCxxx挡 }
  WM_NCMOUSEFIRST = WM_NCMOUSEMOVE;
  WM_NCMOUSELAST  = WM_NCMBUTTONDBLCLK;

var
  { CnGlobalDockManager砆斑承ノㄓ恨瞶氨綼虫 }
  CnGlobalDockPresident: TCnDockPresident = nil;

  { 琌タ眖郎┪爹い杆更氨綼戈癟 }
  IsLoading: Boolean = False;

  { 讽菲公オ龄虫阑虫夹肈逆ウTCnDockClient结倒GlobalDockClient }
  GlobalDockClient: TCnDockClient = nil;
  
implementation

end.
