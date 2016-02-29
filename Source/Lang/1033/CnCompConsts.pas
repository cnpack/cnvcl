{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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

unit CnCompConsts;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：资源字符串定义单元
* 单元作者：CnPack开发组
* 备    注：该单元定义了不可视工具组件类用到的资源字符串
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2002.04.18 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnTimer
  SCnTimerName = 'Thread Timer Component';
  SCnTimerComment = 'Thread Timer Component';
  SCnTimerListName = 'Thread Timer List Component';
  SCnTimerListComment = 'Thread Timer List Component';
  
  // CnControlHook
  SCnControlHookName = 'Control Hook Component';
  SCnControlHookComment = 'Control Hooker by Modify WindowProc Property';

  // CnActionListHook
  SCnActionListHookName = 'ActionList Hook Component';
  SCnActionListHookComment = 'Hook Actions in ActionList';

  // CnActiveScriptSite
  SCnActiveScriptSiteName = 'ActiveScript Site Component';
  SCnActiveScriptSiteComment = 'ActiveScript Site Engine Wrapper Component';

  // CnActiveScriptWindow
  SCnActiveScriptWindowName = 'ActiveScript Window Component';
  SCnActiveScriptWindowComment = 'ActiveScript Window Engine Wrapper Component';

  // CnADOConPool
  SCnADOConPoolName = 'ADO Connection Pool Component';
  SCnADOConPoolComment = 'ADO Connection Pool Component';

  // CnFormScaler
  SCnFormScalerName = 'Form Scale Component';
  SCnFormScalerComment = 'Auto Process Form Scale';

  // CnMDIBackGround
  SCnMDIBackGroundName = 'MDI Background Component';
  SCnMDIBackGroundComment = 'Draw MDI Window Background';

  // CnMenuHook
  SCnMenuHookName = 'Menu Hook Component';
  SCnMenuHookComment = 'Hook Menu and Items';

  // CnObjectPool
  SCnObjectPoolName = 'Object Pool Component';
  SCnObjectPoolComment = 'Object Pool Component';

  // CnThreadPool
  SCnThreadPoolName = 'Thread Pool Component';
  SCnThreadPoolComment = 'Thread Pool Component';

  // CnRestoreSystemMenu
  SCnRestoreSystemMenuName = 'System Menu Restore Component';
  SCnRestoreSystemMenuComment = 'Component to Restore System Menu';

  // CnConsole
  SCnConsoleName = 'Console Component';
  SCnConsoleComment = 'Add Console to GUI Application';
  
  // CnTrayIcon
  SCnTrayIconName = 'Tray Icon Component';
  SCnTrayIconComment = 'Tray Icon Component';

  // CnVolumnCtrl
  SCnVolumnCtrlName = 'Volume Control Component';
  SCnVolumnCtrlComment = 'Volume Control Component';
  SCnMixerOpenError         = 'Open Audio Device Failed!';
  SCnMixerGetDevCapsError   = 'Get Device Caps Failed!';
  SCnMixerGetLineInfoError  = 'Get Line Failed!';
  SCnMixerGetVolumeError    = 'Get Volume Failed!';
  SCnMixerGetMuteError      = 'Get Mute State Failed!';
  SCnMixerSetVolumeError    = 'Set Volume Failed!';
  SCnMixerSetMuteError      = 'Set Mute State Failed!';

  // CnWinampCtrl
  SCnWinampCtrlName = 'Winamp Control Component';
  SCnWinampCtrlComment = 'Winamp Control Component';  
  
  // CnSkinMagic
  SCnSkinMagicName = 'Runtime Skin Frame';
  SCnSkinMagicComment = 'Runtime Skin Frame using Customized Painting';  

  // CnDockServer
  SCnDockServerName = 'Dock Server Component';
  SCnDockServerComment = 'Dock Server Component, Makes a DockSite Form';

  // CnDockClient
  SCnDockClientName = 'Dock Client Component';
  SCnDockClientComment = 'Dock Client Component, Makes a Dockable Form';

  // CnDelphiDockStyle
  SCnDelphiDockStyleName = 'Delphi Dock Style Component';
  SCnDelphiDockStyleComment = 'A Dock Style Component with Delphi Dock Style';

  // CnVCDockStyle
  SCnVCDockStyleName = 'Visual C++ Dock Style Component';
  SCnVCDockStyleComment = 'A Dock Style Component with Visual C++ Dock Style';

  // CnVIDDockStyle
  SCnVIDDockStyleName = 'Visual InterDev Dock Style Component';
  SCnVIDDockStyleComment = 'A Dock Style Component with Visual InterDev Dock Style';

  // CnVSNETDockStyle
  SCnVSNETDockStyleName = 'Visual Studio.NET Dock Style Component';
  SCnVSNETDockStyleComment = 'A Dock Style Component with Visual Studio.NET Dock Style';
  
  // CnFileSystemWatcher
  SCnFileSystemWatcherName = 'File System Watcher';
  SCnFileSystemWatcherComment = 'File System Watcher';
  
  // CnFilePacker
  SCnFilePackerName = 'File Packer and Unpacker';
  SCnFilePackerComment = 'File Packer and Unpacker';

implementation

end.
