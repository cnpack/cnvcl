{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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
* 修改记录：2002.04.18 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnTimer
  SCnTimerName = '高精度定时器组件';
  SCnTimerComment = '高精度定时器组件';
  SCnTimerListName = '高精度定时器列表组件';
  SCnTimerListComment = '高精度定时器列表组件';

  // CnControlHook
  SCnControlHookName = '控件挂接组件';
  SCnControlHookComment = '通过修改控件的 WindowProc 属性来挂接其消息处理主过程';

  // CnActionListHook
  SCnActionListHookName = 'ActionList 挂接组件';
  SCnActionListHookComment = '挂接 ActionList 的各个 Action 的组件';

  // CnActiveScriptSite
  SCnActiveScriptSiteName = 'ActiveScript Site 封装组件';
  SCnActiveScriptSiteComment = 'ActiveScript Site 脚本引擎封装组件';

  // CnActiveScriptWindow
  SCnActiveScriptWindowName = 'ActiveScript Window 封装组件';
  SCnActiveScriptWindowComment = 'ActiveScript Window 脚本引擎封装组件';

  // CnADOConPool
  SCnADOConPoolName = 'ADO Connection 连接池组件';
  SCnADOConPoolComment = 'ADO Connection 连接池组件';

  // CnFormScaler
  SCnFormScalerName = 'Form Scale 自动处理组件';
  SCnFormScalerComment = 'Form Scale 自动处理组件';

  // CnMDIBackGround
  SCnMDIBackGroundName = 'MDI 主窗体背景组件';
  SCnMDIBackGroundComment = 'MDI 主窗体背景绘制与控制组件';

  // CnMenuHook
  SCnMenuHookName = '菜单挂接组件';
  SCnMenuHookComment = '实现菜单挂接功能的组件';

  // CnObjectPool
  SCnObjectPoolName = '对象池组件';
  SCnObjectPoolComment = '实现对象池的组件';

  // CnThreadPool
  SCnThreadPoolName = '线程池组件';
  SCnThreadPoolComment = '实现线程池的组件';

  // CnRestoreSystemMenu
  SCnRestoreSystemMenuName = '系统菜单恢复组件';
  SCnRestoreSystemMenuComment = '恢复编辑器控件右键菜单的组件';
  
  // CnConsole
  SCnConsoleName = '控制台组件';
  SCnConsoleComment = '为 GUI 应用程序增加控制台';
  
  // CnTrayIcon
  SCnTrayIconName = '系统托盘组件';
  SCnTrayIconComment = '系统托盘组件';

  // CnVolumnCtrl
  SCnVolumnCtrlName = '音量控制组件';
  SCnVolumnCtrlComment = '用于控制系统音量，支持多设备多线路';
  SCnMixerOpenError         = '打开音频设备失败!';
  SCnMixerGetDevCapsError   = '获取设备标题失败!';
  SCnMixerGetLineInfoError  = '打开音频线路失败!';
  SCnMixerGetVolumeError    = '获取当前音量失败!';
  SCnMixerGetMuteError      = '获取静音状态失败!';
  SCnMixerSetVolumeError    = '设置当前音量失败!';
  SCnMixerSetMuteError      = '设置静音状态失败!';

  // CnWinampCtrl
  SCnWinampCtrlName = 'Winamp 控制器组件';
  SCnWinampCtrlComment = 'Winamp 控制器组件，可用来控制 Winamp';

  // CnSkinMagic
  SCnSkinMagicName = '运行期皮肤框架组件';
  SCnSkinMagicComment = '运行期皮肤框架组件，使用自定义绘制';

  // CnDockServer
  SCnDockServerName = '停靠服务端组件';
  SCnDockServerComment = '停靠服务端组件，使窗体接受停靠';

  // CnDockClient
  SCnDockClientName = '停靠客户端组件';
  SCnDockClientComment = '停靠客户端组件，使窗体可停靠';

  // CnDelphiDockStyle
  SCnDelphiDockStyleName = '类似 Delphi 的停靠风格组件';
  SCnDelphiDockStyleComment = '类似 Delphi 的停靠风格组件';

  // CnVCDockStyle
  SCnVCDockStyleName = '类似 Visual C++ 的停靠风格组件';
  SCnVCDockStyleComment = '类似 Visual C++ 的停靠风格组件';

  // CnVIDDockStyle
  SCnVIDDockStyleName = '类似 Visual InterDev 的停靠风格组件';
  SCnVIDDockStyleComment = '类似 Visual InterDev 的停靠风格组件';

  // CnVSNETDockStyle
  SCnVSNETDockStyleName = '类似 Visual Studio.NET 的停靠风格组件';
  SCnVSNETDockStyleComment = '类似 Visual Studio.NET 的停靠风格组件';
  
  // CnFileSystemWatcher
  SCnFileSystemWatcherName = '文件目录监视组件';
  SCnFileSystemWatcherComment = '文件目录监视组件';
  
  // CnFilePacker
  SCnFilePackerName = '文件目录打包组件';
  SCnFilePackerComment = '文件目录打包组件';

implementation

end.
