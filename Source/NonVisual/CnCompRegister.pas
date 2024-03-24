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

unit CnCompRegister;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：不可视工具组件包注册单元
* 单元作者：CnPack开发组
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2002.04.18 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
{$IFDEF SUPPORT_ADO}
  {$IFDEF SUPPORT_CROSS_PLATFORM} Data.Win.AdoConEd {$ELSE} AdoConEd {$ENDIF},
{$ENDIF}
{$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  CnTimer, CnFormScaler, CnControlHook, CnActionListHook, CnMenuHook, CnThreadPool,
  CnActiveScript, CnASPropEditors, CnTrayIcon, CnObjectPool, CnConsole,
  CnVolumeCtrl, CnMDIBackGround, CnWinampCtrl, CnRestoreSystemMenu, CnDockFormControl, 
  CnDelphiDockStyle, CnVCDockStyle, CnVIDDockStyle, CnVSNETDockStyle, CnDockPropertyReg,
  CnFileSystemWatcher, CnDragResizer, CnKeyBlocker, CnFilePacker, CnGlobalKeyHook,
  CnOuterControls, CnTaskBar, CnRawInput, CnSystemDebugControl, CnConsts;

procedure Register;
{* 控件、组件编辑器、属性编辑器注册过程}

implementation

procedure Register;
begin
  RegisterComponents(SCnNonVisualPalette, [TCnTimer, TCnTimerList, TCnTrayIcon]);
  RegisterComponents(SCnNonVisualPalette, [TCnControlHook, TCnActionListHook,
    TCnMenuHook]);
  RegisterComponents(SCnNonVisualPalette, [TCnFormScaler, TCnVolumeCtrl, TCnMDIBackGround]);
  RegisterComponents(SCnNonVisualPalette, [TCnActiveScriptSite, TCnActiveScriptWindow]);
  RegisterComponents(SCnNonVisualPalette, [TCnObjectPool, TCnThreadPool, TCnWinampCtrl]);
  RegisterComponents(SCnNonVisualPalette, [TCnRestoreSystemMenu, TCnConsole]);
  RegisterComponents(SCnNonVisualPalette, [TCnFileSystemWatcher]);
  RegisterComponents(SCnNonVisualPalette, [TCnDragResizer]);
  RegisterComponents(SCnNonVisualPalette, [TCnKeyBlocker]);
  RegisterComponents(SCnNonVisualPalette, [TCnFilePacker]);
  RegisterComponents(SCnNonVisualPalette, [TCnGlobalKeyHook, TCnOuterControls]);
  RegisterComponents(SCnNonVisualPalette, [TCnTaskBar, TCnRawKeyboard, TCnSystemDebugControl]);

  RegisterPropertyEditor(TypeInfo(TScriptLanguage), TCnActiveScriptSite, 'ScriptLanguage',
    TCnScriptLangProperty);

  // 注册 Dock 系列组件
  RegisterComponents(SCnNonVisualPalette, [TCnDockServer, TCnDockClient, 
    TCnDelphiDockStyle, TCnVCDockStyle, TCnVIDDockStyle, TCnVSNETDockStyle]);
  RegisterNoIcon([TCnVIDDockTabSheet]);
  RegisterClass(TCnVIDDockTabSheet);
    
  RegisterComponentEditor(TCnDockBaseControl, TCnDockControlEditor);
  RegisterComponentEditor(TCnBasicDockStyle, TCnDockStyleEditor);
  RegisterComponentEditor(TCnVIDTabPageControl, TCnVIDTabPageControlEditor);
  RegisterComponentEditor(TCnVIDDockTabSheet, TCnVIDTabPageControlEditor);
end;

end.
