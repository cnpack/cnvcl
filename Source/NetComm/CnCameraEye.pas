{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2006 CnPack 开发组                       }
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

unit CnCameraEye;

{* |<PRE>
================================================================================
* 软件名称：外接设备组件包
* 单元名称：实现摄像头控制单元
* 单元作者：rarnu(rarnu@cnpack.org)
* 备    注：暂未进行无摄像头的检测
* 开发平台：Windows2003 Server + Delphi2007 up2
* 兼容测试：Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.08.14 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Windows, Messages;

type
  TCnCameraEye = class(TComponent)
  private
    FDisplay: TWinControl;
    FOnStart: TNotifyEvent;
    FOnStartRecord: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnStopRecord: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    {* 开始摄像 }
    procedure Stop;
    {* 停止摄像 }
    procedure SaveToBmp(const FileName: string);
    {* 截图并保存到bmp }
    procedure RecordToAVI(const FileName: string);
    {* 录制AVI }
    procedure StopRecord;
    {* 停止录制 }
  published
    property Display: TWinControl read FDisplay write FDisplay;
    {* 图像显示容器 }
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    {* 开始摄像事件 }
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    {* 停止摄像事件 }
    property OnStartRecord: TNotifyEvent read FOnStartRecord write FOnStartRecord;
    {* 开始录像事件 }
    property OnStopRecord: TNotifyEvent read FOnStopRecord write FOnStopRecord;
    {* 停止录像事件 }
  end;

implementation

{* 消息常量声明 }

const
  WM_CAP_START = WM_USER;
  WM_CAP_STOP = WM_CAP_START + 68;
  WM_CAP_DRIVER_CONNECT = WM_CAP_START + 10;
  WM_CAP_DRIVER_DISCONNECT = WM_CAP_START + 11;
  WM_CAP_SAVEDIB = WM_CAP_START + 25;
  WM_CAP_GRAB_FRAME = WM_CAP_START + 60;
  WM_CAP_SEQUENCE = WM_CAP_START + 62;
  WM_CAP_FILE_SET_CAPTURE_FILEA = WM_CAP_START + 20;
  WM_CAP_SEQUENCE_NOFILE = WM_CAP_START + 63;
  WM_CAP_SET_OVERLAY = WM_CAP_START + 51;
  WM_CAP_SET_PREVIEW = WM_CAP_START + 50;
  WM_CAP_SET_CALLBACK_VIDEOSTREAM = WM_CAP_START + 6;
  WM_CAP_SET_CALLBACK_ERROR = WM_CAP_START + 2;
  WM_CAP_SET_CALLBACK_STATUSA = WM_CAP_START + 3;
  WM_CAP_SET_CALLBACK_FRAME = WM_CAP_START + 5;
  WM_CAP_SET_SCALE = WM_CAP_START + 53;
  WM_CAP_SET_PREVIEWRATE = WM_CAP_START + 52;

{* 声明动态函数，此函数从DLL中调入，动态判断是否可用 }
type
  TFunCap = function(
    lpszWindowName: PCHAR;
    dwStyle: longint;
    x: integer;
    y: integer;
    nWidth: integer;
    nHeight: integer;
    ParentWin: HWND;
    nId: integer): HWND; stdcall;

var
  hWndC: THandle;
  FunCap: TFunCap;
  DllHandle: THandle;

{ TCnCameraEye }

constructor TCnCameraEye.Create(AOwner: TComponent);
var
  FPointer: Pointer;
begin
  inherited Create(AOwner);
  FDisplay := nil;

  {* 通过DLL调入，如果DLL不存在，表示没有驱动 }
  DllHandle := LoadLibrary('AVICAP32.DLL');
  if DllHandle <= 0 then
    raise Exception.Create('Camera driver not installed or invalid.');

  FPointer := GetProcAddress(DllHandle, 'capCreateCaptureWindowA');
  FunCap := TFunCap(FPointer);
end;

destructor TCnCameraEye.Destroy;
begin
  StopRecord;
  Stop;
  FDisplay := nil;
  
  {* 如果已加载DLL，则释放掉 }
  if DllHandle > 0 then
    FreeLibrary(DllHandle);
  inherited;
end;

procedure TCnCameraEye.RecordToAVI(const FileName: string);
begin
  if hWndC <> 0 then
  begin
    SendMessage(hWndC, WM_CAP_FILE_SET_CAPTURE_FILEA, 0, LongInt(PAnsiChar(AnsiString(FileName))));
    SendMessage(hWndC, WM_CAP_SEQUENCE, 0, 0);
    if Assigned(FOnStartRecord) then
      FOnStartRecord(Self);
  end;
end;

procedure TCnCameraEye.SaveToBmp(const FileName: string);
begin
  if hWndC <> 0 then
    SendMessage(hWndC, WM_CAP_SAVEDIB, 0, LongInt(PAnsiChar(AnsiString(FileName))));
end;

procedure TCnCameraEye.Start;
var
  OHandle: THandle;
begin
  if FDisplay = nil then Exit;
  OHandle := TWinControl(Owner).Handle;
  hWndC := FunCap(
    'My Own Capture Window',
    WS_CHILD or WS_VISIBLE,
    FDisplay.Left, FDisplay.Top, FDisplay.Width, FDisplay.Height,
    OHandle, 0);

  if hWndC <> 0 then
  begin
    {* 发送指令 }
    SendMessage(hWndC, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0, 0);
    SendMessage(hWndC, WM_CAP_SET_CALLBACK_ERROR, 0, 0);
    SendMessage(hWndC, WM_CAP_SET_CALLBACK_STATUSA, 0, 0);
    SendMessage(hWndC, WM_CAP_DRIVER_CONNECT, 0, 0);
    SendMessage(hWndC, WM_CAP_SET_SCALE, 1, 0);
    SendMessage(hWndC, WM_CAP_SET_PREVIEWRATE, 66, 0);
    SendMessage(hWndC, WM_CAP_SET_OVERLAY, 1, 0);
    SendMessage(hWndC, WM_CAP_SET_PREVIEW, 1, 0);
  end;

  if Assigned(OnStart) then
    FOnStart(Self);
end;

procedure TCnCameraEye.Stop;
begin
  if hWndC <> 0 then
  begin
    SendMessage(hWndC, WM_CAP_DRIVER_DISCONNECT, 0, 0);
    hWndC := 0;
    if Assigned(FOnStop) then
      FOnStop(Self);
  end;
end;

procedure TCnCameraEye.StopRecord;
begin
  if hWndC <> 0 then
  begin
    SendMessage(hWndC, WM_CAP_STOP, 0, 0);
    if Assigned(FOnStopRecord) then
      FOnStopRecord(Self);
  end;
end;

end.
