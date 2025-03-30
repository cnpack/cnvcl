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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnCameraEye;
{* |<PRE>
================================================================================
* 软件名称：外接设备组件包
* 单元名称：VFW 实现摄像头控制单元
* 单元作者：rarnu(rarnu@cnpack.org)
* 备    注：暂未进行无摄像头的检测
* 开发平台：Windows2003 Server + Delphi2007 up2
* 兼容测试：Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.10.09 V1.1
*                增加一些错误码、状态等的声明，并增加出错异常处理
*           2008.08.14 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Windows, Messages, CnConsts, CnNetConsts,
  CnClasses;

type
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnCameraEye = class(TCnComponent)
  private
    FDllHandle: THandle;
    FDisplay: TWinControl;
    FOnStart: TNotifyEvent;
    FOnStartRecord: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnStopRecord: TNotifyEvent;
    procedure CheckRes(Res: LRESULT);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    {* 开始摄像}
    procedure Stop;
    {* 停止摄像}
    procedure SaveToBmp(const FileName: string);
    {* 截图并保存到 BMP}
    procedure RecordToAVI(const FileName: string);
    {* 录制 AVI}
    procedure StopRecord;
    {* 停止录制}

    procedure ShowVideoFormatDialog;
    {* 显示视频格式设置对话框}
    procedure ShowVideoSourceDialog;
    {* 显示视频来源设置对话框}
    procedure ShowVideoDisplayDialog;
    {* 显示视频显示设置对话框}
    procedure ShowVideoCompressionDialog;
    {* 显示视频压缩设置对话框}
  published
    property Display: TWinControl read FDisplay write FDisplay;
    {* 图像显示容器}
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    {* 开始摄像事件}
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    {* 停止摄像事件}
    property OnStartRecord: TNotifyEvent read FOnStartRecord write FOnStartRecord;
    {* 开始录像事件}
    property OnStopRecord: TNotifyEvent read FOnStopRecord write FOnStopRecord;
    {* 停止录像事件}
  end;

implementation

{* 消息常量声明 }

const
  WM_CAP_START = WM_USER;

  WM_CAP_GET_CAPSTREAMPTR = WM_CAP_START + 1;

  WM_CAP_SET_CALLBACK_ERROR = WM_CAP_START + 2;

  WM_CAP_SET_CALLBACK_STATUS = WM_CAP_START + 3;

  WM_CAP_SET_CALLBACK_YIELD = WM_CAP_START + 4;

  WM_CAP_SET_CALLBACK_FRAME = WM_CAP_START + 5;

  WM_CAP_SET_CALLBACK_VIDEOSTREAM = WM_CAP_START + 6;

  WM_CAP_SET_CALLBACK_WAVESTREAM = WM_CAP_START + 7;

  WM_CAP_GET_USER_DATA = WM_CAP_START + 8;

  WM_CAP_SET_USER_DATA = WM_CAP_START + 9;

  WM_CAP_DRIVER_CONNECT = WM_CAP_START + 10;

  WM_CAP_DRIVER_DISCONNECT = WM_CAP_START + 11;

  WM_CAP_DRIVER_GET_NAME = WM_CAP_START + 12;

  WM_CAP_DRIVER_GET_VERSION = WM_CAP_START + 13;

  WM_CAP_DRIVER_GET_CAPS = WM_CAP_START + 14;

  WM_CAP_FILE_SET_CAPTURE_FILE = WM_CAP_START + 20;

  WM_CAP_FILE_GET_CAPTURE_FILE = WM_CAP_START + 21;

  WM_CAP_FILE_ALLOCATE = WM_CAP_START + 22;

  WM_CAP_FILE_SAVEAS = WM_CAP_START + 23;

  WM_CAP_FILE_SET_INFOCHUNK = WM_CAP_START + 24;

  WM_CAP_FILE_SAVEDIB = WM_CAP_START + 25;

  WM_CAP_EDIT_COPY = WM_CAP_START + 30;

  WM_CAP_SET_AUDIOFORMAT = WM_CAP_START + 35;

  WM_CAP_GET_AUDIOFORMAT = WM_CAP_START + 36;

  WM_CAP_DLG_VIDEOFORMAT = WM_CAP_START + 41;

  WM_CAP_DLG_VIDEOSOURCE = WM_CAP_START + 42;

  WM_CAP_DLG_VIDEODISPLAY = WM_CAP_START + 43;

  WM_CAP_GET_VIDEOFORMAT = WM_CAP_START + 44;

  WM_CAP_SET_VIDEOFORMAT = WM_CAP_START + 45;

  WM_CAP_DLG_VIDEOCOMPRESSION = WM_CAP_START + 46;

  WM_CAP_SET_PREVIEW = WM_CAP_START + 50;

  WM_CAP_SET_OVERLAY = WM_CAP_START + 51;

  WM_CAP_SET_PREVIEWRATE = WM_CAP_START + 52;

  WM_CAP_SET_SCALE = WM_CAP_START + 53;

  WM_CAP_GET_STATUS = WM_CAP_START + 54;

  WM_CAP_SET_SCROLL = WM_CAP_START + 55;

  WM_CAP_GRAB_FRAME = WM_CAP_START + 60;

  WM_CAP_GRAB_FRAME_NOSTOP = WM_CAP_START + 61;

  WM_CAP_SEQUENCE = WM_CAP_START + 62;

  WM_CAP_SEQUENCE_NOFILE = WM_CAP_START + 63;

  WM_CAP_SET_SEQUENCE_SETUP = WM_CAP_START + 64;

  WM_CAP_GET_SEQUENCE_SETUP = WM_CAP_START + 65;

  WM_CAP_SET_MCI_DEVICE = WM_CAP_START + 66;

  WM_CAP_GET_MCI_DEVICE = WM_CAP_START + 67;

  WM_CAP_STOP = WM_CAP_START + 68;

  WM_CAP_ABORT = WM_CAP_START + 69;

  WM_CAP_SINGLE_FRAME_OPEN = WM_CAP_START + 70;

  WM_CAP_SINGLE_FRAME_CLOSE = WM_CAP_START + 71;

  WM_CAP_SINGLE_FRAME = WM_CAP_START + 72;

  WM_CAP_PAL_OPEN = WM_CAP_START + 80;

  WM_CAP_PAL_SAVE = WM_CAP_START + 81;

  WM_CAP_PAL_PASTE = WM_CAP_START + 82;

  WM_CAP_PAL_AUTOCREATE = WM_CAP_START + 83;

  WM_CAP_PAL_MANUALCREATE = WM_CAP_START + 84;

  // Following added post VFW 1.1
  WM_CAP_SET_CALLBACK_CAPCONTROL = WM_CAP_START + 85;

  // CallBackError 的错误码 ID
  IDS_CAP_BEGIN = 300;                { "Capture Start" }
  IDS_CAP_END = 301;                  { "Capture End" }
  IDS_CAP_INFO = 401;                 { "%s" }
  IDS_CAP_OUTOFMEM = 402;             { "Out of memory" }
  IDS_CAP_FILEEXISTS = 403;           { "File '%s' exists -- overwrite it?" }
  IDS_CAP_ERRORPALOPEN = 404;         { "Error opening palette '%s'" }
  IDS_CAP_ERRORPALSAVE = 405;         { "Error saving palette '%s'" }
  IDS_CAP_ERRORDIBSAVE = 406;         { "Error saving frame '%s'" }
  IDS_CAP_DEFAVIEXT = 407;            { "avi" }
  IDS_CAP_DEFPALEXT = 408;            { "pal" }
  IDS_CAP_CANTOPEN = 409;             { "Cannot open '%s'" }
  IDS_CAP_SEQ_MSGSTART = 410;         { "Select OK to start capture\nof video sequence\nto %s." }
  IDS_CAP_SEQ_MSGSTOP = 411;          { "Hit ESCAPE or click to end capture" }
  IDS_CAP_VIDEDITERR = 412;           { "An error occurred while trying to run VidEdit." }
  IDS_CAP_READONLYFILE = 413;         { "The file '%s' is a read-only file." }
  IDS_CAP_WRITEERROR = 414;           { "Unable to write to file '%s'.\nDisk may be full." }
  IDS_CAP_NODISKSPACE = 415;          { "There is no space to create a capture file on the specified device." }
  IDS_CAP_SETFILESIZE = 416;          { "Set File Size" }
  IDS_CAP_SAVEASPERCENT = 417;        { "SaveAs: %2ld%%  Hit Escape to abort." }
  IDS_CAP_DRIVER_ERROR = 418;         { Driver specific error message }
  IDS_CAP_WAVE_OPEN_ERROR = 419;      { "Error: Cannot open the wave input device.\nCheck sample size, frequency, and channels." }
  IDS_CAP_WAVE_ALLOC_ERROR = 420;     { "Error: Out of memory for wave buffers." }
  IDS_CAP_WAVE_PREPARE_ERROR = 421;   { "Error: Cannot prepare wave buffers." }
  IDS_CAP_WAVE_ADD_ERROR = 422;       { "Error: Cannot add wave buffers." }
  IDS_CAP_WAVE_SIZE_ERROR = 423;      { "Error: Bad wave size." }
  IDS_CAP_VIDEO_OPEN_ERROR = 424;     { "Error: Cannot open the video input device." }
  IDS_CAP_VIDEO_ALLOC_ERROR = 425;    { "Error: Out of memory for video buffers." }
  IDS_CAP_VIDEO_PREPARE_ERROR = 426;  { "Error: Cannot prepare video buffers." }
  IDS_CAP_VIDEO_ADD_ERROR = 427;      { "Error: Cannot add video buffers." }
  IDS_CAP_VIDEO_SIZE_ERROR = 428;     { "Error: Bad video size." }
  IDS_CAP_FILE_OPEN_ERROR = 429;      { "Error: Cannot open capture file." }
  IDS_CAP_FILE_WRITE_ERROR = 430;     { "Error: Cannot write to capture file.  Disk may be full." }
  IDS_CAP_RECORDING_ERROR = 431;      { "Error: Cannot write to capture file.  Data rate too high or disk full." }
  IDS_CAP_RECORDING_ERROR2 = 432;     { "Error while recording" }
  IDS_CAP_AVI_INIT_ERROR = 433;       { "Error: Unable to initialize for capture." }
  IDS_CAP_NO_FRAME_CAP_ERROR = 434;   { "Warning: No frames captured.\nConfirm that vertical sync SmallInterrupts\nare configured and enabled." }
  IDS_CAP_NO_PALETTE_WARN = 435;      { "Warning: Using default palette." }
  IDS_CAP_MCI_CONTROL_ERROR = 436;    { "Error: Unable to access MCI device." }
  IDS_CAP_MCI_CANT_STEP_ERROR = 437;  { "Error: Unable to step MCI device." }
  IDS_CAP_NO_AUDIO_CAP_ERROR = 438;   { "Error: No audio data captured.\nCheck audio card settings." }
  IDS_CAP_AVI_DRAWDIB_ERROR = 439;    { "Error: Unable to draw this data format." }
  IDS_CAP_COMPRESSOR_ERROR = 440;     { "Error: Unable to initialize compressor." }
  IDS_CAP_AUDIO_DROP_ERROR = 441;     { "Error: Audio data was lost during capture, reduce capture rate." }

  // CallBackStatus 的状态码
  IDS_CAP_STAT_LIVE_MODE = 500;       { "Live window" }
  IDS_CAP_STAT_OVERLAY_MODE = 501;    { "Overlay window" }
  IDS_CAP_STAT_CAP_INIT = 502;        { "Setting up for capture - Please wait" }
  IDS_CAP_STAT_CAP_FINI = 503;        { "Finished capture, now writing frame %ld" }
  IDS_CAP_STAT_PALETTE_BUILD = 504;   { "Building palette map" }
  IDS_CAP_STAT_OPTPAL_BUILD = 505;    { "Computing optimal palette" }
  IDS_CAP_STAT_I_FRAMES = 506;        { "%d frames" }
  IDS_CAP_STAT_L_FRAMES = 507;        { "%ld frames" }
  IDS_CAP_STAT_CAP_L_FRAMES = 508;    { "Captured %ld frames" }
  IDS_CAP_STAT_CAP_AUDIO = 509;       { "Capturing audio" }
  IDS_CAP_STAT_VIDEOCURRENT = 510;    { "Captured %ld frames (%ld dropped) %d.%03d sec." }
  IDS_CAP_STAT_VIDEOAUDIO = 511;      { "Captured %d.%03d sec.  %ld frames (%ld dropped) (%d.%03d fps).  %ld audio bytes (%d,%03d sps)" }
  IDS_CAP_STAT_VIDEOONLY = 512;       { "Captured %d.%03d sec.  %ld frames (%ld dropped) (%d.%03d fps)" }
  IDS_CAP_STAT_FRAMESDROPPED = 513;   { "Dropped %ld of %ld frames (%d.%02d%%) during capture." }

{* 声明动态函数，此函数从 DLL 中调入，动态判断是否可用 }
type
  TCapCreateCaptureWindow = function(
    lpszWindowName: PChar;
    dwStyle: Longint;
    x: Integer;
    y: Integer;
    nWidth: Integer;
    nHeight: Integer;
    ParentWin: HWND;
    nId: Integer): HWND; stdcall;

var
  FHWndC: THandle;
  FunCap: TCapCreateCaptureWindow;

procedure CapErrorCallback(hWnd: THandle; ID: Integer; Lpsz: LPCSTR); stdcall;
begin
  raise Exception.CreateFmt('ERROR Callback %d: %s', [ID, Lpsz]);
end;

{ TCnCameraEye }

procedure TCnCameraEye.CheckRes(Res: LRESULT);
begin
//  if Res = 0 then
//    raise Exception.Create('ERROR SendMessage');
end;

constructor TCnCameraEye.Create(AOwner: TComponent);
var
  FPointer: Pointer;
begin
  inherited Create(AOwner);
  FDisplay := nil;

  {* 通过 DLL 调入，如果 DLL 不存在，表示没有驱动 }
  FDllHandle := LoadLibrary('AVICAP32.DLL');
  if FDllHandle <= 0 then
    raise Exception.Create('Camera driver not installed or invalid.');

//{$IFDEF UNICODE}
//  FPointer := GetProcAddress(FDllHandle, 'capCreateCaptureWindowW');
//{$ELSE}
  FPointer := GetProcAddress(FDllHandle, 'capCreateCaptureWindowA');
//{$ENDIF}
  FunCap := TCapCreateCaptureWindow(FPointer);
end;

destructor TCnCameraEye.Destroy;
begin
  StopRecord;
  Stop;
  FDisplay := nil;
  
  {* 如果已加载 DLL，则释放掉 }
  if FDllHandle > 0 then
    FreeLibrary(FDllHandle);
  inherited;
end;

procedure TCnCameraEye.RecordToAVI(const FileName: string);
begin
  if FHWndC <> 0 then
  begin
    CheckRes(SendMessage(FHWndC, WM_CAP_FILE_SET_CAPTURE_FILE, 0, LongInt(PAnsiChar(AnsiString(FileName)))));
    CheckRes(SendMessage(FHWndC, WM_CAP_SEQUENCE, 0, 0));
    if Assigned(FOnStartRecord) then
      FOnStartRecord(Self);
  end;
end;

procedure TCnCameraEye.SaveToBmp(const FileName: string);
begin
  if FHWndC <> 0 then
    CheckRes(SendMessage(FHWndC, WM_CAP_FILE_SAVEDIB, 0, LongInt(PAnsiChar(AnsiString(FileName)))));
end;

procedure TCnCameraEye.ShowVideoCompressionDialog;
begin
  if FHWndC <> 0 then
    CheckRes(SendMessage(FHWndC, WM_CAP_DLG_VIDEOCOMPRESSION, 0, 0));
end;

procedure TCnCameraEye.ShowVideoDisplayDialog;
begin
  if FHWndC <> 0 then
    CheckRes(SendMessage(FHWndC, WM_CAP_DLG_VIDEODISPLAY, 0, 0));
end;

procedure TCnCameraEye.ShowVideoFormatDialog;
begin
  if FHWndC <> 0 then
    CheckRes(SendMessage(FHWndC, WM_CAP_DLG_VIDEOFORMAT, 0, 0));
end;

procedure TCnCameraEye.ShowVideoSourceDialog;
begin
  if FHWndC <> 0 then
    CheckRes(SendMessage(FHWndC, WM_CAP_DLG_VIDEOSOURCE, 0, 0));
end;

procedure TCnCameraEye.Start;
var
  OHandle: THandle;
begin
  if FDisplay = nil then
    Exit;

  OHandle := TWinControl(Owner).Handle;
  FHWndC := FunCap(
    'CnPack Capture Window',
    WS_CHILD or WS_VISIBLE,
    FDisplay.Left, FDisplay.Top, FDisplay.Width, FDisplay.Height,
    OHandle, 0);

  if FHWndC <> 0 then
  begin
    {* 发送指令 }
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0, 0));
{$IFDEF WIN64}
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_CALLBACK_ERROR, 0, NativeInt(@CapErrorCallback)));
{$ELSE}
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_CALLBACK_ERROR, 0, Integer(@CapErrorCallback)));
{$ENDIF}
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_CALLBACK_STATUS, 0, 0));
    CheckRes(SendMessage(FHWndC, WM_CAP_DRIVER_CONNECT, 0, 0));
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_SCALE, 1, 0));
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_PREVIEWRATE, 66, 0));
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_OVERLAY, 1, 0));
    CheckRes(SendMessage(FHWndC, WM_CAP_SET_PREVIEW, 1, 0));
  end;

  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TCnCameraEye.Stop;
begin
  if FHWndC <> 0 then
  begin
    CheckRes(SendMessage(FHWndC, WM_CAP_DRIVER_DISCONNECT, 0, 0));
    FHWndC := 0;
    if Assigned(FOnStop) then
      FOnStop(Self);
  end;
end;

procedure TCnCameraEye.StopRecord;
begin
  if FHWndC <> 0 then
  begin
    CheckRes(SendMessage(FHWndC, WM_CAP_STOP, 0, 0));
    if Assigned(FOnStopRecord) then
      FOnStopRecord(Self);
  end;
end;

procedure TCnCameraEye.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnCameraEyeName;
  Author := SCnPack_rarnu;
  Email := SCnPack_rarnuEmail;
  Comment := SCnCameraEyeComment;
end;

end.
