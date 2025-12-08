{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnVolumeCtrl;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：音量控制器组件TCnVolumeCtrl单元
* 单元作者：小冬 (kendling@21cn.com)
* 备    注：- 音量控制组件，用于控制系统音量，支持多设备多线路。
* 开发平台：PWin2003 + Delphi 7.0 (Build 8.1)
* 兼容测试：PWin2003 + Delphi 7.0 (Build 8.1)
* 本 地 化：该单元中有字符串资源
* 修改记录：2025.12.05 v1.3
*               修复Win7/10/11下无法控制系统音量的问题
*               增加对Windows Core Audio API的支持
*           2025.12.05 v1.2
*               修正 64 位下的不兼容问题
*           2005.12.22 v1.1
*               修正未响应关机事件的问题
*           2005.09.23 v1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Messages, Windows, Forms, MMSystem,
  CnClasses, CnConsts, CnCompConsts, ActiveX, ComObj;

const
  CLSID_MMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  IID_IMMDeviceEnumerator: TGUID = '{A95664D2-9614-4F35-A746-DE8DB63617E6}';
  IID_IAudioEndpointVolume: TGUID = '{5CDF2C82-841E-4546-9722-0CF74078229A}';
  DEVICE_STATE_ACTIVE = $00000001;

type
  EDataFlow = TOleEnum;
  ERole = TOleEnum;

  TPropertyKey = record
    fmtid: TGUID;
    pid: DWORD;
  end;

  IAudioEndpointVolumeCallback = interface(IUnknown)
    ['{657804FA-D6AD-4496-8A60-352752AF4F89}']
    function OnNotify(pNotify: Pointer): HResult; stdcall;
  end;

  IAudioEndpointVolume = interface(IUnknown)
    ['{5CDF2C82-841E-4546-9722-0CF74078229A}']
    function RegisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HResult; stdcall;
    function UnregisterControlChangeNotify(pNotify: IAudioEndpointVolumeCallback): HResult; stdcall;
    function GetChannelCount(out pnChannelCount: UINT): HResult; stdcall;
    function SetMasterVolumeLevel(fLevelDB: Single; pguidEventContext: PGUID): HResult; stdcall;
    function SetMasterVolumeLevelScalar(fLevel: Single; pguidEventContext: PGUID): HResult; stdcall;
    function GetMasterVolumeLevel(out pfLevelDB: Single): HResult; stdcall;
    function GetMasterVolumeLevelScalar(out pfLevel: Single): HResult; stdcall;
    function SetChannelVolumeLevel(nChannel: UINT; fLevelDB: Single; pguidEventContext: PGUID): HResult; stdcall;
    function SetChannelVolumeLevelScalar(nChannel: UINT; fLevel: Single; pguidEventContext: PGUID): HResult; stdcall;
    function GetChannelVolumeLevel(nChannel: UINT; out pfLevelDB: Single): HResult; stdcall;
    function GetChannelVolumeLevelScalar(nChannel: UINT; out pfLevel: Single): HResult; stdcall;
    function SetMute(bMute: Cardinal; pguidEventContext: PGUID): HResult; stdcall;
    function GetMute(out pbMute: Cardinal): HResult; stdcall;
    function GetVolumeStepInfo(out pnStep: UINT; out pnStepCount: UINT): HResult; stdcall;
    function VolumeStepUp(pguidEventContext: PGUID): HResult; stdcall;
    function VolumeStepDown(pguidEventContext: PGUID): HResult; stdcall;
    function QueryHardwareSupport(out pdwHardwareSupportMask: DWORD): HResult; stdcall;
    function GetVolumeRange(out pflVolumeMindB: Single; out pflVolumeMaxdB: Single; out pflVolumeIncrementdB: Single): HResult; stdcall;
  end;

  IPropertyStore = interface(IUnknown)
    ['{886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99}']
    function GetCount(out cProps: DWORD): HResult; stdcall;
    function GetAt(iProp: DWORD; out pKey: TPropertyKey): HResult; stdcall;
    function GetValue(const key: TPropertyKey; out pv: TPropVariant): HResult; stdcall;
    function SetValue(const key: TPropertyKey; const propvar: TPropVariant): HResult; stdcall;
    function Commit: HResult; stdcall;
  end;

  IMMNotificationClient = interface(IUnknown)
    ['{7991EEC9-7E89-4D85-8390-6C703CEC60C0}']
    function OnDeviceStateChanged(pwstrDeviceId: PWideChar; dwNewState: DWORD): HResult; stdcall;
    function OnDeviceAdded(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: PWideChar): HResult; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: PWideChar): HResult; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: PWideChar; const key: TPropertyKey): HResult; stdcall;
  end;

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD; pActivationParams: PPropVariant; out ppInterface: Pointer): HResult; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD; out ppProperties: IPropertyStore): HResult; stdcall;
    function GetId(out ppstrId: PWideChar): HResult; stdcall;
    function GetState(out pdwState: DWORD): HResult; stdcall;
  end;

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out pcDevices: UINT): HResult; stdcall;
    function Item(nDevice: UINT; out ppDevice: IMMDevice): HResult; stdcall;
  end;

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: EDataFlow; dwStateMask: DWORD; out ppDevices: IMMDeviceCollection): HResult; stdcall;
    function GetDefaultAudioEndpoint(dataFlow: EDataFlow; role: ERole; out ppEndpoint: IMMDevice): HResult; stdcall;
    function GetDevice(pwstrId: PWideChar; out ppDevice: IMMDevice): HResult; stdcall;
    function RegisterEndpointNotificationCallback(pClient: IMMNotificationClient): HResult; stdcall;
    function UnregisterEndpointNotificationCallback(pClient: IMMNotificationClient): HResult; stdcall;
  end;

  DVOLUME = array[0..1] of DWORD;

  ECnMixException = class(Exception);

  TCnBalance  = -32..32;
  TCnVolume   = 0..255;

  // 操作类型
  TCnMixOperateType = (motGetVol, motSetVol, motGetMute, motSetMute);

  // 事件
  TCnMixVolumeEvent = procedure(Sender: TObject; Volume: TCnVolume; Balance: TCnBalance) of object;

  TCnMixMuteEvent = procedure(Sender: TObject; bMute: Boolean) of object;

  TCnCustomVolumeCtrl = class(TCnComponent)
  {* 音量控制器组件基类}
  private
    Fmx: HMIXER;             // 音频句柄
    FDevice: UINT;           // 当前设备
    FLine: UINT;             // 当前线路
    FCurVolume: DVOLUME;     // 当前线路左右音量
    FCurVol: DWORD;          // 当前线路最高音量，是系统音量乘了 256
    FCurMute: Boolean;       // 当前线路静音状态
    FCurBalance: TCnBalance; // 当前线路左右平行
    FVolume: DVOLUME;
    FVol: DWORD;
    FBalance: TCnBalance;

    FWnd: HWND;
    FLineID: DWORD;
    FControlID: DWORD;
    FOnVolumeChange: TCnMixVolumeEvent;
    FOnMuteChange: TCnMixMuteEvent;

    // Core Audio API 支持
    FCoreAudioSupported: Boolean;
    FEndpointVolume: IAudioEndpointVolume;
    FEndpointCallback: IAudioEndpointVolumeCallback;
    FUseCoreAudio: Boolean;
    FDeviceEnumerator: IMMDeviceEnumerator;
    FDefaultDevice: IMMDevice;

    FDeviceList: TStringList;
    FDeviceIdList: TStringList;

    function mGetMxHandle(uDev: UINT): HMIXER;
    function mGetLineInfo(Hmx: HMIXER; var Mxl: TMixerLine; uInfo: UINT): Boolean;
    function mGetLineControls(Hmx: HMIXER; var Mxlc: TMixerLineControls;
      uInfo: UINT): Boolean;
    function mGetControlDetails(Hmx: HMIXER; var Mxcd: TMixerControlDetails): Boolean;
    function mSetControlDetails(Hmx: HMIXER; var Mxcd: TMixerControlDetails): Boolean;
    function CheckDevice(uDev: UINT): Boolean;
    function CheckLine(uDev, uLine: UINT): Boolean;
    procedure WinProc(var message: TMessage); // 传统 Mix 方式的改变消息通知

    // Core Audio API 方法
    procedure InitializeCoreAudio;
    procedure InitializeCoreAudioForWindowsVistaPlus;
    function GetVolumeViaCoreAudio: TCnVolume;
    function GetMuteViaCoreAudio: Boolean;
    procedure SetVolumeViaCoreAudio(Volume: TCnVolume);
    procedure SetMuteViaCoreAudio(bMute: Boolean);
    function GetBalanceViaCoreAudio: TCnBalance;
    procedure SetBalanceViaCoreAudio(iBalance: TCnBalance);

    procedure SetDev(const Value: UINT);
    procedure SetLine(const Value: UINT);
    function GetDevs: UINT;
    function GetLines: UINT;
    function GetDevCap: string;
    function GetLineCap: string;
    function GetBalance: TCnBalance;
    function GetIsMute: Boolean;
    function GetVolume: Integer;
    procedure SetBalance(const Value: TCnBalance);
    procedure SetIsMute(const Value: Boolean);
    procedure SetVolume(const Value: Integer);
    function GetChannels: DWORD;

    function IsWindowsVistaOrGreater: Boolean;
    procedure HandleCoreAudioNotification(pNotify: Pointer);

    procedure RefreshDeviceList;
    function GetDeviceCount: Integer;
    function GetDeviceName(Index: Integer): string;
    function GetDeviceId(Index: Integer): string;
    procedure SetActiveDeviceById(const DeviceId: string);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}

    // 获取参数
    function GetDevCaption(uDev: UINT): string;
    {* 获取设备名称}
    function GetDevLines(uDev: UINT): DWORD;
    {* 获取设备线路总数}
    function GetLineCaption(uDev, uLine: UINT): string;
    {* 获取线路名称}
    function GetLineChannels(uDev, uLine: UINT): DWORD;
    {* 获取线路通道总数}
    function GetLineHaveBalance(uDev, uLine: UINT): Boolean;
    {* 获取线路是否存在左右平行}
    function GetLineVolume(uDev, uLine: UINT): TCnVolume;
    {* 获取线路音量}
    function GetLineBalance(uDev, uLine: UINT): TCnBalance;
    {* 获取线路静音状态}
    function GetLineMute(uDev, uLine: UINT): Boolean;
    {* 获取线路静音状态}

    // 设置参数
    function SetLineVolume(uDev, uLine: UINT; Volume: TCnVolume): Boolean;
    {* 设置线路音量}
    function SetLineBalance(uDev, uLine: UINT; iBalance: TCnBalance): Boolean;
    {* 设置线路左右平衡}
    function SetLineMute(uDev, uLine: UINT; bMute: Boolean): Boolean;
    {* 设置线路静音}

    // 属性
    property Devs: UINT read GetDevs;
    {* 获取设备总数}
    property CurDev: UINT read FDevice write SetDev;
    {* 设置/获取当前设备}
    property CurDevCaption: string read GetDevCap;
    {* 获取当前设备名称}
    property Lines: UINT read GetLines;
    {* 获取当前设备的线路总数}
    property CurLine: UINT read FLine write SetLine;
    {* 设置/获取当前线路}
    property CurLineCaption: string read GetLineCap;
    {* 获取当前线路名称}
    property CurLineChannels: DWORD read GetChannels;
    {* 获取当前线路通道总数}

    property Volume: Integer read GetVolume write SetVolume;
    {* 设置/获取当前线路音量}
    property Balance: TCnBalance read GetBalance write SetBalance;
    {* 设置/获取当前线路左右平衡}
    property IsMute: Boolean read GetIsMute write SetIsMute;
    {* 设置/获取当前线路静音状态}

    property DeviceCount: Integer read GetDeviceCount;
    property DeviceNames[Index: Integer]: string read GetDeviceName;
    property DeviceIds[Index: Integer]: string read GetDeviceId;
    procedure SetActiveDevice(Index: Integer);

    property OnVolumeChange: TCnMixVolumeEvent read FOnVolumeChange write FOnVolumeChange;
    {* 音量改变事件}
    property OnMuteChange: TCnMixMuteEvent read FOnMuteChange write FOnMuteChange;
    {* 静音状态改变事件}
  end;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnVolumeCtrl = class(TCnCustomVolumeCtrl)
  {* 音量控制器组件}
  published
    property Devs;
    property CurDev;
    property CurDevCaption;
    property Lines;
    property CurLine;
    property CurLineCaption;

    property Volume;
    property Balance;
    property IsMute;

    property OnVolumeChange;
    property OnMuteChange;
  end;

implementation

const
  PKEY_Device_FriendlyName: TPropertyKey = (
    fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: 14);

  eRender = $00000000;
  eCapture = $00000001;
  eAll = $00000002;
  eConsole = $00000000;
  eMultimedia = $00000001;
  eCommunications = $00000002;

type
  // 定义 AUDIO_VOLUME_NOTIFICATION_DATA 结构，用于接收通知数据
  PAudioVolumeNotificationData = ^TAudioVolumeNotificationData;
  TAudioVolumeNotificationData = record
    guidEventContext: TGUID;
    bMuted: BOOL;
    fMasterVolume: Single;
    nChannels: UINT;
    afChannelVolumes: array[0..15] of Single; // 可变数组
  end;

  TEndpointVolumeCallback = class(TInterfacedObject, IAudioEndpointVolumeCallback)
  private
    FOwner: TObject;
  public
    constructor Create(AOwner: TObject);
    function OnNotify(pNotify: Pointer): HResult; stdcall;
  end;

function PropVariantClear(var pvar: PROPVARIANT): HRESULT; stdcall; external 'ole32.dll' name 'PropVariantClear';

{ TEndpointVolumeCallback }

constructor TEndpointVolumeCallback.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TEndpointVolumeCallback.OnNotify(pNotify: Pointer): HResult;
begin
  // 处理 Core Audio 的音量变化通知
  if FOwner is TCnCustomVolumeCtrl then
    TCnCustomVolumeCtrl(FOwner).HandleCoreAudioNotification(pNotify);

  Result := S_OK;
end;

{ TCnCustomVolumeCtrl }

procedure TCnCustomVolumeCtrl.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnVolumnCtrlName;
  Author := SCnPack_Kendling + ';' + SCnPack_SuperYoyoNc;
  Email := SCnPack_KendlingEmail + ';' + SCnPack_SuperYoyoNcEmail;
  Comment := SCnVolumnCtrlComment;
end;

procedure TCnCustomVolumeCtrl.HandleCoreAudioNotification(pNotify: Pointer);
var
  NotificationData: PAudioVolumeNotificationData;
  OldMute: Boolean;
  OldVolume: TCnVolume;
  OldBalance: TCnBalance;
  NewVolume: TCnVolume;
  NewMute: Boolean;
  NewBalance: TCnBalance;
begin
  if not Assigned(pNotify) then
    Exit;

  NotificationData := PAudioVolumeNotificationData(pNotify);

  // 保存旧值
  OldMute := FCurMute;
  OldVolume := FCurVol div 256;
  OldBalance := FCurBalance;

  // 更新当前状态
  NewMute := Boolean(NotificationData^.bMuted);
  NewVolume := Round(NotificationData^.fMasterVolume * 255);
  if NewVolume > 255 then NewVolume := 255;
  if NewVolume < 0 then NewVolume := 0;

  // 计算新的平衡（如果有多个声道）
  NewBalance := 0;
  if NotificationData^.nChannels >= 2 then
  begin
    if (NotificationData^.afChannelVolumes[0] = 0) and
       (NotificationData^.afChannelVolumes[1] = 0) then
      NewBalance := 0
    else if NotificationData^.afChannelVolumes[0] = NotificationData^.afChannelVolumes[1] then
      NewBalance := 0
    else if NotificationData^.afChannelVolumes[0] = 0 then
      NewBalance := 32
    else if NotificationData^.afChannelVolumes[1] = 0 then
      NewBalance := -32
    else if NotificationData^.afChannelVolumes[0] > NotificationData^.afChannelVolumes[1] then
      NewBalance := -Round((NotificationData^.afChannelVolumes[0] - NotificationData^.afChannelVolumes[1]) * 32)
    else
      NewBalance := Round((NotificationData^.afChannelVolumes[1] - NotificationData^.afChannelVolumes[0]) * 32);
  end;

  // 更新内部状态
  FCurMute := NewMute;
  FCurVol := NewVolume * 256;
  FCurVolume[0] := FCurVol;
  FCurVolume[1] := FCurVol;
  FCurBalance := NewBalance;

  // 触发事件（如果有变化）
  if Assigned(FOnMuteChange) and (OldMute <> NewMute) then
    FOnMuteChange(Self, NewMute);

  if Assigned(FOnVolumeChange) and
     ((OldVolume <> NewVolume) or (OldBalance <> NewBalance)) then
    FOnVolumeChange(Self, NewVolume, NewBalance);
end;

function TCnCustomVolumeCtrl.IsWindowsVistaOrGreater: Boolean;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(OSVersionInfo);
  Result := OSVersionInfo.dwMajorVersion >= 6; // Vista 版本号是 6.0
end;

procedure TCnCustomVolumeCtrl.InitializeCoreAudio;
var
  TempEndpointVolume: Pointer;
begin
  FCoreAudioSupported := False;
  FEndpointVolume := nil;
  FDeviceEnumerator := nil;
  FDefaultDevice := nil;

  try
    // 尝试创建 MMDeviceEnumerator
    if CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL,
      IID_IMMDeviceEnumerator, FDeviceEnumerator) = S_OK then
    begin
      // 获取默认音频端点
      if FDeviceEnumerator.GetDefaultAudioEndpoint(eRender, eConsole, FDefaultDevice) = S_OK then
      begin
        TempEndpointVolume := nil;

        // 激活 IAudioEndpointVolume 接口
        if FDefaultDevice.Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, nil, TempEndpointVolume) = S_OK then
        begin
          FEndpointVolume := IAudioEndpointVolume(TempEndpointVolume);
          FCoreAudioSupported := True;

          if Assigned(FEndpointVolume) then
          begin
            // 注册回调
            FEndpointCallback := TEndpointVolumeCallback.Create(Self);
            FEndpointVolume.RegisterControlChangeNotify(FEndpointCallback);
          end;
        end;
      end;
    end;
  except
    FCoreAudioSupported := False;
  end;

  // 如果是 Windows Vista 及以上版本，优先使用 Core Audio API
  FUseCoreAudio := FCoreAudioSupported and IsWindowsVistaOrGreater;
end;

procedure TCnCustomVolumeCtrl.InitializeCoreAudioForWindowsVistaPlus;
begin
  if not IsWindowsVistaOrGreater then
    Exit;

  // 对于 Windows Vista 及更高版本，尝试初始化 Core Audio API
  InitializeCoreAudio;
end;

constructor TCnCustomVolumeCtrl.Create(AOwner: TComponent);
begin
  inherited;
  Fmx := 0;
  FDevice := 0;
  FLine := 0;
  FCurVolume[0] := 0;
  FCurVolume[1] := 0;
  FCurVol := 0;
  FCurBalance := 0;
  FCurMute := False;

  FVolume[0] := 0;
  FVolume[1] := 0;
  FVol := 0;

  FWnd := 0;
  FControlID := 0;
  FOnVolumeChange := nil;
  FOnMuteChange := nil;

  FCoreAudioSupported := False;
  FUseCoreAudio := False;

  // 初始化 COM（Core Audio需要）
  CoInitialize(nil);

  // 对于 Windows Vista 及以上版本，尝试使用 Core Audio API
  InitializeCoreAudioForWindowsVistaPlus;

  if not FUseCoreAudio then
  begin
    // 回退到旧的 Mixer API
    if csDesigning in ComponentState then
    begin
      if mixerOpen(@Fmx, FDevice, 0, 0, CALLBACK_NULL) <> MMSYSERR_NOERROR then
        raise ECnMixException.Create(SCnMixerOpenError);
    end
    else
    begin
      if FWnd = 0 then
        FWnd := AllocateHWnd(WinProc);

      if mixerOpen(@Fmx, FDevice, FWnd, 0, CALLBACK_WINDOW) <> MMSYSERR_NOERROR then
        raise ECnMixException.Create(SCnMixerOpenError);
    end;
    SetDev(0);
  end;

  GetIsMute; // 初始化静音值
end;

destructor TCnCustomVolumeCtrl.Destroy;
begin
  FDeviceList.Free;
  FDeviceIdList.Free;

  // 清理 Core Audio 资源
  if FCoreAudioSupported then
  begin
    if Assigned(FEndpointVolume) and Assigned(FEndpointCallback) then
    begin
      FEndpointVolume.UnregisterControlChangeNotify(FEndpointCallback);
    end;
    FEndpointCallback := nil;
    FEndpointVolume := nil;
    FDefaultDevice := nil;
    FDeviceEnumerator := nil;
  end;

  // 清理 Mixer API 资源
  if Fmx <> 0 then
  begin
    mixerClose(Fmx);
    Fmx := 0;
  end;
  if FWnd <> 0 then
  begin
    DeallocateHWnd(FWnd);
    FWnd := 0;
  end;

  // 清理 COM
  CoUninitialize;

  inherited;
end;

procedure TCnCustomVolumeCtrl.Loaded;
begin
  inherited;
  // 组件加载完成后，重新初始化 Core Audio（如果需要）
  if not (csDesigning in ComponentState) and IsWindowsVistaOrGreater then
    InitializeCoreAudioForWindowsVistaPlus;
end;

function TCnCustomVolumeCtrl.GetVolumeViaCoreAudio: TCnVolume;
var
  VolumeLevel: Single;
begin
  Result := 0;
  if FCoreAudioSupported and Assigned(FEndpointVolume) then
  begin
    if FEndpointVolume.GetMasterVolumeLevelScalar(VolumeLevel) = S_OK then
    begin
      Result := Round(VolumeLevel * 255);
    end;
  end;
end;

function TCnCustomVolumeCtrl.GetMuteViaCoreAudio: Boolean;
var
  bMute: Cardinal;
begin
  Result := False;
  if FCoreAudioSupported and Assigned(FEndpointVolume) then
  begin
    if FEndpointVolume.GetMute(bMute) = S_OK then
      Result := bMute <> 0;
  end;
end;

procedure TCnCustomVolumeCtrl.SetVolumeViaCoreAudio(Volume: TCnVolume);
var
  VolumeLevel: Single;
begin
  if FCoreAudioSupported and Assigned(FEndpointVolume) then
  begin
    VolumeLevel := Volume / 255.0;
    if VolumeLevel < 0.0 then VolumeLevel := 0.0;
    if VolumeLevel > 1.0 then VolumeLevel := 1.0;
    FEndpointVolume.SetMasterVolumeLevelScalar(VolumeLevel, nil);
  end;
end;

procedure TCnCustomVolumeCtrl.SetMuteViaCoreAudio(bMute: Boolean);
var
  HR: HRESULT;
begin
  if FCoreAudioSupported and Assigned(FEndpointVolume) then
  begin
    if bMute then
      HR := FEndpointVolume.SetMute(1, nil)
    else
      HR := FEndpointVolume.SetMute(0, nil);

    // 这里不给 FCurMute 赋值，待改变回调做
    // if HR = S_OK then
    //  FCurMute := bMute;
  end;
end;

function TCnCustomVolumeCtrl.GetBalanceViaCoreAudio: TCnBalance;
var
  ChannelCount: UINT;
  LeftLevel, RightLevel: Single;
begin
  Result := 0;
  if FCoreAudioSupported and Assigned(FEndpointVolume) then
  begin
    if FEndpointVolume.GetChannelCount(ChannelCount) = S_OK then
    begin
      if ChannelCount >= 2 then
      begin
        if (FEndpointVolume.GetChannelVolumeLevelScalar(0, LeftLevel) = S_OK) and
           (FEndpointVolume.GetChannelVolumeLevelScalar(1, RightLevel) = S_OK) then
        begin
          if (LeftLevel = 0) and (RightLevel = 0) then
            Result := 0
          else if LeftLevel = RightLevel then
            Result := 0
          else if LeftLevel = 0 then
            Result := 32
          else if RightLevel = 0 then
            Result := -32
          else if LeftLevel > RightLevel then
            Result := Trunc(-((LeftLevel - RightLevel) * 32))
          else
            Result := Trunc((RightLevel - LeftLevel) * 32);

          if Result > 32 then Result := 32;
          if Result < -32 then Result := -32;
        end;
      end;
    end;
  end;
end;

procedure TCnCustomVolumeCtrl.SetBalanceViaCoreAudio(iBalance: TCnBalance);
var
  ChannelCount: UINT;
  LeftLevel, RightLevel: Single;
  MainVolume: TCnVolume;
begin
  if FCoreAudioSupported and Assigned(FEndpointVolume) then
  begin
    if FEndpointVolume.GetChannelCount(ChannelCount) = S_OK then
    begin
      if ChannelCount >= 2 then
      begin
        MainVolume := GetVolumeViaCoreAudio;
        LeftLevel := MainVolume / 255.0;
        RightLevel := MainVolume / 255.0;

        if iBalance > 0 then
        begin
          // 右声道更大
          LeftLevel := LeftLevel * (1.0 - iBalance / 32.0);
          if LeftLevel < 0.0 then LeftLevel := 0.0;
        end
        else if iBalance < 0 then
        begin
          // 左声道更大
          RightLevel := RightLevel * (1.0 + iBalance / 32.0);
          if RightLevel < 0.0 then RightLevel := 0.0;
        end;

        FEndpointVolume.SetChannelVolumeLevelScalar(0, LeftLevel, nil);
        FEndpointVolume.SetChannelVolumeLevelScalar(1, RightLevel, nil);
      end;
    end;
  end;
end;

function TCnCustomVolumeCtrl.GetDevCap: string;
begin
  Result := GetDevCaption(FDevice);
end;

function TCnCustomVolumeCtrl.GetLineCap: string;
begin
  Result := GetLineCaption(FDevice, FLine);
end;

function TCnCustomVolumeCtrl.GetDevs: UINT;
begin
  if FUseCoreAudio then
  begin
    RefreshDeviceList; // 刷新设备列表
    Result := GetDeviceCount;
    if Result = 0 then
      Result := 1; // 至少有一个默认设备
  end
  else
  begin
    Result := mixerGetNumDevs;
  end;
end;

function TCnCustomVolumeCtrl.GetLines: UINT;
begin
  if FUseCoreAudio then
  begin
    // Core Audio API 中，我们只处理主音量线路
    Result := 1;
  end
  else
  begin
    Result := GetDevLines(FDevice);
  end;
end;

procedure TCnCustomVolumeCtrl.SetDev(const Value: UINT);
begin
  if csLoading in ComponentState then
    Exit;

  if FUseCoreAudio then
  begin
    // Core Audio API只支持默认设备
    FDevice := 0;
    Exit;
  end;

  if not CheckDevice(Value) then
    Exit;

  FDevice := Value;
  if Fmx <> 0 then
    mixerClose(Fmx);

  if csDesigning in ComponentState then
  begin
    if mixerOpen(@Fmx, FDevice, 0, 0, CALLBACK_NULL) <> MMSYSERR_NOERROR then
      raise ECnMixException.Create(SCnMixerOpenError);
  end
  else
  begin
    if FWnd = 0 then
      FWnd := AllocateHWnd(WinProc);

    if mixerOpen(@Fmx, FDevice, FWnd, 0, CALLBACK_WINDOW) <> MMSYSERR_NOERROR then
      raise ECnMixException.Create(SCnMixerOpenError);
  end;
  SetLine(0);
end;

procedure TCnCustomVolumeCtrl.SetLine(const Value: UINT);
var
  Mxl: TMixerLine;
  Mxlc: TMixerLineControls;
  Mxc: TMixerControl;
  uInfo: UINT;
begin
  if csLoading in ComponentState then
    Exit;

  if FUseCoreAudio then
  begin
    // Core Audio API只支持主音量线路
    FLine := 0;
    Exit;
  end;

  if not CheckLine(FDevice, Value) then
    Exit;

  FLine := Value;

  // 获取 ID，用于事件
  FillChar(Mxl, SizeOf(Mxl), 0);
  with Mxl do
  begin
    cbStruct := SizeOf(Mxl);
    dwDestination := 0;
    if FLine = 0 then
      uInfo := MIXER_GETLINEINFOF_DESTINATION
    else
    begin
      dwSource := FLine - 1;
      uInfo := MIXER_GETLINEINFOF_SOURCE;
    end;
  end;
  if not mGetLineInfo(Fmx, Mxl, uInfo) then
    Exit;

  FLineID := Mxl.dwLineID;

  FillChar(Mxlc, SizeOf(Mxlc), 0);
  FillChar(Mxc, SizeOf(Mxc), 0);
  with Mxlc do
  begin
    cbStruct := SizeOf(Mxlc);
    dwLineID := Mxl.dwLineID;
    cControls := 1;
    cbmxctrl := SizeOf(Mxc);
    pamxctrl := @Mxc;
    dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
  end;

  if not mGetLineControls(Fmx, Mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
    Exit;

  FControlID := Mxc.dwControlID;
end;

procedure TCnCustomVolumeCtrl.WinProc(var message: TMessage);
var
  dwVolume: DVOLUME;
  bMute, Equ: Boolean;
begin
  if csDesigning in ComponentState then
    Exit;

  case message.Msg of
    MM_MIXM_CONTROL_CHANGE  :
      begin
{$IFDEF WIN64}
        Equ := (NativeInt(message.WParam) = Fmx) and (message.LParam = NativeInt(FControlID));
{$ELSE}
        Equ := (Integer(message.WParam) = Fmx) and (message.LParam = Integer(FControlID));
{$ENDIF}
        if Equ then
        begin
          dwVolume[0] := FCurVolume[0];
          dwVolume[1] := FCurVolume[1];
          GetVolume;

          if (dwVolume[0] <> FCurVolume[0]) or (dwVolume[1] <> FCurVolume[1]) then
          begin
            if Assigned(FOnVolumeChange) then
              FOnVolumeChange(Self, FCurVol div 256, GetLineBalance(FDevice, FLine));
          end;
        end;
      end;
    MM_MIXM_LINE_CHANGE:
      begin
{$IFDEF WIN64}
        Equ := (NativeInt(message.WParam) = Fmx) and (message.LParam = LPARAM(FLineID));
{$ELSE}
        Equ := (Integer(message.WParam) = Fmx) and (message.LParam = LPARAM(FLineID));
{$ENDIF}
        if Equ then
        begin
          bMute := FCurMute;
          GetIsMute;

          if bMute <> FCurMute then
          begin
            if Assigned(FOnMuteChange) then
              FOnMuteChange(Self, FCurMute);
          end;
        end;
      end;
  else
    with message do
      Result := DefWindowProc(FWnd, Msg, WParam, LParam);
  end;
end;

function TCnCustomVolumeCtrl.GetDevCaption(uDev: UINT): string;
var
  Mxcps: TMixerCaps;
begin
  Result := '';
  if not CheckDevice(uDev) then
    Exit;

  if FUseCoreAudio then
  begin
    Result := FDeviceList[uDev];
  end
  else
  begin
    FillChar(Mxcps, SizeOf(Mxcps), 0);
    if mixerGetDevCaps(uDev, @Mxcps, SizeOf(Mxcps)) <> MMSYSERR_NOERROR then
      Exit;

    Result := Mxcps.szPname;
  end;
end;

function TCnCustomVolumeCtrl.GetLineCaption(uDev, uLine: UINT): string;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  uInfo: UINT;
begin
  if FUseCoreAudio then
  begin
    // Core Audio API 的线路名称
    Result := 'Master Volume';
  end
  else
  begin
    Result := '';

    if not CheckDevice(uDev) then
      Exit;

    if not CheckLine(uDev, uLine) then
      Exit;

    Hmx := mGetMxHandle(uDev);
    if Hmx = 0 then
      Exit;

    try
      FillChar(Mxl, SizeOf(Mxl), 0);
      with Mxl do
      begin
        cbStruct := SizeOf(Mxl);
        dwDestination := 0;
        if uLine = 0 then
          uInfo := MIXER_GETLINEINFOF_DESTINATION
        else
        begin
          dwSource := uLine - 1;
          uInfo := MIXER_GETLINEINFOF_SOURCE;
        end;
      end;

      if not mGetLineInfo(Hmx, Mxl, uInfo) then
        Exit;

      Result := Mxl.szName;
    finally
      mixerClose(Hmx);
    end;
  end;
end;

function TCnCustomVolumeCtrl.GetBalance: TCnBalance;
begin
  if FUseCoreAudio then
  begin
    Result := GetBalanceViaCoreAudio;
    FCurBalance := Result;
  end
  else
  begin
    Result := GetLineBalance(FDevice, FLine);
    FCurBalance := Result;
  end;
end;

function TCnCustomVolumeCtrl.GetIsMute: Boolean;
begin
  if FUseCoreAudio then
  begin
    Result := GetMuteViaCoreAudio;
    FCurMute := Result;
  end
  else
  begin
    Result := GetLineMute(FDevice, FLine);
    FCurMute := Result;
  end;
end;

function TCnCustomVolumeCtrl.GetVolume: Integer;
begin
  if FUseCoreAudio then
  begin
    Result := GetVolumeViaCoreAudio;
    FCurVol := Result * 256;
    FCurVolume[0] := FCurVol;
    FCurVolume[1] := FCurVol;
  end
  else
  begin
    Result := GetLineVolume(FDevice, FLine);
    FCurVol := FVol;
    FCurVolume := FVolume;
  end;
end;

procedure TCnCustomVolumeCtrl.SetBalance(const Value: TCnBalance);
var
  iBalance: Integer;
begin
  if csLoading in ComponentState then
    Exit;

  if FUseCoreAudio then
  begin
    if Value = GetBalanceViaCoreAudio then
      Exit;

    iBalance := Value;

    // 限制数字在 TCnBalance 内
    if iBalance > High(TCnBalance) then
      iBalance := High(TCnBalance)
    else if iBalance < Low(TCnBalance) then
      iBalance := Low(TCnBalance);

    FBalance := iBalance;
    SetBalanceViaCoreAudio(iBalance);
    Exit;
  end;

  if GetLineChannels(FDevice, FLine) = 1 then
    Exit;

  if Value = GetLineBalance(FDevice, FLine) then
    Exit;

  iBalance := Value;

  // 限制数字在 TCnBalance内
  if iBalance > High(TCnBalance) then
    iBalance := High(TCnBalance)
  else if iBalance < Low(TCnBalance) then
    iBalance := Low(TCnBalance);

  // 修复设置音量为 0 时丢失左右平行
  FBalance := iBalance;

  SetLineBalance(FDevice, FLine, iBalance);
end;

procedure TCnCustomVolumeCtrl.SetIsMute(const Value: Boolean);
begin
  if csLoading in ComponentState then
    Exit;

  if FUseCoreAudio then
  begin
    if Value <> GetMuteViaCoreAudio then
      SetMuteViaCoreAudio(Value);
  end
  else
  begin
    if Value <> GetLineMute(FDevice, FLine) then
      SetLineMute(FDevice, FLine, Value);
  end;
end;

// 为什么要用 Integer，而不使用 TCnVolume？
// 因为D的强行转换，会把 -22 转换成 234
procedure TCnCustomVolumeCtrl.SetVolume(const Value: Integer);
var
  iVolume: Integer;
begin
  if csLoading in ComponentState then
    Exit;

  if FUseCoreAudio then
  begin
    if Value = GetVolumeViaCoreAudio then
      Exit;

    iVolume := Value;
    // 限制数字在 TCnVolume内
    if iVolume > High(TCnVolume) then
      iVolume := High(TCnVolume)
    else if iVolume < Low(TCnVolume) then
      iVolume := Low(TCnVolume);

    SetVolumeViaCoreAudio(iVolume);
    Exit;
  end;

  if Value = GetLineVolume(FDevice, FLine) then
    Exit;

  iVolume := Value;
  // 限制数字在 TCnVolume 内
  if iVolume > High(TCnVolume) then
    iVolume := High(TCnVolume)
  else if iVolume < Low(TCnVolume) then
    iVolume := Low(TCnVolume);

  SetLineVolume(FDevice, FLine, iVolume);
end;

function TCnCustomVolumeCtrl.GetChannels: DWORD;
begin
  if FUseCoreAudio then
  begin
    // Core Audio API 通常有 2 个声道（立体声）
    Result := 2;
  end
  else
  begin
    Result := GetLineChannels(FDevice, FLine);
  end;
end;

function TCnCustomVolumeCtrl.GetLineBalance(uDev, uLine: UINT): TCnBalance;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  Mxlc: TMixerLineControls;
  Mxc: TMixerControl;
  Mxcd: tMIXERCONTROLDETAILS;
  uInfo: UINT;
begin
  Result := 0;

  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  if not GetLineHaveBalance(uDev, uLine) then
    Exit;

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    with Mxl do
    begin
      cbStruct := SizeOf(Mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Hmx, Mxl, uInfo) then
      Exit;

    FillChar(Mxlc, SizeOf(Mxlc), 0);
    FillChar(Mxc, SizeOf(Mxc), 0);
    with Mxlc do
    begin
      cbStruct := SizeOf(Mxlc);
      dwLineID := Mxl.dwLineID;
      cbmxctrl := SizeOf(Mxc);
      pamxctrl := @Mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(Hmx, Mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(Mxcd, SizeOf(Mxcd), 0);
    with Mxcd do
    begin
      cbStruct := SizeOf(Mxcd);
      dwControlID := Mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := GetLineChannels(uDev, uLine);
      cbDetails := SizeOf(DWORD);
      paDetails := @FVolume;
    end;

    if not mGetControlDetails(Hmx, Mxcd) then
      Exit;

    if (FVolume[0] = 0) and (FVolume[1] = 0) then
      Result := FBalance
    else if FVolume[0] = FVolume[1] then
      Result := 0
    else if FVolume[0] = 0 then
        Result := 32
    else if FVolume[1] = 0 then
      Result := -32
    else if FVolume[0] > FVolume[1] then
      Result := -((FVolume[0] - FVolume[1]) * 32 div FVolume[0])
    else
      Result := (FVolume[1] - FVolume[0]) * 32 div FVolume[1];
  finally
    mixerClose(Hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetLineHaveBalance(uDev, uLine: UINT): Boolean;
begin
  Result := GetLineChannels(uDev, uLine) > 1;
end;

function TCnCustomVolumeCtrl.GetLineChannels(uDev, uLine: UINT): DWORD;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  uInfo: UINT;
begin
  Result := 1;

  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    with Mxl do
    begin
      cbStruct := SizeOf(Mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Hmx, Mxl, uInfo) then
      Exit;

    Result := Mxl.cChannels;
  finally
    mixerClose(Hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetLineMute(uDev, uLine: UINT): Boolean;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  Mxlc: TMixerLineControls;
  Mxc: TMixerControl;
  Mxcd: tMIXERCONTROLDETAILS;
  MxMute: tMIXERCONTROLDETAILS_BOOLEAN;
  uInfo: UINT;
begin
  Result := False;

  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    with Mxl do
    begin
      cbStruct := SizeOf(Mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Hmx, Mxl, uInfo) then
      Exit;

    FillChar(Mxlc, SizeOf(Mxlc), 0);
    FillChar(Mxc, SizeOf(Mxc), 0);
    with Mxlc do
    begin
      cbStruct := SizeOf(Mxlc);
      dwLineID := Mxl.dwLineID;
      cbmxctrl := SizeOf(Mxc);
      pamxctrl := @Mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
    end;

    if not mGetLineControls(Hmx, Mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(Mxcd, SizeOf(Mxcd), 0);
    FillChar(MxMute, SizeOf(MxMute), 0);
    with Mxcd do
    begin
      cbStruct := SizeOf(Mxcd);
      dwControlID := Mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := 1;
      cbDetails := SizeOf(MxMute);
      paDetails := @MxMute;
    end;

    if not mGetControlDetails(Hmx, Mxcd) then
      Exit;

    Result := Boolean(MxMute.fValue);
  finally
    mixerClose(Hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetLineVolume(uDev, uLine: UINT): TCnVolume;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  Mxlc: TMixerLineControls;
  Mxc: TMixerControl;
  Mxcd: tMIXERCONTROLDETAILS;
  uInfo: UINT;
begin
  Result := 0;
  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    with Mxl do
    begin
      cbStruct := SizeOf(Mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Hmx, Mxl, uInfo) then
      Exit;

    FillChar(Mxlc, SizeOf(Mxlc), 0);
    FillChar(Mxc, SizeOf(Mxc), 0);
    with Mxlc do
    begin
      cbStruct := SizeOf(Mxlc);
      dwLineID := Mxl.dwLineID;
      cControls := 1;
      cbmxctrl := SizeOf(Mxc);
      pamxctrl := @Mxc;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(Hmx, Mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(Mxcd, SizeOf(Mxcd), 0);
    with Mxcd do
    begin
      cbStruct := SizeOf(Mxcd);
      dwControlID := Mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := GetLineChannels(uDev, uLine);
      cbDetails := SizeOf(DWORD);
      if cChannels = 1 then
        paDetails := @FVol
      else
        paDetails := @FVolume;
    end;

    if not mGetControlDetails(Hmx, Mxcd) then
      Exit;

    if Mxcd.cChannels = 1 then
    begin
      FVolume[0] := FVol;
      FVolume[1] := FVol;
    end else
      if FVolume[0] > FVolume[1] then
        FVol := FVolume[0]
      else
        FVol := FVolume[1];

    Result := FVol div 256;
  finally
    mixerClose(Hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetDevLines(uDev: UINT): DWORD;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
begin
  Result := 0;

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    Mxl.cbStruct := SizeOf(Mxl);
    if not mGetLineInfo(Hmx, Mxl, MIXER_GETLINEINFOF_DESTINATION) then
      Exit;

    Result := Mxl.cConnections + 1;   // 所加的1是主音量
  finally
    mixerClose(Hmx);
  end;
end;

function TCnCustomVolumeCtrl.CheckDevice(uDev: UINT): Boolean;
begin
  Result := uDev < GetDevs;
end;

function TCnCustomVolumeCtrl.CheckLine(uDev, uLine: UINT): Boolean;
begin
  Result := uLine < GetDevLines(uDev);
end;

function TCnCustomVolumeCtrl.mGetMxHandle(uDev: UINT): HMIXER;
var
  Hmx: HMIXER;
begin
  Hmx := 0;

  if mixerOpen(@Hmx, uDev, 0, 0, 0) <> MMSYSERR_NOERROR then
    raise ECnMixException.Create(SCnMixerOpenError);

  Result := Hmx;
end;

function TCnCustomVolumeCtrl.mGetLineInfo(Hmx: HMIXER; var Mxl: TMixerLine;
  uInfo: UINT): Boolean;
begin
  Result := False;
  if Hmx = 0 then
    Exit;

  if mixerGetLineInfo(Hmx, @Mxl, uInfo) <> MMSYSERR_NOERROR then
    raise ECnMixException.Create(SCnMixerGetLineInfoError);

  Result := True;
end;

function TCnCustomVolumeCtrl.mGetLineControls(Hmx: HMIXER;
  var Mxlc: TMixerLineControls; uInfo: UINT): Boolean;
begin
  Result := False;
  if Hmx = 0 then
    Exit;

  if mixerGetLineControls(Hmx,@ Mxlc, uInfo) <> MMSYSERR_NOERROR then
    raise ECnMixException.Create(SCnMixerGetLineInfoError);

  Result := True;
end;

function TCnCustomVolumeCtrl.mGetControlDetails(Hmx: HMIXER;
  var Mxcd: TMixerControlDetails): Boolean;
begin
  Result := False;
  if Hmx = 0 then
    Exit;

  if mixerGetControlDetails(Hmx, @Mxcd, MIXER_SETCONTROLDETAILSF_VALUE)
    <> MMSYSERR_NOERROR then
    raise ECnMixException.Create(SCnMixerGetLineInfoError);

  Result := True;
end;

function TCnCustomVolumeCtrl.mSetControlDetails(Hmx: HMIXER;
  var Mxcd: TMixerControlDetails): Boolean;
begin
  Result := False;
  if Hmx = 0 then
    Exit;

  if mixerSetControlDetails(Hmx, @Mxcd, MIXER_SETCONTROLDETAILSF_VALUE)
    <> MMSYSERR_NOERROR then
  begin
    raise ECnMixException.Create(SCnMixerGetLineInfoError);
    Exit;
  end;
  Result := True;
end;

function TCnCustomVolumeCtrl.SetLineBalance(uDev, uLine: UINT;
  iBalance: TCnBalance): Boolean;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  Mxlc: TMixerLineControls;
  Mxc: TMixerControl;
  Mxcd: tMIXERCONTROLDETAILS;
  uInfo: UINT;
begin
  Result := False;
  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  if not GetLineHaveBalance(uDev, uLine) then
    Exit;

  GetLineVolume(uDev, uLine);

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    with Mxl do
    begin
      cbStruct := SizeOf(Mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Hmx, Mxl, uInfo) then
      Exit;

    FillChar(Mxlc, SizeOf(Mxlc), 0);
    FillChar(Mxc, SizeOf(Mxc), 0);
    with Mxlc do
    begin
      cbStruct := SizeOf(Mxlc);
      dwLineID := Mxl.dwLineID;
      cbmxctrl := SizeOf(Mxc);
      pamxctrl := @Mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(Hmx, Mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(Mxcd, SizeOf(Mxcd), 0);
    with Mxcd do
    begin
      cbStruct := SizeOf(Mxcd);
      dwControlID := Mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := GetLineChannels(uDev, uLine);
      cbDetails := SizeOf(DWORD);
      paDetails := @FVolume;
    end;

    FVolume[0] := FVol;
    FVolume[1] := FVol;
    if iBalance > 0 then
    begin
      FVolume[0] := Integer(FVol) - (Integer(FVol) * iBalance) div 32;
    end else
    if iBalance < 0 then
    begin
      FVolume[1] := Integer(FVol) + (Integer(FVol) * iBalance) div 32;
    end;
    if FVolume[0] > FVolume[1] then
      FVol := FVolume[0]
    else
      FVol := FVolume[1];

    if not mSetControlDetails(Hmx, Mxcd) then
      Exit;
  finally
    mixerClose(Hmx);
  end;
  Result := True;
end;

function TCnCustomVolumeCtrl.SetLineMute(uDev, uLine: UINT;
  bMute: Boolean): Boolean;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  Mxlc: TMixerLineControls;
  Mxc: TMixerControl;
  Mxcd: tMIXERCONTROLDETAILS;
  mxMute: tMIXERCONTROLDETAILS_BOOLEAN;
  uInfo: UINT;
begin
  Result := False;
  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    with Mxl do
    begin
      cbStruct := SizeOf(Mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Hmx, Mxl, uInfo) then
      Exit;

    FillChar(Mxlc, SizeOf(Mxlc), 0);
    FillChar(Mxc, SizeOf(Mxc), 0);
    with Mxlc do
    begin
      cbStruct := SizeOf(Mxlc);
      dwLineID := Mxl.dwLineID;
      cbmxctrl := SizeOf(Mxc);
      pamxctrl := @Mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
    end;

    if not mGetLineControls(Hmx, Mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(Mxcd, SizeOf(Mxcd), 0);
    FillChar(mxMute, SizeOf(mxMute), 0);
    with Mxcd do
    begin
      cbStruct := SizeOf(Mxcd);
      dwControlID := Mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := 1;
      cbDetails := SizeOf(mxMute);
      paDetails := @mxMute;
    end;
    mxMute.fValue := Ord(bMute);

    if not mSetControlDetails(Hmx, Mxcd) then
      Exit;
  finally
    mixerClose(Hmx);
  end;
  Result := True;
end;

function TCnCustomVolumeCtrl.SetLineVolume(uDev, uLine: UINT;
  Volume: TCnVolume): Boolean;
var
  Hmx: HMIXER;
  Mxl: TMixerLine;
  Mxlc: TMixerLineControls;
  Mxc: TMixerControl;
  Mxcd: tMIXERCONTROLDETAILS;
  uInfo: UINT;
  iBalance: TCnBalance;
begin
  Result := False;
  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  Hmx := mGetMxHandle(uDev);
  if Hmx = 0 then
    Exit;

  try
    FillChar(Mxl, SizeOf(Mxl), 0);
    with Mxl do
    begin
      cbStruct := SizeOf(Mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Hmx, Mxl, uInfo) then
      Exit;

    FillChar(Mxlc, SizeOf(Mxlc), 0);
    FillChar(Mxc, SizeOf(Mxc), 0);
    with Mxlc do
    begin
      cbStruct := SizeOf(Mxlc);
      dwLineID := Mxl.dwLineID;
      cbmxctrl := SizeOf(Mxc);
      pamxctrl := @Mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(Hmx, Mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(Mxcd, SizeOf(Mxcd), 0);
    with Mxcd do
    begin
      cbStruct := SizeOf(Mxcd);
      dwControlID := Mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := GetLineChannels(uDev, uLine);
      cbDetails := SizeOf(DWORD);
      paDetails := @FVolume;
    end;

    iBalance := GetLineBalance(uDev, uLine);
    FVol := Volume * 256;
    FVolume[0] := FVol;
    FVolume[1] := FVol;
    if iBalance > 0 then
    begin
      FVolume[0] := Integer(FVol) - (Integer(FVol) * iBalance) div 32;
    end else
    if iBalance < 0 then
    begin
      FVolume[1] := Integer(FVol) + (Integer(FVol) * iBalance) div 32;
    end;

    if not mSetControlDetails(Hmx, Mxcd) then
      Exit;
  finally
    mixerClose(Hmx);
  end;
  Result := True;
end;

procedure TCnCustomVolumeCtrl.RefreshDeviceList;
var
  HR: HResult;
  pEnumerator: IMMDeviceEnumerator;
  pCollection: IMMDeviceCollection;
  pDevice: IMMDevice;
  pPropertyStore: IPropertyStore;
  Prop: TPropVariant;
  Cnt: UINT;
  I: Integer;
  pwszDeviceId: PWideChar;
  DeviceName: WideString;
begin
  if not FCoreAudioSupported then Exit;

  if FDeviceList = nil then
    FDeviceList := TStringList.Create;
  if FDeviceIdList = nil then
    FDeviceIdList := TStringList.Create;

  FDeviceList.Clear;
  FDeviceIdList.Clear;

  try
    // 创建设备枚举器
    HR := CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL,
      IID_IMMDeviceEnumerator, pEnumerator);
    if Failed(HR) then Exit;

    // 枚举所有音频设备
    HR := pEnumerator.EnumAudioEndpoints(eRender, DEVICE_STATE_ACTIVE, pCollection);
    if Failed(HR) then Exit;

    // 获取设备数量
    HR := pCollection.GetCount(Cnt);
    if Failed(HR) then Exit;

    // 遍历所有设备
    for I := 0 to Cnt - 1 do
    begin
      // 获取设备
      HR := pCollection.Item(I, pDevice);
      if Failed(HR) then Continue;

      // 获取设备 ID
      HR := pDevice.GetId(pwszDeviceId);
      if Failed(HR) then Continue;

      // 获取设备属性
      HR := pDevice.OpenPropertyStore(STGM_READ, pPropertyStore);
      if Failed(HR) then
      begin
        CoTaskMemFree(pwszDeviceId);
        Continue;
      end;

      // 获取设备友好名称
      HR := pPropertyStore.GetValue(PKEY_Device_FriendlyName, Prop);
      if Failed(HR) then
      begin
        CoTaskMemFree(pwszDeviceId);
        Continue;
      end;

      DeviceName := Prop.pwszVal;

      // 添加到列表
      FDeviceList.Add(DeviceName);
      FDeviceIdList.Add(pwszDeviceId);

      // 清理
      PropVariantClear(Prop);
      CoTaskMemFree(pwszDeviceId);
    end;
  except
    // 忽略异常
  end;
end;

function TCnCustomVolumeCtrl.GetDeviceCount: Integer;
begin
  if FUseCoreAudio then
  begin
    if FDeviceList = nil then
      RefreshDeviceList;
    Result := FDeviceList.Count;
  end
  else
  begin
    Result := mixerGetNumDevs;
  end;
end;

function TCnCustomVolumeCtrl.GetDeviceName(Index: Integer): string;
begin
  if FUseCoreAudio then
  begin
    if (FDeviceList = nil) or (Index < 0) or (Index >= FDeviceList.Count) then
      Result := 'Default Audio Device'
    else
      Result := FDeviceList[Index];
  end
  else
  begin
    Result := GetDevCaption(Index);
  end;
end;

function TCnCustomVolumeCtrl.GetDeviceId(Index: Integer): string;
begin
  if FUseCoreAudio then
  begin
    if (FDeviceIdList = nil) or (Index < 0) or (Index >= FDeviceIdList.Count) then
      Result := ''
    else
      Result := FDeviceIdList[Index];
  end
  else
  begin
    Result := IntToStr(Index);
  end;
end;

procedure TCnCustomVolumeCtrl.SetActiveDevice(Index: Integer);
var
  DeviceId: string;
begin
  if FUseCoreAudio then
  begin
    if (Index < 0) or (Index >= GetDeviceCount) then Exit;

    DeviceId := GetDeviceId(Index);
    SetActiveDeviceById(DeviceId);
  end
  else
  begin
    SetDev(Index);
  end;
end;

procedure TCnCustomVolumeCtrl.SetActiveDeviceById(const DeviceId: string);
var
  HR: HResult;
  pEnumerator: IMMDeviceEnumerator;
  pDevice: IMMDevice;
  TempEndpointVolume: Pointer;
  W: WideString;
begin
  if not FCoreAudioSupported or (DeviceId = '') then Exit;

  try
    // 清理旧的回调
    if Assigned(FEndpointVolume) and Assigned(FEndpointCallback) then
      FEndpointVolume.UnregisterControlChangeNotify(FEndpointCallback);

    FEndpointVolume := nil;

    // 创建设备枚举器
    HR := CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL,
      IID_IMMDeviceEnumerator, pEnumerator);
    if Failed(HR) then Exit;

    // 根据 ID 获取设备
    W := WideString(DeviceId);
    HR := pEnumerator.GetDevice(PWideChar(W), pDevice);
    if Failed(HR) then Exit;

    TempEndpointVolume := nil;

    // 激活 IAudioEndpointVolume 接口
    HR := pDevice.Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, nil, TempEndpointVolume);
    if HR = S_OK then
    begin
      FEndpointVolume := IAudioEndpointVolume(TempEndpointVolume);

      if Assigned(FEndpointVolume) then
      begin
        // 注册回调
        FEndpointCallback := TEndpointVolumeCallback.Create(Self);
        FEndpointVolume.RegisterControlChangeNotify(FEndpointCallback);
      end;
    end;
  except
    // 忽略异常
  end;
end;

end.
