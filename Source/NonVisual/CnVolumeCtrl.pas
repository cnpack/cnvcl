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
* 修改记录：2005.12.22 v1.1
*               修正未响应关机事件的问题
*           2005.09.23 v1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Messages, Windows, Forms, MMSystem,
  CnClasses, CnConsts, CnCompConsts;

type
  DVOLUME = array[0..1] of DWORD;

  ECnMixException = class(Exception);

  TCnBalance  = -32..32;
  TCnVolume   = 0..255;
  //操作类型
  TCnMixOperateType = (motGetVol, motSetVol, motGetMute, motSetMute);

  //事件
  TCnMixVolumeEvent = procedure(Volume: TCnVolume; Balance: TCnBalance) of object;
  TCnMixMuteEvent = procedure(bMute: Boolean) of object;

  TCnCustomVolumeCtrl = class(TCnComponent)
  {* 音量控制器组件基类}
  private
    Fmx: HMIXER;          //音频句柄
    FDevice: UINT;        //当前设备
    FLine: UINT;          //当前线路
    FCurVolume: DVOLUME;  //当前线路左右音量
    FCurVol:DWORD;        //当前线路最高音量
    FCurMute: Boolean;     //当前线路静音状态
    FCurBalance: TCnBalance; //当前线路左右平行

    FVolume: DVOLUME;
    FVol: DWORD;
    FBalance: TCnBalance;

    FWnd: HWND;
    FLineID: DWORD;
    FControlID: DWORD;
    FOnVolumeChange: TCnMixVolumeEvent;
    FOnMuteChange: TCnMixMuteEvent;

    //Common
    function InLoading: Boolean;
    function mGetMxHandle(uDev: UINT): HMIXER;
    function mGetLineInfo(hmx: HMIXER; var mxl: TMixerLine; uInfo: UINT): Boolean;
    function mGetLineControls(hmx: HMIXER; var mxlc: TMixerLineControls;
      uInfo: UINT): Boolean;
    function mGetControlDetails(hmx: HMIXER; var mxcd: TMixerControlDetails): Boolean;
    function mSetControlDetails(hmx: HMIXER; var mxcd: TMixerControlDetails): Boolean;
    function CheckDevice(uDev: UINT): Boolean;
    function CheckLine(uDev, uLine: UINT): Boolean;
    procedure WinProc(var message: TMessage);

    //property
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
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}

    //获取参数
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

    //设置参数
    function SetLineVolume(uDev, uLine: UINT; Volume: TCnVolume): Boolean;
    {* 获取线路音量}
    function SetLineBalance(uDev, uLine: UINT; iBalance: TCnBalance): Boolean;
    {* 获取线路左右平行}
    function SetLineMute(uDev, uLine: UINT; bMute: Boolean): Boolean;
    {* 获取线路通道总数}

    //属性
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
    {* 设置/获取当前线路左右平行}
    property IsMute: Boolean read GetIsMute write SetIsMute;
    {* 设置/获取当前线路静音状态}

    property OnVolumeChange: TCnMixVolumeEvent read FOnVolumeChange write FOnVolumeChange;
    property OnMuteChange: TCnMixMuteEvent read FOnMuteChange write FOnMuteChange;
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

{ TCnCustomVolumeCtrl }

procedure TCnCustomVolumeCtrl.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnVolumnCtrlName;
  Author := SCnPack_Kendling + ';' + SCnPack_SuperYoyoNc;
  Email := SCnPack_KendlingEmail + ';' + SCnPack_SuperYoyoNcEmail;
  Comment := SCnVolumnCtrlComment;
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

  if csDesigning in ComponentState then
  begin
    if mixerOpen(@Fmx, FDevice, 0, 0,
      CALLBACK_NULL) <> MMSYSERR_NOERROR then
    begin
      raise ECnMixException.Create(SCnMixerOpenError);
      Exit;
    end;
  end else
  begin
    if FWnd = 0 then
      FWnd := AllocateHWnd(WinProc);

    if mixerOpen(@Fmx, FDevice, FWnd, 0,
      CALLBACK_WINDOW) <> MMSYSERR_NOERROR then
    begin
      raise ECnMixException.Create(SCnMixerOpenError);
      Exit;
    end;
  end;
  SetDev(0);
end;

destructor TCnCustomVolumeCtrl.Destroy;
begin
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
  inherited;  
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
  Result := mixerGetNumDevs;
end;

function TCnCustomVolumeCtrl.GetLines: UINT;
begin
  Result := GetDevLines(FDevice);
end;

function TCnCustomVolumeCtrl.InLoading: Boolean;
begin
 Result := csLoading in ComponentState;
end;

procedure TCnCustomVolumeCtrl.SetDev(const Value: UINT);
begin
  if InLoading then
    Exit;

  if not CheckDevice(Value) then
    Exit;

  FDevice := Value;
  if Fmx <> 0 then
    mixerClose(Fmx);
  if csDesigning in ComponentState then
  begin
    if mixerOpen(@Fmx, FDevice, 0, 0,
      CALLBACK_NULL) <> MMSYSERR_NOERROR then
    begin
      raise ECnMixException.Create(SCnMixerOpenError);
      Exit;
    end;
  end else
  begin
    if FWnd = 0 then
      FWnd := AllocateHWnd(WinProc);

    if mixerOpen(@Fmx, FDevice, FWnd, 0,
      CALLBACK_WINDOW) <> MMSYSERR_NOERROR then
    begin
      raise ECnMixException.Create(SCnMixerOpenError);
      Exit;
    end;
  end;
  SetLine(0);
end;

procedure TCnCustomVolumeCtrl.SetLine(const Value: UINT);
var
  mxl: TMixerLine;
  mxlc: TMixerLineControls;
  mxc: TMixerControl;
  uInfo: UINT;
begin
  if InLoading then
    Exit;

  if not CheckLine(FDevice, Value) then
    Exit;

  FLine := Value;

  //获取ID，用于事件
  FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if FLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := FLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(Fmx, mxl, uInfo) then
      Exit;

    FLineID := mxl.dwLineID;

    FillChar(mxlc, SizeOf(mxlc), 0);
    FillChar(mxc, SizeOf(mxc), 0);
    with mxlc do
    begin
      cbStruct := SizeOf(mxlc);
      dwLineID := mxl.dwLineID;
      cControls := 1;
      cbmxctrl := SizeOf(mxc);
      pamxctrl := @mxc;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(Fmx, mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FControlID := mxc.dwControlID;
end;

procedure TCnCustomVolumeCtrl.WinProc(var message: TMessage);
var
  dwVolume: DVOLUME;
//  dwVol: DWORD;
  bMute: Boolean;
begin
  if csDesigning in ComponentState then
    Exit;
  case message.Msg of
    MM_MIXM_CONTROL_CHANGE  :
    begin
      if (Integer(message.WParam) = Fmx)
        and (message.LParam = Integer(FControlID)) then
      begin
        dwVolume[0] := FCurVolume[0];
        dwVolume[1] := FCurVolume[1];
//        dwVol := FCurVol;
        GetVolume;

        if (dwVolume[0] <> FCurVolume[0])
          or (dwVolume[1] <> FCurVolume[1]) then
          if Assigned(FOnVolumeChange) then
            FOnVolumeChange(FCurVol div 256, GetLineBalance(FDevice, FLine));
      end;
    end;
    MM_MIXM_LINE_CHANGE     :
    begin
      if (Integer(message.WParam) = Fmx)
        and (message.LParam = LPARAM(FLineID)) then
      begin
        bMute := FCurMute;
        GetIsMute;

        if bMute <> FCurMute then
          if Assigned(FOnMuteChange) then
            FOnMuteChange(FCurMute);
      end;
    end;
  else
    with message do
      Result := DefWindowProc(FWnd, Msg, WParam, LParam);
  end;
end;

function TCnCustomVolumeCtrl.GetDevCaption(uDev: UINT): string;
var
  mxcps: TMixerCaps;
begin
  Result := '';
  if not CheckDevice(uDev) then
    Exit;

  FillChar(mxcps, SizeOf(mxcps), 0);
  if mixerGetDevCaps(uDev, @mxcps, SizeOf(mxcps)) <> MMSYSERR_NOERROR then
    Exit;

  Result := mxcps.szPname;
end;

function TCnCustomVolumeCtrl.GetLineCaption(uDev, uLine: UINT): string;
var
  hmx: HMIXER;
  mxl: TMixerLine;
  uInfo: UINT;
begin
  Result := '';

  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    Result := mxl.szName;
  finally
    mixerClose(hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetBalance: TCnBalance;
begin
  Result := GetLineBalance(FDevice, FLine);
  FCurBalance := Result;
end;

function TCnCustomVolumeCtrl.GetIsMute: Boolean;
begin
  Result := GetLineMute(FDevice, FLine);
  FCurMute := Result;
end;

function TCnCustomVolumeCtrl.GetVolume: Integer;
begin
  Result := GetLineVolume(FDevice, FLine);
  FCurVol := FVol;
  FCurVolume := FVolume;
end;

procedure TCnCustomVolumeCtrl.SetBalance(const Value: TCnBalance);
var
  iBalance: Integer;
begin
  if InLoading then
    Exit;
  if GetLineChannels(FDevice, FLine) = 1 then
    Exit;
  if Value = GetLineBalance(FDevice, FLine) then
    Exit;

  iBalance := Value;
  // 限制数字在TCnBalance内
  if iBalance > High(TCnBalance) then
    iBalance := High(TCnBalance)
  else if iBalance < Low(TCnBalance) then
    iBalance := Low(TCnBalance);

  // 修复设置音量为0时丢失左右平行
  FBalance := iBalance;

  SetLineBalance(FDevice, FLine, iBalance);
end;

procedure TCnCustomVolumeCtrl.SetIsMute(const Value: Boolean);
begin
  if InLoading then
    Exit;
  if Value = GetLineMute(FDevice, FLine) then
    Exit;

  SetLineMute(FDevice, FLine, Value);
end;

// 为什么要用Integer，而不使用TCnVolume？
// 因为D的强行转换，会把-22转换成234
procedure TCnCustomVolumeCtrl.SetVolume(const Value: Integer);
var
  iVolume: Integer;
begin
  if InLoading then
    Exit;
  if Value = GetLineVolume(FDevice, FLine) then
    Exit;

  iVolume := Value;
  // 限制数字在TCnVolume内
  if iVolume > High(TCnVolume) then
    iVolume := High(TCnVolume)
  else if iVolume < Low(TCnVolume) then
    iVolume := Low(TCnVolume);

  SetLineVolume(FDevice, FLine, iVolume);
end;

function TCnCustomVolumeCtrl.GetChannels: DWORD;
begin
  Result := GetLineChannels(FDevice, FLine);
end;

function TCnCustomVolumeCtrl.GetLineBalance(uDev, uLine: UINT): TCnBalance;
var
  hmx: HMIXER;
  mxl: TMixerLine;
  mxlc: TMixerLineControls;
  mxc: TMixerControl;
  mxcd: tMIXERCONTROLDETAILS;
  uInfo: UINT;
begin
  Result := 0;

  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  if not GetLineHaveBalance(uDev, uLine) then
    Exit;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    FillChar(mxlc, SizeOf(mxlc), 0);
    FillChar(mxc, SizeOf(mxc), 0);
    with mxlc do
    begin
      cbStruct := SizeOf(mxlc);
      dwLineID := mxl.dwLineID;
      cbmxctrl := SizeOf(mxc);
      pamxctrl := @mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(hmx, mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(mxcd, SizeOf(mxcd), 0);
    with mxcd do
    begin
      cbStruct := SizeOf(mxcd);
      dwControlID := mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := GetLineChannels(uDev, uLine);
      cbDetails := SizeOf(DWORD);
      paDetails := @FVolume;
    end;

    if not mGetControlDetails(hmx, mxcd) then
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
    mixerClose(hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetLineHaveBalance(uDev, uLine: UINT): Boolean;
begin
  Result := GetLineChannels(uDev, uLine) > 1;
end;

function TCnCustomVolumeCtrl.GetLineChannels(uDev, uLine: UINT): DWORD;
var
  hmx: HMIXER;
  mxl: TMixerLine;
  uInfo: UINT;
begin
  Result := 1;

  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    Result := mxl.cChannels;
  finally
    mixerClose(hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetLineMute(uDev, uLine: UINT): Boolean;
var
  hmx: HMIXER;
  mxl: TMixerLine;
  mxlc: TMixerLineControls;
  mxc: TMixerControl;
  mxcd: tMIXERCONTROLDETAILS;
  mxMute: tMIXERCONTROLDETAILS_BOOLEAN;
  uInfo: UINT;
begin
  Result := False;

  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    FillChar(mxlc, SizeOf(mxlc), 0);
    FillChar(mxc, SizeOf(mxc), 0);
    with mxlc do
    begin
      cbStruct := SizeOf(mxlc);
      dwLineID := mxl.dwLineID;
      cbmxctrl := SizeOf(mxc);
      pamxctrl := @mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
    end;

    if not mGetLineControls(hmx, mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(mxcd, SizeOf(mxcd), 0);
    FillChar(mxMute, SizeOf(mxMute), 0);
    with mxcd do
    begin
      cbStruct := SizeOf(mxcd);
      dwControlID := mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := 1;
      cbDetails := SizeOf(mxMute);
      paDetails := @mxMute;
    end;

    if not mGetControlDetails(hmx, mxcd) then
      Exit;

    Result := Boolean(mxMute.fValue);
  finally
    mixerClose(hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetLineVolume(uDev, uLine: UINT): TCnVolume;
var
  hmx: HMIXER;
  mxl: TMixerLine;
  mxlc: TMixerLineControls;
  mxc: TMixerControl;
  mxcd: tMIXERCONTROLDETAILS;
  uInfo: UINT;
begin
  Result := 0;
  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    FillChar(mxlc, SizeOf(mxlc), 0);
    FillChar(mxc, SizeOf(mxc), 0);
    with mxlc do
    begin
      cbStruct := SizeOf(mxlc);
      dwLineID := mxl.dwLineID;
      cControls := 1;
      cbmxctrl := SizeOf(mxc);
      pamxctrl := @mxc;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(hmx, mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(mxcd, SizeOf(mxcd), 0);
    with mxcd do
    begin
      cbStruct := SizeOf(mxcd);
      dwControlID := mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := GetLineChannels(uDev, uLine);
      cbDetails := SizeOf(DWORD);
      if cChannels = 1 then
        paDetails := @FVol
      else
        paDetails := @FVolume;
    end;

    if not mGetControlDetails(hmx, mxcd) then
      Exit;

    if mxcd.cChannels = 1 then
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
    mixerClose(hmx);
  end;
end;

function TCnCustomVolumeCtrl.GetDevLines(uDev: UINT): DWORD;
var
  hmx: HMIXER;
  mxl: TMixerLine;
begin
  Result := 0;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    mxl.cbStruct := SizeOf(mxl);
    if not mGetLineInfo(hmx, mxl, MIXER_GETLINEINFOF_DESTINATION) then
      Exit;

    Result := mxl.cConnections + 1;   // 所加的1是主音量
  finally
    mixerClose(hmx);
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
  hmx: HMIXER;
begin
  hmx := 0;

  if mixerOpen(@hmx, uDev, 0, 0, 0) <> MMSYSERR_NOERROR then
  begin
    raise ECnMixException.Create(SCnMixerOpenError);
    Exit;
  end;

  Result := hmx;
end;

function TCnCustomVolumeCtrl.mGetLineInfo(hmx: HMIXER; var mxl: TMixerLine;
  uInfo: UINT): Boolean;
begin
  Result := False;
  if hmx = 0 then
    Exit;

  if mixerGetLineInfo(hmx, @mxl, uInfo)
    <> MMSYSERR_NOERROR then
  begin
    raise ECnMixException.Create(SCnMixerGetLineInfoError);
    Exit;
  end;
  
  Result := True;   
end;

function TCnCustomVolumeCtrl.mGetLineControls(hmx: HMIXER;
  var mxlc: TMixerLineControls; uInfo: UINT): Boolean;
begin
  Result := False;
  if hmx = 0 then
    Exit;

  if mixerGetLineControls(hmx,@ mxlc, uInfo)
    <> MMSYSERR_NOERROR then
  begin
    raise ECnMixException.Create(SCnMixerGetLineInfoError);
    Exit;
  end;
  Result := True;
end;

function TCnCustomVolumeCtrl.mGetControlDetails(hmx: HMIXER;
  var mxcd: TMixerControlDetails): Boolean;
begin
  Result := False;
  if hmx = 0 then
    Exit;

  if mixerGetControlDetails(hmx, @mxcd, MIXER_SETCONTROLDETAILSF_VALUE)
    <> MMSYSERR_NOERROR then
  begin
    raise ECnMixException.Create(SCnMixerGetLineInfoError);
    Exit;
  end;
  Result := True;
end;

function TCnCustomVolumeCtrl.mSetControlDetails(hmx: HMIXER;
  var mxcd: TMixerControlDetails): Boolean;
begin
  Result := False;
  if hmx = 0 then
    Exit;

  if mixerSetControlDetails(hmx, @mxcd, MIXER_SETCONTROLDETAILSF_VALUE)
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
  hmx: HMIXER;
  mxl: TMixerLine;
  mxlc: TMixerLineControls;
  mxc: TMixerControl;
  mxcd: tMIXERCONTROLDETAILS;
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

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    FillChar(mxlc, SizeOf(mxlc), 0);
    FillChar(mxc, SizeOf(mxc), 0);
    with mxlc do
    begin
      cbStruct := SizeOf(mxlc);
      dwLineID := mxl.dwLineID;
      cbmxctrl := SizeOf(mxc);
      pamxctrl := @mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(hmx, mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(mxcd, SizeOf(mxcd), 0);
    with mxcd do
    begin
      cbStruct := SizeOf(mxcd);
      dwControlID := mxc.dwControlID;
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

    if not mSetControlDetails(hmx, mxcd) then
      Exit;
  finally
    mixerClose(hmx);
  end;
  Result := True;
end;

function TCnCustomVolumeCtrl.SetLineMute(uDev, uLine: UINT;
  bMute: Boolean): Boolean;
var
  hmx: HMIXER;
  mxl: TMixerLine;
  mxlc: TMixerLineControls;
  mxc: TMixerControl;
  mxcd: tMIXERCONTROLDETAILS;
  mxMute: tMIXERCONTROLDETAILS_BOOLEAN;
  uInfo: UINT;
begin
  Result := False;
  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    FillChar(mxlc, SizeOf(mxlc), 0);
    FillChar(mxc, SizeOf(mxc), 0);
    with mxlc do
    begin
      cbStruct := SizeOf(mxlc);
      dwLineID := mxl.dwLineID;
      cbmxctrl := SizeOf(mxc);
      pamxctrl := @mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
    end;

    if not mGetLineControls(hmx, mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(mxcd, SizeOf(mxcd), 0);
    FillChar(mxMute, SizeOf(mxMute), 0);
    with mxcd do
    begin
      cbStruct := SizeOf(mxcd);
      dwControlID := mxc.dwControlID;
      cMultipleItems := 0;
      cChannels := 1;
      cbDetails := SizeOf(mxMute);
      paDetails := @mxMute;
    end;
    mxMute.fValue := Ord(bMute);

    if not mSetControlDetails(hmx, mxcd) then
      Exit;
  finally
    mixerClose(hmx);
  end;
  Result := True;
end;

function TCnCustomVolumeCtrl.SetLineVolume(uDev, uLine: UINT;
  Volume: TCnVolume): Boolean;
var
  hmx: HMIXER;
  mxl: TMixerLine;
  mxlc: TMixerLineControls;
  mxc: TMixerControl;
  mxcd: tMIXERCONTROLDETAILS;
  uInfo: UINT;
  iBalance: TCnBalance;
begin
  Result := False;
  if not CheckDevice(uDev) then
    Exit;

  if not CheckLine(uDev, uLine) then
    Exit;

  hmx := mGetMxHandle(uDev);
  if hmx = 0 then
    Exit;

  try
    FillChar(mxl, SizeOf(mxl), 0);
    with mxl do
    begin
      cbStruct := SizeOf(mxl);
      dwDestination := 0;
      if uLine = 0 then
        uInfo := MIXER_GETLINEINFOF_DESTINATION
      else
      begin
        dwSource := uLine - 1;
        uInfo := MIXER_GETLINEINFOF_SOURCE;
      end;
    end;
    if not mGetLineInfo(hmx, mxl, uInfo) then
      Exit;

    FillChar(mxlc, SizeOf(mxlc), 0);
    FillChar(mxc, SizeOf(mxc), 0);
    with mxlc do
    begin
      cbStruct := SizeOf(mxlc);
      dwLineID := mxl.dwLineID;
      cbmxctrl := SizeOf(mxc);
      pamxctrl := @mxc;
      cControls := 1;
      dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
    end;

    if not mGetLineControls(hmx, mxlc, MIXER_GETLINECONTROLSF_ONEBYTYPE) then
      Exit;

    FillChar(mxcd, SizeOf(mxcd), 0);
    with mxcd do
    begin
      cbStruct := SizeOf(mxcd);
      dwControlID := mxc.dwControlID;
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

    if not mSetControlDetails(hmx, mxcd) then
      Exit;
  finally
    mixerClose(hmx);
  end;
  Result := True;
end;

end.
