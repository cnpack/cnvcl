{******************************************************************************}
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

unit CnModem;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：CnModem标准调制解调器组件单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：CnModem 组件由 CnRS232 串口通讯组件派生而来
*           提供利用 AT 命令通过串口直接操作调制解调器的功能
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2002.04.08 V1.0
*                创建单元，增加注释
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnConsts, CnNetConsts, CnRS232, IniFiles;

type

//------------------------------------------------------------------------------
// 标准调制解调器组件
//------------------------------------------------------------------------------

{ TCnModem }

  TDialResult = (drConnect, drOpenCommFail, drNoModem, drNoDialtone, drBusy,
    drNoAnswer, drNoCarrier, drTimeout, drUnknow);
  {* Modem 拨号结果类型
   |<PRE>
     drConnect:         - 连接成功
     drOpenCommFail:    - 打开串口失败
     drNoModem:         - 没有检测到 Modem
     drNoDialtone:      - 无拨号音
     drBusy:            - 检测到忙信号
     drNoAnswer:        - 无应答信号
     drNoCarrier:       - 没有检测到载波信号
     drTimeout:         - 超时错
     drUnknow:          - 未知错误
   |</PRE>}

  TATResult = (arOk, arConnect, arRing, arNoCarrier, arError, arNoDialtone,
    arBusy, arNoAnswer, arTimeout, arUnknow);
  {* AT 命令执行结果类型
   |<PRE>
     arOk:              - 成功
     arConnect:         - 已连接
     arRing:            - 振铃信号
     arNoCarrier:       - 没有检测到载波信号
     arError:           - 执行错误
     arNoDialtone:      - 无拨号音
     arBusy:            - 检测到忙信号
     arNoAnswer:        - 无应答信号
     arTimeout:         - 超时错
     arUnknow:          - 未知错误
   |</PRE>}

  TModemVolume = (mvLowest, mvLow, mvMiddle, mvHigh);
  {* Modem 音量
   |<PRE>
     mvLowest:          - 最小音量
     mvLow:             - 小音量
     mvMiddle:          - 中等音量
     mvHigh:            - 大音量
   |</PRE>}

  TRingEvent = procedure(Sender: TObject; var Answer: Boolean) of object;
  {* 接收到振铃事件，变量参数 Answer 决定是否应答}
  TConnectEvent = procedure(Sender: TObject; Rate: Integer) of object;
  {* 已连接成功事件，参数 Rate 为连接速度}
  TInvalidCommandEvent = procedure(Sender: TObject; const Command: string) of object;
  {* 非法的 AT 命令事件，参数为出错的命令行}
  TModemState = (msUnknow, msOffline, msOnline, msOnlineCommand, msConnecting);
  {* 当前Modem状态类型
   |<PRE>
     msUnknow:          - 未知状态
     msOffline:         - 离线状态
     msOnline:          - 在线状态
     msOnlineCommand:   - 在线命令状态
     msConnecting:      - 正在连接状态
   |</PRE>}
  TStateChangeEvent = procedure(Sender: TObject; State: TModemState) of object;
  {* 当前 Modem 状态改变事件}

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnModem = class(TCnRS232)
  {* 标准调制解调器通讯组件
   |<PRE>
     * 组件由 TCnRS232 派生而来，通过向串口发送AT命令来控制标准 Modem 通讯。
     * 使用时可直接调用 Dial 方法进行拨号连接，拨号完成返回执行结果。
     * 当 Modem 检测到振铃信号时，产生 OnRing 事件。
     * Hangup 方法可挂机拆除连接，通讯时如果连接中断，将产生 OnDisConnect 事件。
     * 连接成功后通过使用继承来的方法 WriteCommData 向 Modem 发送数据。
     * 只有当 Modem 处于在线状态时，收到数据才会产生 OnReceiveData 事件。
   |</PRE>}
  private
    FCheckDialtone: Boolean;
    FCheckBusy: Boolean;
    FAutoAnswer: Boolean;
    FVolume: TModemVolume;
    FWaitEscapeTime: Integer;
    FWaitDialtoneTime: Integer;
    FWaitCarrierTime: Integer;
    FInitATCommand: string;
    FModemState: TModemState;
    FOnConnect: TConnectEvent;
    FOnDisConnect: TNotifyEvent;
    FOnRing: TRingEvent;
    FOnInvalidCommand: TInvalidCommandEvent;
    FOnStateChange: TStateChangeEvent;
    FWaitATResult: Boolean;
    FATResult: string;
    FConnectRate: Integer;
    procedure SetAutoAnswer(const Value: Boolean);
    procedure SetVolume(const Value: TModemVolume);
    procedure SetInitATCommand(const Value: string);
    procedure SetWaitCarrierTime(const Value: Integer);
    procedure SetWaitDialtoneTime(const Value: Integer);
    procedure SetWaitEscapeTime(const Value: Integer);
    procedure SetCheckBusy(const Value: Boolean);
    procedure SetCheckDialtone(const Value: Boolean);
    procedure SetModemState(const Value: TModemState);
    function WaitATResult(Delay: Cardinal): string;
    function SendATOk(AT: string; Delay: Cardinal = 200): Boolean;
    function StrToIntEx(const Str: string): Integer;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function CommOpened: Boolean;
    function OpenComm: Boolean;
    procedure Changed;
    procedure ReceiveData(Buffer: PAnsiChar; BufferLength: WORD); override;
    procedure _SendDataEmpty; override;
    procedure Ring; virtual;
    procedure Connect(Rate: Integer); virtual;
    procedure DisConnect; virtual;
    procedure InvalidCommand(const Command: string); virtual;
    procedure Escape;
    procedure Resume;
    function Answer: TDialResult;
    property ModemState: TModemState read FModemState write SetModemState;
  public
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方式}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InitModem: Boolean;
    {* 初始化 Modem，一般不需要手工调用}
    function Dial(const Number: string): TDialResult;
    {* 拨号方法，参数为对方电话机号码}
    procedure WriteATCommand(const Command: string; Return: Boolean = True);
    {* 写 AT 命令方法，允许用户手工向 Modem 发送 AT 命令。
     |<PRE>
       Command: string  - AT 命令行
       Return: Boolean  - 是否自动在命令行末尾增加回车，默认为是
     |</PRE>}
    procedure Hangup;
    {* 挂机拆除当前连接}
    procedure ReadFromIni(Ini: TCustomIniFile; const Section: string); override;
    procedure WriteToIni(Ini: TCustomIniFile; const Section: string); override;
    property State: TModemState read FModemState;
    {* 当前的 Modem 状态，运行期只读属性}
    property ConnectRate: Integer read FConnectRate;
    {* 当前的连接速度，运行期只读属性}
  published
    { Published declarations }
    property CheckDialtone: Boolean read FCheckDialtone write SetCheckDialtone default
      True;
    {* 拨号前是否检测拨号音}
    property CheckBusy: Boolean read FCheckBusy write SetCheckBusy default True;
    {* 拨号前是否检测忙信号}
    property AutoAnswer: Boolean read FAutoAnswer write SetAutoAnswer default False;
    {* 是否允许自动应答。如果允许，不管在 OnRing 事件是否允许，都将自动应答}
    property Volume: TModemVolume read FVolume write SetVolume default mvMiddle;
    {* Modem 音量}
    property WaitDialtoneTime: Integer read FWaitDialtoneTime write SetWaitDialtoneTime
      default 2;
    {* 等待拨号音的最长时间，单位为秒}
    property WaitCarrierTime: Integer read FWaitCarrierTime write SetWaitCarrierTime
      default 50;
    {* 等待检测载波的最长时间，单位为秒}
    property WaitEscapeTime: Integer read FWaitEscapeTime write SetWaitEscapeTime
      default 50;
    {* 切换到在线命令状态的等待时间，单位为 20 毫秒}
    property InitATCommand: string read FInitATCommand write SetInitATCommand;
    {* 用于初始化 Modem 的额外AT命令，仅建议高级用户使用}
    property OnRing: TRingEvent read FOnRing write FOnRing;
    {* 振铃事件}
    property OnConnect: TConnectEvent read FOnConnect write FOnConnect;
    {* 连接成功事件}
    property OnInvalidCommand: TInvalidCommandEvent read FOnInvalidCommand write
      FOnInvalidCommand;
    {* 检测到无效的AT命令事件}
    property OnDisConnect: TNotifyEvent read FOnDisConnect write FOnDisConnect;
    {* 连接中断事件}
    property OnStateChange: TStateChangeEvent read FOnStateChange write FOnStateChange;
    {* Modem 状态改变事件}
  end;

implementation

//------------------------------------------------------------------------------
// 标准调制解调器组件
//------------------------------------------------------------------------------

{ TCnModem }

// 对象赋值方法
procedure TCnModem.Assign(Source: TPersistent);
begin
  if Source is TCnModem then
  begin
    FCheckDialtone := TCnModem(Source).FCheckDialtone;
    FCheckBusy := TCnModem(Source).FCheckBusy;
    FAutoAnswer := TCnModem(Source).FAutoAnswer;
    FWaitEscapeTime := TCnModem(Source).FWaitEscapeTime;
    FWaitDialtoneTime := TCnModem(Source).FWaitDialtoneTime;
    FWaitCarrierTime := TCnModem(Source).FWaitCarrierTime;
    FInitATCommand := TCnModem(Source).FInitATCommand;
  end;
  inherited;
end;

// 初始化
constructor TCnModem.Create(AOwner: TComponent);
begin
  inherited;
  FCheckDialtone := True;
  FCheckBusy := True;
  FAutoAnswer := False;
  FVolume := mvMiddle;
  FWaitDialtoneTime := 2;
  FWaitCarrierTime := 50;
  FWaitEscapeTime := 50;
  FInitATCommand := '';
  FModemState := msOffline;
  FWaitATResult := False;
  FATResult := '';
  FConnectRate := 0;
  CommConfig.Outx_CtsFlow := True;
  CommConfig.Outx_DsrFlow := True;
end;

// 释放
destructor TCnModem.Destroy;
begin
  Hangup;
  inherited;
end;

// 通讯状态
function TCnModem.CommOpened: Boolean;
begin
  Result := Handle <> 0;
end;

// 打开串口，返回成功标记
function TCnModem.OpenComm: Boolean;
begin
  Result := CommOpened;
  if not Result then
  begin
    try
      StartComm;
      Result := True;
    except
      Exit;
    end;
  end;
end;

// 属性已变更
procedure TCnModem.Changed;
begin
  if (ComponentState * [csDesigning, csLoading, csDestroying] = [])
    and CommOpened then
    InitModem;
end;

// 发送数据缓冲区空
procedure TCnModem._SendDataEmpty;
begin
  if ModemState = msOnline then // 仅在离线状态下产生事件
    inherited;
end;

// 字符串转换为整数
function TCnModem.StrToIntEx(const Str: string): Integer;
var
  SInt: string;
  I: Integer;
begin
  SInt := '';
  for I := 1 to Length(Str) do
    if {$IFDEF UNICODE}CharInSet(Str[I], ['0'..'9']){$ELSE}Str[I] in ['0'..'9']{$ENDIF} then // 仅取数字字符
      SInt := SInt + Str[I];
  if SInt <> '' then
    Result := StrToInt(SInt)
  else
    Result := 0;
end;

// 发送 AT 命令到串口
procedure TCnModem.WriteATCommand(const Command: string; Return: Boolean);
var
  S: AnsiString;
begin
  if (csDesigning in ComponentState) or not CommOpened then
    Exit;
  if Return then
    S := {$IFDEF UNICODE}AnsiString{$ENDIF}(Command) + #13
  else
    S := {$IFDEF UNICODE}AnsiString{$ENDIF}(Command);
  WriteCommData(PAnsiChar(S), Length(S));
end;

// 等待一条 AT 命令执行结果
function TCnModem.WaitATResult(Delay: Cardinal): string;
var
  Tick: Cardinal;
begin
  FWaitATResult := True;
  try
    FATResult := '';
    Tick := GetTickCount;
    while (GetTickCount - Tick < Delay) and (FATResult = '') do
      Application.HandleMessage;
    Result := FATResult;
    FATResult := '';
  finally
    FWaitATResult := False;
  end;
end;

// 发送一条 AT 命令，返回是否成功
function TCnModem.SendATOk(AT: string; Delay: Cardinal): Boolean;
var
  I, J: Integer;
  S: string;
begin
  Result := False;
  for I := 0 to 2 do
  begin
    WriteATCommand(AT);
    for J := 0 to 2 do
    begin
      S := Trim(UpperCase(WaitATResult(Delay)));
      if Pos('OK', S) > 0 then
      begin
        Result := True;
        Exit;
      end
      else if Pos('ERROR', S) > 0 then
      begin
        InvalidCommand(AT);
        Exit;
      end;
    end;
  end;
end;

// 接收到数据
procedure TCnModem.ReceiveData(Buffer: PAnsiChar; BufferLength: WORD);
var
  S: AnsiString;
begin
  if FWaitATResult then       // 正在等待 AT 命令执行结果
  begin
    FATResult := {$IFDEF UNICODE}String{$ENDIF}(Buffer);
    Exit;
  end;
  S := Buffer;
  S := {$IFDEF UNICODE}AnsiString{$ENDIF}(Trim(UpperCase({$IFDEF UNICODE}String{$ENDIF}(S))));
  if (ModemState in [msOffline, msOnlineCommand, msConnecting]) and (S = 'RING') then
    Ring                      // 振铃信号
  else if (ModemState = msOnline) and (S = 'NO CARRIER') then
    DisConnect                // 载波丢失
  else
    inherited;
end;

// 拨号
function TCnModem.Dial(const Number: string): TDialResult;
var
  S: string;
begin
  if not OpenComm then
  begin
    Result := drOpenCommFail;
    Exit;
  end;
  Result := drNoModem;
  if InitModem then
  begin
    WriteATCommand('ATD' + Number);
    ModemState := msConnecting;
    S := Trim(UpperCase(WaitATResult(Round(WaitCarrierTime * 1000 * 1.2))));
    if Pos('CONNECT', S) > 0 then
    begin
      Result := drConnect;
      FConnectRate := StrToIntEx(S);
      ModemState := msOnline;
      Exit;
    end;
    if Pos('NO DIALTONE', S) > 0 then
      Result := drNoDialtone
    else if Pos('BUSY', S) > 0 then
      Result := drBusy
    else if Pos('NO CARRIER', S) > 0 then
      Result := drNoCarrier
    else if Pos('NO ANSWER', S) > 0 then
      Result := drNoAnswer
    else if S = '' then
      Result := drTimeout
    else
      Result := drUnknow;
    ModemState := msOffline;
  end;
end;

// 应答
function TCnModem.Answer: TDialResult;
var
  S: string;
begin
  Result := drUnknow;
  if CommOpened and (ModemState = msOffline) then
  begin
    WriteATCommand('ATA');
    ModemState := msConnecting;
    S := Trim(UpperCase(WaitATResult(Round(WaitCarrierTime * 1000 * 1.2))));
    if Pos('CONNECT', S) > 0 then
    begin
      FConnectRate := StrToIntEx(S);
      ModemState := msOnline;
      Connect(FConnectRate);
      Result := drConnect;
      Exit;
    end;
    if Pos('NO DIALTONE', S) > 0 then
      Result := drNoDialtone
    else if Pos('BUSY', S) > 0 then
      Result := drBusy
    else if Pos('NO CARRIER', S) > 0 then
      Result := drNoCarrier
    else if Pos('NO ANSWER', S) > 0 then
      Result := drNoAnswer
    else if S = '' then
      Result := drTimeout
    else
      Result := drUnknow;
    ModemState := msOffline;
  end;
end;

// 切换到在线命令状态
procedure TCnModem.Escape;
var
  Tick: Integer;
begin
  if CommOpened and (ModemState = msOnline) then
  begin
    Tick := Round(FWaitEscapeTime * 20 * 1.3);
    Sleep(Tick);
    WriteATCommand('+++', False);
    Sleep(Tick);
    ModemState := msOnlineCommand;
  end;
end;

// 回到在线状态
procedure TCnModem.Resume;
begin
  if CommOpened and (ModemState = msOnlineCommand) then
  begin
    if SendATOk('ATO') then
      ModemState := msOnline
    else
      Hangup;
  end;
end;

// 挂机
procedure TCnModem.Hangup;
begin
  if CommOpened then
  begin
    Escape;
    WriteATCommand('ATH');
    Sleep(1000);
    ModemState := msOffline;
    StopComm;
  end;
end;

// 初始化 Modem
function TCnModem.InitModem: Boolean;
const
  AutoAnswers: array[Boolean] of Integer = (0, 1);
  Checks: array[Boolean, Boolean] of Integer = ((0, 2), (3, 4));
begin
  Result := False;
  if not OpenComm then
    Exit;
  if ModemState <> msOffline then
    Hangup;
  if not SendATOk('ATQ0E0V1') then Exit; //命令不回显，以字符形式显示结果码
  if not SendATOk('ATX' + IntToStr(Checks[CheckDialtone, CheckBusy])) then Exit;
  if not SendATOk('ATL' + IntToStr(Ord(FVolume))) then Exit;
  if not SendATOk('ATS0=' + IntToStr(AutoAnswers[AutoAnswer])) then Exit;
  if not SendATOk('ATS6=' + IntToStr(WaitDialtoneTime)) then Exit;
  if not SendATOk('ATS7=' + IntToStr(WaitCarrierTime)) then Exit;
  if not SendATOk('ATS12=' + IntToStr(WaitEscapeTime)) then Exit;
  Result := True;
  if InitATCommand <> '' then
    SendATOk(InitATCommand);
end;

// 非法AT命令
procedure TCnModem.InvalidCommand(const Command: string);
begin
  if Assigned(FOnInvalidCommand) then
    FOnInvalidCommand(Self, Command);
end;

// 已连接
procedure TCnModem.Connect(Rate: Integer);
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self, Rate);
end;

// 连接中断
procedure TCnModem.DisConnect;
begin
  if Assigned(FOnDisConnect) then
    FOnDisConnect(Self);
end;

// 振铃事件
procedure TCnModem.Ring;
var
  Ans: Boolean;
begin
  Ans := True;
  if Assigned(FOnRing) then
    FOnRing(Self, Ans);
  if not AutoAnswer and Ans then
    Answer;
end;

// 设置 Modem 状态
procedure TCnModem.SetModemState(const Value: TModemState);
begin
  if FModemState <> Value then
  begin
    FModemState := Value;
    if Assigned(FOnStateChange) then
      FOnStateChange(Self, FModemState);
  end;
end;

// 设置自动应答
procedure TCnModem.SetAutoAnswer(const Value: Boolean);
begin
  if FAutoAnswer <> Value then
  begin
    FAutoAnswer := Value;
    Changed;
  end;
end;

// 设置音量
procedure TCnModem.SetVolume(const Value: TModemVolume);
begin
  if FVolume <> Value then
  begin
    FVolume := Value;
    Changed;
  end;
end;

// 设置初始化AT命令
procedure TCnModem.SetInitATCommand(const Value: string);
begin
  if FInitATCommand <> Value then
  begin
    FInitATCommand := UpperCase(Trim(Value));
    if Pos('AT', FInitATCommand) <> 1 then
      FInitATCommand := 'AT' + FInitATCommand;
    if FInitATCommand = 'AT' then
      FInitATCommand := '';
    Changed;
  end;
end;

// 设置等待载波时间
procedure TCnModem.SetWaitCarrierTime(const Value: Integer);
begin
  if FWaitCarrierTime <> Value then
  begin
    FWaitCarrierTime := Value;
    Changed;
  end;
end;

// 设置等待拨号音时间
procedure TCnModem.SetWaitDialtoneTime(const Value: Integer);
begin
  if FWaitDialtoneTime <> Value then
  begin
    FWaitDialtoneTime := Value;
    Changed;
  end;
end;

// 设置切换到在线命令状态的等待时间
procedure TCnModem.SetWaitEscapeTime(const Value: Integer);
begin
  if FWaitEscapeTime <> Value then
  begin
    FWaitEscapeTime := Value;
    Changed;
  end;
end;

// 设置检测忙音
procedure TCnModem.SetCheckBusy(const Value: Boolean);
begin
  if FCheckBusy <> Value then
  begin
    FCheckBusy := Value;
    Changed;
  end;
end;

// 设置检测拨号音
procedure TCnModem.SetCheckDialtone(const Value: Boolean);
begin
  if FCheckDialtone <> Value then
  begin
    FCheckDialtone := Value;
    Changed;
  end;
end;

const
  csCheckDialtone = 'CheckDialtone';
  csCheckBusy = 'CheckBusy';
  csAutoAnswer = 'AutoAnswer';
  csWaitEscapeTime = 'WaitEscapeTime';
  csWaitDialtoneTime = 'WaitDialtoneTime';
  csWaitCarrierTime = 'WaitCarrierTime';
  csInitATCommand = 'InitATCommand';

// 从 INI 中读参数
procedure TCnModem.ReadFromIni(Ini: TCustomIniFile;
  const Section: string);
begin
  inherited;
  FCheckDialtone := Ini.ReadBool(Section, csCheckDialtone, FCheckDialtone);
  FCheckBusy := Ini.ReadBool(Section, csCheckBusy, FCheckBusy);
  FAutoAnswer := Ini.ReadBool(Section, csAutoAnswer, FAutoAnswer);
  FWaitEscapeTime := Ini.ReadInteger(Section, csWaitEscapeTime, FWaitEscapeTime);
  FWaitDialtoneTime := Ini.ReadInteger(Section, csWaitDialtoneTime, FWaitDialtoneTime);
  FWaitCarrierTime := Ini.ReadInteger(Section, csWaitCarrierTime, FWaitCarrierTime);
  FInitATCommand := Ini.ReadString(Section, csInitATCommand, FInitATCommand);
  FInitATCommand := UpperCase(Trim(FInitATCommand));
  if Pos('AT', FInitATCommand) <> 1 then
    FInitATCommand := 'AT' + FInitATCommand;
  if FInitATCommand = 'AT' then
    FInitATCommand := '';
end;

// 写参数到 INI
procedure TCnModem.WriteToIni(Ini: TCustomIniFile; const Section: string);
begin
  inherited;
  Ini.WriteBool(Section, csCheckDialtone, FCheckDialtone);
  Ini.WriteBool(Section, csCheckBusy, FCheckBusy);
  Ini.WriteBool(Section, csAutoAnswer, FAutoAnswer);
  Ini.WriteInteger(Section, csWaitEscapeTime, FWaitEscapeTime);
  Ini.WriteInteger(Section, csWaitDialtoneTime, FWaitDialtoneTime);
  Ini.WriteInteger(Section, csWaitCarrierTime, FWaitCarrierTime);
  Ini.WriteString(Section, csInitATCommand, FInitATCommand);
end;

// 取组件注释
procedure TCnModem.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnModemName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnModemComment;
end;

end.

