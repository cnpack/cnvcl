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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnPing;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：Ping 通讯单元
* 单元作者：胡昌洪Sesame (sesamehch@163.com)
* 备    注：定义了 TCnPing
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.04.04 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF MSWINDOWS} Windows, WinSock, {$ELSE}
  Posix.ArpaInet, Posix.NetinetIn, Posix.SysSocket, {$ENDIF}
  SysUtils, Classes, Controls, StdCtrls,
  CnClasses, CnConsts, CnNetConsts, CnNetwork, CnSocket;

type
  PCnIPOptionInformation = ^TCnIPOptionInformation;
  TCnIPOptionInformation = packed record
    TTL: Byte;              // Time To Live (used for traceroute)
    TOS: Byte;              // Type Of Service (usually 0)
    Flags: Byte;            // IP header flags (usually 0)
    OptionsSize: Byte;      // Size of options data (usually 0, max 40)
    OptionsData: PAnsiChar; // Options data buffer
  end;

  PCnIcmpEchoReply = ^TCnIcmpEchoReply;
  TCnIcmpEchoReply = packed record
    Address: Cardinal; // replying address
    Status: Cardinal;  // IP status value (see below)
    RTT: Cardinal;     // Round Trip Time in milliseconds
    DataSize: Word;    // reply data size
    Reserved: Word;
    Data: Pointer;     // pointer to reply data buffer
    Options: TCnIPOptionInformation; // reply options
  end;

  TCnIpInfo = record
    Address: Int64;
    IP: string;
    Host: string;
  end;

  TOnPingReceive = procedure(Sender: TComponent; IpAddr, HostName: string;
    TTL, TOS: Byte) of object;

  TOnPingError = procedure(Sender: TComponent; IpAddr, HostName: string;
    TTL, TOS: Byte; ErrorMsg: string) of object;

//==============================================================================
// Ping 通讯类
//==============================================================================

  { TCnPing }

  TCnPing = class(TCnComponent)
  {* 通过调用 ICMP.DLL 库中的函数来实现 Ping 功能。}
  private
    FRemoteHost: string;
    FRemoteIP: string;
    FIPAddress: Int64;
    FTTL: Byte;
    FTimeOut: Cardinal;
    FPingCount: Integer;
    FDelay: Integer;
    FOnError: TOnPingError;
    FOnReceived: TOnPingReceive;
    FDataString: string;
{$IFDEF MSWINDOWS}
    FHICMP: THandle;
    FWSAData: TWSAData;
{$ENDIF}
    FIP: TCnIpInfo;

    procedure SetPingCount(const Value: Integer);
    procedure SetRemoteHost(const Value: string);
    procedure SetTimeOut(const Value: Cardinal);
    procedure SetTTL(const Value: Byte);
    procedure SetDataString(const Value: string);
    procedure SetRemoteIP(const Value: string);
    function PingIP_Host(const aIP: TCnIpInfo; const Data; Count: Cardinal;
      var aReply: string): Integer;
    {* 以设定的数据 Data (无类型缓冲区) Ping 一次并返回结果。Count 表示数据长度
      返回值为 SCN_ICMP_ERROR_* 系列常量 }
    function GetReplyString(aResult: Integer; aIP: TCnIpInfo;
      pIPE: PCnIcmpEchoReply): string;
    {* 返回结果字符串。}
    function GetDataString: string;
    function GetIPByName(const aName: string; var aIP: string): Boolean;
    {* 通过机器名称获取 IP 地址}
    function SetIP(aIPAddr, aHost: string; var aIP: TCnIpInfo): Boolean;
    {* 通过机器名称或 IP 地址填充完整 IP 信息}
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Ping(var aReply: string): Boolean;
    {* 进行循环 Ping，循环次数在 PingCount 属性中指定。}
    function PingOnce(var aReply: string): Boolean; overload;
    {* 以设定的数据 Ping 一次并返回结果。}
    function PingOnce(const aIP: string; var aReply: string): Boolean; overload;
    {* 向指定 IP 进行一次 Ping 并返回结果。}
    function PingFromBuffer(var Buffer; Count: Integer; var aReply: string):
      Boolean;
    {* 以参数 Buffer 的数据 Ping 一次并读取返回结果。}
  published
    property RemoteIP: string read FRemoteIP write SetRemoteIP;
    {* 要 Ping 的目标主机地址，只支持 IP}
    property RemoteHost: string read FRemoteHost write SetRemoteHost;
    {* 要 Ping 的目标主机名，有主机名存在时会覆盖 RemoteIP 的内容}
    property PingCount: Integer read FPingCount write SetPingCount default 4;
    {* 调用 Ping 方法时进行多少次数据发送，默认是 4 次。}
    property Delay: Integer read FDelay write FDelay default 0;
    {* 相邻两次 Ping 间的时间间隔，单位毫秒，默认 0 也就是不延时}
    property TTL: Byte read FTTL write SetTTL;
    {* 设置的 TTL 值，Time to Live}
    property TimeOut: Cardinal read FTimeOut write SetTimeOut;
    {* 设置的超时值}
    property DataString: string read GetDataString write SetDataString;
    {* 欲发送的数据，以字符串形式表示，默认为"CnPack Ping"。}
    property OnReceived: TOnPingReceive read FOnReceived write FOnReceived;
    {* Ping 一次成功时返回数据所触发的事件}
    property OnError: TOnPingError read FOnError write FOnError;
    {* Ping 出错时返回的内容和信息。包括目的未知、不可达、超时等。}
  end;

implementation

{$R-}

uses
  CnIP;

const
  SCnPingData = 'CnPack Ping.';

  SCN_ICMP_ERROR_OK         = 0;
  SCN_ICMP_ERROR_BAD_ADDR   = -1;
  SCN_ICMP_ERROR_TIME_OUT   = -2;
  SCN_ICMP_ERROR_GENERAL    = -3;
  SCN_ICMP_ERROR_SOCKET     = -4;
  SCN_ICMP_ERROR_UNKNOWN    = -100;

{$IFDEF MSWINDOWS}
  ICMPDLL = 'icmp.dll';

type

//==============================================================================
// 辅助过程  从icmp.dll导入的函数
//==============================================================================

  TIcmpCreateFile = function (): THandle; stdcall;

  TIcmpCloseHandle = function (IcmpHandle: THandle): Boolean; stdcall;

  TIcmpSendEcho = function (IcmpHandle: THandle;
                            DestAddress: DWORD;
                            RequestData: Pointer;
                            RequestSize: Word;
                            RequestOptions: PCnIPOptionInformation;
                            ReplyBuffer: Pointer;
                            ReplySize: DWORD;
                            TimeOut: DWORD): DWORD; stdcall;

var
  IcmpCreateFile: TIcmpCreateFile = nil;
  IcmpCloseHandle: TIcmpCloseHandle = nil;
  IcmpSendEcho: TIcmpSendEcho = nil;

  IcmpDllHandle: THandle = 0;

procedure InitIcmpFunctions;
begin
  IcmpDllHandle := LoadLibrary(ICMPDLL);
  if IcmpDllHandle <> 0 then
  begin
    @IcmpCreateFile := GetProcAddress(IcmpDllHandle, 'IcmpCreateFile');
    @IcmpCloseHandle := GetProcAddress(IcmpDllHandle, 'IcmpCloseHandle');
    @IcmpSendEcho := GetProcAddress(IcmpDllHandle, 'IcmpSendEcho');
  end;
end;

procedure FreeIcmpFunctions;
begin
  if IcmpDllHandle <> 0 then
    FreeLibrary(IcmpDllHandle);
end;

{$ENDIF}

//==============================================================================
// Ping 通讯类
//==============================================================================

{ TCnPing }

constructor TCnPing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF MSWINDOWS}
  if IcmpDllHandle = 0 then
    InitIcmpFunctions;
{$ENDIF}

  FRemoteIP := '127.0.0.1';
  FTTL := 64;
  FPingCount := 4;
  FDelay := 0;
  FTimeOut := 10;
  FDataString := SCnPingData;

{$IFDEF MSWINDOWS}
  FHICMP := IcmpCreateFile(); // 取得 DLL 句柄
  if FHICMP = INVALID_HANDLE_VALUE then
    raise Exception.Create(SICMPRunError);
{$ENDIF}
end;

destructor TCnPing.Destroy;
begin
{$IFDEF MSWINDOWS}
  if FHICMP <> INVALID_HANDLE_VALUE then
    IcmpCloseHandle(FHICMP);
{$ENDIF}
  inherited Destroy;
end;

procedure TCnPing.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnPingName;
  Author := SCnPack_Sesame;
  Email := SCnPack_SesameEmail;
  Comment := SCnPingComment;
end;

procedure TCnPing.SetPingCount(const Value: Integer);
begin
  if Value > 0 then
    FPingCount := Value;
end;

procedure TCnPing.SetRemoteIP(const Value: string);
begin
  if FRemoteIP <> Value then
  begin
    FRemoteIP := Value;
    if SetIP(FRemoteIP, '', FIP) then
    begin
      FRemoteHost := FIP.Host;
      FIPAddress := FIP.Address;
    end;
  end;
end;

procedure TCnPing.SetRemoteHost(const Value: string);
begin
  if FRemoteHost <> Value then
  begin
    // RemoteHost 更改的话，RemoteIP 自动清空
    FRemoteHost := Value;
    if SetIP('', FRemoteHost, FIP) then
    begin
      FRemoteIP := FIP.IP;
      FIPAddress := FIP.Address;
    end;
  end;
end;

procedure TCnPing.SetTimeOut(const Value: Cardinal);
begin
  FTimeOut := Value;
end;

procedure TCnPing.SetTTL(const Value: Byte);
begin
  FTTL := Value;
end;

procedure TCnPing.SetDataString(const Value: string);
begin
  FDataString := Value;
end;

function TCnPing.GetDataString: string;
begin
  if FDataString = '' then
    FDataString := SCnPingData;
  Result := FDataString;
end;

function TCnPing.Ping(var aReply: string): Boolean;
var
  iCount, iResult: Integer;
  sReply: string;
begin
  aReply := '';
  iResult := 0;
  try
    SetIP(RemoteIP, RemoteHost, FIP);
    for iCount := 1 to PingCount do
    begin
      iResult := PingIP_Host(FIP, Pointer(FDataString)^, Length(DataString) * SizeOf(Char),
        sReply);
      aReply := aReply + #13#10 + sReply;
      if iResult < 0 then
        Break;

      if FDelay > 0 then
        Sleep(FDelay);
    end;
  finally
    Result := iResult >= 0;
  end;
end;

function TCnPing.PingOnce(var aReply: string): Boolean;
begin
  SetIP(RemoteIP, RemoteHost, FIP);
  Result := PingIP_Host(FIP, Pointer(FDataString)^, Length(DataString) * SizeOf(Char),
    aReply) >= 0;
end;

function TCnPing.PingOnce(const aIP: string; var aReply: string): Boolean;
begin
  SetIP(aIP, aIP, FIP);
  Result := PingIP_Host(FIP, Pointer(FDataString)^, Length(DataString) * SizeOf(Char),
    aReply) >= 0;
end;

function TCnPing.PingFromBuffer(var Buffer; Count: Integer;
  var aReply: string): Boolean;
begin
  SetIP(RemoteIP, RemoteHost, FIP);
  Result := PingIP_Host(FIP, Buffer, Count, aReply) >= 0;
end;

function TCnPing.PingIP_Host(const aIP: TCnIpInfo; const Data;
  Count: Cardinal; var aReply: string): Integer;
{$IFDEF MSWINDOWS}
var
  IPOpt: TCnIPOptionInformation; // 发送数据结构
  pReqData, pRevData: PAnsiChar;
  pCIER: PCnIcmpEchoReply;
{$ENDIF}
begin
  Result := SCN_ICMP_ERROR_UNKNOWN;

  if Count <= 0 then
  begin
    aReply := GetReplyString(Result, aIP, nil);
    Exit;
  end;
  if aIP.Address = INADDR_NONE then
  begin
    Result := SCN_ICMP_ERROR_BAD_ADDR;
    aReply := GetReplyString(Result, aIP, nil);
    Exit;
  end;

{$IFDEF MSWINDOWS}
  pReqData := nil;
  GetMem(pCIER, SizeOf(TCnICMPEchoReply) + Count);
  GetMem(pRevData, Count);
  try
    FillChar(pCIER^, SizeOf(TCnICMPEchoReply) + Count, 0); // 初始化接收数据结构
    pCIER^.Data := pRevData;
    GetMem(pReqData, Count);
    Move(Data, pReqData^, Count); // 准备发送的数据
    FillChar(IPOpt, Sizeof(IPOpt), 0); // 初始化发送数据结构
    IPOpt.TTL := FTTL;

    try // Ping开始
      if WSAStartup(MAKEWORD(2, 0), FWSAData) <> 0 then
        raise Exception.Create(SInitFailed);

      if IcmpSendEcho(FHICMP, // dll handle
        aIP.Address, // target
        pReqData,    // data
        Count,       // data length
        @IPOpt,      // addree of ping option
        pCIER,
        SizeOf(TCnICMPEchoReply) + Count, // pack size
        FTimeOut     // timeout value
        ) <> 0 then
      begin
        Result := SCN_ICMP_ERROR_OK; // Ping 正常返回
        if Assigned(FOnReceived) then
          FOnReceived(Self, aIP.IP, aIP.Host, IPOpt.TTL, IPOpt.TOS);
      end
      else
      begin
        Result := SCN_ICMP_ERROR_TIME_OUT; // 没有响应
        if Assigned(FOnError) then
          FOnError(Self, aIP.IP, aIP.Host, IPOpt.TTL, IPOpt.TOS, SNoResponse);
      end;
    except
      on E: Exception do
      begin
        Result := SCN_ICMP_ERROR_GENERAL; // 发生错误
        if Assigned(FOnError) then
          FOnError(Self, aIP.IP, aIP.Host, IPOpt.TTL, IPOpt.TOS, E.Message);
      end;
    end;
  finally
    WSACleanUP;

    aReply := GetReplyString(Result, aIP, pCIER);
    if pRevData <> nil then
    begin
      FreeMem(pRevData); // 释放内存
      pCIER.Data := nil;
    end;
    if pReqData <> nil then
      FreeMem(pReqData); //释放内存
    FreeMem(pCIER);      //释放内存
  end;
{$ELSE}
  // TODO: POSIX sendto Ping and recvfrom
  raise Exception.Create('NOT Implemented.');
{$ENDIF}
end;

function TCnPing.GetReplyString(aResult: Integer; aIP: TCnIpInfo;
  pIPE: PCnIcmpEchoReply): string;
var
  sHost: string;
begin
  Result := SInvalidAddr;
  case aResult of
    SCN_ICMP_ERROR_UNKNOWN: Result := SICMPRunError;
    SCN_ICMP_ERROR_BAD_ADDR: Result := SInvalidAddr;
    SCN_ICMP_ERROR_TIME_OUT: Result := Format(SNoResponse, [RemoteHost]);
    else
      if pIPE <> nil then
      begin
        sHost := aIP.IP;
        if aIP.Host <> '' then
          sHost := aIP.Host + ': ' + sHost;
        Result := (Format(SPingResultString, [sHost, pIPE^.DataSize, pIPE^.RTT,
          pIPE^.Options.TTL]));
      end;
  end;
end;

function TCnPing.GetIPByName(const aName: string; var aIP: string): Boolean;
begin
  Result := TCnIp.GetIPByName(aIP, aName);
end;

function TCnPing.SetIP(aIPAddr, aHost: string; var aIP: TCnIpInfo): Boolean;
var
  pIPAddr: PAnsiChar;
begin
  Result := False;
  aIP.Address := INADDR_NONE;
  aIP.IP := aIPAddr;
  aIP.Host := aHost;
  if aIP.IP = '' then
  begin
    if (aIP.Host = '') or (not GetIPByName(aIP.Host, aIP.IP)) then
      Exit;
  end;

  GetMem(pIPAddr, Length(aIP.IP) + 1);
  try
    FillChar(pIPAddr^, Length(aIP.IP) + 1, 0);
    StrPCopy(pIPAddr, {$IFDEF UNICODE}AnsiString{$ENDIF}(aIP.IP));
    aIP.Address := inet_addr(PAnsiChar(pIPAddr)); // IP转换成无点整型
  finally
    FreeMem(pIPAddr); // 释放申请的动态内存
  end;
  Result := aIP.Address <> INADDR_NONE;
end;

{$IFDEF MSWINDOWS}

initialization

finalization
  FreeIcmpFunctions;

{$ENDIF}

end.
