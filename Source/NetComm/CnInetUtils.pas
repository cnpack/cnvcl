{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2017 CnPack 开发组                       }
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

unit CnInetUtils;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：使WinInet 封装单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：定义了 TCnHTTP，使用 WinInet 来读取 HTTP 数据
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2005.09.14 V1.1
*                增加 UserAgent 和 Proxy 设置(由 illk 提供)
*           2003.03.09 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, WinInet;

type

//==============================================================================
// 使用 WinInet 读取 HTTP 文件的类
//==============================================================================

{ TCnInet }

  TCnInetProgressEvent = procedure (Sender: TObject; TotalSize, CurrSize: Integer;
    var Abort: Boolean) of object;
  {* 数据下载进度事件
   |<PRE>
     Sender     - 线程对象
     TotalSize  - 总字节数，如果为 -1，表示长度未知
     CurrSize   - 当前完成字节数
     Abort      - 是否中断
   |</PRE>}

  TCnURLInfo = record
    Protocol: string;
    Host: string;
    Port: string;
    PathName: string;
    Username: string;
    Password: string;
  end;

  TCnInetProxyMode = (pmDirect, pmIE, pmProxy);
  {* 使用代理的方式：直连、IE设置、指定代理 }

  TCnInet = class
  {* 使用 WinInet 读取 HTTP(S)/FTP 文件的类。}
  private
    hSession: HINTERNET;
    FAborted: Boolean;
    FGetDataFail: Boolean;
    FOnProgress: TCnInetProgressEvent;
    FUserAgent: string;
    FDecoding: Boolean;
    FDecodingValid: Boolean;
    FProxyServer: string;
    FProxyUserName: string;
    FProxyPassWord: string;
    FHttpRequestHeaders: TStringList;
    FSendTimeOut: Cardinal;
    FConnectTimeOut: Cardinal;
    FReceiveTimeOut: Cardinal;
    FProxyMode: TCnInetProxyMode;
    FNoCookie: Boolean;
    FEncodeUrlPath: Boolean;
    function ParseURL(URL: string; var Info: TCnURLInfo): Boolean;
  protected
    procedure DoProgress(TotalSize, CurrSize: Integer);
    function InitInet: Boolean;
    procedure CloseInet;
    function GetStreamFromHandle(Handle: HINTERNET; TotalSize: Integer;
      Stream: TStream): Boolean;
    function GetHTTPStream(Info: TCnURLInfo; Stream: TStream; APost: TStrings): Boolean;
    function GetFTPStream(Info: TCnURLInfo; Stream: TStream): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    {* 中断当前处理}
    function GetStream(const AURL: string; Stream: TStream; APost: TStrings = nil): Boolean;
    {* 从 AURL 地址读取数据到流 Stream，如果 APost 不为 nil 则执行 Post 调用}
    function GetString(const AURL: string; APost: TStrings = nil): AnsiString;
    {* 从 AURL 地址返回一个字符串，如果 APost 不为 nil 则执行 Post 调用}
    function GetFile(const AURL, FileName: string; APost: TStrings = nil): Boolean;
    {* 从 AURL 地址读取数据保存到文件 FileName，如果 APost 不为 nil 则执行 Post 调用}
    property OnProgress: TCnInetProgressEvent read FOnProgress write FOnProgress;
    {* 数据进度事件}
    property Aborted: Boolean read FAborted;
    {* 是否已被中断}
    property GetDataFail: Boolean read FGetDataFail;
    {* 上一次的数据读取是否成功}

    property Decoding: Boolean read FDecoding write FDecoding default True;
    {* 是否支持 gzip, deflate 解压}
    property UserAgent: string read FUserAgent write FUserAgent;
    {* 设置UserAgent 浏览器识别标示}
    property ProxyMode: TCnInetProxyMode read FProxyMode write FProxyMode;
    {* 使用代理的方式}
    property ProxyServer: string read FProxyServer write FProxyServer;
    {* 代理服务器设置: [协议=][协议://]服务器[:端口] 如 127.0.0.1:8080}
    property ProxyUserName: string read FProxyUserName write FProxyUserName;
    {* 代理服务器用户名}
    property ProxyPassWord: string read FProxyPassWord write FProxyPassWord;
    {* 代理服务器用户密码}
    property HttpRequestHeaders: TStringList read FHttpRequestHeaders;
    {* 请求信息头}
    property NoCookie: Boolean read FNoCookie write FNoCookie;
    {* 是否不使用 Cookie，如果需要在 HttpRequestHeaders 中指定 Cookie，应设为 True}
    property EncodeUrlPath: Boolean read FEncodeUrlPath write FEncodeUrlPath default True;
    {* 是否自动为 Url 路径中的特殊字符编码}
    property ConnectTimeOut: Cardinal read FConnectTimeOut write FConnectTimeOut;
    {* 连接超时}
    property SendTimeOut: Cardinal read FSendTimeOut write FSendTimeOut;
    {* 发送超时}
    property ReceiveTimeOut: Cardinal read FReceiveTimeOut write FReceiveTimeOut;
    {* 接收超时}
  end;

  TCnHTTP = class(TCnInet);

  TCnFTP = class(TCnInet);

function EncodeURL(const URL: string): string;
{* 将 URL 中的特殊字符转换成 %XX 的形式}

function CnInet_GetStream(const AURL: string; Stream: TStream; APost: TStrings = nil): Boolean;
function CnInet_GetString(const AURL: string; APost: TStrings = nil): AnsiString;
function CnInet_GetFile(const AURL, FileName: string; APost: TStrings = nil): Boolean;

implementation

const
  csBufferSize = 4096;
  INTERNET_OPTION_HTTP_DECODING = 65;
  SAcceptEncoding = 'Accept-Encoding: gzip,deflate';

function EncodeURL(const URL: string): string;
const
  UnsafeChars = ['*', '#', '%', '<', '>', '+', ' '];
var
  i: Integer;
  InStr, OutStr: AnsiString;
begin
  InStr := AnsiString(URL);
  OutStr := '';
  for i := 1 to Length(InStr) do begin
    if (InStr[i] in UnsafeChars) or (InStr[i] >= #$80) or (InStr[i] < #32) then
      OutStr := OutStr + '%' + AnsiString(IntToHex(Ord(InStr[i]), 2))
    else
      OutStr := OutStr + InStr[i];
  end;
  Result := string(OutStr);
end;

function CnInet_GetStream(const AURL: string; Stream: TStream; APost: TStrings): Boolean;
begin
  with TCnInet.Create do
  try
    Result := GetStream(AURL, Stream, APost);
  finally
    Free;
  end;
end;

function CnInet_GetString(const AURL: string; APost: TStrings): AnsiString;
begin
  with TCnInet.Create do
  try
    Result := GetString(AURL, APost);
  finally
    Free;
  end;
end;

function CnInet_GetFile(const AURL, FileName: string; APost: TStrings): Boolean;
begin
  with TCnInet.Create do
  try
    Result := GetFile(AURL, FileName, APost);
  finally
    Free;
  end;
end;

//==============================================================================
// 使用 WinInet 读取 HTTP 文件的类
//==============================================================================

{ TCnInet }

constructor TCnInet.Create;
begin
  inherited;
  FDecoding := True;
  FUserAgent := 'CnPack Internet Utils';
  FHttpRequestHeaders := TStringList.Create;
  FProxyMode := pmIE;
end;

destructor TCnInet.Destroy;
begin
  CloseInet;
  FHttpRequestHeaders.Free;
  inherited;
end;

procedure TCnInet.CloseInet;
begin
  if hSession <> nil then
  begin
    InternetCloseHandle(hSession);
    hSession := nil;
  end;
end;

function TCnInet.InitInet: Boolean;
var
  Flag: LongBool;
begin
  if hSession = nil then
  begin
    if (FProxyMode <> pmProxy) or (Length(FProxyServer) = 0) then
    begin
      if FProxyMode = pmDirect then
        hSession := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_DIRECT,
          nil, nil, 0)
      else
        hSession := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PRECONFIG,
          nil, nil, 0);
    end
    else
    begin
      hSession := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PROXY,
        PChar(FProxyServer), nil, 0);
      if Length(FProxyUserName) > 0 then
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_USERNAME, PChar(FProxyUserName), Length(FProxyUserName));
      if Length(FProxyPassWord) > 0 then
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_PASSWORD, PChar(FProxyPassWord), Length(FProxyPassWord));
        
      if FConnectTimeOut <> 0 then
        InternetSetOption(hSession, INTERNET_OPTION_CONNECT_TIMEOUT, @FConnectTimeOut, SizeOf(Cardinal));
      if FSendTimeOut <> 0 then
        InternetSetOption(hSession, INTERNET_OPTION_SEND_TIMEOUT, @FSendTimeOut, SizeOf(Cardinal));
      if FReceiveTimeOut <> 0 then
        InternetSetOption(hSession, INTERNET_OPTION_RECEIVE_TIMEOUT, @FReceiveTimeOut, SizeOf(Cardinal));
    end;
    if FDecoding then
    begin
      Flag := True;
      FDecodingValid := InternetSetOption(hSession, INTERNET_OPTION_HTTP_DECODING, PChar(@Flag), SizeOf(Flag));
    end;
  end;
  Result := hSession <> nil;
end;

procedure TCnInet.Abort;
begin
  FAborted := True;
end;

procedure TCnInet.DoProgress(TotalSize, CurrSize: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, TotalSize, CurrSize, FAborted);
end;

function TCnInet.ParseURL(URL: string; var Info: TCnURLInfo): Boolean;
var
  Idx: Integer;
  Buff: string;
  
  function ExtractStr(var ASrc: string; ADelim: string;
    ADelete: Boolean = True): string;
  var
    Idx: Integer;
  begin
    Idx := Pos(ADelim, ASrc);
    if Idx = 0 then
    begin
      Result := ASrc;
      if ADelete then
        ASrc := '';
    end
    else
    begin
      Result := Copy(ASrc, 1, Idx - 1);
      if ADelete then
        ASrc := Copy(ASrc, Idx + Length(ADelim), MaxInt);
    end;
  end;
begin
  Result := False;
  URL := Trim(URL);
  Idx := Pos('://', URL);
  if Idx > 0 then
  begin
    Info.Protocol := Copy(URL, 1, Idx  - 1);
    Delete(URL, 1, Idx + 2);
    if URL = '' then Exit;

    Buff := ExtractStr(URL, '/');
    Idx := Pos('@', Buff);
    Info.Password := Copy(Buff, 1, Idx  - 1);
    if Idx > 0 then Delete(Buff, 1, Idx);

    Info.UserName := ExtractStr(Info.Password, ':');
    if Length(Info.UserName) = 0 then
      Info.Password := '';

    Info.Host := ExtractStr(Buff, ':');
    Info.Port := Buff;
    Info.PathName := URL;
    Result := True;
  end;
end;

function TCnInet.GetStream(const AURL: string; Stream: TStream; APost: TStrings = nil): Boolean;
var
  Info: TCnURLInfo;
begin
  Result := False;
  if not ParseURL(AURL, Info) then
    Exit;

  FAborted := False;
  if not InitInet or FAborted then
    Exit;

  if SameText(Info.Protocol, 'http') or SameText(Info.Protocol, 'https') then
    Result := GetHTTPStream(Info, Stream, APost)
  else if SameText(Info.Protocol, 'ftp') then
    Result := GetFTPStream(Info, Stream);

  if FAborted then
    Result := False;
    
  FGetDataFail := not Result;
end;

function TCnInet.GetStreamFromHandle(Handle: HINTERNET; TotalSize: Integer;
  Stream: TStream): Boolean;
var
  CurrSize, Readed: Cardinal;
  Buf: array[0..csBufferSize - 1] of Byte;
begin
  Result := False;
  CurrSize := 0;
  Readed := 0;
  repeat
    if not InternetReadFile(Handle, @Buf, csBufferSize, Readed) then
      Exit;
    if Readed > 0 then
    begin
      Stream.Write(Buf, Readed);
      Inc(CurrSize, Readed);
      DoProgress(TotalSize, CurrSize);
      if Aborted then Exit;
    end;
  until Readed = 0;
  Result := True;
end;

function TCnInet.GetFTPStream(Info: TCnURLInfo; Stream: TStream): Boolean;
var
  hConnect, hFtp: HINTERNET;
  FindData: TWin32FindData;
  TotalSize: Integer;
begin
  Result := False;
  hConnect := nil;
  hFtp := nil;
  try
    hConnect := InternetConnect(hSession, PChar(Info.Host),
      StrToIntDef(Info.Port, INTERNET_DEFAULT_FTP_PORT),
      PChar(Info.Username), PChar(Info.Password),
      INTERNET_SERVICE_FTP, 0, 0);
    if (hConnect = nil) or FAborted then
      Exit;

    hFtp := FtpFindFirstFile(hConnect, PChar(Info.PathName), FindData,
      INTERNET_FLAG_NEED_FILE, 0);
    if hFtp <> nil then
    begin
      InternetCloseHandle(hFtp);
      TotalSize := FindData.nFileSizeLow;
    end
    else
      TotalSize := -1;

    hFtp := FtpOpenFile(hConnect, PChar(Info.PathName), GENERIC_READ,
      FTP_TRANSFER_TYPE_BINARY, 0);
    if (hFtp = nil) or FAborted then
      Exit;
      
    Result := GetStreamFromHandle(hFtp, TotalSize, Stream);
  finally
    if hFtp <> nil then InternetCloseHandle(hFtp);
    if hConnect <> nil then InternetCloseHandle(hConnect);
  end;
end;

function TCnInet.GetHTTPStream(Info: TCnURLInfo; Stream: TStream; APost: TStrings): Boolean;
var
  IsHttps: Boolean;
  PathName: string;
  hConnect, hRequest: HINTERNET;
  SizeStr: array[0..63] of Char;
  BufLen, Index: Cardinal;
  i: Integer;
  Port: Word;
  Flag: Cardinal;
  Verb, Opt: string;
  POpt: PChar;
  OptLen: Integer;
begin
  Result := False;
  hConnect := nil;
  hRequest := nil;
  try
    IsHttps := SameText(Info.Protocol, 'https');
    if IsHttps then
    begin
      Port := StrToIntDef(Info.Port, INTERNET_DEFAULT_HTTPS_PORT);
      Flag := INTERNET_FLAG_RELOAD or INTERNET_FLAG_SECURE or
        INTERNET_FLAG_IGNORE_CERT_CN_INVALID or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
    end
    else
    begin
      Port := StrToIntDef(Info.Port, INTERNET_DEFAULT_HTTP_PORT);
      Flag := INTERNET_FLAG_RELOAD;
    end;
    if FNoCookie then
      Flag := Flag + INTERNET_FLAG_NO_COOKIES;

    hConnect := InternetConnect(hSession, PChar(Info.Host), Port, nil, nil,
      INTERNET_SERVICE_HTTP, 0, 0);
    if (hConnect = nil) or FAborted then
      Exit;

    if APost <> nil then
    begin
      Verb := 'POST';
      Opt := '';
      for i := 0 to APost.Count - 1 do
        if Opt = '' then
          Opt := EncodeURL(APost[i])
        else
          Opt := Opt + '&' + EncodeURL(APost[i]);
      POpt := PChar(Opt);
      OptLen := Length(Opt);
    end
    else
    begin
      Verb := 'GET';
      POpt := nil;
      OptLen := 0;
    end;

    PathName := Info.PathName;
    if EncodeUrlPath then
      PathName := EncodeURL(PathName);
    hRequest := HttpOpenRequest(hConnect, PChar(Verb), PChar(PathName),
      HTTP_VERSION, nil, nil, Flag, 0);
    if (hRequest = nil) or FAborted then
      Exit;

    if FDecoding and FDecodingValid then
      HttpAddRequestHeaders(hRequest, PChar(SAcceptEncoding),
        Length(SAcceptEncoding), HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);
    for i := 0 to FHttpRequestHeaders.Count - 1 do
      HttpAddRequestHeaders(hRequest, PChar(FHttpRequestHeaders[i]),
        Length(FHttpRequestHeaders[i]), HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);

    if HttpSendRequest(hRequest, nil, 0, POpt, OptLen) then
    begin
      if FAborted then Exit;

      FillChar(SizeStr, SizeOf(SizeStr), 0);
      BufLen := SizeOf(SizeStr);
      Index := 0;
      HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH, @SizeStr, BufLen, Index);
        
      if FAborted then Exit;

      Result := GetStreamFromHandle(hRequest, StrToIntDef(SizeStr, -1), Stream);
    end;
  finally
    if hRequest <> nil then InternetCloseHandle(hRequest);
    if hConnect <> nil then InternetCloseHandle(hConnect);
  end;
end;

function TCnInet.GetString(const AURL: string; APost: TStrings): AnsiString;
var
  Stream: TMemoryStream;
begin
  try
    Stream := TMemoryStream.Create;
    try
      if GetStream(AURL, Stream, APost) then
      begin
        SetLength(Result, Stream.Size);
        Move(Stream.Memory^, PAnsiChar(Result)^, Stream.Size);
      end
      else
        Result := '';
    finally
      Stream.Free;
    end;
  except
    Result := '';
  end;
end;

function TCnInet.GetFile(const AURL, FileName: string; APost: TStrings): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    try
      Stream.Size := 0;
      Result := GetStream(AURL, Stream, APost);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

end.
