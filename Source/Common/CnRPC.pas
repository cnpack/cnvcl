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

unit CnRPC;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：JSON-RPC 2.0 解析与组装单元
* 单元作者：CnPack 开发组
* 备    注：基于 CnJSON 的 JSON-RPC 2.0 的数据包解析与组装单元
* 开发平台：PWin7 + Delphi 7
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.06.14 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

{$I CnPack.inc}

interface

uses
  SysUtils, Classes, Contnrs, CnJSON;

const
  CN_JSON_RPC_ERROR_PARSE_ERROR        = -32700;
  CN_JSON_RPC_ERROR_INVALID_REQUEST    = -32600;
  CN_JSON_RPC_ERROR_METHOD_NOT_FOUND   = -32601;
  CN_JSON_RPC_ERROR_INVALID_PARAMS     = -32602;
  CN_JSON_RPC_ERROR_INTERNAL_ERROR     = -32603;

  CN_JSON_RPC_ERROR_SERVER_ERROR_BEGIN = -32000;
  CN_JSON_RPC_ERROR_SERVER_ERROR_END   = -32099;

type
  TCnJSONRPCBase = class(TPersistent)
  {* JSON-RPC 数据包基类}
  private
    FVersion: string;
    FID: Integer;
  protected
    procedure DoToJSON(Root: TCnJSONObject); virtual; abstract;
    {* 子类添加要输出的内容供最终转换成 JSON}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString;
    {* 生成 UTF8 格式的 JSON 字符串}

    property Version: string read FVersion write FVersion;
    {* 版本号字符串，默认 2.0}
    property ID: Integer read FID write FID;
    {* 标识，用于关联请求和回应}
  end;

  TCnJSONRPCRequest = class(TCnJSONRPCBase)
  {* JSON-RPC 请求类}
  private
    FMethod: string;
    FParams: TCnJSONValue;
    procedure SetParams(const Value: TCnJSONValue);
  protected
    procedure DoToJSON(Root: TCnJSONObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Method: string read FMethod write FMethod;
    {* 调用的方法名}
    property Params: TCnJSONValue read FParams write SetParams;
    {* 参数，可以是 TCnJSONObject}
  end;

  TCnJSONRPCResponse = class(TCnJSONRPCBase)
  {* JSON-RPC 回应类}
  private
    FRPCResult: TCnJSONValue;
    procedure SetRPCResult(const Value: TCnJSONValue);
  protected
    procedure DoToJSON(Root: TCnJSONObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property RPCResult: TCnJSONValue read FRPCResult write SetRPCResult;
    {* 结果，可以是 TCnJSONObject}
  end;

  TCnJSONRPCError = class(TCnJSONRPCBase)
  {* JSON-RPC 错误类}
  private
    FErrorData: TCnJSONValue;
    FErrorCode: Integer;
    FErrorMessage: string;
    procedure SetErrorData(const Value: TCnJSONValue);
  protected
    procedure DoToJSON(Root: TCnJSONObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property ErrorCode: Integer read FErrorCode write FErrorCode;
    {* 错误码}
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    {* 错误信息}
    property ErrorData: TCnJSONValue read FErrorData write SetErrorData;
    {* 错误数据，可以是 TCnJSONObject}
  end;

  TCnJSONRPCNoficiation = class(TCnJSONRPCBase)
  {* JSON-RPC 通知类}
  private
    FMethod: string;
    FParams: TCnJSONValue;
    procedure SetParams(const Value: TCnJSONValue);
  protected
    procedure DoToJSON(Root: TCnJSONObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Method: string read FMethod write FMethod;
    {* 调用的方法名}
    property Params: TCnJSONValue read FParams write SetParams;
    {* 参数，可以是 TCnJSONObject}
  end;

function CnParseJSONRPC(const JsonStr: AnsiString): TCnJSONRPCBase;
{* 解析 UTF8 格式的 JSON 字符串为一个 JSONRPC 实例，有 method 的为
  Request 或 Notification，有 ID 的为前者否则为后者；有 error 的是错误，
  有 result 的是 Response}

function CnParseJSONRPCs(const JsonStr: AnsiString; RPCs: TObjectList): Boolean;
{* 如果 JSON 字符串是 [ 开头 ] 结尾，则本函数将其以数组方式解析成多个 JSONRPC 对象
  并放入 RPCs 列表中，返回解析是否成功。如果字符串不是以 [ 开头，则返回 False}

implementation

const
  SCN_JSONRPC_VERSION       = 'jsonrpc';
  SCN_JSONRPC_METHOD        = 'method';
  SCN_JSONRPC_PARAMS        = 'params';
  SCN_JSONRPC_RESULT        = 'result';
  SCN_JSONRPC_ID            = 'id';
  SCN_JSONRPC_ERROR         = 'error';
  SCN_JSONRPC_ERRORMESSAGE  = 'message';
  SCN_JSONRPC_ERRORCODE     = 'code';
  SCN_JSONRPC_ERRORDATA     = 'data';

  SCN_JSONRPC_VERSION_VALUE = '2.0';
  CN_INVALID_ID             = -1;

function ParseJSONObjectToRPC(Obj: TCnJSONObject): TCnJSONRPCBase;
var
  Tmp: TCnJSONObject;
begin
  Result := nil;
  if Obj <> nil then
  begin
    if Obj[SCN_JSONRPC_RESULT] <> nil then
    begin
      Result := TCnJSONRPCResponse.Create;
      TCnJSONRPCResponse(Result).RPCResult := Obj[SCN_JSONRPC_RESULT].Clone;
    end
    else if (Obj[SCN_JSONRPC_ERROR] <> nil) and (Obj[SCN_JSONRPC_ERROR] is TCnJSONObject) then
    begin
      Result := TCnJSONRPCError.Create;
      Tmp := TCnJSONObject(Obj[SCN_JSONRPC_ERROR]);

      if Tmp[SCN_JSONRPC_ERRORCODE] <> nil then
        TCnJSONRPCError(Result).ErrorCode := Tmp[SCN_JSONRPC_ERRORCODE].AsInteger;
      if Tmp[SCN_JSONRPC_ERRORMESSAGE] <> nil then
        TCnJSONRPCError(Result).ErrorMessage := Tmp[SCN_JSONRPC_ERRORMESSAGE].AsString;

      if Tmp[SCN_JSONRPC_ERRORDATA] <> nil then
        TCnJSONRPCError(Result).ErrorData := Tmp[SCN_JSONRPC_ERRORDATA].Clone;
    end
    else if Obj[SCN_JSONRPC_METHOD] <> nil then
    begin
      if Obj[SCN_JSONRPC_ID] <> nil then
      begin
        Result := TCnJSONRPCRequest.Create;
        TCnJSONRPCRequest(Result).Method := Obj[SCN_JSONRPC_METHOD].AsString;

        if Obj[SCN_JSONRPC_PARAMS] <> nil then
          TCnJSONRPCRequest(Result).Params := Obj[SCN_JSONRPC_PARAMS].Clone
        else
          TCnJSONRPCRequest(Result).Params := nil;
      end
      else
      begin
        Result := TCnJSONRPCNoficiation.Create;
        TCnJSONRPCNoficiation(Result).Method := Obj[SCN_JSONRPC_METHOD].AsString;

        if Obj[SCN_JSONRPC_PARAMS] <> nil then
          TCnJSONRPCNoficiation(Result).Params := Obj[SCN_JSONRPC_PARAMS].Clone
        else
          TCnJSONRPCNoficiation(Result).Params := nil;
      end;
    end;

    // 设置通用属性
    if Result <> nil then
    begin
      if Obj[SCN_JSONRPC_VERSION] <> nil then
        Result.Version := Obj[SCN_JSONRPC_VERSION].AsString;
      if Obj[SCN_JSONRPC_ID] <> nil then
        Result.ID := Obj[SCN_JSONRPC_ID].AsInteger;
    end;
  end;
end;

function CnParseJSONRPC(const JsonStr: AnsiString): TCnJSONRPCBase;
var
  Obj: TCnJSONObject;
  S: AnsiString;
begin
  Result := nil;
  S := Trim(JsonStr);
  if (Length(S) > 2) and (S[1] = '{') then
  begin
    Obj := CnJSONParse(S);
    try
      Result := ParseJSONObjectToRPC(Obj);
    finally
      Obj.Free;
    end;
  end;  
end;

function CnParseJSONRPCs(const JsonStr: AnsiString; RPCs: TObjectList): Boolean;
var
  I: Integer;
  Arr: TCnJSONArray;
  Rpc: TCnJSONRPCBase;
begin
  Arr := CnJSONParseToArray(JsonStr);
  Result := Arr <> nil;

  if Result and (Arr.Count > 0) then
  begin
    for I := 0 to Arr.Count - 1 do
    begin
      if Arr[I] is TCnJSONObject then
      begin
        Rpc := ParseJSONObjectToRPC(TCnJSONObject(Arr[I]));
        if Rpc <> nil then
          RPCs.Add(Rpc);
      end;
    end;
  end;
end;

{ TCnJSONRPCBase }

constructor TCnJSONRPCBase.Create;
begin
  inherited;
  FVersion := SCN_JSONRPC_VERSION_VALUE;
  FID := CN_INVALID_ID;
end;

destructor TCnJSONRPCBase.Destroy;
begin

  inherited;
end;

function TCnJSONRPCBase.ToJSON(UseFormat: Boolean;
  Indent: Integer): AnsiString;
var
  Root: TCnJSONObject;
begin
  Root := TCnJSONObject.Create;
  try
    Root.AddPair(SCN_JSONRPC_VERSION, FVersion);
    DoToJSON(Root);

    if FID <> CN_INVALID_ID then
      Root.AddPair(SCN_JSONRPC_ID, FID);

    Result := Root.ToJSON(UseFormat, Indent);
  finally
    Root.Free;
  end;
end;

{ TCnJSONRPCError }

constructor TCnJSONRPCError.Create;
begin
  inherited;
  FErrorData := TCnJSONObject.Create;
end;

destructor TCnJSONRPCError.Destroy;
begin
  FreeAndNil(FErrorData);
  inherited;
end;

procedure TCnJSONRPCError.DoToJSON(Root: TCnJSONObject);
var
  Obj: TCnJSONObject;
begin
  Obj := TCnJSONObject.Create;
  Obj.AddPair(SCN_JSONRPC_ERRORCODE, FErrorCode);
  Obj.AddPair(SCN_JSONRPC_ERRORMESSAGE, FErrorMessage);
  Obj.AddPair(SCN_JSONRPC_ERRORDATA, FErrorData.Clone);
  Root.AddPair(SCN_JSONRPC_ERROR, Obj);
end;

procedure TCnJSONRPCError.SetErrorData(const Value: TCnJSONValue);
begin
  if FErrorData <> Value then
  begin
    FreeAndNil(FErrorData);
    FErrorData := Value;
  end;
end;

{ TCnJSONRPCRequest }

constructor TCnJSONRPCRequest.Create;
begin
  inherited;
  FParams := TCnJSONObject.Create;
end;

destructor TCnJSONRPCRequest.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TCnJSONRPCRequest.DoToJSON(Root: TCnJSONObject);
begin
  Root.AddPair(SCN_JSONRPC_METHOD, FMethod);
  Root.AddPair(SCN_JSONRPC_PARAMS, FParams.Clone);
end;

procedure TCnJSONRPCRequest.SetParams(const Value: TCnJSONValue);
begin
  if FParams <> Value then
  begin
    FreeAndNil(FParams);
    FParams := Value;
  end;
end;

{ TCnJSONRPCResponse }

constructor TCnJSONRPCResponse.Create;
begin
  inherited;
  FRPCResult := TCnJSONObject.Create;
end;

destructor TCnJSONRPCResponse.Destroy;
begin
  FreeAndNil(FRPCResult);
  inherited;
end;

procedure TCnJSONRPCResponse.DoToJSON(Root: TCnJSONObject);
begin
  Root.AddPair(SCN_JSONRPC_RESULT, FRPCResult.Clone);
end;

procedure TCnJSONRPCResponse.SetRPCResult(const Value: TCnJSONValue);
begin
  if FRPCResult <> Value then
  begin
    FreeAndNil(FRPCResult);
    FRPCResult := Value;
  end;
end;

{ TCnJSONRPCNoficiation }

constructor TCnJSONRPCNoficiation.Create;
begin
  inherited;
  FParams := TCnJSONObject.Create;
end;

destructor TCnJSONRPCNoficiation.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

procedure TCnJSONRPCNoficiation.DoToJSON(Root: TCnJSONObject);
begin
  Root.AddPair(SCN_JSONRPC_METHOD, FMethod);
  Root.AddPair(SCN_JSONRPC_PARAMS, FParams.Clone);
end;

procedure TCnJSONRPCNoficiation.SetParams(const Value: TCnJSONValue);
begin
  if FParams <> Value then
  begin
    FreeAndNil(FParams);
    FParams := Value;
  end;
end;

end.
