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

unit CnASHostServices;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：ActiveScript Host 服务单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元定义了供 ActiveScript 使用的 Host 公共服务
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2003.10.31
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF WIN32}

{$IFNDEF COMPILER6_UP}
  'Error: This unit can used only for Delphi / C++Builder 6 or up.'
{$ENDIF COMPILER6_UP}

uses
  Windows, SysUtils, Classes, TypInfo, ComObj, CnASIDispatchProxy, CnCommon;

type
  
//==============================================================================
// ActiveScript 公共服务基础类
//==============================================================================

{ ICnASService }

  ICnASService = interface(IActiveScriptInvokable)
  ['{5640170F-9C03-400E-9E46-549A80F3ABDD}']
  end;

{ TCnASService }

  TCnASService = class(TInterfacedObject, IActiveScriptInvokable, ICnASService)
  {* ActiveScript 公共服务基础类，用来提供服务给脚本使用，
     脚本可调用 HostServices.CreateObject('ServiceName', CreateParam) 来使用。}
  public
    constructor Create(const CreateParam: OleVariant); virtual;
    {* 类构造器，CreateParam 为脚本传入的参数}
  end;

  TCnASServiceClass = class of TCnASService;

//==============================================================================
// ActiveScript Host 服务类
//==============================================================================

{ ICnASHostServices }

  ICnASHostServices = interface(IActiveScriptInvokable)
  ['{E5B6A915-69AE-4CB2-BFBE-9411B75F8E49}']
    // 对象服务
    function CreateObject(const ServiceName: string; CreateParam: OleVariant): IUnknown; stdcall;
    {* 构造并返回一个指定标识名称的对象接口，ServiceName 为对象名称，
       CreateParam 为相关的参数}

    // 对话框服务
    function MessageBox(const Text, Caption: string; Flags: Integer): Integer; stdcall;
    {* 消息对话框}
    procedure InfoDlg(const Text: string; const Caption: string = ''); stdcall;
    {* 显示提示窗口}
    function InfoOk(Text: string; Caption: string = ''): Boolean; stdcall;
    {* 显示提示确认窗口}
    procedure ErrorDlg(Text: string; Caption: string = ''); stdcall;
    {* 显示错误窗口}
    procedure WarningDlg(Text: string; Caption: string = ''); stdcall;
    {* 显示警告窗口}
    function QueryDlg(Text: string; DefaultNo: Boolean = False;
      Caption: string = ''): Boolean; stdcall;
    {* 显示询问对话框}
    function InputQuery(const Caption, Prompt: string): string; stdcall;
    {* 显示一个输入窗口，如果取消，返回空字符串}

    { TODO : 扩充其它 Host 公共服务功能。 }
  end;

function GetCnASHostServices: IDispatch;
{* 返回一个支持 ICnASHostServices 和 IDispatch 的接口。
   可供 ActiveScriptSite.AddNamedItem 使用。}

//==============================================================================
// Host 服务类列表相关过程
//==============================================================================

procedure RegisterCnASService(const ServiceName: string;
  const AClass: TCnASServiceClass; IntfTypeInfo: PTypeInfo);
{* 注册一个 TCnASService 服务类引用，每个服务类实现应在该单元的 initialization
   节调用该过程注册相关服务类 }

{$ENDIF}

implementation

{$IFDEF WIN32}

//==============================================================================
// Host 服务类列表相关过程
//==============================================================================

var
  CnASServiceClassList: TStrings = nil; // Host 服务类引用列表
  CnASServiceIntfTypeInfoList: TList = nil; // 实现的接口信息列表

// 注册一个 TCnASServiceClass 服务类引用
procedure RegisterCnASService(const ServiceName: string;
  const AClass: TCnASServiceClass; IntfTypeInfo: PTypeInfo);
begin
  if CnASServiceClassList.IndexOf(ServiceName) < 0 then
  begin
    CnASServiceClassList.AddObject(ServiceName, TObject(AClass));
    CnASServiceIntfTypeInfoList.Add(IntfTypeInfo);
  end;
end;

// 根据服务类名取指定的服务类引用
function GetCnASServiceClass(const ServiceName: string; var IntfTypeInfo:
  PTypeInfo): TCnASServiceClass;
var
  Idx: Integer;
begin
  Idx := CnASServiceClassList.IndexOf(ServiceName);
  if Idx >= 0 then
  begin
    Result := TCnASServiceClass(CnASServiceClassList.Objects[Idx]);
    IntfTypeInfo := CnASServiceIntfTypeInfoList[Idx];
  end
  else
  begin
    Result := nil;
    IntfTypeInfo := nil;
  end;
end;

//==============================================================================
// ActiveScript 公共服务基础类
//==============================================================================

{ TCnASService }

constructor TCnASService.Create(const CreateParam: OleVariant);
begin
  inherited Create;
end;

//==============================================================================
// ActiveScript Host 服务类
//==============================================================================

{ ICnASHostServices }

type
  TCnASHostServices = class(TInterfacedObject, ICnASHostServices)
  public
    // 对象服务
    function CreateObject(const ServiceName: string; CreateParam: OleVariant): IUnknown; stdcall;

    // 对话框服务
    function MessageBox(const Text, Caption: string; Flags: Integer): Integer; stdcall;
    procedure InfoDlg(const Text: string; const Caption: string = ''); stdcall;
    function InfoOk(Text: string; Caption: string = ''): Boolean; stdcall;
    procedure ErrorDlg(Text: string; Caption: string = ''); stdcall;
    procedure WarningDlg(Text: string; Caption: string = ''); stdcall;
    function QueryDlg(Text: string; DefaultNo: Boolean = False;
      Caption: string = ''): Boolean; stdcall;
    function InputQuery(const Caption, Prompt: string): string; stdcall;
  end;

// 返回一个支持 ICnASHostServices 和 IDispatch 的接口。
function GetCnASHostServices: IDispatch;
begin
  Result := GetIDispatchProxy(TCnASHostServices.Create, TypeInfo(ICnASHostServices));
end;

{ TCnASHostServices }

function TCnASHostServices.CreateObject(const ServiceName: string;
  CreateParam: OleVariant): IUnknown;
var
  IntfTypeInfo: PTypeInfo;
  ServiceClass: TCnASServiceClass;
begin
  ServiceClass := GetCnASServiceClass(ServiceName, IntfTypeInfo);
  if Assigned(ServiceClass) then
    Result := GetIDispatchProxy(ServiceClass.Create(CreateParam), IntfTypeInfo)
  else
    Result := nil;
end;

procedure TCnASHostServices.ErrorDlg(Text, Caption: string);
begin
  CnCommon.ErrorDlg(Text, Caption);
end;

procedure TCnASHostServices.InfoDlg(const Text, Caption: string);
begin
  CnCommon.InfoDlg(Text, Caption);
end;

function TCnASHostServices.InfoOk(Text, Caption: string): Boolean;
begin
  Result := CnCommon.InfoOk(Text, Caption);
end;

function TCnASHostServices.InputQuery(const Caption,
  Prompt: string): string;
begin
  CnCommon.CnInputQuery(Caption, Prompt, Result);
end;

function TCnASHostServices.MessageBox(const Text, Caption: string;
  Flags: Integer): Integer;
begin
  Result := Windows.MessageBox(0, PChar(Text), PChar(Caption), Flags);
end;

function TCnASHostServices.QueryDlg(Text: string; DefaultNo: Boolean;
  Caption: string): Boolean;
begin
  Result := CnCommon.QueryDlg(Text, DefaultNo, Caption);
end;

procedure TCnASHostServices.WarningDlg(Text, Caption: string);
begin
  CnCommon.WarningDlg(Text, Caption);
end;

initialization
  CnASServiceClassList := TStringList.Create;
  CnASServiceIntfTypeInfoList := TList.Create;

finalization
  FreeAndNil(CnASServiceClassList);
  FreeAndNil(CnASServiceIntfTypeInfoList);

{$ENDIF}
end.
