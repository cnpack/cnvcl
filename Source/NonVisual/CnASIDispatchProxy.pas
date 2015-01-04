{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2015 CnPack 开发组                       }
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

unit CnASIDispatchProxy;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：ActiveScript Host 对象 IDispatch 代理接口单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2003.07.11
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF COMPILER6_UP}
  'Error: This unit can used only for Delphi / C++Builder 6 or up.'
{$ENDIF COMPILER6_UP}

uses
  Windows, Classes, TypInfo;

type

{$M+}
  IActiveScriptInvokable = interface(IUnknown)
  end;
{$M-}

function GetIDispatchProxy(AItemObject: TObject; IntfTypeInfo: PTypeInfo): IDispatch;

implementation

uses
  Sysutils, ActiveX, Variants, CnASInvoker;

type

{ TIDispatchProxy }

  TIDispatchProxy = class(TInterfacedObject, IDispatch)
  protected
    FObject: TObject;
    FIntf: IUnknown;
    FIntfMD: TIntfMetaData;
    function GetTypeInfoCount(out Count: Integer): hResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): hResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): hResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): hResult;
      stdcall;
  public
    constructor Create(AItemObject: TObject; IntfTypeInfo: PTypeInfo);
    destructor Destroy; override;
    property ItemObject: TObject read FObject write FObject;
  end;

function GetIDispatchProxy(AItemObject: TObject; IntfTypeInfo: PTypeInfo): IDispatch;
begin
  Result := TIDispatchProxy.Create(AItemObject, IntfTypeInfo) as IDispatch;
end;

constructor TIDispatchProxy.Create(AItemObject: TObject; IntfTypeInfo: PTypeInfo);
resourcestring
  SNoInterfaceGUID = 'Class %s does not implement interface GUID %s';
begin
  Assert(Assigned(AItemObject));
  Assert(Assigned(IntfTypeInfo));
  inherited Create;
  FObject := AItemObject;
  GetIntfMetaData(IntfTypeInfo, FIntfMD, True);
  // 保存一个接口引用
  if not FObject.GetInterface(FIntfMD.IID, FIntf) then
    raise Exception.CreateFmt(SNoInterfaceGUID,
      [FObject.ClassName, GuidToString(FIntfMD.IID)]);
end;

destructor TIDispatchProxy.Destroy;
begin
  FIntf := nil;
  inherited;
end;

function TIDispatchProxy.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): hResult;
type
  TDispIDsArray = array[0..0] of TDispID;
  PDispIDsArray = ^TDispIDsArray;
var
  IDs: PDispIDsArray absolute DispIDs;
  i: Integer;
  Name: WideString;
  Id: Integer;
begin
  if NameCount > 1 then
    Result := DISP_E_UNKNOWNNAME
  else if NameCount < 1 then
    Result := E_INVALIDARG
  else
    Result := S_OK;
    
  for i := 0 to NameCount - 1 do
    IDs[i] := DISPID_UNKNOWN;
    
  if NameCount = 1 then
  begin
    Name := PWideChar(Names^);
    //Name := UpperCase(Name);
    Id := GetMethNum(FIntfMD, Name);
    if Id <> 0 then
    begin
      IDs[0] := Id;
    end
    else
    begin
      Result := DISP_E_UNKNOWNNAME;
    end;
  end;
end;

function TIDispatchProxy.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo):
  hResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TIDispatchProxy.GetTypeInfoCount(out Count: Integer): hResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TIDispatchProxy.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): hResult;
var
  DispParams: TDispParams;
  MethEntry: TIntfMethEntry;
  V: OleVariant;
  i: Integer;
  Context: TInvContext;
begin
  if (DispID >= Low(FIntfMD.MDA)) and (DispID <= High(FIntfMD.MDA)) then
  begin
    try
      MethEntry := FIntfMD.MDA[DispID];
      DispParams := TDispParams(Params);
      // 允许后面几个参数使用默认值
      if DispParams.cArgs <= MethEntry.ParamCount then
      begin
        // Var 或 Out 类型的参数不能省略
        for i := DispParams.cArgs to MethEntry.ParamCount - 1 do
          if [pfVar, pfOut] * MethEntry.Params[i].Flags <> [] then
          begin
            Result := DISP_E_BADPARAMCOUNT;
            Exit;
          end;

        Context := TInvContext.Create;
        try
          Context.SetMethodInfo(MethEntry);
          Context.AllocServerData(MethEntry);
          
          // 调用方法前转换 OleVariant 参数为方法参数
          for i := 0 to MethEntry.ParamCount - 1 do
          begin
            // 如果参数转换出错，此处定义出错的参数序号
            PInteger(ArgErr)^ := i;
            // 传进来的参数和定义的顺序相反
            if i < DispParams.cArgs then
              V := OleVariant(DispParams.rgvarg^[DispParams.cArgs - 1 - i])
            else
              V := Null;
            TypeTranslator.CastVariantToNative(MethEntry.Params[i].Info,
              V, Context.GetParamPointer(i));
          end;

          // 调用接口方法
          InterfaceInvoker.Invoke(FObject, FIntfMD, DispID, Context);

          // 调用完成后转换 var 和 out 参数为 OleVariant 参数
          { TODO : JScript 和 VBScript 似乎不支持变量参数？ }
          for i := 0 to DispParams.cArgs - 1 do
            if [pfVar, pfOut] * MethEntry.Params[i].Flags <> [] then
            begin
              // 如果参数转换出错，此处定义出错的参数序号
              PInteger(ArgErr)^ := i;
              // 传进来的参数和定义的顺序相反
              TypeTranslator.CastNativeToVariant(MethEntry.Params[i].Info,
                V, Context.GetParamPointer(i));
              OleVariant(DispParams.rgvarg^[DispParams.cArgs - 1 - i]) := V;
            end;

          PInteger(ArgErr)^ := 0;
          
          // 返回方法执行结果
          if Assigned(MethEntry.ResultInfo) and Assigned(VarResult) then
          begin
            TypeTranslator.CastNativeToVariant(MethEntry.ResultInfo,
              V, Context.GetResultPointer);
            OleVariant(VarResult^) := V;
          end
          else if Assigned(VarResult) then
            OleVariant(VarResult^) := Null;
        finally
          Context.Free;
        end;

        Result := S_OK;
      end
      else
        Result := DISP_E_BADPARAMCOUNT;
    except
      on E: Exception do
      begin
        if E is ETypeTransException then
          Result := DISP_E_TYPEMISMATCH
        else if E is EInvalidCast then
          Result := DISP_E_TYPEMISMATCH
        else if E is EConvertError then
          Result := DISP_E_TYPEMISMATCH
        else if E is EOverflow then
          Result := DISP_E_OVERFLOW
        else
        begin
          Result := DISP_E_EXCEPTION;
          { TODO -oyygw : 返回异常时的错误信息 }
        end;
      end;
    end;
  end
  else
    Result := DISP_E_MEMBERNOTFOUND;
end;

end.

