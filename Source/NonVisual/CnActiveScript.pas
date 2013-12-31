{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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

unit CnActiveScript;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：ActiveScript 脚本引擎封装组件单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2003.07.10
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Sysutils, ActiveX, ComObj, Contnrs, Classes,
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF COMPILER6_UP}
  Forms,
  CnConsts, CnClasses, CnCompConsts;

const
  SCATID_ActiveScript =               '{F0B7A1A1-9847-11cf-8F20-00805F2CD064}';
  SCATID_ActiveScriptParse =          '{F0B7A1A2-9847-11cf-8F20-00805F2CD064}';
  SID_IActiveScript =                 '{BB1A2AE1-A4F9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptParse =            '{BB1A2AE2-A4F9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptParseProcedureOld ='{1CFF0050-6FDD-11d0-9328-00A0C90DCAA9}';
  SID_IActiveScriptParseProcedure =   '{AA5B6A80-B834-11d0-932F-00A0C90DCAA9}';
  SID_IActiveScriptSite =             '{DB01A1E3-A42B-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptSiteWindow =       '{D10F6761-83E9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptSiteInterruptPoll ='{539698A0-CDCA-11CF-A5EB-00AA0047A063}';
  SID_IActiveScriptError =            '{EAE1BA61-A4ED-11cf-8F20-00805F2CD064}';
  SID_IBindEventHandler =             '{63CDBCB0-C1B1-11d0-9336-00A0C90DCAA9}';
  SID_IActiveScriptStats =            '{B8DA6310-E19B-11d0-933C-00A0C90DCAA9}';

  CATID_ActiveScript:                 TGUID = SCATID_ActiveScript;
  CATID_ActiveScriptParse:            TGUID = SCATID_ActiveScriptParse;
  IID_IActiveScript:                  TGUID = SID_IActiveScript;
  IID_IActiveScriptParse:             TGUID = SID_IActiveScriptParse;
  IID_IActiveScriptParseProcedureOld: TGUID = SID_IActiveScriptParseProcedureOld;
  IID_IActiveScriptParseProcedure:    TGUID = SID_IActiveScriptParseProcedure;
  IID_IActiveScriptSite:              TGUID = SID_IActiveScriptSite;
  IID_IActiveScriptSiteWindow:        TGUID = SID_IActiveScriptSiteWindow;
  IID_IActiveScriptSiteInterruptPoll: TGUID = SID_IActiveScriptSiteInterruptPoll;
  IID_IActiveScriptError:             TGUID = SID_IActiveScriptError;
  IID_IBindEventHandler:              TGUID = SID_IBindEventHandler;
  IID_IActiveScriptStats:             TGUID = SID_IActiveScriptStats;

// Constants used by ActiveX Scripting:
//

(* IActiveScript::AddNamedItem() input flags *)

  SCRIPTITEM_ISVISIBLE     = $00000002;
  SCRIPTITEM_ISSOURCE      = $00000004;
  SCRIPTITEM_GLOBALMEMBERS = $00000008;
  SCRIPTITEM_ISPERSISTENT  = $00000040;
  SCRIPTITEM_CODEONLY      = $00000200;
  SCRIPTITEM_NOCODE        = $00000400;
  SCRIPTITEM_ALL_FLAGS     =(SCRIPTITEM_ISSOURCE or
                             SCRIPTITEM_ISVISIBLE or
                             SCRIPTITEM_ISPERSISTENT or
                             SCRIPTITEM_GLOBALMEMBERS or
                             SCRIPTITEM_NOCODE or
                             SCRIPTITEM_CODEONLY);

(* IActiveScript::AddTypeLib() input flags *)

  SCRIPTTYPELIB_ISCONTROL    = $00000010;
  SCRIPTTYPELIB_ISPERSISTENT = $00000040;
  SCRIPTTYPELIB_ALL_FLAGS    = (SCRIPTTYPELIB_ISCONTROL or
                                SCRIPTTYPELIB_ISPERSISTENT);

(* IActiveScriptParse::AddScriptlet() and
   IActiveScriptParse::ParseScriptText() input flags *)

  SCRIPTTEXT_DELAYEXECUTION    = $00000001;
  SCRIPTTEXT_ISVISIBLE         = $00000002;
  SCRIPTTEXT_ISEXPRESSION      = $00000020;
  SCRIPTTEXT_ISPERSISTENT      = $00000040;
  SCRIPTTEXT_HOSTMANAGESSOURCE = $00000080;
  SCRIPTTEXT_ALL_FLAGS         = (SCRIPTTEXT_DELAYEXECUTION or
                                  SCRIPTTEXT_ISVISIBLE or
                                  SCRIPTTEXT_ISEXPRESSION or
                                  SCRIPTTEXT_ISPERSISTENT or
                                  SCRIPTTEXT_HOSTMANAGESSOURCE);

(* IActiveScriptParseProcedure::ParseProcedureText() input flags *)

  SCRIPTPROC_HOSTMANAGESSOURCE = $00000080;
  SCRIPTPROC_IMPLICIT_THIS     = $00000100;
  SCRIPTPROC_IMPLICIT_PARENTS  = $00000200;
  SCRIPTPROC_ALL_FLAGS         = (SCRIPTPROC_HOSTMANAGESSOURCE or
                                  SCRIPTPROC_IMPLICIT_THIS or
                                  SCRIPTPROC_IMPLICIT_PARENTS);

(* IActiveScriptSite::GetItemInfo() input flags *)

  SCRIPTINFO_IUNKNOWN  = $00000001;
  SCRIPTINFO_ITYPEINFO = $00000002;
  SCRIPTINFO_ALL_FLAGS = (SCRIPTINFO_IUNKNOWN or
                          SCRIPTINFO_ITYPEINFO);

(* IActiveScript::Interrupt() Flags *)

  SCRIPTINTERRUPT_DEBUG          = $00000001;
  SCRIPTINTERRUPT_RAISEEXCEPTION = $00000002;
  SCRIPTINTERRUPT_ALL_FLAGS      = (SCRIPTINTERRUPT_DEBUG or
                                    SCRIPTINTERRUPT_RAISEEXCEPTION);

(* IActiveScriptStats::GetStat() values *)

  SCRIPTSTAT_STATEMENT_COUNT   = 1;
  SCRIPTSTAT_INSTRUCTION_COUNT = 2;
  SCRIPTSTAT_INTSTRUCTION_TIME = 3;
  SCRIPTSTAT_TOTAL_TIME        = 4;

(* script state values *)

type
  tagSCRIPTSTATE = integer;
  SCRIPTSTATE = tagSCRIPTSTATE;
const
  SCRIPTSTATE_UNINITIALIZED = $00000000;
  SCRIPTSTATE_INITIALIZED   = $00000005;
  SCRIPTSTATE_STARTED       = $00000001;
  SCRIPTSTATE_CONNECTED     = $00000002;
  SCRIPTSTATE_DISCONNECTED  = $00000003;
  SCRIPTSTATE_CLOSED        = $00000004;

(* script thread state values *)

type
  tagSCRIPTTHREADSTATE = integer;
  SCRIPTTHREADSTATE = tagSCRIPTTHREADSTATE;
const
  SCRIPTTHREADSTATE_NOTINSCRIPT = $00000000;
  SCRIPTTHREADSTATE_RUNNING     = $00000001;

(* Thread IDs *)

type
  SCRIPTTHREADID = DWORD;
  
const
  SCRIPTTHREADID_CURRENT = SCRIPTTHREADID(-1);
  SCRIPTTHREADID_BASE    = SCRIPTTHREADID(-2);
  SCRIPTTHREADID_ALL     = SCRIPTTHREADID(-3);

type
  IActiveScriptSite =           interface;
  IActiveScriptSiteWindow =     interface;
  IActiveScript =               interface;
  IActiveScriptParse =          interface;
  IActiveScriptParseProcedure = interface;
  IActiveScriptError =          interface;
  LPCOLESTR = PWideChar;

  IActiveScriptSite = interface(IUnknown)
    [SID_IActiveScript]
    function GetLCID(out plcid: LCID): HResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
    function GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function OnScriptTerminate(
      var pvarResult: OleVariant;
      var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function OnStateChange(ssScriptState: SCRIPTSTATE): HResult; stdcall;
    function OnScriptError(
      const pscripterror: IActiveScriptError): HResult; stdcall;
    function OnEnterScript: HResult; stdcall;
    function OnLeaveScript: HResult; stdcall;
  end;

  IActiveScriptError = interface(IUnknown)
    [SID_IActiveScriptError]
    function GetExceptionInfo(out pexcepinfo: EXCEPINFO): HResult; stdcall;
    function GetSourcePosition(
      out pdwSourceContext: DWORD;
      out pulLineNumber: ULONG;
      out plCharacterPosition: Integer): HResult; stdcall;
    function GetSourceLineText(out pbstrSourceLine: WideString): HResult; stdcall;
  end;

  IActiveScriptSiteWindow = interface(IUnknown)
    [SID_IActiveScriptSiteWindow]
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

  IActiveScriptSiteInterruptPoll = interface(IUnknown)
    [SID_IActiveScriptSiteInterruptPoll]
    function QueryContinue: HResult; stdcall;
  end;

  IActiveScript = interface(IUnknown)
    [SID_IActiveScript]
    function SetScriptSite(const pass: IActiveScriptSite): HResult; stdcall;
    function GetScriptSite(
      const riid: TGUID;
      out ppvObject: Pointer): HResult; stdcall;
    function SetScriptState(ss: SCRIPTSTATE): HResult; stdcall;
    function GetScriptState(out pssState: SCRIPTSTATE): HResult; stdcall;
    function Close: HResult; stdcall;
    function AddNamedItem(
      pstrName: LPCOLESTR;
      dwFlags: DWORD): HResult; stdcall;
    function AddTypeLib(
      const rguidTypeLib: TGUID;
      dwMajor: DWORD;
      dwMinor: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetScriptDispatch(
      pstrItemName: LPCOLESTR;
      out ppdisp: IDispatch): HResult; stdcall;
    function GetCurrentScriptThreadID(
      out pstidThread: SCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadID(dwWin32ThreadId: DWORD;
      out pstidThread: SCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadState(
      stidThread: SCRIPTTHREADID;
      out pstsState: SCRIPTTHREADSTATE): HResult; stdcall;
    function InterruptScriptThread(
      stidThread: SCRIPTTHREADID;
      var pexcepinfo: EXCEPINFO;
      dwFlags: DWORD): HResult; stdcall;
    function Clone(out ppscript: IActiveScript): HResult; stdcall;
  end;

  IActiveScriptParse = interface(IUnknown)
    [SID_IActiveScriptParse]
    function InitNew: HResult; stdcall;
    function AddScriptlet(
      pstrDefaultName: LPCOLESTR;
      pstrCode: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      pstrSubItemName: LPCOLESTR;
      pstrEventName: LPCOLESTR;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out pbstrName: WideString;
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
    function ParseScriptText(
      pstrCode: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out pvarResult: OleVariant;
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
  end;

  IActiveScriptParseProcedureOld = interface(IUnknown)
    [SID_IActiveScriptParseProcedureOld]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  IActiveScriptParseProcedure = interface(IUnknown)
    [SID_IActiveScriptParseProcedure]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrProcedureName: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  IBindEventHandler = interface(IUnknown)
    [SID_IBindEventHandler]
    function BindHandler(
      pstrEvent: LPCOLESTR;
      const pdisp: IDispatch): HResult; stdcall;
  end;

  IActiveScriptStats = interface(IUnknown)
    [SID_IActiveScriptStats]
    function GetStat(
      stid: DWORD;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function GetStatEx(
      const guid: TGUID;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function ResetStats: HResult; stdcall;
  end;

type
  TOnActiveScriptError = procedure(Sender: TObject; Line, Pos: Integer; ASrc:
    string; ADescription: string) of object;

  TCnScriptGlobalObjects = class(TObject)
  private
    FIntfList: IInterfaceList;
    FNamedList: TStrings;
    function GetNamedItemCount: Integer;
    function GetNamedItemName(I: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNamedIntf(const AName: string; AIntf: IUnknown);
    procedure Clear;
    function FindNamedItemIntf(const AName: string): IUnknown;
    
    property NamedItemCount: Integer read GetNamedItemCount;
    property NamedItemName[I: Integer]: string read GetNamedItemName;
  end;

  TScriptLanguage = type string;

  TCnActiveScriptSite = class(TCnComponent, IActiveScriptSite)
  private
    FUseSafeSubset: Boolean;
    FDisp: OleVariant;
    FGlobalObjects: TCnScriptGlobalObjects;
    FOnError: TOnActiveScriptError;
    FEngine: IActiveScript;
    FParser: IActiveScriptParse;
    FScriptLanguage: TScriptLanguage;
    FCleanBeforeRun: Boolean;
    procedure CreateScriptEngine(language: string);
    procedure CloseScriptEngine;
  protected
    { IActiveScriptSite }
    function GetLCID(out plcid: Longword): hResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): hResult; stdcall;
    function GetDocVersionString(out pbstrVersion: WideString): hResult; stdcall;
    function OnScriptTerminate(var pvarResult: OleVariant; var PExcepInfo:
      ExcepInfo): hResult; stdcall;
    function OnStateChange(ssScriptState: tagSCRIPTSTATE): hResult; stdcall;
    function OnScriptError(const pscripterror: IActiveScriptError): hResult;
      stdcall;
    function OnEnterScript: hResult; stdcall;
    function OnLeaveScript: hResult; stdcall;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RunExpression(ACode: WideString): OleVariant;
    function Execute(ACode: WideString): OleVariant;
    procedure AddNamedItem(AName: string; AIntf: IUnknown);
    procedure Clear;
    property ScriptInterface: OleVariant read FDisp;
  published
    property ScriptLanguage: TScriptLanguage read FScriptLanguage write FScriptLanguage;
    property OnError: TOnActiveScriptError read FOnError write FOnError;
    property CleanBeforeRun: Boolean read FCleanBeforeRun write FCleanBeforeRun;
    property UseSafeSubset: Boolean read FUseSafeSubset write FUseSafeSubset default
      False;
  end;

  TCnActiveScriptWindow = class(TCnActiveScriptSite, IActiveScriptSiteWindow)
  protected
    {IActiveSriptSiteWindow}
    function GetWindow(out phwnd: HWND): hResult; stdcall;
    function EnableModeless(fEnable: BOOL): hResult; stdcall;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  end;

procedure GetActiveScriptParse(List: TStrings);

implementation

const
  INTERFACESAFE_FOR_UNTRUSTED_CALLER = $00000001; // Caller of interface may be untrusted
  INTERFACESAFE_FOR_UNTRUSTED_DATA = $00000002; // Data passed into interface may be untrusted
  INTERFACE_USES_DISPEX = $00000004; // Object knows to use IDispatchEx
  INTERFACE_USES_SECURITY_MANAGER = $00000008; // Object knows to use IInternetHostSecurityManager

procedure GetActiveScriptParse(List: TStrings);
var
  ProgID: string;

  function ValidProgID: Boolean;
  var
    PID: string;
  begin
    if Length(ProgID) > 7 then
      Result := AnsiCompareStr('.Encode', Copy(ProgID, Length(ProgID) - 6, 7)) <> 0
    else
      Result := True;
     // Exclude XML script engine
    if CompareText(Copy(ProgID, 1, 3), 'XML') = 0 then
      Result := False;
     // Exclude "signed" script engines
    PID := UpperCase(ProgID);
    if Pos('SIGNED', PID) <> 0 then
      Result := False;
  end;
var
  EnumGUID: IEnumGUID;
  Fetched: Cardinal;
  Guid: TGUID;
  Rslt: hResult;
  CatInfo: ICatInformation;
  I, BufSize: Integer;
  ClassIDKey: HKEY;
  S: string;
  Buffer: array[0..255] of Char;
begin
  List.Clear;
  Rslt := CoCreateInstance(CLSID_StdComponentCategoryMgr, nil,
    CLSCTX_INPROC_SERVER, ICatInformation, CatInfo);
  if Succeeded(Rslt) then
  begin
    OleCheck(CatInfo.EnumClassesOfCategories(1, @CATID_ActiveScriptParse, 0, nil,
      EnumGUID));
    while EnumGUID.Next(1, Guid, Fetched) = S_OK do
    begin
      try
        ProgID := ClassIDToProgID(Guid);
        if ValidProgID then
          List.Add(ProgID);
      except
        ProgID := ClassIDToProgID(StringToGuid(Buffer));
        List.Add('Invalid Entry In Categories');
      end;
    end;
  end else
  begin
    if RegOpenKey(HKEY_CLASSES_ROOT, 'CLSID', ClassIDKey) <> 0 then
    try
      I := 0;
      while RegEnumKey(ClassIDKey, I, Buffer, SizeOf(Buffer)) = 0 do
      begin
        S := Format('%s\Implemented Categories\%s', [Buffer, { do not localize }
          GuidToString(CATID_ActiveScriptParse)]);
        if RegQueryValue(ClassIDKey, PChar(S), nil, BufSize) = 0 then
        begin
          ProgID := ClassIDToProgID(StringToGuid(Buffer));
          if ValidProgID then
            List.Add(ProgID);
        end;
        Inc(I);
      end;
    finally
      RegCloseKey(ClassIDKey);
    end;
  end;
end;

{ TCnScriptGlobalObjects }

procedure TCnScriptGlobalObjects.AddNamedIntf(const AName: string; AIntf: IUnknown);
begin
  if FNamedList.IndexOf(AName) < 0 then
  begin
    FNamedList.Add(AName);
    FIntfList.Add(AIntf);
  end;
end;

procedure TCnScriptGlobalObjects.Clear;
begin
  FNamedList.Clear;
  FIntfList.Clear;
end;

constructor TCnScriptGlobalObjects.Create;
begin
  inherited Create;
  FNamedList := TStringList.Create;
  FIntfList := TInterfaceList.Create;
end;

destructor TCnScriptGlobalObjects.Destroy;
begin
  FNamedList.Free;
  inherited;
end;

function TCnScriptGlobalObjects.FindNamedItemIntf(const AName: string): IUnknown;
var
  I: Integer;
begin
  I := FNamedList.IndexOf(AName);
  if I >= 0 then
    Result := FIntfList[I]
  else
    Result := nil;
end;

function TCnScriptGlobalObjects.GetNamedItemCount: Integer;
begin
  Result := FNamedList.Count;
end;

function TCnScriptGlobalObjects.GetNamedItemName(I: Integer): string;
begin
  Result := FNamedList[I];
end;

{ TCnActiveScriptSite }

constructor TCnActiveScriptSite.Create(AOwner: TComponent);
begin
  inherited;
  FScriptLanguage := 'VBScript';
  FGlobalObjects := TCnScriptGlobalObjects.Create;
  FUseSafeSubset := False;
  CleanBeforeRun := True;
  FEngine := nil;
  FDisp := Null;
  FParser := nil;
end;

destructor TCnActiveScriptSite.Destroy;
begin
  CloseScriptEngine;
  FGlobalObjects.Free;
  inherited;
end;

procedure TCnActiveScriptSite.AddNamedItem(AName: string;
  AIntf: IUnknown);
begin
  FGlobalObjects.AddNamedIntf(AName, AIntf);
end;

procedure TCnActiveScriptSite.CreateScriptEngine(
  language: string);
const
  NULL_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
var
  ScriptCLSID: TGUID;
  LanguageW: WideString;
  hr: hResult;
  i: Integer;
  Disp: IDispatch;
  Pos: IObjectSafety;
  dwSupported: DWORD;
  dwEnabled: DWORD;
begin
  if FEngine <> nil then Exit;
  LanguageW := language;
  if CLSIDFromProgID(PWideChar(LanguageW), ScriptCLSID) <> S_OK
    then ScriptCLSID := NULL_GUID;
  FEngine := CreateComObject(ScriptCLSID) as IActiveScript;
  if FUseSafeSubset then
  begin
    dwSupported := 0;
    dwEnabled := 0;
    FEngine.QueryInterface(IObjectSafety, Pos);
    if Assigned(Pos) then
    begin
      Pos.GetInterfaceSafetyOptions(IDispatch, @dwSupported, @dwEnabled);
      if (INTERFACE_USES_SECURITY_MANAGER and dwSupported) =
        INTERFACE_USES_SECURITY_MANAGER then
      begin
        dwEnabled := dwEnabled or INTERFACE_USES_SECURITY_MANAGER;
      end;
      Pos.SetInterfaceSafetyOptions(IDispatch, INTERFACE_USES_SECURITY_MANAGER,
        dwEnabled);
    end;
  end;

  hr := FEngine.QueryInterface(IActiveScriptParse, FParser);
  OleCheck(hr);

  hr := FEngine.SetScriptSite(Self);
  OleCheck(hr);

  hr := FParser.InitNew();
  OleCheck(hr);

  for I := 0 to FGlobalObjects.NamedItemCount - 1 do
    FEngine.AddNamedItem(PWideChar(WideString(FGlobalObjects.NamedItemName[I])),
      SCRIPTITEM_ISVISIBLE);

  FEngine.GetScriptDispatch(nil, Disp);
  FDisp := Disp;
end;

procedure TCnActiveScriptSite.CloseScriptEngine;
begin
  FParser := nil;
  if FEngine <> nil then FEngine.Close;
  FEngine := nil;
  FDisp := Unassigned;
end;

function TCnActiveScriptSite.RunExpression(ACode: WideString): OleVariant;
var
  AResult: OleVariant;
  ExcepInfo: TExcepInfo;
begin
  if FCleanBeforeRun then CloseScriptEngine;
  CreateScriptEngine(FScriptLanguage);
  if FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0,
    SCRIPTTEXT_ISEXPRESSION, AResult, ExcepInfo) = S_OK then
    Result := AResult
  else
    Result := Null;
  if FCleanBeforeRun then CloseScriptEngine;
end;

function TCnActiveScriptSite.Execute(ACode: WideString): OleVariant;
var
  AResult: OleVariant;
  ExcepInfo: TExcepInfo;
begin
  if FCleanBeforeRun then CloseScriptEngine;
  CreateScriptEngine(FScriptLanguage);
  if FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0, 0, AResult,
    ExcepInfo) = S_Ok then
    Result := AResult
  else
    Result := Null;
  FEngine.SetScriptState(SCRIPTSTATE_CONNECTED);
  if FCleanBeforeRun then CloseScriptEngine;
end;

function TCnActiveScriptSite.GetDocVersionString(
  out pbstrVersion: WideString): hResult;
begin
  Result := E_NOTIMPL;
end;

function TCnActiveScriptSite.GetItemInfo(pstrName: LPCOLESTR;
  dwReturnMask: DWORD;
  out ppiunkItem: IUnknown;
  out ppti: ITypeInfo): hResult; stdcall;
begin
  if @ppiunkItem <> nil then Pointer(ppiunkItem) := nil;
  if @ppti <> nil then Pointer(ppti) := nil;
  if (dwReturnMask and SCRIPTINFO_IUNKNOWN) <> 0
    then ppiunkItem := FGlobalObjects.FindNamedItemIntf(pstrName);
  Result := S_OK;
end;

function TCnActiveScriptSite.GetLCID(out plcid: Longword): hResult;
begin
  plcid := GetSystemDefaultLCID;
  Result := S_OK;
end;

function TCnActiveScriptSite.OnEnterScript: hResult;
begin
  Result := S_OK;
end;

function TCnActiveScriptSite.OnLeaveScript: hResult;
begin
  Result := S_OK;
end;

function TCnActiveScriptSite.OnScriptError(
  const pscripterror: IActiveScriptError): hResult;
var
  wCookie: DWORD;
  ExcepInfo: TExcepInfo;
  CharNo: Integer;
  LineNo: DWORD;
  SourceLineW: WideString;
  SourceLine: string;
  Desc: string;
begin
  Result := S_OK;
  wCookie := 0;
  LineNo := 0;
  CharNo := 0;
  if Assigned(pscripterror) then
  begin
    pscripterror.GetExceptionInfo(ExcepInfo);
    Desc := ExcepInfo.bstrDescription;
    pscripterror.GetSourcePosition(wCookie, LineNo, CharNo);
    pscripterror.GetSourceLineText(SourceLineW);
    SourceLine := SourceLineW;
    if Assigned(FOnError) then
      FOnError(Self, LineNo, CharNo, SourceLine, Desc);
  end;
end;

function TCnActiveScriptSite.OnScriptTerminate(var pvarResult: OleVariant;
  var PExcepInfo: ExcepInfo): hResult;
begin
  Result := S_OK;
end;

function TCnActiveScriptSite.OnStateChange(
  ssScriptState: tagSCRIPTSTATE): hResult;
begin
  case ssScriptState of
    SCRIPTSTATE_UNINITIALIZED: ;
    SCRIPTSTATE_INITIALIZED: ;
    SCRIPTSTATE_STARTED: ;
    SCRIPTSTATE_CONNECTED: ;
    SCRIPTSTATE_DISCONNECTED: ;
    SCRIPTSTATE_CLOSED: ;
  end;

  Result := S_OK;
end;

procedure TCnActiveScriptSite.Clear;
begin
  CloseScriptEngine;
  FGlobalObjects.Clear;
end;

procedure TCnActiveScriptSite.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnActiveScriptSiteName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnActiveScriptSiteComment;
end;

{ TCnActiveScriptWindow }

function TCnActiveScriptWindow.EnableModeless(fEnable: BOOL): hResult;
begin
  Result := S_OK;
end;

procedure TCnActiveScriptWindow.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnActiveScriptWindowName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnActiveScriptWindowComment;
end;

function TCnActiveScriptWindow.GetWindow(out phwnd: HWND): hResult;
begin
  if (Owner is TCustomForm) then
  begin
    phwnd := (Owner as TCustomForm).Handle;
    Result := S_OK;
  end
  else
  begin
    phwnd := 0;
    Result := S_FALSE;
  end;
end;

end.

