{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnLangMgr;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：多语管理器基础类单元
* 单元作者：CnPack开发组 刘啸 (liuxiao@cnpack.org)
* 备    注：该单元定义了多语管理器基础类
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2009.08.18 V1.5
*               将字符串常量注册机制与多语管理器独立出来
*           2009.07.15 V1.5
*               修改资源字符串常量存储机制，直接保存在 PResStringRec的Identifier
*               中，由翻译时统一改动，不挂接以减少问题。
*           2009.07.11 V1.4
*               增加字符串常量的注册机制，注册了的字符串能在改变语言时被自动翻译
*               而无需在事件中手工调用 TranslateStr，资源字符串的自动翻译也已通
*               过挂接 LoadResString 完成。
*           2008.05.30 V1.3
*               不处理只读的 string 属性，加入某 Tag 值不翻译的机制
*           2007.09.18 V1.10
*               增加翻译事件以便让用户控制是否翻译某些对象和属性。
*           2006.08.21 V1.9
*               修正手工创建多语管理器时未释放的问题。
*           2006.08.19 V1.8
*               修改为允许多实例，但全局函数只返回第一个实例。
*           2006.08.17 V1.7
*               增加字符串数组翻译函数。
*           2005.04.02 V1.6
*               根据崔东伟的建议，合并 AList 的使用以避免循环引用。
*           2004.10.25 V1.5
*               增加基于字符串进行搜索的翻译模式。
*           2004.07.16 V1.4
*               增加单独翻译某一 Component 的功能；改做设计期不进行翻译，
*               （翻译整个 IDE 窗口总是不好）
*           2004.07.12 V1.3
*               进行初步的性能测试，确定 List 的使用未增加显著的开销
*           2004.06.01 V1.2
*               修改对 Form 的处理，不区分 Form 的 Parent 是否为 nil 了
*           2003.12.10 V1.1
*               增加对字体的额外处理
*           2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Graphics, TypInfo, Windows, Forms, ComCtrls, ActnList,
  Dialogs, ExtCtrls, Controls, Contnrs, {$IFDEF COMPILER6_UP}Variants, {$ENDIF}
  CnConsts, CnClasses, CnCommon, CnLangStorage, CnIniStrUtils;

const
  CN_MULTI_LANG_TAG_NOT_TRANSLATE = 2001;
  {* 组件 Tag 值为此值时，不翻译}

type
  ECnLanguageManagerError = class(Exception);

  PCnLangChangedNotifierRecord = ^TCnLangChangedNotifierRecord;
  TCnLangChangedNotifierRecord = record
    Notifier: TMethod;
  end;

  TCnAutoTransOption = (atApplication, atForms, atDataModules);
  TCnAutoTransOptions = set of TCnAutoTransOption;

  TCnTranslationMode = (tmByComponents, tmByStrings);
  {* 翻译模式，根据窗体和控件等遍历还是根据翻译字符串内容遍历 }

  TCnStrObjType = (csotString, csotAnsi, csotWide);

  TCnStringObj = class
  {* 描述一自动翻译的字符串}
  private
    FStringAddr: Pointer;
    FStringName: WideString;
    FType: TCnStrObjType;
  public
    property StringAddr: Pointer read FStringAddr write FStringAddr;
    property StringName: WideString read FStringName write FStringName;
    property AType: TCnStrObjType read FType write FType;
  end;

  TCnResourceStringObj = class
  {* 描述一自动翻译的资源字符串}
  private
    FStringRecAddr: Pointer;
    FStringName: WideString;
    FDstStr: string;
  public
    property StringRecAddr: Pointer read FStringRecAddr write FStringRecAddr;
    property StringName: WideString read FStringName write FStringName;
  end;

  TTranslateStringEvent = procedure (Sender: TObject; const Src: WideString;
    var Dst: WideString) of object;
  {* 翻译字符串事件，可用于统一检查或修改目标字符串 }

  TCnBaseLangManager = class(TCnComponent)
  {* 多语言管理器类 }
  private
    FDefaultLanguageIndex: Integer;
    FCurrentLanguageIndex: Integer;
    FOnStorageChanged: TNotifyEvent;
    FOnLanguageChanged: TNotifyEvent;
    FOnTranslateString: TTranslateStringEvent;
    FAutoTranslateStrings: Boolean;
    procedure SetLanguageStorage(Value: TCnCustomLangStorage);
    procedure AdjustNewLanguage(AID: LongWord);
    function GetCurrentLanguageIndex: Integer;
  protected
    FLanguageStorage: TCnCustomLangStorage;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure DoStorageChanged; virtual;
    procedure DoLanguageChanged; virtual;
    procedure SetCurrentLanguageIndex(const Value: Integer); virtual;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    {* 构造方法 }
    destructor Destroy; override;
    {* 销毁方法 }
    function Translate(Src: WideString): WideString;
    {* 根据当前语言获得翻译的字符串 }
    function TranslateString(Src: WideString): WideString;
    {* 根据当前语言获得翻译的字符串，无则返回空 }
    function TranslateStrFmt(Src: WideString; Args: array of const): WideString;
    {* 根据当前语言获得格式化的翻译字符串 }

    property AutoTranslateStrings: Boolean read FAutoTranslateStrings
      write FAutoTranslateStrings default True;
    {* 是否在语言改变时自动翻译已经注册了的字符串与资源字符串，默认为 True}

    property LanguageStorage: TCnCustomLangStorage read FLanguageStorage
      write SetLanguageStorage;
    {* 多语言存储元件引用 }
    property CurrentLanguageIndex: Integer read GetCurrentLanguageIndex
      write SetCurrentLanguageIndex default -1;
    {* 当前语言号，影响到整个程序的语言设置。语言号含义由存储元件条目内容决定 }
    property OnStorageChanged: TNotifyEvent read FOnStorageChanged
      write FOnStorageChanged;
    {* 存储元件引用改变时触发 }
    property OnLanguageChanged: TNotifyEvent read FOnLanguageChanged
      write FOnLanguageChanged;
    {* 当前语言发生改变时触发 }
    property OnTranslateString: TTranslateStringEvent read FOnTranslateString
      write FOnTranslateString;
    {* 当翻译字符串时触发 }
  end;
  
  TCnTranslateObjectEvent = procedure (AObject: TObject; var Translate: Boolean) of object;
  {* 翻译一对象时的事件原型 }

  TCnTranslateObjectPropertyEvent = procedure (AObject: TObject; const PropName: string;
    var Translate: Boolean) of object;
  {* 翻译一对象及其某个属性时的事件原型 }
    
  TCnCustomLangManager = class(TCnBaseLangManager)
  {* 具有翻译窗体能力的多语言管理器 }
  private
    FNotifier: TList;
    FAutoTranslate: Boolean;
    FTranslateTreeNode: Boolean;
    FTranslateListItem: Boolean;
    FUseDefaultFont: Boolean;
    FTranslateOtherFont: Boolean;
    FAutoTransOptions: TCnAutoTransOptions;
    FTranslationMode: TCnTranslationMode;
    FOldTransForms: TList;
    FOldTransDMs: TList;
    FOldFormPrefix: WideString;
    FOldDMPrefix: WideString;
    FIgnoreAction: Boolean;
    FOnTranslateObjectProperty: TCnTranslateObjectPropertyEvent;
    FOnTranslateObject: TCnTranslateObjectEvent;
    procedure SetTranslationMode(const Value: TCnTranslationMode);
  protected
    procedure TranslateRecurComponent(AComponent: TComponent;
      AList: TList; const BaseName: WideString); virtual;
    {* 递归翻译一 Component 和其 Children }
    procedure TranslateRecurObject(AObject: TObject; AList: TList;
      const BaseName: WideString = ''); virtual;
    {* 递归翻译一 Object 和其属性中的 Object }
    function GetRecurOwner(AComponent: TComponent): WideString;
    {* 回溯获得一 Component 的祖先标识字符串 }
    procedure TranslateKeyToValue(const Key, Value: WideString);
    {* 翻译级联的字符串 }
    procedure SetCurrentLanguageIndex(const Value: Integer); override;
    procedure DoLanguageChanged; override;
    function DoTranslateObject(AObject: TObject): Boolean; virtual;
    function DoTranslateObjectProperty(AObject: TObject;
      const PropName: WideString): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddChangeNotifier(Notify: TNotifyEvent);
    {* 增加语言改变时的事件通知 }
    procedure RemoveChangeNotifier(Notify: TNotifyEvent);
    {* 删除语言改变时的事件通知 }
    procedure TranslateComponent(AComponent: TComponent; const BaseName: WideString = '');
    {* 翻译一个元件及其子对象和子属性 }
    procedure TranslateForm(AForm: TCustomForm);
    {* 翻译一个 Form 及其子对象和子属性 }
    procedure TranslateObject(AObject: TObject; const BaseName: WideString = '');
    {* 翻译一个对象及其子对象和子属性 }
    property AutoTranslate: Boolean read FAutoTranslate
      write FAutoTranslate default True;
    {* 是否在当前语言号改变后自动翻译所有已经存在的窗体和其他内容 }
    property TranslationMode: TCnTranslationMode read FTranslationMode
       write SetTranslationMode;
    {* 翻译模式，默认根据窗体和控件等遍历 }
    property AutoTransOptions: TCnAutoTransOptions read FAutoTransOptions
      write FAutoTransOptions;
    {* 自动翻译选项，控制是否翻译窗体、数据模块和 Application 实例 }
    property TranslateListItem: Boolean read FTranslateListItem
      write FTranslateListItem default False;
    {* 是否翻译 ListView 中的 ListItem }
    property TranslateTreeNode: Boolean read FTranslateTreeNode
      write FTranslateTreeNode default False;
    {* 是否翻译 TreeView 中的 TreeNode }
    property UseDefaultFont: Boolean read FUseDefaultFont
      write FUseDefaultFont default True;
    {* 是否翻译完窗体后使用 DefaultFont 来设置窗体字体 }
    property TranslateOtherFont: Boolean read FTranslateOtherFont
      write FTranslateOtherFont default True;
    {* 是否将其他的 Font 属性翻译成字符串 }
    property IgnoreAction: Boolean read FIgnoreAction
      write FIgnoreAction default True;
    {* 是否翻译 Action 属性不为空的控件的 Caption 和 Hint 属性}
    property OnTranslateObject: TCnTranslateObjectEvent read FOnTranslateObject
      write FOnTranslateObject;
    {* 翻译一对象时的事件 }
    property OnTranslateObjectProperty: TCnTranslateObjectPropertyEvent
      read FOnTranslateObjectProperty write FOnTranslateObjectProperty;
    {* 翻译一对象的某个属性时的事件 }
  end;

  TCnLangManager = class(TCnCustomLangManager)
  {* 具有窗体翻译能力的多语言管理器 }
  published
    property LanguageStorage;
    {* 多语言存储元件引用 }
    property CurrentLanguageIndex;
    {* 当前语言号，影响到整个程序的语言设置。语言号含义由存储元件条目内容决定 }
    property AutoTranslate;
    {* 是否在当前语言号改变后自动翻译已经存在的窗体和其他内容 }
    property AutoTranslateStrings;
    {* 是否在语言改变时自动翻译已经注册了的字符串与资源字符串，默认为 True}
    property TranslationMode;
    {* 翻译模式，默认根据窗体和控件等遍历 }
    property AutoTransOptions;
    {* 自动翻译选项，控制是否翻译窗体、数据模块和 Application 实例 }
    property TranslateListItem;
    {* 是否翻译 ListView 中的 ListItem }
    property TranslateTreeNode;
    {* 是否翻译 TreeView 中的 TreeNode  }
    property UseDefaultFont;
    {* 是否翻译完窗体后使用 DefaultFont 来设置窗体字体 }
    property TranslateOtherFont;
    {* 是否将其他的 Font 属性翻译成字符串 }
    property IgnoreAction;
    {* 是否翻译 Action 属性不为空的控件的 Caption 和 Hint 属性}
    property OnStorageChanged;
    {* 存储元件引用改变时触发 }
    property OnLanguageChanged;
    {* 当前语言发生改变时触发 }
    property OnTranslateObject;
    {* 翻译一对象时的事件 }
    property OnTranslateObjectProperty;
    {* 翻译一对象的某个属性时的事件 }
  end;

function CnLanguageManager: TCnCustomLangManager;
{* 全局函数，用于返回多语言管理器的实例 }

procedure CreateLanguageManager(AOwner: TComponent = nil);
{* 创建多语言管理器，用于非可视化或手工创建多语言管理器的场合 }

function GetPropName(Instance: TObject; Index: Integer): WideString;
{* 获得某对象的第 n 个 published 的属性名 }

function GetValueByTransName(Owner: TComponent; const Name: WideString): WideString;
{* 获得级联字符串的属性值 }

procedure SetValueByTransName(Owner: TComponent; const Name, Value: WideString);
{* 设置级联字符串的属性值 }

procedure TranslateStr(var SrcStr: string; const IDStr: string); overload;
procedure TranslateStr(var SrcStr: WideString; const IDStr: WideString); overload;
{* 翻译某个字符串，如无翻译管理器或不存在翻译后的条目，则 SrcStr 保持不变 }

procedure TranslateStrArray(var StrArray: array of string; const IDStr: string);
procedure TranslateWideStrArray(var StrArray: array of WideString; const IDStr: WideString);
{* 翻译某个字符串数组 }

type
  PCnString = ^string;

procedure RegisterTranslateString(const StringAddr: PCnString; const IDStr: WideString);
procedure RegisterTranslateStringA(const StringAddr: PAnsiString; const IDStr: WideString);
procedure RegisterTranslateStringW(const StringAddr: PWideString; const IDStr: WideString);
{* 注册一字符串，传入地址与名称，可在语言改变时被自动翻译，无需手工调 Translate}

procedure RegisterTranslateResourceString(const ResStringAddr: Pointer; const IDStr: WideString);
{* 注册一资源字符串，传入地址与名称，可在语言改变时被自动翻译}

procedure TranslateReggedStrings;
{* 翻译注册了的字符串与资源字符串，多语管理器的语言改变后会自动调用
   这里开放出来给需要手工调用的场合，如初始化了多语管理器但未改变语言的场合}

implementation

uses
{$IFDEF DEBUG_MULTILANG}
  CnDebug,
{$ENDIF}
  CnLangConsts;

type
  TCnIterateByTransName = (itGet, itSet);

var
  FLangMgrList: TList = nil;

  FRegStrings: TObjectList;
  FRegResStrings: TObjectList;

// 使用所有多语管理器实例中的第一个作为全局返回的实例
function CnLanguageManager: TCnCustomLangManager;
var
  I: Integer;
begin
  Result := nil;
  if (FLangMgrList <> nil) and (FLangMgrList.Count > 0) then
    for I := 0 to FLangMgrList.Count - 1 do
      if TObject(FLangMgrList.Items[I]) is TCnCustomLangManager then
      begin
        Result := TObject(FLangMgrList.Items[I]) as TCnCustomLangManager;
        Exit;
      end;
end;

procedure CreateLanguageManager(AOwner: TComponent);
begin
  if CnLanguageManager = nil then
    TCnLangManager.Create(AOwner);
end;

procedure TranslateStr(var SrcStr: string; const IDStr: string);
var
  DstStr: WideString;
begin
  if CnLanguageManager <> nil then
  begin
    DstStr := CnLanguageManager.Translate(IDStr);
    if DstStr <> '' then
      SrcStr := DstStr;
  end;
end;

procedure TranslateStr(var SrcStr: WideString; const IDStr: WideString);
var
  DstStr: WideString;
begin
  if CnLanguageManager <> nil then
  begin
    DstStr := CnLanguageManager.Translate(IDStr);
    if DstStr <> '' then
      SrcStr := DstStr;
  end;
end;

procedure TranslateStrArray(var StrArray: array of string; const IDStr: string);
var
  I: Integer;
  DstStr: WideString;
begin
  if CnLanguageManager <> nil then
  begin
    for I := Low(StrArray) to High(StrArray) do
    begin
      DstStr := CnLanguageManager.Translate(IDStr + IntToStr(I));
      if DstStr <> '' then
        StrArray[I] := DstStr;
    end;
  end;
end;

procedure TranslateWideStrArray(var StrArray: array of WideString; const IDStr: WideString);
var
  I: Integer;
  DstStr: WideString;
begin
  if CnLanguageManager <> nil then
  begin
    for I := Low(StrArray) to High(StrArray) do
    begin
      DstStr := CnLanguageManager.Translate(IDStr + IntToStr(I));
      if DstStr <> '' then
        StrArray[I] := DstStr;
    end;
  end;
end;

//==============================================================================
// TCnBaseLangManager
//==============================================================================

procedure TCnBaseLangManager.AdjustNewLanguage(AID: LongWord);
var
  i: Integer;
begin
  if AID = 0 then
    AID := GetSystemDefaultLangID;
  if Assigned(FLanguageStorage) then
    for i := 0 to FLanguageStorage.LanguageCount - 1 do
      if FLanguageStorage.Languages.Items[i].LanguageID = AID then
      begin
        CurrentLanguageIndex := i;
        Exit;
      end;
end;

constructor TCnBaseLangManager.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  if FLangMgrList = nil then
    FLangMgrList := TList.Create;

  FLangMgrList.Add(Self);

  FDefaultLanguageIndex := -1;
  FCurrentLanguageIndex := -1;

  FAutoTranslateStrings := True;

  if (csDesigning in ComponentState) then
    for I := 0 to AOwner.ComponentCount - 1 do
      if AOwner.Components[i] is TCnCustomLangFileStorage then
      begin
        LanguageStorage := AOwner.Components[i] as TCnCustomLangFileStorage;
        Exit;
      end;
end;

destructor TCnBaseLangManager.Destroy;
begin
  FLangMgrList.Remove(Self);
  inherited;
end;

procedure TCnBaseLangManager.DoLanguageChanged;
begin
  if FAutoTranslateStrings then
    TranslateReggedStrings;

  if Assigned(FOnLanguageChanged) then
    FOnLanguageChanged(Self);
end;

procedure TCnBaseLangManager.DoStorageChanged;
begin
  if Assigned(FOnStorageChanged) then
    FOnStorageChanged(Self);
end;

function TCnBaseLangManager.GetCurrentLanguageIndex: Integer;
begin
  Result := FCurrentLanguageIndex;
end;

procedure TCnBaseLangManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(aComponent, Operation);
  if (Operation = opRemove) and (AComponent = FLanguageStorage) then
    FLanguageStorage := nil;
end;

procedure TCnBaseLangManager.SetCurrentLanguageIndex(
  const Value: Integer);
begin
  FCurrentLanguageIndex := Value;
  if Assigned(FLanguageStorage) then
    if (Value >= 0) and (Value < FLanguageStorage.LanguageCount) then
    begin
      FLanguageStorage.CurrentLanguageIndex := Value;
      DoLanguageChanged;
    end;
end;

procedure TCnBaseLangManager.SetLanguageStorage(Value:
  TCnCustomLangStorage);
var
  AID: LongWord;
begin
  if Value <> FLanguageStorage then
  begin
    if Assigned(FLanguageStorage) then
      FLanguageStorage.RemoveFreeNotification(Self);

    FLanguageStorage := Value;
    if (Value <> nil) and (FCurrentLanguageIndex <> -1) then
      if FCurrentLanguageIndex <> FLanguageStorage.CurrentLanguageIndex then
         FLanguageStorage.CurrentLanguageIndex := FCurrentLanguageIndex;

    if Assigned(Value) then
      Value.FreeNotification(Self);
    if FLanguageStorage.CurrentLanguage <> nil then
    begin
      AID := FLanguageStorage.CurrentLanguage.LanguageID;
      AdjustNewLanguage(AID);
    end;
    DoStorageChanged;
  end;
end;

function TCnBaseLangManager.Translate(Src: WideString): WideString;
begin
  Result := TranslateString(Src);
end;

function TCnBaseLangManager.TranslateString(Src: WideString): WideString;
begin
  if FLanguageStorage <> nil then
  begin
    if CurrentLanguageIndex <> FLanguageStorage.CurrentLanguageIndex then
      FLanguageStorage.CurrentLanguageIndex := CurrentLanguageIndex;
    FLanguageStorage.GetString(Src, Result);
    if Assigned(FOnTranslateString) then
      FOnTranslateString(Self, Src, Result);
  end
  else
    Result := '';
end;

function TCnBaseLangManager.TranslateStrFmt(Src: WideString; Args:
  array of const): WideString;
begin
{$IFDEF COMPILER6_UP}
  Result := WideFormat(Translate(Src), Args);
{$ELSE}
  // todo: D5 doesn't support WideFormat
  Result := Format(Translate(Src), Args);
{$ENDIF}
end;

function GetPropName(Instance: TObject; Index: Integer): WideString;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.Classinfo);
  GetMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfoName(PropInfo);
  finally
    FreeMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  end;
end;

function IterateTransName(Owner: TComponent; const Name, Value: WideString;
  Mode: TCnIterateByTransName): WideString;
var
  S, R, P, Q, Prefix, SubFix: WideString;
  OutS: string;
  I, J, K, OutN: Integer;
  AObject: TObject;
begin
  Result := '';
  if Owner = nil then Exit;

  I := Pos(DefDelimeter, Name);
  if I > 0 then   // I 是第一点位置
  begin
    S := Copy(Name, 1, I - 1);
    if S = Owner.ClassName then
    begin
      R := Copy(Name, I + 1, Length(Name) - I); // R 是第一点后的字串
      J := Pos(DefDelimeter, R);
      if J > 0 then  // J 是第二点位置
      begin
        P := Copy(R, 1, J - 1); // P 此时是第一和第二点中间的字串
        if Owner.FindComponent(P) <> nil then // 子控件的属性优先
        begin
          Result := VartoStr(GetPropValueIncludeSub(Owner.FindComponent(P),
            Copy(R, J + 1, Length(R) - J)));
          if Mode = itSet then
            SetPropValueIncludeSub(Owner.FindComponent(P), Copy(R, J + 1, Length(R) - J), Value);
        end  // 然后才是属性的属性
        else
        begin
          Result := VartoStr(GetPropValueIncludeSub(Owner, Copy(Name, I + 1, Length(Name) - I)));
          if Mode = itSet then
            SetPropValueIncludeSub(Owner, Copy(Name, I + 1, Length(Name) - I), Value);
        end;

        if Result = '' then
        begin
          // 处理 Item0 之类的情况。
          K := 1;
          while (CharPosWithCounter(DefDelimeter, R, K) <> 0) and
            (CharPosWithCounter(DefDelimeter, R, K + 1) <> 0) do
          begin
            Q := Copy(R, CharPosWithCounter(DefDelimeter, R, K) + 1,
              CharPosWithCounter(DefDelimeter, R, K + 1) - CharPosWithCounter(DefDelimeter, R, K) - 1);
            SeparateStrAndNum(Q, OutS, OutN);
            if (OutN = -1) or ((OutS <> 'Item') and (OutS <> 'ListItem')
              and (OutS <> 'TreeNode')) then
            begin
              Inc(K);
              Continue;
            end;

            Prefix := Copy(R, 1, CharPosWithCounter(DefDelimeter, R, K) - 1);
            Subfix := Copy(R, CharPosWithCounter(DefDelimeter, R, K + 1) + 1,
              Length(R) - CharPosWithCounter(DefDelimeter, R, K + 1));
            // Prefix 是 Listview1 形式的字符串
            AObject := Owner.FindComponent(P); // 先找到子控件，可以直接是 ListView1
            try
              if Prefix <> P then // 说明 Prefix 层数多
                AObject := TObject(Integer(GetPropValueIncludeSub(AObject,
                  Copy(Prefix, CharPosWithCounter(DefDelimeter, R) + 1,
                  Length(Prefix) - CharPosWithCounter(DefDelimeter, R)))));
            except
              Inc(K);
              Continue;
            end;

            if AObject = nil then // 找到待处理Item0的该对象
            begin
              Inc(K);
              Continue;
            end;

            if (AObject is TCollection) and (OutS = 'Item') then
            begin
              if OutN < (AObject as TCollection).Count then
              begin
                if Mode = itGet then
                  Result := VartoStr(GetPropValueIncludeSub((AObject as TCollection).
                    Items[OutN], Subfix));
                if Mode = itSet then
                  SetPropValueIncludeSub((AObject as TCollection).Items[OutN],
                    Subfix, Value);
              end;
            end
            else if (AObject is TListView) and (OutS = 'ListItem') then
            begin
              if OutN < (AObject as TListView).Items.Count then
              begin
                if Subfix = 'Caption' then // ListItem 的 Caption 属性并非 published
                begin
                  if Mode = itGet then
                    Result := (AObject as TListView).Items[OutN].Caption;
                  if Mode = itSet then
                    (AObject as TListView).Items[OutN].Caption := Value;
                end
                else // 可无必要，因为 TListItem 无 published 属性
                begin
                  if Mode = itGet then
                    Result := VartoStr(GetPropValueIncludeSub((AObject as TListView).
                      Items[OutN], Subfix));
                  if Mode = itSet then
                    SetPropValueIncludeSub((AObject as TListView).Items[OutN],
                      Subfix, Value);
                end;
              end;
            end
            else if (AObject is TTreeView) and (OutS = 'TreeNode') then
            begin
              if OutN < (AObject as TTreeView).Items.Count then
              begin
                if (Subfix = 'Text') then // TreeNode 的 Text 属性并非 published
                begin
                  if Mode = itGet then
                    Result := (AObject as TTreeView).Items[OutN].Text;
                  if Mode = itSet then
                    (AObject as TTreeView).Items[OutN].Text := Value;
                end
                else // 可无必要，因为 TTreeNode 无 published 属性
                begin
                  if Mode = itGet then
                    Result := VartoStr(GetPropValueIncludeSub((AObject as TTreeView).
                      Items[OutN], Subfix));
                  if Mode = itSet then
                    SetPropValueIncludeSub((AObject as TTreeView).Items[OutN],
                      Subfix, Value);
                end;
              end;
            end;
            Inc(K);
          end;
        end;
      end
      else // 直接是属性
      begin
        if Mode = itGet then
          Result := VartoStr(GetPropValueIncludeSub(Owner, Copy(Name, I + 1, Length(Name) - I)));
        if Mode = itSet then
          SetPropValueIncludeSub(Owner, Copy(Name, I + 1, Length(Name) - I), Value);
      end;
    end
    else if (S = 'Application') and (Owner = Application) then
    begin
      if Mode = itGet then
        Result := VartoStr(GetPropValueIncludeSub(Application,
          Copy(Name, I + 1, Length(Name) - I)));
      if Mode = itSet then
        SetPropValueIncludeSub(Application, Copy(Name, I + 1, Length(Name) - I), Value);
    end
    else
    begin
      if Mode = itGet then
        Result := VartoStr(GetPropValueIncludeSub(Owner.FindComponent(S),
          Copy(Name, I + 1, Length(Name) - I)));
      if Mode = itSet then
        SetPropValueIncludeSub(Owner.FindComponent(S), Copy(Name, I + 1, Length(Name) - I), Value);
    end;
  end;
end;

function GetValueByTransName(Owner: TComponent; const Name: WideString): WideString;
begin
  Result := IterateTransName(Owner, Name, '', itGet);
end;

procedure SetValueByTransName(Owner: TComponent; const Name, Value: WideString);
begin
  IterateTransName(Owner, Name, Value, itSet);
end;

//==============================================================================
// TCnCustomLangManager
//==============================================================================

constructor TCnCustomLangManager.Create;
begin
  inherited;
  FNotifier := TList.Create;
  FAutoTranslate := True;
  FAutoTransOptions := [atApplication, atForms, atDataModules];
  FUseDefaultFont := True;
  FTranslateOtherFont := True;
  FTranslateListItem := False;
  FTranslateTreeNode := False;
  FIgnoreAction := True;
end;

destructor TCnCustomLangManager.Destroy;
var
  i: Integer;
  P: Pointer;
begin
  for i := 0 to FNotifier.Count - 1 do
  begin
    P := FNotifier[i];
    Dispose(P);
  end;
  FreeAndNil(FNotifier);
  
  FreeAndNil(FOldTransForms);
  FreeAndNil(FOldTransDMs);
  inherited Destroy;
end;

procedure TCnCustomLangManager.TranslateComponent(AComponent: TComponent;
  const BaseName: WideString);
var
  List: TList;
  ABaseName, Prefix: WideString;
  Iterator: ICnLangStringIterator;
  AKey, AValue: WideString;
  APos: Integer;
begin
  if (AComponent <> nil) and (AComponent.Tag = CN_MULTI_LANG_TAG_NOT_TRANSLATE) then
    Exit;

  ABaseName := BaseName;
  if ABaseName = '' then
    ABaseName := GetRecurOwner(AComponent);

  if FTranslationMode = tmByComponents then
  begin
    List := TList.Create;
    List.Add(AComponent); // 必须加入自身，防止被子控件引用而重复翻译
    try
      if AComponent.ComponentCount > 0 then
        TranslateRecurComponent(AComponent, List, ABaseName)
      else
        TranslateRecurObject(AComponent, List, ABaseName);
    finally
      List.Free;
    end;
  end
  else
  begin
    Iterator := FLanguageStorage.CreateIterator;
    if Iterator <> nil then
    begin
      APos := Pos(DefDelimeter, ABaseName);
      if APos > 0 then
        Prefix := Copy(ABaseName, 1, APos - 1)
      else
        Prefix := ABaseName;

      Iterator.StartIterate(Prefix);
      try
        while not Iterator.Eof do
        begin
          Iterator.GetCurrentKeyValue(AKey, AValue);
          TranslateKeyToValue(AKey, AValue);
          Iterator.Next;
        end;
      finally
        Iterator.EndIterate;
      end;
    end;
  end;
end;

procedure TCnCustomLangManager.TranslateRecurComponent(
  AComponent: TComponent;  AList: TList; const BaseName: WideString);
var
  I: Integer;
  T: TComponent;
  IsInList, IsApplication: Boolean;
begin
{$IFDEF DEBUG_MULTILANG}
  CnDebugger.LogEnter('TranslateRecurComponent: ' + BaseName + ' ' + AComponent.Name);
{$ENDIF}
  IsApplication := AComponent is TApplication;
  if AComponent <> nil then
  begin
    if AComponent.Tag = CN_MULTI_LANG_TAG_NOT_TRANSLATE then
      Exit;

    TranslateObject(AComponent, BaseName);
    // 使用 AList 避免子属性和父 Component 重复
    for I := 0 to AComponent.ComponentCount - 1 do
    begin
      T := AComponent.Components[I];
      if IsApplication and (T is TCustomForm) then
        Continue; // 不翻译 Application 的下属 Form，留给 TranslateForm 等来处理

      if T.Tag = CN_MULTI_LANG_TAG_NOT_TRANSLATE then
        Continue;

      IsInList := AList <> nil;
      if IsInList and (AList.IndexOf(T) = -1) then
      begin
        IsInList := False;
        AList.Add(T);
      end;  // 列表为 nil 时不判断，不为 nil 时检测是否已包含

      if not IsInList then            // 不处理某一 Form 有 Parent 的情况。2004.06.01 by Passion
      begin
        if (AComponent is TCustomForm) {and ((AComponent as TCustomForm).Parent = nil)} then
          TranslateRecurComponent(T, AList, BaseName)
        else
          TranslateRecurComponent(T, AList, BaseName + DefDelimeter + AComponent.Name);
      end;
    end;
  end;
{$IFDEF DEBUG_MULTILANG}
  CnDebugger.LogLeave('TranslateRecurComponent: ' + BaseName + ' ' + AComponent.Name);
{$ENDIF}
end;

procedure TCnCustomLangManager.TranslateForm(AForm: TCustomForm);
begin
  LockWindowUpdate(AForm.Handle);
  try
    if FUseDefaultFont and Assigned(FLanguageStorage) then
    begin
      with FLanguageStorage do
      begin
        if FontInited then
        begin
        {$IFDEF DEBUG_MULTILANG}
          CnDebugger.LogMsg('LangManager: FontInited. ');
        {$ENDIF}
          if CurrentLanguageIndex <> -1 then
          begin
            AForm.Font.Name := DefaultFont.Name;
            AForm.Font.Size := DefaultFont.Size;
            AForm.Font.Charset := DefaultFont.Charset;
          end;
        end;
      end;
    end;
    TranslateComponent(AForm, AForm.ClassName);
  finally
    LockWindowUpdate(0);
  end;
end;

procedure TCnCustomLangManager.TranslateObject(AObject: TObject;
  const BaseName: WideString = '');
var
  AList: TList;
begin
{$IFDEF DEBUG_MULTILANG}
  CnDebugger.LogEnter('TranslateObject: ' + BaseName + ' ' + AObject.ClassName);
{$ENDIF}
  AList := TList.Create;
  AList.Add(AObject); // 必须加入自身来防止被子属性引用而重复翻译
  try
    if DoTranslateObject(AObject) then
      TranslateRecurObject(AObject, AList, BaseName);
  finally
    AList.Free;
  end;
{$IFDEF DEBUG_MULTILANG}
  CnDebugger.LogLeave('TranslateObject: ' + BaseName + ' ' + AObject.ClassName);
{$ENDIF}
end;

procedure TCnCustomLangManager.TranslateRecurObject(AObject: TObject;
  AList: TList; const BaseName: WideString);
var
  i: Integer;
  APropName, APropValue, TransStr, AStr: WideString;
  APropType: TTypeKind;
  Data: PTypeData;
  ActionObj, SubObj: TObject;
  AItem: TCollectionItem;
  AListItem: TListItem;
  ATreeNode: TTreeNode;
  IsForm, IsInList: Boolean;
  NeedCheckIgnoreAction: Boolean;
  ActionCaption, ActionHint: WideString;
  Info: PPropInfo;
begin
  if (AObject <> nil) {and (AList <> nil)} and Assigned(FLanguageStorage) then
  begin
    // 避免传入一些野了的 AObject 导致死循环，曾在 IDE 内部出现过
    try
      if AObject.ClassType = AObject.ClassParent then
        Exit;
      
      if (AObject.ClassParent <> nil) and (AObject.ClassParent.ClassParent = AObject.ClassType) then
        Exit;
    except
      Exit;
    end;
    
    if (AObject is TCnCustomLangStorage) or (AObject is TCnCustomLangStorage)
      or ((AObject is TComponent) and ((AObject as TComponent).Name = '')) then
        Exit;

    if (AObject is TStrings) then  // Strings的对象直接翻译 Text 属性。
    begin
      AStr := 'Text';
      // 调用翻译某属性前的事件
      if not DoTranslateObjectProperty(AObject, AStr) then
        Exit;

      if BaseName <> '' then
        AStr := BaseName + DefDelimeter + AStr;

      TransStr := TranslateString(AStr);
      if TransStr <> '' then
        (AObject as TStrings).Text := TransStr;

      Exit;
    end
    else if (AObject is TCollection) then // TCollection 对象遍历其 Item
    begin
      for i := 0 to (AObject as TCollection).Count - 1 do
      begin
        AItem := (AObject as TCollection).Items[i];

        IsInList := AList <> nil;
        if IsInList and (AList.IndexOf(AItem) = -1) then
        begin
          IsInList := False;
          AList.Add(AItem);
        end;

        if not IsInList then
        begin
          if BaseName <> '' then
            TranslateRecurObject(AItem, AList, BaseName + DefDelimeter +
              'Item' + InttoStr(i))
          else
            TranslateRecurObject(AItem, AList, 'Item' + InttoStr(i));
        end;
      end;
    end
    // ListView 在需要时遍历其 Item
    else if FTranslateListItem and (AObject is TListView) then
    begin
      for i := 0 to (AObject as TListView).Items.Count - 1 do
      begin
        AListItem := (AObject as TListView).Items[i];

        IsInList := AList <> nil;
        if IsInList and (AList.IndexOf(AListItem) = -1) then
        begin
          IsInList := False;
          AList.Add(AListItem);
        end;

        if not IsInList then
        begin
          if BaseName <> '' then
            TranslateRecurObject(AListItem, AList, BaseName + DefDelimeter +
              TComponent(AObject).Name + DefDelimeter + 'ListItem' + InttoStr(i))
          else
            TranslateRecurObject(AListItem, AList, TComponent(AObject).Name +
              DefDelimeter + 'ListItem' + InttoStr(i));
        end;
      end;
    end
    // ListItem 翻译其 Caption 属性和 SubItems 属性
    else if FTranslateListItem and (AObject is TListItem) then
    begin
      AStr := 'Caption';
      // 调用翻译某属性前的事件
      if DoTranslateObjectProperty(AObject, AStr) then
      begin
        if BaseName <> '' then
          AStr := BaseName + DefDelimeter + AStr;

        TransStr := TranslateString(AStr);
        if TransStr <> '' then
          (AObject as TListItem).Caption := TransStr;
      end;

      AStr := 'SubItems.Text';
      if BaseName <> '' then
        AStr := BaseName + DefDelimeter + AStr;

      TransStr := TranslateString(AStr);
      if TransStr <> '' then
        (AObject as TListItem).SubItems.Text := TransStr;
      Exit;
    end
    // TreeView 在需要时遍历其 Item
    else if FTranslateTreeNode and (AObject is TTreeView) then
    begin
      for i := 0 to (AObject as TTreeView).Items.Count - 1 do
      begin
        ATreeNode := (AObject as TTreeView).Items[i];

        IsInList := AList <> nil;
        if IsInList and (AList.IndexOf(ATreeNode) = -1) then
        begin
          IsInList := False;
          AList.Add(ATreeNode);
        end;

        if not IsInList then
        begin
          if BaseName <> '' then
            TranslateRecurObject(ATreeNode, AList, BaseName + DefDelimeter +
              TComponent(AObject).Name + DefDelimeter + 'TreeNode' + InttoStr(i))
          else
            TranslateRecurObject(ATreeNode, AList, TComponent(AObject).Name +
              DefDelimeter + 'TreeNode' + InttoStr(i));
        end;
      end;
    end
    // TreeNode 翻译其 Text 属性。
    else if FTranslateTreeNode and (AObject is TTreeNode) then
    begin
      AStr := 'Text';
      // 调用翻译某属性前的事件
      if not DoTranslateObjectProperty(AObject, AStr) then
        Exit;
              
      if BaseName <> '' then
        AStr := BaseName + DefDelimeter + AStr;

      TransStr := TranslateString(AStr);
      if TransStr <> '' then
        (AObject as TTreeNode).Text := TransStr;
      Exit;
    end;

    IsForm := (AObject is TCustomForm) or (AObject is TCustomFrame);
    try
      Data := GetTypeData(AObject.Classinfo);
    except
      Exit; // TChartSeriesList 会在此处出错，不得不抓住屏蔽
    end;

    NeedCheckIgnoreAction := False;
    if FIgnoreAction then
    begin
      // 查找是否有 Action 属性，看是否 nil
      for I := 0 to Data^.PropCount - 1 do
      begin
        APropName := GetPropName(AObject, I);
        if (PropType(AObject, APropName) = tkClass) and (APropName = 'Action') then
        begin
          // 存在 Action 属性，为tkClass
          ActionObj := GetObjectProp(AObject, APropName);
          if (ActionObj <> nil) and (ActionObj is TCustomAction)then
          begin
            // 有 Action 属性不为 nil 的，需要记录对应 Aciton 的 Caption 和 Hint 供比对
            NeedCheckIgnoreAction := True;
            ActionCaption := (ActionObj as TCustomAction).Caption;
            ActionHint := (ActionObj as TCustomAction).Hint;
            Break;
          end;
        end;
      end;
    end;

    for I := 0 to Data^.PropCount - 1 do
    begin
      APropName := GetPropName(AObject, I);
      
      // 不翻译 TComponent 的 Name 属性
      if (AObject is TComponent) and (APropName = 'Name') then
        Continue;

      // 不翻译 TCnComponent 的 About 属性
      if (AObject is TCnComponent) and (APropName = 'About') then
        Continue;

      APropType := PropType(AObject, APropName);
      if (APropType in [tkString, tkLString, tkWString //, tkWChar
        {$IFDEF UNICODE_STRING}, tkUString{$ENDIF}]) then
      begin
        if NeedCheckIgnoreAction then
        begin
          APropValue := VartoStr(GetPropValue(AObject, APropName));
          if ((APropName = 'Caption') and (ActionCaption = APropValue)) or
            ((APropName = 'Hint') and (ActionHint = APropValue)) then
          begin
{$IFDEF DEBUG_MULTILANG}
            CnDebugger.LogFmt('Ignore Property %s because has Action Value %s', [APropName, APropValue]);
{$ENDIF}
            Continue;
          end;
        end;

        Info := GetPropInfo(AObject, APropName);
        if (Info <> nil) and (Info^.SetProc = nil) then // 只读不能写的，躲开
          Continue;

        // 调用翻译某属性前的事件
        if not DoTranslateObjectProperty(AObject, APropName) then
          Continue;

        if IsForm then
          AStr := AObject.ClassName + DefDelimeter + APropName
        else if AObject is TComponent then
          AStr := TComponent(AObject).Name + DefDelimeter + APropName
        else
          AStr := APropName;

        if (BaseName <> '') and not IsForm then
          AStr := BaseName + DefDelimeter + AStr;

        TransStr := TranslateString(AStr);

{$IFDEF DEBUG_MULTILANG}
        CnDebugger.LogFmt('Get Translation Value: %s=%s', [AStr, TransStr]);
{$ENDIF}

        if TransStr <> '' then
          SetPropValue(AObject, APropName, TransStr);
      end
      else if APropType = tkClass then
      begin
        SubObj := GetObjectProp(AObject, APropName);
        if SubObj = nil then
          Continue;

        IsInList := AList <> nil;
        if IsInList and (AList.IndexOf(SubObj) = -1) then
        begin
          IsInList := False;
          AList.Add(SubObj);
        end;

        // 调用翻译某属性前的事件
        if not DoTranslateObjectProperty(AObject, APropName) then
          Continue;
        
        if AObject is TComponent then // 是 Component 则进行复杂的处理
        begin
          if not IsInList then
          begin
      {* 是子对象不是引用或 Owner 不是任何控件的控件，因 Owner 循环的方式访问不到，
         便只有在此处以主控件名.属性名的形式访问。但如果出现这样的情况：一个控件
         有两个属性，连接到两个子控件，这俩子控件的 Owner 都是 nil，但由父控件负
         责创建释放。这两个子控件都有一个属性指向对方，这样就会出现循环引用，所以
         还是得通过 List 方式来避免死循环。  }
            if (AObject is TControl) and (SubObj is TFont) and (APropName = 'Font') then
            begin
              if not IsParentFont(AObject as TControl) then // 不使用 ParentFont 时存字体
              begin
                if not IsForm then
                  AStr := TComponent(AObject).Name + DefDelimeter + SCnControlFont
                else
                  AStr := SCnControlFont;

                if BaseName <> ''  then
                  AStr := BaseName + DefDelimeter + AStr;

                TransStr := TranslateString(AStr);
                if TransStr <> '' then
                  StringToFontEx(TransStr, TCnFontControl(AObject).Font,
                    GetParentFont(AObject as TComponent));
              end;
            end // 不按常规处理 TControl 的字体
            else if FTranslateOtherFont and (SubObj is TFont) then
            begin
              if not IsForm then
                AStr := TComponent(AObject).Name + DefDelimeter +
                  SystemNamePrefix + APropName
              else
                AStr := SystemNamePrefix + APropName;

              if BaseName <> ''  then
                AStr := BaseName + DefDelimeter + AStr;

              TransStr := TranslateString(AStr);

              try
                if TransStr <> '' then
                  StringToFontEx(TransStr, SubObj as TFont,
                    GetParentFont(AObject as TComponent));
              except
                ; // 屏蔽万一碰上的异常
              end;
            end // 处理其他 Font。
            else if (not (SubObj is TComponent)) or     // 如果 SubObj 不是 TComponent 则只能在此通过属性遍历
              ((SubObj as TComponent).Owner = nil) then // 如果 SubObj 的 Owner 不为 nil，则等它的 Owner 遍历下来再说，此处不处理。
            begin
              if IsForm then
                TranslateRecurObject(SubObj, AList, TComponent(AObject).ClassName
                  + DefDelimeter + APropName)
              else if (InheritsFromClassName(AObject, 'TNotebook') or InheritsFromClassName(AObject, 'TTabbedNotebook'))
                and (APropName = 'Pages') then
              // 不翻译 TNotebook/TTabbedNotebook 的 Pages 属性以免出现页面内容丢失。
              else if InheritsFromClassName(AObject, 'TJvWizard') and (APropName = 'Pages') then
              // 不翻译 JVcl Wizards 的 Pages 属性以免 Crash
              else if not (SubObj is TComponent) then
              // 此处应该判断 SubObj 是否是 TComponet 然后决定是否调用 TranslateRecurComponent
                TranslateRecurObject(SubObj, AList, BaseName + DefDelimeter +
                  TComponent(AObject).Name + DefDelimeter + APropName)
              else
                TranslateRecurComponent((SubObj as TComponent), AList, BaseName + DefDelimeter +
                  TComponent(AObject).Name + DefDelimeter + APropName)
            end;
          end;
        end
        else // AObject 不是 Component 则直接翻译它和它的属性
        begin
          if not IsInList then
            TranslateRecurObject(SubObj, AList, BaseName + DefDelimeter + APropName);
        end;
      end;
    end;
  end;
end;

procedure TCnCustomLangManager.SetCurrentLanguageIndex(
  const Value: Integer);
var
  I: Integer;
  Iterator: ICnLangStringIterator;
  AKey, AValue: WideString;
begin
  inherited;

  // 设计期不进行翻译
  if not (csDesigning in ComponentState) and FAutoTranslate
    and (LanguageStorage <> nil) then
  begin
    if FTranslationMode = tmByComponents then
    begin
      if atForms in FAutoTransOptions then
        for I := 0 to Screen.CustomFormCount - 1 do
          TranslateForm(Screen.CustomForms[I]);

      if atDataModules in FAutoTransOptions then
        for I := 0 to Screen.DataModuleCount - 1 do
          TranslateComponent(Screen.DataModules[I]);

      if atApplication in FAutoTransOptions then
        TranslateComponent(Application);
    end
    else // 基于翻译条目
    begin
      Iterator := FLanguageStorage.CreateIterator;
      if Iterator <> nil then
      begin
        Iterator.StartIterate;
        try
          while not Iterator.Eof do
          begin
            Iterator.GetCurrentKeyValue(AKey, AValue);
            TranslateKeyToValue(AKey, AValue);
            Iterator.Next;
          end;
        finally
          Iterator.EndIterate;
        end;
      end;
    end;
  end;
end;

procedure TCnCustomLangManager.AddChangeNotifier(Notify: TNotifyEvent);
var
  P: PCnLangChangedNotifierRecord;
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := 0 to FNotifier.Count - 1 do
    if SameMethod(TMethod(PCnLangChangedNotifierRecord(FNotifier[I])^.Notifier),
      TMethod(Notify)) then
    begin
      Found := True;
      Break;
    end;

  if not Found then
  begin
    New(P);
    P^.Notifier := TMethod(Notify);
    FNotifier.Add(P);
  end;
end;

procedure TCnCustomLangManager.RemoveChangeNotifier(Notify: TNotifyEvent);
var
  P: PCnLangChangedNotifierRecord;
  Idx, I: Integer;
begin
  Idx := -1;
  for I := 0 to FNotifier.Count - 1 do
    if SameMethod(TMethod(PCnLangChangedNotifierRecord(FNotifier[I])^.Notifier),
      TMethod(Notify)) then
    begin
      Idx := I;
      Break;
    end;

  if Idx >= 0 then
  begin
    P := FNotifier[Idx];
    Dispose(P);
    FNotifier.Delete(Idx);
  end;
end;

procedure TCnCustomLangManager.DoLanguageChanged;
var
  I: Integer;
begin
  inherited; // 先响应父类的语言改变事件，再实施通知。
  for I := 0 to FNotifier.Count - 1 do
    TNotifyEvent(PCnLangChangedNotifierRecord(FNotifier.Items[I])^.Notifier)(Self);
end;

function TCnCustomLangManager.DoTranslateObject(AObject: TObject): Boolean;
begin
  Result := True;
  if Assigned(FOnTranslateObject) then
    FOnTranslateObject(AObject, Result);
end;

function TCnCustomLangManager.DoTranslateObjectProperty(AObject: TObject;
  const PropName: WideString): Boolean;
begin
  Result := True;
  if Assigned(FOnTranslateObjectProperty) then
    FOnTranslateObjectProperty(AObject, PropName, Result);
end;

function TCnCustomLangManager.GetRecurOwner(AComponent: TComponent): WideString;
begin
  if (AComponent is TCustomForm) or (AComponent is TDataModule) then
    Result := AComponent.ClassName
  else if AComponent.Owner <> nil then
  begin
    if AComponent.Owner is TCustomForm then
      Result := AComponent.Owner.ClassName
    else
      Result := GetRecurOwner(AComponent.Owner) + DefDelimeter + AComponent.Owner.Name;
  end;
end;

procedure TCnCustomLangManager.SetTranslationMode(
  const Value: TCnTranslationMode);
begin
  FTranslationMode := Value;
end;

procedure TCnCustomLangManager.TranslateKeyToValue(const Key,
  Value: WideString);
var
  I, APos: Integer;
  Prefix: WideString;
begin
  if Key = '' then
    Exit;

  APos := Pos(DefDelimeter, Key);
  if APos = 0 then // 不带点号的不在此翻译
    Exit;

  Prefix := Copy(Key, 1, APos - 1);

  if atForms in FAutoTransOptions then
  begin
    if (Prefix <> FOldFormPrefix) or not Assigned(FOldTransForms) then
    begin
      if not Assigned(FOldTransForms) then
        FOldTransForms := TList.Create
      else
        FOldTransForms.Clear;

      for I := 0 to Screen.CustomFormCount - 1 do
        if Screen.CustomForms[I].ClassNameIs(Prefix) then
          FOldTransForms.Add(Screen.CustomForms[I]);
    end;

    for I := 0 to FOldTransForms.Count - 1 do
      SetValueByTransName(TComponent(FOldTransForms.Items[I]), Key, Value);
  end;

  if atDataModules in FAutoTransOptions then
  begin
    if (Prefix <> FOldDMPrefix) or not Assigned(FOldTransDMs) then
    begin
      if not Assigned(FOldTransDMs) then
        FOldTransDMs := TList.Create
      else
        FOldTransDMs.Clear;

      for I := 0 to Screen.DataModuleCount - 1 do
        if Screen.DataModules[I].ClassNameIs(Prefix) then
          FOldTransDMs.Add(Screen.DataModules[I]);
    end;

    for I := 0 to FOldTransDMs.Count - 1 do
      SetValueByTransName(TComponent(FOldTransDMs.Items[I]), Key, Value);
  end;

  if atApplication in FAutoTransOptions then
    if Prefix = 'Application' then
      SetValueByTransName(Application, Key, Value);
end;

procedure FreeLanguageManagers;
var
  I: Integer;
begin
  if Assigned(FLangMgrList) then
  begin
    if FLangMgrList.Count > 0 then
      for I := FLangMgrList.Count - 1 downto 0 do
        if FLangMgrList.Items[I] <> nil then
          TObject(FLangMgrList.Items[I]).Free;

    FreeAndNil(FLangMgrList);
  end;
end;

procedure TCnBaseLangManager.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnLangMgrName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnLangMgrComment;
end;

procedure RegisterTranslateResourceString(
  const ResStringAddr: Pointer; const IDStr: WideString);
var
  AObj: TCnResourceStringObj;
begin
  if ResStringAddr <> nil then
  begin
    AObj := TCnResourceStringObj.Create;
    AObj.StringRecAddr := ResStringAddr;
    AObj.StringName := IDStr;
    FRegResStrings.Add(AObj);
  end;
end;

procedure DoRegisterTranslateString(const StringAddr: Pointer; const IDStr: WideString; AType: TCnStrObjType);
var
  AObj: TCnStringObj;
begin
  if StringAddr <> nil then
  begin
    AObj := TCnStringObj.Create;
    AObj.StringAddr := StringAddr;
    AObj.StringName := IDStr;
    AObj.FType := AType;
    FRegStrings.Add(AObj);
  end;
end;

procedure RegisterTranslateString(const StringAddr: PCnString; const IDStr: WideString);
begin
  DoRegisterTranslateString(StringAddr, IDStr, csotString);
end;

procedure RegisterTranslateStringA(const StringAddr: PAnsiString; const IDStr: WideString);
begin
  DoRegisterTranslateString(StringAddr, IDStr, csotAnsi);
end;

procedure RegisterTranslateStringW(const StringAddr: PWideString; const IDStr: WideString);
begin
  DoRegisterTranslateString(StringAddr, IDStr, csotWide);
end;

procedure TranslateReggedStrings;
var
  I: Integer;
  AObj: TCnStringObj;
  BObj: TCnResourceStringObj;
  DstStr: WideString;
  OldProtect: Cardinal;
begin
  if CnLanguageManager = nil then
    raise Exception.Create('Language Manager NOT initialized.');

  for I := 0 to FRegStrings.Count - 1 do
  begin
    AObj := TCnStringObj(FRegStrings[I]);
    DstStr := CnLanguageManager.TranslateString(AObj.StringName);
    if DstStr <> '' then
    begin
      if AObj.FType = csotString then
        PCnString(AObj.FStringAddr)^ := string(DstStr)
      else if AObj.FType = csotWide then
        PWideString(AObj.FStringAddr)^ := DstStr
      else
        PAnsiString(AObj.FStringAddr)^ := AnsiString(DstStr);
    end;
  end;

  for I := 0 to FRegResStrings.Count - 1 do
  begin
    BObj := TCnResourceStringObj(FRegResStrings[I]);
    DstStr := CnLanguageManager.TranslateString(BObj.StringName);
    if DstStr <> '' then
    begin
      BObj.FDstStr := DstStr; // 保存一份字符串引用
      VirtualProtect(BObj.StringRecAddr, SizeOf(TResStringRec), PAGE_EXECUTE_READWRITE, @OldProtect);
{$IFDEF WIN64}
      PResStringRec(BObj.StringRecAddr)^.Identifier := NativeUint(BObj.FDstStr);
{$ELSE}
      PResStringRec(BObj.StringRecAddr)^.Identifier := Integer(BObj.FDstStr);
{$ENDIF}
      VirtualProtect(BObj.StringRecAddr, SizeOf(TResStringRec), OldProtect, nil);
    end;
  end;
end;

initialization
  FRegStrings := TObjectList.Create(True);
  FRegResStrings := TObjectList.Create(True);

finalization
  FreeLanguageManagers;
  FRegStrings.Free;
  FRegResStrings.Free;

end.
