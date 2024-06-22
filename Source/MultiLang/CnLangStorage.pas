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

unit CnLangStorage;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：多语包存储组件基类单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：该单元实现了多语包的存储抽象基类
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2004.10.23 V1.2
*               修改初始化文件的处理方式，增加对目录的搜索
*           2003.12.13 V1.1
*               将 DefaultFont 的属性来源移动到 LanguageItem 中
*           2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, FileCtrl, CnCommon,
  CnConsts, CnClasses, CnLangCollection, CnLangConsts, CnIniStrUtils;

{$IFDEF Linux}
  {$I QLangIDs.inc}
{$ENDIF}

const
  DefDelimeter        = '.';
  DefEqual            = '=';
  
  SystemNamePrefix    = '!';
  SCnLanguageID       = 'LanguageID';
  SCnLanguageName     = 'LanguageName';
  SCnAuthor           = 'TranslationAuthor';
  SCnAuthorEmail      = 'TranslationAuthorEmail';
  SCnDefaultFont      = 'TranslationDefaultFont';
  SCnSize             = 'Size';
  SCnName             = 'Name';
  SCnCharset          = 'Charset';

  SCnControlFont      = SystemNamePrefix + 'Font';

type
  TLanguageChangeEvent = procedure(Sender: TObject; ALanguageIndex: Integer)
    of object;
  TLanguageChangingEvent = procedure(Sender: TObject; ALanguageIndex: Integer;
    var AllowChange: Boolean) of object;

  ELanguageStorageError = class(Exception)
  end;

  ICnLangStringIterator = interface(IUnknown)
  {* 获得全部字符串的 Iterator 接口定义 }
    ['{247CB225-2257-41C0-87F8-F43834E2966F}']
    procedure StartIterate(const FrontPattern: TCnLangString = '');
    procedure Previous;
    procedure Next;
    procedure EndIterate;
    procedure GetCurrentKeyValue(var Key: TCnLangString; var Value: TCnLangString);
    function GetCurrentString: TCnLangString;
    function GetEof: Boolean;
    function GetBof: Boolean;
    
    property Eof: Boolean read GetEof;
    property Bof: Boolean read GetBof;
  end;

  TCnCustomLangStorage = class(TCnComponent)
  private
    FDefaultFont: TFont;
    FFontInited: Boolean;
    FLanguages: TCnLanguageCollection;
    FOnLanguageChanged: TLanguageChangeEvent;
    FOnLanguageChanging: TLanguageChangingEvent;
    function GetCurrentLanguage: TCnLanguageItem;
    function GetLanguageCount: Integer;
    procedure SetCurrentLanguageIndex(Value: Integer);
    procedure SetLanguages(Value: TCnLanguageCollection);
  protected
    FCurrentLanguageIndex: Integer;
    FDefaultLanguageID: Integer;
    procedure DoLanguageChanged(ALanguageIndex: Integer); virtual;
    procedure DoLanguageChanging(ALanguageIndex: Integer;
      var AllowChange: Boolean); virtual;

    function GetAuthor(var Value: TCnLangString): Boolean;

    function GetAuthorEmail(var Value: TCnLangString): Boolean;

    function GetDefaultFont(const Value: TFont): Boolean;
    
    function GetLanguageID(var Value: LongWord): Boolean;
    
    procedure InternalInit; virtual; abstract;
    {* 抽象方法，从存储介质中初始化所有语言条目 }
    
    procedure CreateCurrentLanguage; virtual; abstract;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure AddLanguage(ALanguageID: LongWord);
    {* 增加一种语言 }
    function GetString(Name: TCnLangString; var Value: TCnLangString): Boolean; virtual;
      abstract;
    {* 抽象方法，获得翻译字串 }
    procedure GetNamesList(List: TStrings); virtual; abstract;
    {* 抽象方法，获得当前语言的所有翻译条目名称列表 }
    procedure ClearCurrentLanguage; virtual; abstract;
    {* 抽象方法，删除当前语言的所有翻译条目列表 }
    function LoadCurrentLanguage: Boolean; virtual; abstract;
    {* 抽象方法，可以是从存储介质中载入当前语言条目，为翻译字串做准备 }
    procedure SaveCurrentLanguage; virtual; abstract;
    {* 抽象方法，可以是保存当前语言条目到存储介质中 }
    procedure SetString(Name, Value: TCnLangString); virtual; abstract;
    {* 抽象方法，设置翻译字串 }
    function CreateIterator: ICnLangStringIterator; virtual;
    {* 抽象方法，获得遍历器，如果子类不支持遍历，则必须返回 nil}
    property CurrentLanguage: TCnLanguageItem read GetCurrentLanguage;
    {* 当前语言条目对象 }
    property CurrentLanguageIndex: Integer read FCurrentLanguageIndex write
      SetCurrentLanguageIndex;
    {* 当前语言号 }
    property DefaultFont: TFont read FDefaultFont; // write SetDefaultFont;
    {* 默认的字体 }
    property DefaultLanguageID: Integer read FDefaultLanguageID;
    {* 默认语言的 ID }
    property FontInited: Boolean read FFontInited write FFontInited;
    {* 字体是否已经初始化 }
    property LanguageCount: Integer read GetLanguageCount;
    {* 语言数 }
    property Languages: TCnLanguageCollection read FLanguages write
      SetLanguages;
    {* 所有语言列表 }
  published
    property OnLanguageChanged: TLanguageChangeEvent read FOnLanguageChanged
      write FOnLanguageChanged;
    {* 当前语言号改变后触发 }
    property OnLanguageChanging: TLanguageChangingEvent read FOnLanguageChanging
      write FOnLanguageChanging;
    {* 当前语言号改变前触发，可控制是否允许改变 }
  end;

  TCnStorageMode = (smByFile, smByDirectory);
  {* 存储类型，按同一目录下多文件存储还是不同目录下的同一文件名存储 }

  TCnCustomLangFileStorage = class (TCnCustomLangStorage)
  private
    FLanguagePath: TCnLangString;
    FAutoDetect: Boolean;
    FStorageMode: TCnStorageMode;
    FFileName: TCnLangString;
    FDesignLangPath: TCnLangString;
    FDesignLangFile: TCnLangString;
    procedure SetLanguagePath(Value: TCnLangString);
    procedure SetAutoDetect(const Value: Boolean);
    procedure SetStorageMode(const Value: TCnStorageMode);
    procedure SetFileName(const Value: TCnLangString);
    // LanguagePath 为空时，调整为需要的目录
    procedure AdjustLangPath;
    procedure AdjustLangFile;
  protected
    procedure InternalInit; override;
    procedure Loaded; override;
    procedure InitFromAFile(const AFileName: TCnLangString); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDesignLangPath(const aPath: TCnLangString);
    procedure SetDesignLangFile(const aFile: TCnLangString);

    function GetCurrentLanguageFileName: TCnLangString; virtual;
   {* 获得当前语言的语言文件名，包括扩展名 }
    class function GetLanguageFileExt: TCnLangString; virtual;
    {* 抽象方法，获得所有语言文件的统一扩展名 }
    function IsLanguageFile(const FileName: TCnLangString): Boolean; virtual; abstract;
    {* 抽象方法，判断某一文件是否是合法的语言文件 }

    property StorageMode: TCnStorageMode read FStorageMode write SetStorageMode;
    {* 多语文件按目录存储还是按文件存储 }
    property LanguagePath: TCnLangString read FLanguagePath write SetLanguagePath;
    {* 所有语言文件存储的统一目录名 }
    property FileName: TCnLangString read FFileName write SetFileName;
    {* 多语文件按目录存储时具有的统一文件名 }
    property AutoDetect: Boolean read FAutoDetect write SetAutoDetect default True;
    {* LanguagePath 改变时是否自动检测语言 }

    // 此两属性用来做设计期的实际的 LangPath/File 使用，
    // 适用于 LanguagePath/File 为空而具体需要设置目录/文件的场合
    // 只能由 Translator 的组件编辑器赋值。不对用户开放
    property DesignLangPath: TCnLangString read FDesignLangPath;
    property DesignLangFile: TCnLangString read FDesignLangFile;
  end;

implementation

uses
  CnLangMgr;

//==============================================================================
// TCustomLanguageStorage
//==============================================================================

constructor TCnCustomLangStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultLanguageID := GetSystemDefaultLCID;
  FDefaultFont := TFont.Create;
  FDefaultFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
  FCurrentLanguageIndex := -1;
  FLanguages := TCnLanguageCollection.Create(Self);

  if CnLanguageManager <> nil then
    if CnLanguageManager.LanguageStorage = nil then
      CnLanguageManager.LanguageStorage := Self;
end;

destructor TCnCustomLangStorage.Destroy;
begin
  FreeAndNil(FDefaultFont);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

procedure TCnCustomLangStorage.AddLanguage(ALanguageID: LongWord);
begin
  with Languages.Add do
  begin
    
    LanguageID := ALanguageID;
  end;
  CreateCurrentLanguage;
end;

procedure TCnCustomLangStorage.DoLanguageChanged(ALanguageIndex: Integer);
begin
  if Assigned(FOnLanguageChanged) then
    FOnLanguageChanged(Self, ALanguageIndex);
end;

procedure TCnCustomLangStorage.DoLanguageChanging(ALanguageIndex: Integer;
  var AllowChange: Boolean);
begin
  if Assigned(FOnLanguageChanging) then
    FOnLanguageChanging(Self, ALanguageIndex, AllowChange);
end;

function TCnCustomLangStorage.GetAuthor(var Value: TCnLangString): Boolean;
begin
  Result := GetString(SystemNamePrefix + SCnAuthor, Value);
end;

function TCnCustomLangStorage.GetAuthorEmail(var Value: TCnLangString): Boolean;
begin
  Result := GetString(SystemNamePrefix + SCnAuthorEmail, Value);
end;

function TCnCustomLangStorage.GetCurrentLanguage: TCnLanguageItem;
begin
  if CurrentLanguageIndex <> -1 then
    Result := TCnLanguageItem(Languages.Items[CurrentLanguageIndex])
  else
    Result := nil;
end;

function TCnCustomLangStorage.GetDefaultFont(const Value: TFont): Boolean;
var
  S: TCnLangString;
begin
  S := '';
  if GetString(SystemNamePrefix + SCnDefaultFont, S) then
    StringToFont(S, Value);
  Result := (S <> '');
end;

function TCnCustomLangStorage.GetLanguageCount: Integer;
begin
  Result := FLanguages.Count;
end;

function TCnCustomLangStorage.GetLanguageID(var Value: LongWord): Boolean;
var
  S: TCnLangString;
begin
  Result := GetString(SystemNamePrefix + SCnLanguageID, S);
  if Result then
    Value := StrToIntDef(S, 0);
end;

procedure TCnCustomLangStorage.SetCurrentLanguageIndex(Value: Integer);
var
  AllowChange: Boolean;
begin
  if (Value >= 0) and (Value < LanguageCount) then
  begin
    //if Value <> FCurrentLanguageIndex then
    begin
      AllowChange := True;
      DoLanguageChanging(Value, AllowChange);
      if not AllowChange then
        Exit;

      FCurrentLanguageIndex := Value;
      if LoadCurrentLanguage then
      begin
        FDefaultFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
        GetDefaultFont(FDefaultFont);
        FontInited := True;
        DoLanguageChanged(Value);
      end;
    end;
  end;
end;

procedure TCnCustomLangStorage.SetLanguages(Value: TCnLanguageCollection);
begin
  FLanguages.Assign(Value);
end;

//==============================================================================
// TCustomLanguageFileStorage
//==============================================================================

procedure TCnCustomLangFileStorage.AdjustLangPath;
begin
  FDesignLangPath := FLanguagePath;
  if FLanguagePath = '' then
  begin
    if not (csDesigning in ComponentState) then
    begin
      // 运行期，只有采用可执行文件的所在目录
      FDesignLangPath := IncludeTrailingBackslash(_CnExtractFilePath(Application.ExeName));
    end;
  end;
end;

procedure TCnCustomLangFileStorage.AdjustLangFile;
begin
  FDesignLangFile := FFileName;
  if FFileName = '' then
  begin
    if not (csDesigning in ComponentState) then
    begin
      // 运行期，只有采用可执行文件的文件名加自己的扩展名
      FFileName := _CnChangeFileExt(_CnExtractFileName(Application.ExeName), GetLanguageFileExt);
    end;
  end;
end;

constructor TCnCustomLangFileStorage.Create(AOwner: TComponent);
begin
  inherited;
  FAutoDetect := True;
end;

destructor TCnCustomLangFileStorage.Destroy;
begin
  inherited;
end;

function TCnCustomLangFileStorage.GetCurrentLanguageFileName: TCnLangString;
begin
  if Assigned(CurrentLanguage) then
  begin
    if FStorageMode = smByFile then
      Result := CurrentLanguage.LanguageFileName + GetLanguageFileExt
    else if (FFileName = '') and (FDesignLangFile <> '') then // 设计期采用组件编辑器设置的工程文件名
      Result := IncludeTrailingBackslash(CurrentLanguage.LanguageDirName) + _CnChangeFileExt(FDesignLangFile, GetLanguageFileExt)
    else if Pos('.', FFileName) > 0 then
      Result := IncludeTrailingBackslash(CurrentLanguage.LanguageDirName) + FFileName
    else
      Result := IncludeTrailingBackslash(CurrentLanguage.LanguageDirName) + _CnChangeFileExt(FFileName, GetLanguageFileExt);
  end
  else
    Result := '';
end;

// 返回语言文件的扩展名，默认为txt，子类一般重载。
class function TCnCustomLangFileStorage.GetLanguageFileExt: TCnLangString;
begin
  Result := '.txt';
end;

procedure TCnCustomLangFileStorage.InitFromAFile(const AFileName: TCnLangString);
var
  AStr: TCnLangString;
  AID: LongWord;
begin
  // 几乎没用，可以不写
  with Languages.Add do
  begin
    LanguageFileName := _CnExtractFileName(_CnChangeFileExt(AFileName, ''));
    Self.CurrentLanguageIndex := Index;
    try
      if GetLanguageID(AID) then
        LanguageID := AID;
      if GetString(SystemNamePrefix + SCnLanguageName, AStr) then
        LanguageName := AStr;
      if GetString(SystemNamePrefix + SCnAuthor, AStr) then
        Author := AStr;
      if GetString(SystemNamePrefix + SCnAuthorEmail, AStr) then
        AuthorEmail := AStr;
      if GetString(SystemNamePrefix + SCnDefaultFont, AStr) then
      begin
        StringToFont(AStr, DefaultFont);
      end;
    except
      Self.FCurrentLanguageIndex := -1;
      Free;
    end;
  end;
end;

procedure TCnCustomLangFileStorage.InternalInit;
var
  Sr: TSearchRec;
  ActualPath,  AFileName, ASearchPath: TCnLangString;
begin
  Languages.BeginUpdate;
  try
    Languages.Clear;
    Self.FCurrentLanguageIndex := -1;

    ActualPath := FLanguagePath;
    if (csDesigning in ComponentState) then
    begin
      if (ActualPath = '') and (FDesignLangPath <> '') then
      begin
        ActualPath := FDesignLangPath;
      end;
    end
    else if ActualPath = '' then
      ActualPath := _CnExtractFileDir(Application.ExeName);

    if ActualPath = '' then
      Exit;
      
    ActualPath := IncludeTrailingBackslash(ActualPath);

    if FStorageMode = smByFile then
    begin
      ASearchPath := ActualPath + '*' + GetLanguageFileExt;
      if FindFirst(ASearchPath, faAnyFile, Sr) = 0 then
      begin
        repeat
          AFileName := ActualPath + Sr.Name;
          if IsLanguageFile(AFileName) then
            InitFromAFile(AFileName);
        until FindNext(Sr) <> 0;
        FindClose(Sr);
      end;
    end
    else
    begin
      // 设计期如未设置 FileName，则无法自动搜索各个语言文件
      if FFileName = '' then
        Exit;

      ASearchPath := ActualPath + '*';
      if FindFirst(ASearchPath, faDirectory, Sr) = 0 then
      begin
        repeat
          if (Sr.Name = '.') or (Sr.Name = '..') or (Sr.Attr and faDirectory = 0) then
            Continue;

          AFileName := ActualPath + Sr.Name + '\' + _CnChangeFileExt(FFileName, GetLanguageFileExt);

          if FileExists(AFileName) and IsLanguageFile(AFileName) then
            InitFromAFile(AFileName);

        until FindNext(Sr) <> 0;
        FindClose(Sr);
      end;
    end;
  finally
    Languages.EndUpdate;
  end;
end;

// 运行期如未设置路径则以可执行文件所在路径为准，未设置文件名则以可执行文件名为准
procedure TCnCustomLangFileStorage.Loaded;
begin
  inherited;
  if csDesigning in ComponentState then Exit;

  if Self.FLanguagePath = '' then
    Self.FLanguagePath := _CnExtractFilePath(Application.ExeName);
  AdjustLangPath;
  if Self.FFileName = '' then
    Self.FFileName := _CnChangeFileExt(_CnExtractFileName(Application.ExeName), '');
  AdjustLangFile;
end;

procedure TCnCustomLangFileStorage.SetAutoDetect(const Value: Boolean);
begin
  FAutoDetect := Value;
  if Value then
  begin
    AdjustLangPath;
    AdjustLangFile;
    InternalInit;
  end;
end;

procedure TCnCustomLangFileStorage.SetFileName(const Value: TCnLangString);
begin
  FFileName := Value;
  if FAutoDetect and (StorageMode = smByDirectory) then
  begin
    AdjustLangPath;
    AdjustLangFile;
    InternalInit;
  end;
end;

procedure TCnCustomLangFileStorage.SetLanguagePath(Value: TCnLangString);
begin
  if FLanguagePath <> Value then
  begin
    if Value <> '' then
      FLanguagePath := IncludeTrailingBackslash(Value)
    else
      FLanguagePath := Value;

    if FAutoDetect then
    begin
      AdjustLangPath;
      AdjustLangFile;
      InternalInit;
    end;
  end;
end;

procedure TCnCustomLangFileStorage.SetDesignLangPath(const aPath: TCnLangString);
begin
  if csDesigning in ComponentState then
    FDesignLangPath := aPath;
end;

procedure TCnCustomLangFileStorage.SetDesignLangFile(const aFile: TCnLangString);
begin
  if csDesigning in ComponentState then
    FDesignLangFile := aFile;
end;

procedure TCnCustomLangFileStorage.SetStorageMode(
  const Value: TCnStorageMode);
begin
  if (FStorageMode <> Value) or (csLoading in ComponentState) then
  begin
    FStorageMode := Value;
    AdjustLangPath;
    AdjustLangFile;
    InternalInit;
  end;
end;

function TCnCustomLangStorage.CreateIterator: ICnLangStringIterator;
begin
  Result := nil;
end;

procedure TCnCustomLangStorage.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
// 基类无信息
end;

end.
