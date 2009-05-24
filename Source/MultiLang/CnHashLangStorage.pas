{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnHashLangStorage;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：Hash 文本多语存储组件单元
* 单元作者：CnPack开发组
* 备    注：该单元实现了Hash TXT 多语存储组件类
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnHashLangStorage.pas,v 1.16 2009/01/02 08:27:39 liuxiao Exp $
* 修改记录：2004.10.23 V1.2
*               修改初始化文件的处理方式
*           2003.12.13 V1.1
*               InternalInit 中增加对 DefaultFont 的读入处理
*           2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, IniFiles, Dialogs, FileCtrl,
  CnConsts, CnLangConsts, CnHashMap, CnLangStorage, CnIniStrUtils;

type
  TCnCustomHashLangStorage = class;

  TCnHashStringIterator = class(TInterfacedObject, ICnLangStringIterator)
  private
    FHashStorage: TCnCustomHashLangStorage;
    FEof: Boolean;
    FBof: Boolean;
    FFrontPattern: string;
    FKey: string;
    FValue: string;
  public
    constructor Create(AHashStorage: TCnCustomHashLangStorage);
    destructor Destroy; override;

    procedure StartIterate(const FrontPattern: string = '');
    procedure Previous;
    procedure Next;
    procedure EndIterate;
    procedure GetCurrentKeyValue(var Key:string; var Value: string);
    function GetCurrentString: string;
    function GetEof: Boolean;
    function GetBof: Boolean;

    property Eof: Boolean read GetEof;
    property Bof: Boolean read GetBof;
  end;

  TCnCustomHashLangStorage = class(TCnCustomLangFileStorage)
  private
    FHashMap: TCnStrToStrHashMap;
    FListLength: Integer;
    FIncSize: Integer;
    procedure SetIncSize(const Value: Integer);
    procedure SetListLength(const Value: Integer);
  protected
    procedure InitHashMap;
    procedure AddStringToHashMap(const Key: string; const Value: string);
    procedure InitFromAFile(const AFileName: string); override;
    procedure CreateCurrentLanguage; override;
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    property HashMap: TCnStrToStrHashMap read FHashMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageFileExt: string; override;
    {* 返回多语言文件的扩展名.TXT }
    function GetString(Name: string; var Value: string): Boolean; override;
    {* 获得一语言条目的翻译后的字符串 }
    procedure GetNamesList(List: TStrings); override;
    {* 获得当前语言的所有语言条目名称列表 }
    function IsLanguageFile(const FileName: string): Boolean; override;
    {* 判断一文件是否合法的语言文件 }
    procedure SetString(Name, Value: string); override;
    {* 保存一语言条目，如存在则覆盖，否则新增 }
    function CreateIterator: ICnLangStringIterator; override;
    {* 获得遍历器，如果子类不支持遍历，则必须返回 nil}
    function LoadCurrentLanguage: Boolean; override;
    {* 从 TXT 文件中载入当前语言条目，为翻译字串做准备 }
    procedure SaveCurrentLanguage; override;
    {* 保存当前语言文件 }
    procedure ClearCurrentLanguage; override;
    {* 删除当前语言的所有翻译条目列表，同时也保存本语言 }
  published
    property StorageMode;
    {* 多语存储方式 }
    property LanguagePath;
    {* 语言文件存储的统一路径 }
    property FileName;
    {* 按文件存储时的统一多语文件名 }
    property Languages;
    {* 语言对象列表 }
    property ListLength: Integer read FListLength write SetListLength;
    {* 初始列表大小 }
    property IncSize: Integer read FIncSize write SetIncSize;
    {* 重分配时增加的大小 }
    property AutoDetect;
    {* LanguagePath 改变时是否自动检测语言 }
  end;

  TCnHashLangFileStorage = class(TCnCustomHashLangStorage)
  published
    property StorageMode;
    {* 多语存储方式 }
    property LanguagePath;
    {* 语言文件存储的统一路径 }
    property Languages;
    {* 语言对象列表 }
    property FileName;
    {* 按文件存储时的统一多语文件名 }
    property ListLength;
    {* 初始列表大小 }
    property IncSize;
    {* 重分配时增加的大小 }
    property AutoDetect;
    {* LanguagePath 改变时是否自动检测语言 }
  end;

implementation

{************************** TCnCustomHashLangStorage **************************}

constructor TCnCustomHashLangStorage.Create(AOwner: TComponent);
begin
  inherited;
  Self.FListLength := 1024;
  Self.FIncSize := 2;
end;

destructor TCnCustomHashLangStorage.Destroy;
begin
  if Assigned(FHashMap) then
    FHashMap.Free;
  inherited;
end;

procedure TCnCustomHashLangStorage.CreateCurrentLanguage;
begin
  InitHashMap;
end;

function TCnCustomHashLangStorage.GetString(Name: string; var Value: string):
  Boolean;
begin
  Result := False;
  if Assigned(FHashMap) then
  begin
    Result := FHashMap.Find(Name, Value);
    if Result then
      Value := StringReplace(Value, SCnBR, SCnCRLF, [rfReplaceAll, rfIgnoreCase])
    else
      Value := '';
  end;
end;

function TCnCustomHashLangStorage.LoadCurrentLanguage: Boolean;
var
  List: TStrings;
  i, EPos: Integer;
  S: string;
begin
  Result := True;
  InitHashMap;
  
  List := TStringList.Create;
  try
    S := LanguagePath + GetCurrentLanguageFileName;
    List.LoadFromFile(S);
  except
    Result := False;
    List.Free;
    Exit;
  end;

  for i := 0 to List.Count - 1 do
  begin
    S := List[i];
    EPos := Pos(DefEqual, S);
    if EPos > 0 then
      AddStringToHashMap(Copy(S, 1, EPos - 1), Copy(S, EPos + 1,
        Length(S) - EPos))
    else
      AddStringToHashMap(Copy(S, 1, EPos - 1), '');
  end;
  List.Free;
end;

procedure TCnCustomHashLangStorage.SaveCurrentLanguage;
var
  Key, Value, aFileName: string;
  List: TStringList;
begin
  if Assigned(FHashMap) then
  begin
    List := TStringList.Create;
    FHashMap.StartEnum;
    while FHashMap.GetNext(Key, Value) do
      List.Add(Key + DefEqual + Value);
    List.Sort;

    // 设计期如果被赋值了设计期文件存储的目录，则存到此目录下
    if (csDesigning in ComponentState) and (LanguagePath = '') and (DesignLangPath <> '') then
      aFileName := IncludeTrailingBackslash(DesignLangPath) + GetCurrentLanguageFileName
    else
      aFileName := IncludeTrailingBackslash(LanguagePath) + GetCurrentLanguageFileName;

    if not ForceDirectories(ExtractFilePath(aFileName)) then
      raise ELanguageStorageError.Create(SCnCanNotCreateDir + ExtractFilePath(aFileName));

    List.SaveToFile(aFileName);
    List.Free;
  end;
end;

procedure TCnCustomHashLangStorage.SetString(Name, Value: string);
var
  myValue: string;
begin
  if Assigned(FHashMap) then
  begin
    if FHashMap.Find(Name, myValue) then
      FHashMap.Delete(Name);
    AddStringToHashMap(Name, StringReplace(Value, SCnCRLF, SCnBR, [rfReplaceAll, rfIgnoreCase]));
  end;
end;

procedure TCnCustomHashLangStorage.GetNamesList(List: TStrings);
var
  Key, Value: string;
begin
  if List <> nil then
  begin
    List.Clear;
    if Assigned(FHashMap) then
    begin
      FHashMap.StartEnum;
      while FHashMap.GetNext(Key, Value) do
        List.Add(Key);
      if List is TStringList then
        (List as TStringList).Sort;
    end;
  end;
end;

procedure TCnCustomHashLangStorage.ClearCurrentLanguage;
begin
  InitHashMap;
  SaveCurrentLanguage;
end;

class function TCnCustomHashLangStorage.GetLanguageFileExt: string;
begin
  Result := '.txt';
end;

function TCnCustomHashLangStorage.IsLanguageFile(
  const FileName: string): Boolean;
var
  F: Text;
  S: String;
begin
  Result := False;
  try
    AssignFile(F, FileName);
    Reset(F);
  except
    Exit;
  end;
  Readln(F, S);
  CloseFile(F);

  Result := Copy(S, 1, Length(SystemNamePrefix + SCnLanguageID)) =
    SystemNamePrefix + SCnLanguageID;
end;

procedure TCnCustomHashLangStorage.InitHashMap;
begin
  if Assigned(FHashMap) then
    FreeAndNil(FHashMap);
  FHashMap := TCnStrToStrHashMap.Create(FListLength, FIncSize);
end;

procedure TCnCustomHashLangStorage.SetIncSize(const Value: Integer);
begin
  if Value > 0 then
    FIncSize := Value;
end;

procedure TCnCustomHashLangStorage.SetListLength(const Value: Integer);
begin
  if Value > 0 then
    FListLength := Value;
end;

function TCnCustomHashLangStorage.CreateIterator: ICnLangStringIterator;
begin
  Result := TCnHashStringIterator.Create(Self);
end;

procedure TCnCustomHashLangStorage.InitFromAFile(const AFileName: string);
var
  F: Text;
  List: TStrings;
  I: Integer;
  S: string;
begin
  List := TStringList.Create;

  with Languages.Add do
  begin
    LanguageFileName := ExtractFileName(ChangeFileExt(AFileName, ''));
    AssignFile(F, AFileName); Reset(F); List.Clear;

    for I := 0 to 4 do
    begin
      Readln(F, S);
      List.Add(S);
    end;
    Close(F);

    try
      LanguageID := StrToIntDef(List.Values[SystemNamePrefix + SCnLanguageID], 0);
    except
      LanguageID := 0;
    end;

    if LanguageID <> 0 then
    begin
      LanguageName := List.Values[SystemNamePrefix + SCnLanguageName];
      Author := List.Values[SystemNamePrefix + SCnAuthor];
      AuthorEmail := List.Values[SystemNamePrefix + SCnAuthorEmail];
      if List.Values[SystemNamePrefix + SCnDefaultFont] <> '' then
        StringToFont(List.Values[SystemNamePrefix + SCnDefaultFont], DefaultFont);
    end
    else
    begin
      Self.FCurrentLanguageIndex := -1;
      Self.Languages.Delete(Index);
    end;
  end;
  List.Free;
end;

procedure TCnCustomHashLangStorage.GetComponentInfo(var AName, Author,
  Email, Comment: string);
begin
  AName := SCnHashLangStorageName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnHashLangStorageComment;
end;

procedure TCnCustomHashLangStorage.AddStringToHashMap(const Key,
  Value: string);
begin
  FHashMap.Add(Key, Value);
end;

{ TCnHashStringIterator }

constructor TCnHashStringIterator.Create(
  AHashStorage: TCnCustomHashLangStorage);
begin
  inherited Create;
  FHashStorage := AHashStorage;
end;

destructor TCnHashStringIterator.Destroy;
begin
  inherited;

end;

procedure TCnHashStringIterator.EndIterate;
begin
// Do Almost NOTHING.
  FKey := '';
  FValue := '';
end;

function TCnHashStringIterator.GetBof: Boolean;
begin
  Result := FBof;
end;

procedure TCnHashStringIterator.GetCurrentKeyValue(var Key, Value: string);
begin
  Key := FKey;
  Value := FValue;
end;

function TCnHashStringIterator.GetCurrentString: string;
begin
  Result := FKey + DefEqual + FValue;
end;

function TCnHashStringIterator.GetEof: Boolean;
begin
  Result := FEof;
end;

procedure TCnHashStringIterator.Next;
begin
  if FHashStorage.FHashMap <> nil then
  begin
    repeat
      FEof := not FHashStorage.FHashMap.GetNext(FKey, FValue);
    until FEof or (FFrontPattern = '') or (Pos(FFrontPattern, FKey) = 1);
  end
  else
    FEof := True;
end;

procedure TCnHashStringIterator.Previous;
begin
  raise ELanguageStorageError.Create('Previous operation NOT supported.');
end;

procedure TCnHashStringIterator.StartIterate(const FrontPattern: string);
begin
  Assert(FHashStorage <> nil);

  if FHashStorage.FHashMap <> nil then
  begin
    FHashStorage.FHashMap.StartEnum;
    FFrontPattern := FrontPattern;
    repeat
      FEof := not FHashStorage.FHashMap.GetNext(FKey, FValue);
    until FEof or (FFrontPattern = '') or (Pos(FFrontPattern, FKey) = 1);
    FBof := FEof;
  end;
end;

end.
