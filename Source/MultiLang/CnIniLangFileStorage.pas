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

unit CnIniLangFileStorage;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：Ini 多语存储组件单元
* 单元作者：CnPack开发组
* 备    注：该单元实现了 Ini 多语存储组件类，内部使用了 HashMap
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.03.30 V1.2
*               内部改用宽字符串版 IniFile 类以实现 Utf8 存储加载支持
*           2008.11.19 V1.1
*               Efeis 修正 Ini 载入出错的问题
*           2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Dialogs, FileCtrl, CnCommon, CnLangCollection,
  CnConsts, CnIni, CnIniStrUtils, CnWideStrings, CnLangStorage, CnHashLangStorage;

const
  SCnGlobalSectionName = SystemNamePrefix + 'Global';
  SCnStringsSectionName = SystemNamePrefix + 'Strings';

type
  TCnCustomIniLangFileStorage = class(TCnCustomHashLangStorage)
  private
    FIniFile: TCnWideIniFile;
    FCompatAnsiBoundary: Boolean;
  protected
    function IniUtf8ToCompat(const S: string): string;
    function CompatToIniUtf8(const S: string): string;
    procedure InternalInit; override;
    procedure CreateCurrentLanguage; override;
    procedure InitFromAFile(const AFileName: TCnLangString); override;
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageFileExt: TCnLangString; override;
    {* 返回多语言文件的扩展名.INI }

    function IsLanguageFile(const FileName: TCnLangString): Boolean; override;
    {* 判断一文件是否合法的语言文件 }
    function LoadCurrentLanguage: Boolean; override;
    {* 从 Ini 文件中载入当前语言条目，为翻译字串做准备 }
    procedure SaveCurrentLanguage; override;
    {* 保存当前语言文件 }

  published
    property StorageMode;
    {* 多语文件存储模式，按目录存储还是按文件存储 }
    property LanguagePath;
    {* 语言文件存储的统一路径 }
    property FileName;
    {* 按文件存储时的统一多语文件名 }
    property Languages;
    {* 语言对象列表 }
    property AutoDetect;
    {* LanguagePath 改变时是否自动检测语言 }    
    property CompatAnsiBoundary: Boolean read FCompatAnsiBoundary write FCompatAnsiBoundary default True;
    {* 仅在非 Unicode 编译器下生效，控制与外部/界面的字符串边界兼容行为。
       True：按旧版兼容方式处理，读取时将 UTF-8 转为本地 Ansi，保存时将本地 Ansi 转回 UTF-8。
       False：不做兼容转换，边界字符串按 UTF-8 字节串直接传递，FPC 下有用。
       在 Unicode 编译器下该属性无效，始终按 UnicodeString 处理。}
  end;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnIniLangFileStorage = class(TCnCustomIniLangFileStorage)
  end;

implementation

uses
  CnLangConsts;

{ TCnCustomIniLangFileStorage}

constructor TCnCustomIniLangFileStorage.Create(AOwner: TComponent);
begin
  inherited;
  FCompatAnsiBoundary := True;
end;

function TCnCustomIniLangFileStorage.IniUtf8ToCompat(const S: string): string;
begin
{$IFDEF UNICODE}
  Result := S;
{$ELSE}
  if FCompatAnsiBoundary then
    Result := string(CnUtf8DecodeToWideString(AnsiString(S)))
  else
    Result := S;
{$ENDIF}
end;

function TCnCustomIniLangFileStorage.CompatToIniUtf8(const S: string): string;
begin
{$IFDEF UNICODE}
  Result := S;
{$ELSE}
  if FCompatAnsiBoundary then
    Result := string(CnUtf8EncodeWideString(WideString(S)))
  else
    Result := S;
{$ENDIF}
end;

destructor TCnCustomIniLangFileStorage.Destroy;
begin
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  inherited;
end;

procedure TCnCustomIniLangFileStorage.CreateCurrentLanguage;
begin

end;

class function TCnCustomIniLangFileStorage.GetLanguageFileExt: TCnLangString;
begin
  Result := '.ini';
end;

procedure TCnCustomIniLangFileStorage.InternalInit;
begin
  inherited;

end;

function TCnCustomIniLangFileStorage.IsLanguageFile(const FileName: TCnLangString): Boolean;
var
  IniFile: TCnWideIniFile;
begin
  Result := True;
  IniFile := TCnWideIniFile.Create(FileName);
  try
    if not IniFile.SectionExists(SCnGlobalSectionName) then
      Result := False
    else if not IniFile.ValueExists(SCnGlobalSectionName, SystemNamePrefix
      + SCnLanguageID) then
        Result := False;
  finally
    IniFile.Free;
  end;
end;

function TCnCustomIniLangFileStorage.LoadCurrentLanguage: Boolean;
var
  S, OrigSection, SectionName, OrigKey, KeyName: string;
  Sections, Lines: TStrings;
  I, J: Integer;
begin
  Result := True;
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);

  // Added by Efeis on 2008-11-18 没有这行会出错，在父类中的过程有这行，应该需要初始化一下的
  InitHashMap;

  try
    // 设计期如果被赋值了设计期文件存储的目录，则存到此目录下
    if (csDesigning in ComponentState) and (LanguagePath = '') and (DesignLangPath <> '') then
      S := IncludeTrailingBackslash(DesignLangPath) + GetCurrentLanguageFileName
    else
      S := IncludeTrailingBackslash(LanguagePath) + GetCurrentLanguageFileName;

    if not ForceDirectories(_CnExtractFilePath(S)) then
      raise ELanguageStorageError.Create(SCnCanNotCreateDir + _CnExtractFilePath(S));

    FIniFile := TCnWideIniFile.Create(S);
    Sections := TStringList.Create;
    Lines := TStringList.Create;
    FIniFile.ReadSections(Sections);

    try
      for I := 0 to Sections.Count - 1 do
      begin
        OrigSection := Sections[I];
        SectionName := IniUtf8ToCompat(OrigSection);
        if (SectionName = SCnGlobalSectionName) or
          (SectionName = SCnStringsSectionName) then // 是内部保留区或常量区
        begin
          FIniFile.ReadSection(OrigSection, Lines);
          for J := 0 to Lines.Count - 1 do
          begin
            OrigKey := Lines[J];
            KeyName := IniUtf8ToCompat(OrigKey);
            AddString(KeyName, IniUtf8ToCompat(FIniFile.ReadString(OrigSection, OrigKey, '')));
          end;
        end
        else // 是普通窗体的
        begin
          FIniFile.ReadSection(OrigSection, Lines);
          for J := 0 to Lines.Count - 1 do
          begin
            OrigKey := Lines[J];
            KeyName := IniUtf8ToCompat(OrigKey);
            AddString(SectionName + DefDelimeter + KeyName,
              IniUtf8ToCompat(FIniFile.ReadString(OrigSection, OrigKey, '')));
          end;
        end;
      end;
    finally
      Sections.Free;
      Lines.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TCnCustomIniLangFileStorage.SaveCurrentLanguage;
var
  Sections, List: TStringList;
  Key, Value, Sec: TCnLangString;
  IniSec, IniKey, IniValue: string;
  I, EPos: Integer;
begin
  if Assigned(FIniFile) then
  begin
    Sections := TStringList.Create;
    try
      FIniFile.ReadSections(Sections);
      for I := 0 to Sections.Count - 1 do
        FIniFile.EraseSection(Sections[I]);
    finally
      Sections.Free;
    end;

    List := TStringList.Create;
    HashMap.StartEnum;
    while HashMap.GetNext(Key, Value) do
      List.Add(Key + DefEqual + Value);
    List.Sort;

    for I := 0 to List.Count - 1 do
    begin
      if Pos(SystemNamePrefix, List[I]) = 1 then // 判断第一个是不是感叹号
      begin
        Sec := SCnGlobalSectionName;
      end
      else  // 再判断有无点号
      begin
        EPos := Pos(DefDelimeter, List[I]);
        if EPos > 0 then  // 有点号，取出第一个点号前的做 Section 名字
        begin
          Sec := Copy(List[I], 1, EPos - 1);
          List[I] := Copy(List[I], EPos + 1, MaxInt);
        end
        else
          Sec := SCnStringsSectionName; // 无点号，是字符串
      end;

      // 拆分
      EPos := Pos(DefEqual, List[I]);
      if EPos > 0 then // 有等号
      begin
        Key := Copy(List[I], 1, EPos - 1);
        Value := Copy(List[I], EPos + 1, MaxInt);
      end
      else
      begin
        Key := List[I];
        Value := '';
      end;
      IniSec := CompatToIniUtf8(Sec);
      IniKey := CompatToIniUtf8(Key);
      IniValue := CompatToIniUtf8(Value);
      FIniFile.WriteString(IniSec, IniKey, IniValue);
    end;
    FIniFile.UpdateFile;
  end;
end;

procedure TCnCustomIniLangFileStorage.InitFromAFile(const AFileName: TCnLangString);
begin
  // 从一语言文件读入语言内容
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);

  with Languages.Add do
  begin
    LanguageFileName := _CnExtractFileName(_CnChangeFileExt(AFileName, ''));

    FIniFile := TCnWideIniFile.Create(AFileName);
    try
      LanguageID := StrToIntDef(FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnLanguageID, ''), 0);
    except
      LanguageID := 0;
    end;

    if LanguageID <> 0 then
    begin
      LanguageName := IniUtf8ToCompat(FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnLanguageName, ''));
      Author := IniUtf8ToCompat(FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnAuthor, ''));
      AuthorEmail := IniUtf8ToCompat(FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnAuthorEmail, ''));
      if IniUtf8ToCompat(FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnDefaultFont, '')) <> '' then
        StringToFont(IniUtf8ToCompat(FIniFile.ReadString(SCnGlobalSectionName,
          SystemNamePrefix + SCnDefaultFont, '')), DefaultFont);
    end
    else
    begin
      Self.FCurrentLanguageIndex := -1;
      Languages.Delete(Index);
    end;
  end;
end;

procedure TCnCustomIniLangFileStorage.GetComponentInfo(var AName, Author,
  Email, Comment: string);
begin
  AName := SCnIniLangStorageName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnIniLangStorageComment;
end;

end.
