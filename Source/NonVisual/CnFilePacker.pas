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

unit CnFilePacker;
{* |<PRE>
================================================================================
* 软件名称：开发包不可视组件库
* 单元名称：文件目录打包组件实现单元
* 单元作者：CnPack开发组 子旻
* 备    注：
* 开发平台：PWinXP + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2011.09.04 V0.03
*               修正一处根目录结构错误的问题。
*           2009.07.08 V0.02
*               修正一处指针释放问题，增加对 D2009 的支持。
*           2008.06.27 V0.01
*               创建单元（整理而来）
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Windows, CnConsts, CnCompConsts, CnClasses, CnCommon;

const
  SCnIncCounts = 20;
  SFileNameError = 'Destination FileName is Empty.';

type

//文件结构
//PPackHeader |PPackDir| （TDataBlock|data|，...）

// 压缩模式
  TCompressMode = (cmNONE, cmCustom, cmZIP, cmRAR);

  TBytes = array of Byte;

//------------------------------------------------------------------------------
// 文件头
//------------------------------------------------------------------------------
  PPackHeader = ^TPackHeader;

  TPackHeader = record
    ZipName: array[0..7] of AnsiChar;      //= ('cnpacker');
    FileInfoCount: Cardinal;
    Compress: TCompressMode;
    FileSize: Int64;
  end;

//------------------------------------------------------------------------------
// 文件信息
//------------------------------------------------------------------------------
  PPackFileInformation = ^TPackFileInformation;

  TPackFileInformation = record
    Name: array[0..255] of AnsiChar;
    DataStart: Cardinal;
  end;

  TArrayPackFileInformation = array of TPackFileInformation;

//------------------------------------------------------------------------------
// 数据头
//------------------------------------------------------------------------------
  TDataBlock = record
    FileName: array[0..255] of AnsiChar;
  //MD5:TMD5Digest;
    DataLength: Cardinal;
  end;

//------------------------------------------------------------------------------
// 文件描述元
//------------------------------------------------------------------------------
  TFileCell = record
    ReadFileName: string;
    ConvertFileName: string;
  end;

  TFileCells = array of TFileCell;

   // 压缩接口
  ICnCompress = interface
    ['{F2379CD7-824B-4D8A-89C3-D897BF95F34C}']
    function GetCompressMode: TCompressMode;
    procedure DoCompressData(var AStream: TBytes; var ALength: Cardinal);
    procedure DoDeCompressData(var AStream: TBytes; var ALength: Cardinal);
  end;

{ TCnFilePacker }

  ECnFilePackerException = class(Exception)
  end;

  TCnFilePacker = class(TCnComponent)
  private
    {*文件头}
    FPackHeaderInfo: PPackHeader;
    {*打包文件的文件信息}
    FPackFileInformations: TArrayPackFileInformation;
    {*供外部使用的文件信息}
    FImportPackFileInfo: TArrayPackFileInformation;
    {*供外部使用的文件目录信息}
    FImprotPackDirectoryInfo: TArrayPackFileInformation;
    {*标志是否创建了保存主目录，即解包文件的目录}
    FCreateSavePath: Boolean;
    {*压缩模式}
    FCompressMode: TCompressMode;
    {*是否压缩}
    FCompress: Boolean;
    {*是否包含子目录}
    FPackedSubDirectory: Boolean;
    {*形成文件列表}
    FFiles: TFileCells;
    {*文件信息的当前数量，总数量}
    FCurrent, FCount: Cardinal;
    {*文件信息的数量，打包时=fcurrent，解包时从包中得到的}
    FFileinfoCount: Cardinal;
    {*fDestFilename是打包后的文件的文件路径}
    {*FSavePath是解包后存放的目录}
    FDestFileName, FSavePath: string;

    {*标志是否使用addfile函数增加了文件}
    FAddFilesCount: integer;
    {*传入的自定义压缩类}
    FCompressInterface: ICnCompress;
    {* 属性字段用到的函数，前边加prop区别}
    function GetPropGetPackFileDirectoryInfo: TArrayPackFileInformation;
    function GetPropGetPackFileInformation: TArrayPackFileInformation;
    function GetPropGetPackHeader: TPackHeader;
    {*压缩数据函数}
    procedure CompressData(var AStream: TBytes; var ALength: Cardinal);
    {*解压缩数据函数}
    procedure DeCompressData(var AStream: TBytes; var ALength: Cardinal);
  protected
    FPack, FDestFile: TFileStream;
    procedure CheckFileCellsCounts;
    {*如果需要压缩，不适用压缩接口的话，重载这两个虚函数！}
    procedure DoCompressData(var AStream: TBytes; var ALength: Cardinal); virtual;
    procedure DoDeCompressData(var AStream: TBytes; var ALength: Cardinal); virtual;
    {*得到打包文件的文件头}
    function GetPackHeader: PPackHeader;
    {*分配一块内存并得到打包文件文件的信息，由外部负责释放}
    function GetPackFileInformation: TArrayPackFileInformation;
    
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {*形成打包文件的主函数}
    procedure DoPack();
    {*存储一个文件}
    procedure SaveToFile(APackFileInfo: TPackFileInformation);
    {*存储所以文件}
    procedure SaveToFiles;
    {*创建文件目录}
    procedure CreateDirectory;
    {*添加目录}
    procedure AddDircetory(ADirName: string); overload;
    procedure AddDircetory(ARootName, ADirName: string); overload;
    {*添加文件}
    procedure AddFile(ADirName, AFileName: string); overload;
    procedure AddFile(AFileName: string); overload;
    {*添加压缩类}
    procedure AddCompressClass(ACompressClass: TInterfacedClass);
    {*得到文件信息}
    property PackFileInformation: TArrayPackFileInformation read GetPropGetPackFileInformation;
    property PackFileDirectoryInfo: TArrayPackFileInformation read GetPropGetPackFileDirectoryInfo;
  published
    property DestFileName: string read FDestFileName write FDestFileName;
    property SavePath: string read FSavePath write FSavePath;
    property PackedSubDirectory: Boolean read FPackedSubDirectory write FPackedSubDirectory;
    property Compress: Boolean read FCompress write FCompress;
    property CompressMode: TCompressMode read FCompressMode write FCompressMode;
    {*得到文件头信息}
    property PackHeaderInformation: TPackHeader read GetPropGetPackHeader;
  end;

implementation

//以最后一个'\' 为界得到后面部分
function GetFileName(AFileName: string): string;
var
  Len, i: Cardinal;
begin
  Len := Length(AFileName);
  for I := Len - 1 downto 1 do
    if AFileName[i] = '\' then
      Break;
  Result := Copy(AFileName, i + 1, Len - i);
end;

procedure Check(var ADirName: string);
begin
  if ADirName[Length(ADirName)] <> '\' then
    ADirName := ADirName + '\';
end;

{ TCnFilePacker }

procedure TCnFilePacker.AddCompressClass(ACompressClass: TInterfacedClass);
begin
  FCompressInterface := ACompressClass.Create as ICnCompress;
end;

procedure TCnFilePacker.AddDircetory(ARootName, ADirName: string);
var
  CurrentDirectory, LastNameofCurrentDirectory: string;
  //递归目录，形成文件列表

  procedure FindFile(ADirName: string);
  var
    SRec: TSearchRec;
    tmpCurrentDirectory, tmpLastNameofCurrentDirectory: string;     //保存当前目录层递归没有退栈，nnd
  begin
    if FindFirst(ADirName, faAnyFile, SRec) = 0 then
    begin
      repeat
        CheckFileCellsCounts;
        if (SRec.Name = '.') or (SRec.Name = '..') then
          Continue;
          
        if (SRec.Attr and faDirectory) <> 0 then
        begin
          FFiles[FCurrent].ReadFileName := CurrentDirectory + SRec.Name + '\' + IntToStr(SRec.Attr) + '?';
          FFiles[FCurrent].ConvertFileName := ARootName + LastNameofCurrentDirectory + SRec.Name + '\' + IntToStr(SRec.Attr) + '?';
          Inc(FCurrent);
          
          if FPackedSubDirectory then
          begin
            tmpLastNameofCurrentDirectory := LastNameofCurrentDirectory;
            tmpCurrentDirectory := CurrentDirectory;
            LastNameofCurrentDirectory := LastNameofCurrentDirectory + SRec.Name + '\';
            CurrentDirectory := CurrentDirectory + SRec.Name + '\';
            FindFile(copy(ADirName, 1, Length(ADirName) - 3) + SRec.Name + '\*.*');
            LastNameofCurrentDirectory := tmpLastNameofCurrentDirectory;
            CurrentDirectory := tmpCurrentDirectory;
            Continue;
          end;
        end;
        FFiles[FCurrent].ReadFileName := CurrentDirectory + SRec.Name;
        FFiles[FCurrent].ConvertFileName := ARootName + LastNameofCurrentDirectory + SRec.Name;
        Inc(FCurrent);
      until FindNext(SRec) <> 0;
      SysUtils.FindClose(SRec);
    end;
  end;

begin
  CheckFileCellsCounts;
  Check(ADirName);
  if ARootName = ' ' then
    ARootName := ''
  else
    Check(ARootName);
    
  CurrentDirectory := ADirName;
  LastNameofCurrentDirectory := _CnExtractFileName(CurrentDirectory);
  
  if Length(ADirName) = 3 then //is 'xyz:\'
    LastNameofCurrentDirectory := '';
  FFiles[FCurrent].ReadFileName := ADirName + IntToStr(GetFileAttributes(PChar(ADirName))) + '?';
  FFiles[FCurrent].ConvertFileName := ARootName + IntToStr(GetFileAttributes(PChar(ADirName))) + '?';
  Inc(FCurrent);
  ADirName := ADirName + '*.*';
  FindFile(ADirName);
end;

procedure TCnFilePacker.AddDircetory(ADirName: string);
begin
  AddDircetory(' ', ADirName);
end;

procedure TCnFilePacker.AddFile(AFileName: string);
begin
  CheckFileCellsCounts;
  FFiles[FCurrent].ReadFileName := '?';
  FFiles[FCurrent].ConvertFileName := '16' + '?';
  Inc(FCurrent);
  FFiles[FCurrent].ReadFileName := AFileName;
  FFiles[FCurrent].ConvertFileName := _CnExtractFilename(AFileName);
  Inc(FCurrent);
end;

procedure TCnFilePacker.AddFile(ADirName, AFileName: string);
begin
  CheckFileCellsCounts;
  check(ADirName);
  FFiles[FCurrent].ReadFileName := ADirName + '?';
  FFiles[FCurrent].ConvertFileName := ADirName + '16' + '?';
  Inc(FCurrent);
  FFiles[FCurrent].ReadFileName := AFileName;
  FFiles[FCurrent].ConvertFileName := ADirName + _CnExtractFilename(AFileName);
  Inc(FCurrent);
end;

procedure TCnFilePacker.CheckFileCellsCounts;
begin
  if FCurrent >= FCount then
  begin
    FCount := FCount + SCnIncCounts;
    SetLength(FFiles, FCount);
  end;
end;

procedure TCnFilePacker.CompressData(var AStream: TBytes; var ALength: Cardinal);
begin
  if FCompressInterface = nil then
    DoCompressData(AStream, ALength)
  else if CompressMode = FCompressInterface.GetCompressMode then
    FCompressInterface.DoCompressData(AStream, ALength);
end;

constructor TCnFilePacker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //fms := TMemoryStream.Create;
  FCompress := False;
  FPackedSubDirectory := true;
  FAddFilesCount := -1;
  FCurrent := 0;
  FCount := 20;
  SetLength(FFiles, FCount);
  FCreateSavePath := False;
end;

procedure TCnFilePacker.CreateDirectory;
var
  i: Integer;
  S, DirName: string;
  attr: Byte;
begin
  if not FCreateSavePath then
  begin
    ForceDirectories(SavePath);
    FCreateSavePath := True;
  end;
  
  for I := 0 to FFileinfoCount - 1 do
  begin
    S := {$IFDEF UNICODE}String{$ENDIF}(FPackFileInformations[i].Name);
    if Length(s) < 7 then
      Continue;     //xyz:\16?
      
    if s[Length(s)] = '?' then
    begin
      attr := StrToInt(Copy(s, Length(s) - 2, 2));
      s := Copy(s, 1, Length(s) - 3);
      DirName := SavePath + '\' + s;
      ForceDirectories(DirName);
      SetFileAttributes(PChar(dirname), attr);
    end
  end;
end;

procedure TCnFilePacker.DeCompressData(var AStream: TBytes; var ALength: Cardinal);
begin
  if FCompressInterface = nil then
    DoDeCompressData(AStream, ALength)
  else if CompressMode = FCompressInterface.GetCompressMode then
    FCompressInterface.DoDeCompressData(AStream, ALength);
end;

destructor TCnFilePacker.Destroy;
begin
  if FPackHeaderInfo <> nil then
  begin
    FreeMem(FPackHeaderInfo);
    FPackHeaderInfo := nil;
  end;
  inherited;
end;

procedure TCnFilePacker.DoCompressData(var AStream: TBytes; var ALength: Cardinal);
begin

end;

procedure TCnFilePacker.DoDeCompressData(var AStream: TBytes; var ALength: Cardinal);
begin

end;

procedure TCnFilePacker.SaveToFile(APackFileInfo: TPackFileInformation);
var
  f: TFileStream;   //临时文件流，保存文件
  db: TDataBlock;
  Tdb: TBytes;   //临时缓冲区，存中间数据
  S: string;
begin
  S := {$IFDEF UNICODE}String{$ENDIF}(APackFileInfo.Name);
  if (s = '') or (s[Length(s)] = '?') then
    Exit;
  
  try
    FDestFile := TFileStream.Create(DestFileName, fmOpenReadWrite);
    if fSavePath[length(fSavePath)] = '\' then
      SetLength(fSavePath, Length(fSavePath) - 1);
    FDestFile.Position := APackFileInfo.DataStart;
    FDestFile.Read(db, SizeOf(db));
    
    if db.DataLength <> 0 then
    begin
      SetLength(Tdb, db.DataLength);
      FDestFile.Read(Tdb[0], db.DataLength);
      f := TFileStream.Create(SavePath + '\' + S, fmCreate or fmOpenReadWrite);
      if CompressMode <> cmNONE then
        DeCompressData(Tdb, db.DataLength);
      f.Write(tdb[0], db.DataLength);
      f.Free;
    end
    else
    begin
      f := TFileStream.Create(SavePath + '\' + S, fmCreate or fmOpenReadWrite);
      f.Free;
    end;
  finally
    FreeAndNil(FDestFile);
  end;
end;

function TCnFilePacker.GetPackFileInformation: TArrayPackFileInformation;
var
  i: Integer;
  db: TDataBlock;
  fms: TFileStream;  //临时文件流
begin
  if FPackHeaderInfo <> nil then
  begin
    FreeMem(FPackHeaderInfo);
    FPackHeaderInfo := nil;
  end;

  FPackHeaderInfo := GetPackHeader;
  CompressMode := FPackHeaderInfo^.Compress;
  if FPackHeaderInfo^.ZipName <> 'CNPACKER' then//文件头不是cnpacker，退出
    Exit;
  Fms := TFileStream.Create(DestFileName, fmOpenRead);
  Fms.Position := SizeOf(TpackHeader);
  SetLength(Result, FPackHeaderInfo^.FileInfoCount);
  FFileinfoCount := FPackHeaderInfo^.FileInfoCount;
  
  for I := 0 to FPackHeaderInfo^.FileInfoCount - 1 do
  begin
    Fms.Read(db, SizeOf(db));
    StrCopy(Result[i].Name, db.FileName);
    Result[i].DataStart := Fms.Position - SizeOf(db);
    Fms.Position := Fms.Position + LongInt(db.DataLength);
  end;
  Fms.Free;
end;

function TCnFilePacker.GetPackHeader: PPackHeader;
var
  fms: TFileStream;
begin
  GetMem(Result, SizeOf(TPackHeader));
  Fms := TFileStream.Create(DestFileName, fmOpenRead);
  Fms.Position := 0;
  Fms.Read(Result^, SizeOf(TPackHeader));
  FreeAndNil(fms);
end;

procedure TCnFilePacker.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnFilePackerName;
  Author := SCnPack_ZiMin;
  Email := SCnPack_ZiMinEmail;
  Comment := SCnFilePackerComment;
end;

function TCnFilePacker.GetPropGetPackFileDirectoryInfo: TArrayPackFileInformation;
var
  i: Cardinal;
  S: string;
  count, current: Cardinal;
begin
  count := SCnIncCounts;
  current := 0;
  SetLength(FImprotPackDirectoryInfo, count);
  FPackFileInformations := GetPackFileInformation;
  
  for I := 0 to FFileinfoCount - 1 do
  begin
    S := {$IFDEF UNICODE}String{$ENDIF}(FPackFileInformations[i].Name);
    if S[Length(s)] = '?' then
    begin
      S := IncludeTrailingBackslash(_CnExtractFilePath(S));

      if current = count then
      begin
        count := count + SCnIncCounts;
        SetLength(FImprotPackDirectoryInfo, count);
      end;
      
      StrPCopy(FImprotPackDirectoryInfo[current].Name, {$IFDEF UNICODE}AnsiString{$ENDIF}(S));
      FImprotPackDirectoryInfo[current].DataStart := FPackFileInformations[i].DataStart;
      Inc(current);
    end;
  end;
  SetLength(FImprotPackDirectoryInfo, current);
  Result := FImprotPackDirectoryInfo;
end;

function TCnFilePacker.GetPropGetPackFileInformation: TArrayPackFileInformation;
var
  i: Cardinal;
  S: string;
  count, current: Cardinal;
begin
  count := SCnIncCounts;
  current := 0;
  SetLength(FImportPackFileInfo, count);
  FPackFileInformations := GetPackFileInformation;
  
  for I := 0 to FFileinfoCount - 1 do
  begin
    S := {$IFDEF UNICODE}String{$ENDIF}(FPackFileInformations[i].Name);
    if S[Length(s)] <> '?' then
    begin
      if current = count then
      begin
        count := count + SCnIncCounts;
        SetLength(FImportPackFileInfo, count);
      end;
      
      FImportPackFileInfo[current].Name := FPackFileInformations[i].Name;
      FImportPackFileInfo[current].DataStart := FPackFileInformations[i].DataStart;
      Inc(current);
    end;
  end;
  
  SetLength(FImportPackFileInfo, current);
  Result := FImportPackFileInfo;
end;

function TCnFilePacker.GetPropGetPackHeader: TPackHeader;
begin
  if FPackHeaderInfo <> nil then
  begin
    FreeMem(FPackHeaderInfo);
    FPackHeaderInfo := nil;
  end;
  FPackHeaderInfo := GetPackHeader;
  Result := FPackHeaderInfo^;
end;

procedure TCnFilePacker.DoPack();
var
  ph: TPackHeader;
  db: TDataBlock;
  i: Integer;
  Tdb: TBytes;
  f: TFileStream;
begin
  FillChar(ph, SizeOf(Tpackheader), #0);
  if DestFileName = '' then
    ECnFilePackerException.Create(SFileNameError);

  if not FileExists(DestFileName) then
  begin
    FPack := TFileStream.Create(DestFileName, fmCreate);
    FPack.Position := 0;
        //步过文件头
    FPack.Seek(SizeOf(TPackHeader), soFromCurrent);
  end
  else
  begin
    FPack := TFileStream.Create(DestFileName, fmOpenReadWrite);
    FPack.Read(ph, SizeOf(ph));
    FPack.Position := FPack.Size;
  end;
  
   //循环all文件
  for I := 0 to FCurrent - 1 do
  begin
    if FFiles[i].ReadFileName[Length(FFiles[i].ReadFileName)] = '?' then
    begin
      strpcopy(db.FileName, {$IFDEF UNICODE}AnsiString{$ENDIF}(Ffiles[i].ConvertFileName));
      db.DataLength := 0;
      FPack.Write(db, SizeOf(db));
    end
    else
    begin
      f := TFileStream.Create(FFiles[i].ReadFileName, fmOpenRead);
      strpcopy(db.FileName, {$IFDEF UNICODE}AnsiString{$ENDIF}(Ffiles[i].ConvertFileName));
      db.DataLength := F.Size;
      if db.DataLength <> 0 then
      begin
        SetLength(Tdb, db.DataLength);
        f.Read(Tdb[0], db.DataLength);
        if CompressMode <> cmNONE then
          CompressData(tdb, db.DataLength);
        FPack.Write(db, SizeOf(db));
        FPack.Write(tdb[0], db.DataLength);
        FreeAndNil(f);
      end
      else
      begin
        FPack.Write(db, SizeOf(db));
        FreeAndNil(f);
      end;
    end;
  end;
  
  //写文件头
  ph.ZipName := 'CNPACKER';
  ph.Compress := CompressMode;
  ph.FileSize := FPack.Size;
  Inc(ph.FileInfoCount, FCurrent);
  FPack.Position := 0;
  FPack.Write(ph, SizeOf(ph));
  FreeAndNil(FPack);
  FCurrent := 0;
  FCount := 20;
  SetLength(Ffiles, FCount);
end;

procedure TCnFilePacker.SaveToFiles;
var
  i: integer;
begin
  if FPackFileInformations = nil then
    FPackFileInformations := self.GetPackFileInformation;      //先得到目录，
  Self.CreateDirectory;     //创建目录
  for I := 0 to Length(FPackFileInformations) - 1 do
  begin
    Self.SaveToFile(FPackFileInformations[i]);      //枚举调用解包每个文件
  end;
  FreeAndNil(FDestFile);
end;

end.
