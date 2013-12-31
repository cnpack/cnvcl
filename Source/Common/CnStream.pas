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

unit CnStream;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：扩展的 Stream 类单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2009.04.03 V1.2
*               Xor 加密流修正加密过程中字符串读写位置可能不对的问题
*           2003.03.02 V1.1
*               Xor 加密流改为字符串做加密值
*           2002.10.28 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

type

//==============================================================================
// 扩展的 TStream 类
//==============================================================================
   
{ TCnStream }

  ECnStreamError = class(EStreamError);
  {* 流操作异常}
  ECnReadStreamError = class(ECnStreamError);
  {* 从流中读取数据出错}
  ECnWriteStreamError = class(ECnStreamError);
  {* 写数据到流出错}

  TCnStreamDataType = (dtInteger, dtBool, dtDateTime, dtDouble, dtString, dtData);

  TCnStream = class (TStream)
  {* 扩展的流操作类，继承自 TStream，通过包装其它的流来实现功能扩展。主要用来
     保存一些基本的数据类型。}
  private
    FStream: TStream;
    FOwned: Boolean;
    function GetHandle: Integer;
    function GetMemory: Pointer;
  protected
    class procedure ReadError;
    class procedure WriteError;
    procedure WriteDataType(DataType: TCnStreamDataType);
    function ReadDataType: TCnStreamDataType;
    procedure DoRead(var Buffer; Count: Longint);
    procedure DoWrite(const Buffer; Count: Longint);

  {$IFDEF COMPILER7_UP}
    function GetSize: Int64; override;
  {$ENDIF}
    procedure SetSize(NewSize: Longint); override;
  {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); overload; override;
  {$ENDIF}

    property Owned: Boolean read FOwned;
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream; AOwned: Boolean = False); overload;
    constructor Create; overload;
    constructor Create(const FileName: string; Mode: Word); overload;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure BeginRead(Flag: Longint = -1);
    procedure EndRead(Flag: Longint = -1);
    procedure BeginWrite(Flag: Longint = -1);
    procedure EndWrite(Flag: Longint = -1);
    
    function ReadInteger: Longint;
    function ReadBool: Boolean;
    function ReadDateTime: TDateTime;
    function ReadFloat: Double;
    function ReadString: string;
    procedure ReadData(var Buffer; Count: Longint);

    procedure WriteInteger(Value: Longint);
    procedure WriteBool(Value: Boolean);
    procedure WriteDateTime(Value: TDateTime);
    procedure WriteFloat(Value: Double);
    procedure WriteString(Value: string);
    procedure WriteData(const Buffer; Count: Longint);

    property Memory: Pointer read GetMemory;
    property Handle: Integer read GetHandle;
  end;

//==============================================================================
// 加密的 TStream 抽象基类
//==============================================================================

{ TCnEncryptStream }

  TCnEncryptStream = class (TStream)
  {* 加密的 TStream 抽象基类，支持数据读写时进行加密处理。}
  private
    FStream: TStream;
    FOwned: Boolean;
  protected
    procedure DeEncrypt(var Buffer; Count: Longint); virtual; abstract;
    {* 解密方法，抽象方法。}
    procedure Encrypt(var Buffer; Count: Longint); virtual; abstract;
    {* 加密方法，抽象方法。}

    procedure DoBeforeEncrypt(const Buffer; Count: Longint); virtual;
    procedure DoAfterEncrypt(const Buffer; Count: Longint); virtual;
    procedure DoBeforeDeEncrypt(const Buffer; Count: Longint); virtual;
    procedure DoAfterDeEncrypt(const Buffer; Count: Longint); virtual;

  {$IFDEF COMPILER7_UP}
    function GetSize: Int64; override;
  {$ENDIF}
    procedure SetSize(NewSize: Longint); override;
  {$IFDEF COMPILER6_UP}
    procedure SetSize(const NewSize: Int64); overload; override;
  {$ENDIF}
  
    property Owned: Boolean read FOwned;
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream; AOwned: Boolean = False);
    {* 类构造器，AStream 参数为需要进行加密处理的流，AOwned 表示是否
       在释放加密流时同时释放 AStream。}
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): LongInt; override;
    function Seek(Offset: Longint; Origin: Word): LongInt; override;
    function Write(const Buffer; Count: Longint): LongInt; override;
  end;
  
//==============================================================================
// Xor 方式加密的 TStream 类
//==============================================================================
   
{ TCnXorStream }

  TCnXorStream = class (TCnEncryptStream)
  {* Xor 加密的 TStream 类，支持数据读写时进行 Xor 加密处理。}
  private
    FXorStr: AnsiString;
    FSeedPos: Integer;
  protected
    procedure DeEncrypt(var Buffer; Count: Longint); override;
    procedure Encrypt(var Buffer; Count: Longint); override;

    procedure DoBeforeEncrypt(const Buffer; Count: Longint); override;
    procedure DoAfterEncrypt(const Buffer; Count: Longint); override;
    procedure DoBeforeDeEncrypt(const Buffer; Count: Longint); override;
    procedure DoAfterDeEncrypt(const Buffer; Count: Longint); override;
  public
    constructor Create(AStream: TStream; const AXorStr: AnsiString;
      AOwned: Boolean = False);
    {* 类构造器
     |<PRE>
       AStream: TStream         - 需要进行加密处理的流
       AXorStr: string          - 用于加密处理的字符串
       AOwned: Boolean          - 是否在释放加密流时同时释放 AStream
     |</PRE>}
    property XorStr: AnsiString read FXorStr write FXorStr;
    {* 用于加密处理的字符串 }
  end;

//==============================================================================
// 字符映射表方式加密的 TStream 类
//==============================================================================

{ TCnCodeMapStream }

  TCnCodeMap = array[Byte] of Byte;

  TCnCodeMapStream = class (TCnEncryptStream)
  {* 字符映射表方式加密的 TStream 类，支持数据读写时进行加密处理。}
  private
    FSeedStr: AnsiString;
    FEnMap: TCnCodeMap;
    FDeMap: TCnCodeMap;
    procedure SetSeedStr(const Value: AnsiString);
  protected
    procedure DeEncrypt(var Buffer; Count: Longint); override;
    procedure Encrypt(var Buffer; Count: Longint); override;
  public
    constructor Create(AStream: TStream; const ASeedStr: AnsiString;
      AOwned: Boolean = False);
    {* 类构造器
     |<PRE>
       AStream: TStream         - 需要进行加密处理的流
       ASeedStr: string         - 用于加密处理的种子字符串
       AOwned: Boolean          - 是否在释放加密流时同时释放 AStream
     |</PRE>}
    property SeedStr: AnsiString read FSeedStr write SetSeedStr;
    {* 用于加密处理的字符串 }
  end;

function CnFastMemoryStreamCopyFrom(Dest, Source: TStream; Count: Int64): Int64;
{* 快速的 MemoryStream 的 CopyFrom 方法，用于 Dest 或 Source 之一是 MemoryStream
   的情况，直接读写内存，避免了分配缓冲区和重复读写的开销。如传入的俩 Stream 都
   不是 TCustomMemoryStream，则调用原 CopyFrom 方法。}

function CnGenerateCodeMap(ASeedStr: AnsiString; var EnMap, DeMap: TCnCodeMap): Boolean;
{* 根据一个字符串生成用于字节加解密的编码表，字符串为空时返回为 False *}

implementation

resourcestring
  SCnReadStreamError = 'Read stream error';
  SCnWriteStreamError = 'Write stream error';

const
  csBeginFlagInt = Longint($00FF00FF);
  csEndFlagInt = Longint($FF00FF00);
  csDefSeedStr = '{A53108FC-BD75-42B8-BD10-DA3DC166D0B0}';

function CnFastMemoryStreamCopyFrom(Dest, Source: TStream; Count: Int64): Int64;
var
  aNewSize: Longint;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  //注意此方法能工作的首要条件是Dest, Source两者之一是TCustomMemoryStream,否则调用原函数
  
  //判断Dest或Source是不是TCustomMemoryStream,如果是的话，直接从Memory的地址上进行操作 ,
  //因为CustomMemoryStream 的read或writer都用到System.move
  //这样省去了原TStream不停地创建和写buffer 及设置Capacity ,原copyfrom这里最浪费时间
  //用此方法可直接将Source的内存块追加到Dest的Memory地址的后面

  if Source is TCustomMemoryStream then     //直接写入到Dest的地址内
    Dest.WriteBuffer(Pointer(Longint(TCustomMemoryStream(Source).Memory) + Source.Position)^,Count)
  else if Dest is TCustomMemoryStream then
  begin
    aNewSize := Dest.Position + Count;
    TCustomMemoryStream(Dest).Size := aNewSize; //先设置内存的大小，将Dest.Memory扩大,不然下面找不到地址
    //source直接从Dest.memory后面写
    Source.ReadBuffer(Pointer(Longint(TCustomMemoryStream(Dest).Memory) + Dest.Position)^, Count);
  end
  else
  begin
    Dest.CopyFrom(Source, Count);
  end;
end;

function CnGenerateCodeMap(ASeedStr: AnsiString; var EnMap, DeMap: TCnCodeMap): Boolean;
var
  i: Integer;
  C: Byte;
  List: TList;
begin
  Result := False;
  if ASeedStr = '' then
    Exit;

  List := TList.Create;
  try
    for i := 0 to 255 do
      List.Add(Pointer(i));
    for i := 0 to 255 do
    begin
      C := Byte(ASeedStr[i mod Length(ASeedStr) + 1]) xor $3E;
      C := (C * 3 + 7) mod List.Count;
      EnMap[i] := Byte(List[C]);
      DeMap[Byte(List[C])] := i;
      List.Delete(C);
    end;
    Result := True;
  finally
    List.Free;
  end;
end;

//==============================================================================
// 扩展的 TStream 类
//==============================================================================

{ TCnStream }

constructor TCnStream.Create(AStream: TStream; AOwned: Boolean);
begin
  inherited Create;
  Assert(Assigned(AStream));
  FStream := AStream;
  FOwned := AOwned;
end;

constructor TCnStream.Create;
begin
  Create(TMemoryStream.Create, True);
end;

constructor TCnStream.Create(const FileName: string; Mode: Word);
begin
  Create(TFileStream.Create(FileName, Mode), True);
end;

destructor TCnStream.Destroy;
begin
  if FOwned then
    FreeAndNil(FStream);
  inherited;
end;

class procedure TCnStream.ReadError;
begin
  raise ECnReadStreamError.CreateRes(@SCnReadStreamError);
end;

class procedure TCnStream.WriteError;
begin
  raise ECnWriteStreamError.CreateRes(@SCnWriteStreamError);
end;

//------------------------------------------------------------------------------
// 调用被包装的 Stream 访问方法
//------------------------------------------------------------------------------

{$IFDEF COMPILER7_UP}
function TCnStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;
{$ENDIF}

function TCnStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TCnStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TCnStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

procedure TCnStream.SetSize(NewSize: Integer);
begin
  FStream.Size := NewSize;
end;

{$IFDEF COMPILER6_UP}
procedure TCnStream.SetSize(const NewSize: Int64);
begin
  FStream.Size := NewSize;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// 数据块标志操作方法
//------------------------------------------------------------------------------
   
procedure TCnStream.BeginRead(Flag: Integer);
begin
  if Flag = -1 then Flag := csBeginFlagInt;
  if ReadInteger <> Flag then ReadError;
end;

procedure TCnStream.EndRead(Flag: Integer);
begin
  if Flag = -1 then Flag := csEndFlagInt;
  if ReadInteger <> Flag then ReadError;
end;

procedure TCnStream.BeginWrite(Flag: Integer);
begin
  if Flag = -1 then Flag := csBeginFlagInt;
  WriteInteger(Flag);
end;

procedure TCnStream.EndWrite(Flag: Integer);
begin
  if Flag = -1 then Flag := csEndFlagInt;
  WriteInteger(Flag);
end;

//------------------------------------------------------------------------------
// 辅助方法
//------------------------------------------------------------------------------

procedure TCnStream.DoRead(var Buffer; Count: Integer);
begin
  if Read(Buffer, Count) <> Count then
    ReadError;
end;

procedure TCnStream.DoWrite(const Buffer; Count: Integer);
begin
  if Write(Buffer, Count) <> Count then WriteError;
end;

function TCnStream.ReadDataType: TCnStreamDataType;
begin
  DoRead(Result, SizeOf(Result));
end;

procedure TCnStream.WriteDataType(DataType: TCnStreamDataType);
begin
  DoWrite(DataType, SizeOf(DataType));
end;

//------------------------------------------------------------------------------
// 扩展的数据存取方法
//------------------------------------------------------------------------------

function TCnStream.ReadBool: Boolean;
begin
  if ReadDataType <> dtBool then ReadError;
  DoRead(Result, SizeOf(Result));
end;

procedure TCnStream.ReadData(var Buffer; Count: Integer);
var
  ACount: Integer;
begin
  if ReadDataType <> dtData then ReadError;
  DoRead(ACount, SizeOf(ACount));
  if ACount <> Count then ReadError;
  DoRead(Buffer, Count);
end;

function TCnStream.ReadDateTime: TDateTime;
begin
  if ReadDataType <> dtDateTime then ReadError;
  DoRead(Result, SizeOf(Result));
end;

function TCnStream.ReadFloat: Double;
begin
  if ReadDataType <> dtDouble then ReadError;
  DoRead(Result, SizeOf(Result));
end;

function TCnStream.ReadInteger: Longint;
begin
  if ReadDataType <> dtInteger then ReadError;
  DoRead(Result, SizeOf(Result));
end;

function TCnStream.ReadString: string;
var
  Len: Integer;
begin
  if ReadDataType <> dtString then ReadError;
  DoRead(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(Result, Len);
    DoRead(PChar(Result)^, Len);
  end
  else
    Result := '';
end;

procedure TCnStream.WriteBool(Value: Boolean);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtBool;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteData(const Buffer; Count: Integer);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtData;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Count, SizeOf(Count));
  DoWrite(Buffer, Count);
end;

procedure TCnStream.WriteDateTime(Value: TDateTime);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtDateTime;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteFloat(Value: Double);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtDouble;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteInteger(Value: Integer);
var
  DataType: TCnStreamDataType;
begin
  DataType := dtInteger;
  DoWrite(DataType, SizeOf(DataType));
  DoWrite(Value, SizeOf(Value));
end;

procedure TCnStream.WriteString(Value: string);
var
  DataType: TCnStreamDataType;
  Len: Integer;
begin
  DataType := dtString;
  DoWrite(DataType, SizeOf(DataType));
  Len := Length(Value);
  DoWrite(Len, SizeOf(Len));
  if Len > 0 then
    DoWrite(PChar(Value)^, Len);
end;

//------------------------------------------------------------------------------
// 属性读写方法
//------------------------------------------------------------------------------

function TCnStream.GetHandle: Integer;
begin
  if FStream is THandleStream then
    Result := THandleStream(FStream).Handle
  else
    Result := -1;
end;

function TCnStream.GetMemory: Pointer;
begin
  if FStream is TCustomMemoryStream then
    Result := TCustomMemoryStream(FStream).Memory
  else
    Result := nil;
end;

//==============================================================================
// 加密的 TStream 抽象基类
//==============================================================================

{ TCnEncryptStream }

constructor TCnEncryptStream.Create(AStream: TStream; AOwned: Boolean);
begin
  inherited Create;
  Assert(Assigned(AStream));
  FStream := AStream;
  FOwned := AOwned;
end;

destructor TCnEncryptStream.Destroy;
begin
  if FOwned then
    FreeAndNil(FStream);
  inherited;
end;

procedure TCnEncryptStream.DoAfterDeEncrypt(const Buffer; Count: Integer);
begin

end;

procedure TCnEncryptStream.DoAfterEncrypt(const Buffer; Count: Integer);
begin

end;

procedure TCnEncryptStream.DoBeforeDeEncrypt(const Buffer; Count: Integer);
begin

end;

procedure TCnEncryptStream.DoBeforeEncrypt(const Buffer; Count: Integer);
begin

end;

//------------------------------------------------------------------------------
// 调用被包装的 Stream 访问方法
//------------------------------------------------------------------------------

{$IFDEF COMPILER7_UP}
function TCnEncryptStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;
{$ENDIF}

function TCnEncryptStream.Read(var Buffer; Count: Longint): LongInt;
begin
  Result := FStream.Read(Buffer, Count);
  DoBeforeDeEncrypt(Buffer, Count);
  DeEncrypt(Buffer, Count);
  DoAfterDeEncrypt(Buffer, Count);
end;

function TCnEncryptStream.Seek(Offset: Longint; Origin: Word): LongInt;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TCnEncryptStream.SetSize(NewSize: Integer);
begin
  FStream.Size := NewSize;
end;

{$IFDEF COMPILER6_UP}
procedure TCnEncryptStream.SetSize(const NewSize: Int64);
begin
  FStream.Size := NewSize;
end;
{$ENDIF}

function TCnEncryptStream.Write(const Buffer; Count: Longint): LongInt;
var
  MemBuff: Pointer;
begin
  GetMem(MemBuff, Count);
  try
    DoBeforeEncrypt(Buffer, Count);
    CopyMemory(MemBuff, @Buffer, Count);
    Encrypt(MemBuff^, Count);
    DoAfterEncrypt(Buffer, Count);
    Result := FStream.Write(MemBuff^, Count);
  finally
    FreeMem(MemBuff);
  end;
end;

//==============================================================================
// Xor 方式加密的 TStream 类
//==============================================================================
   
{ TCnXorStream }

constructor TCnXorStream.Create(AStream: TStream; const AXorStr: AnsiString;
  AOwned: Boolean);
begin
  inherited Create(AStream, AOwned);
  FXorStr := AXorStr;
end;

procedure TCnXorStream.Encrypt(var Buffer; Count: Longint);
var
  i, p, l: Integer;
begin
  l := Length(FXorStr);
  if l > 0 then
  begin
    p := FSeedPos;
    for i := 0 to Count - 1 do
      PByteArray(@Buffer)^[i] := PByteArray(@Buffer)^[i] xor
        Byte(FXorStr[(p + i) mod l + 1]);
  end;
end;

procedure TCnXorStream.DeEncrypt(var Buffer; Count: Longint);
begin
  Encrypt(Buffer, Count);
end;

procedure TCnXorStream.DoAfterEncrypt(const Buffer; Count: Integer);
begin

end;

procedure TCnXorStream.DoBeforeEncrypt(const Buffer; Count: Integer);
begin
  // 读写前后需要记录位置，和流中的xor加密的种子字符位置对的上号
  FSeedPos := Position;
end;

procedure TCnXorStream.DoAfterDeEncrypt(const Buffer; Count: Integer);
begin

end;

procedure TCnXorStream.DoBeforeDeEncrypt(const Buffer; Count: Integer);
begin
  // 读写前后需要记录位置，和流中的xor加密的种子字符位置对的上号
  FSeedPos := Position - Count;
end;

//==============================================================================
// 字符映射表方式加密的 TStream 类
//==============================================================================

{ TCnCodeMapStream }

constructor TCnCodeMapStream.Create(AStream: TStream;
  const ASeedStr: AnsiString; AOwned: Boolean);
begin
  inherited Create(AStream, AOwned);
  SeedStr := ASeedStr;
end;

procedure TCnCodeMapStream.DeEncrypt(var Buffer; Count: Integer);
var
  i: Integer;
  P: PByte;
begin
  P := PByte(@Buffer);
  for i := 0 to Count - 1 do
  begin
    P^ := FDeMap[P^];
    Inc(P);
  end;
end;

procedure TCnCodeMapStream.Encrypt(var Buffer; Count: Integer);
var
  i: Integer;
  P: PByte;
begin
  P := PByte(@Buffer);
  for i := 0 to Count - 1 do
  begin
    P^ := FEnMap[P^];
    Inc(P);
  end;
end;

procedure TCnCodeMapStream.SetSeedStr(const Value: AnsiString);
begin
  FSeedStr := Value;
  if FSeedStr = '' then
    CnGenerateCodeMap(csDefSeedStr, FEnMap, FDeMap)
  else
    CnGenerateCodeMap(FSeedStr, FEnMap, FDeMap);
end;

end.
